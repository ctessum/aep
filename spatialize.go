/*
Copyright (C) 2012-2014 Regents of the University of Minnesota.
This file is part of AEP.

AEP is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

AEP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with AEP.  If not, see <http://www.gnu.org/licenses/>.
*/

package aep

import (
	"bufio"
	"encoding/csv"
	"encoding/gob"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"

	"bitbucket.org/ctessum/sparse"
	"github.com/camlistore/camlistore/pkg/lru"
	"github.com/ctessum/geom/encoding/shp"
	"github.com/ctessum/geom/proj"
)

// SpatialProcessor spatializes emissions records.
type SpatialProcessor struct {
	srgSpecs               *SrgSpecs
	grids                  []*GridDef
	gridRef                *GridRef
	surrogateGeneratorChan chan *srgRequest
	srgCache               map[string]*sparse.SparseArray

	// inputSR is the spatial reference of the input data. It will usually be
	// "+longlat".
	inputSR proj.SR

	// matchFullSCC indicates whether partial SCC matches are okay.
	matchFullSCC bool

	// report information
	totals    map[Period]*SpatialTotals
	totalGrid map[*GridDef]map[Period]map[string]*sparse.SparseArray

	// DiskCachePath specifies a directory to cache surrogate files in. If it is
	// empty or invalid, surrogates will not be stored on the disk for later use.
	DiskCachePath string

	// MemCacheSize specifies the number of surrogates to hold in the memory cache.
	// A larger number results in potentially faster performance but more memory use.
	// The default is 100.
	MemCacheSize int

	// MaxMergeDepth is the maximum number of nested merged surrogates.
	// For example, if surrogate 100 is a combination of surrogates 110 and 120,
	// and surrogate 110 is a combination of surrogates 130 and 140, then
	// MaxMergeDepth should be set to 3, because 100 depends on 110 and 110
	// depends on 130. If MaxMergeDepth is set too low, the program may hang
	// when attempting to create a merged surrogate.
	// The default value is 10.
	MaxMergeDepth int

	// SimplifyTolerance specifies the length of features up to which to remove
	// when simplifying shapefiles for spatial surrogate creation. The default is
	// 0 (i.e., no simplification). Simplifying decreases processing time and
	// memory use. The value should be in the units of the output grid
	// (e.g., meters or degrees).
	SimplifyTolerance float64
}

// NewSpatialProcessor creates a new spatial processor.
func NewSpatialProcessor(srgSpecs *SrgSpecs, grids []*GridDef, gridRef *GridRef, inputSR proj.SR, matchFullSCC bool) *SpatialProcessor {
	sp := new(SpatialProcessor)
	sp.srgSpecs = srgSpecs
	sp.grids = grids
	sp.gridRef = gridRef
	sp.inputSR = inputSR

	sp.totals = make(map[Period]*SpatialTotals)
	sp.totalGrid = make(map[*GridDef]map[Period]map[string]*sparse.SparseArray)

	sp.MemCacheSize = 100
	sp.MaxMergeDepth = 10
	sp.surrogateGeneratorChan = make(chan *srgRequest)
	go sp.surrogateGenerator(sp.diskCache(sp.memCache(sp.surrogateGeneratorChan)))
	return sp
}

// WRFProjection calculates the spatial projection of a WRF configuration.
func WRFProjection(WPSnamelist, WRFnamelist string, EarthRadius float64) (proj.SR, error) {
	d, err := ParseWRFConfig(WPSnamelist, WRFnamelist)
	if err != nil {
		return proj.SR{}, err
	}

	var mapProj string
	switch d.Map_proj {
	case "lambert":
		mapProj = "lcc"
	case "lat-lon":
		mapProj = "longlat"
	case "merc":
		mapProj = "merc"
	default:
		return proj.SR{}, fmt.Errorf("ERROR: `lambert', `lat-lon', and `merc' "+
			"are the only map projections"+
			" that are currently supported (your projection is `%v').",
			d.Map_proj)
	}
	projInfo := new(proj.ParsedProj4)
	projInfo.Proj = mapProj
	projInfo.Lat_1 = d.Truelat1
	projInfo.Lat_2 = d.Truelat2
	projInfo.Lat_0 = d.Ref_lat
	projInfo.Lon_0 = d.Ref_lon
	projInfo.EarthRadius_a = EarthRadius
	projInfo.EarthRadius_b = EarthRadius
	projInfo.To_meter = 1.
	return proj.FromProj4(projInfo.ToString())
}

// SpatialTotals hold summary results of spatialized emissions records.
type SpatialTotals struct {
	InsideDomainTotals  map[string]map[string]*SpecValUnits
	OutsideDomainTotals map[string]map[string]*SpecValUnits
}

func newSpatialTotals() *SpatialTotals {
	out := new(SpatialTotals)
	out.InsideDomainTotals = make(map[string]map[string]*SpecValUnits)
	out.OutsideDomainTotals = make(map[string]map[string]*SpecValUnits)
	return out
}

func (h *SpatialTotals) add(pol, grid string, emis float64,
	gridEmis *sparse.SparseArray, units string) {
	t := *h
	if _, ok := t.InsideDomainTotals[grid]; !ok {
		t.InsideDomainTotals[grid] = make(map[string]*SpecValUnits)
		t.OutsideDomainTotals[grid] = make(map[string]*SpecValUnits)
	}
	if _, ok := t.InsideDomainTotals[grid][pol]; !ok {
		t.InsideDomainTotals[grid][pol] = new(SpecValUnits)
		t.InsideDomainTotals[grid][pol].Units = units
		t.OutsideDomainTotals[grid][pol] = new(SpecValUnits)
		t.OutsideDomainTotals[grid][pol].Units = units
	} else {
		if t.InsideDomainTotals[grid][pol].Units != units {
			err := fmt.Errorf("Units problem: %v! = %v",
				t.InsideDomainTotals[grid][pol].Units, units)
			panic(err)
		}
	}
	gridTotal := gridEmis.Sum()
	t.InsideDomainTotals[grid][pol].Val += gridTotal
	t.OutsideDomainTotals[grid][pol].Val += emis - gridTotal
	*h = t
}

// GriddedEmissions returns gridded emissions for a given grid index and period.
// sectorType can be "point", "area", or "mobile".
func (r *ParsedRecord) GriddedEmissions(sp *SpatialProcessor, sectorType string,
	gi int, p Period) (emis map[string]*sparse.SparseArray, units map[string]string, err error) {

	emis = make(map[string]*sparse.SparseArray)
	units = make(map[string]string)
	if r.GridSrgs == nil {
		r.inGrid = make([]bool, len(sp.grids))
		r.inGrid = make([]bool, len(sp.grids))
		err := r.spatialize(sp, sectorType)
		if err != nil {
			return nil, nil, err
		}
	}
	if r.GridSrgs[gi] == nil {
		return
	}
	for pol, data := range r.ANN_EMIS[p] {
		emis[pol] = r.GridSrgs[gi].ScaleCopy(data.Val)
		units[pol] = data.Units
	}
	return
}

// spatialize spatializes a record. sectorType can be "point", "area", or "mobile".
func (r *ParsedRecord) spatialize(sp *SpatialProcessor, sectorType string) error {
	r.inGrid = make([]bool, len(sp.grids))
	r.coveredByGrid = make([]bool, len(sp.grids))
	switch sectorType {
	case "point":
		// assume all grids have the same spatial reference.
		ct, err := proj.NewCoordinateTransform(sp.inputSR, sp.grids[0].Sr)

		r.GridSrgs = make([]*sparse.SparseArray, len(sp.grids))
		for i, grid := range sp.grids {
			var row, col int
			r.PointXcoord, r.PointYcoord, row, col,
				r.inGrid[i], err = grid.GetIndex(r.XLOC, r.YLOC, ct)
			if err != nil {
				return err
			}
			// for points, inGrid and coveredByGrid are the same thing.
			r.coveredByGrid[i] = r.inGrid[i]
			if r.inGrid[i] {
				r.GridSrgs[i] = sparse.ZerosSparse(grid.Ny,
					grid.Nx)
				// For points, all emissions are in the same cell.
				r.GridSrgs[i].Set(1., row, col)
			}
		}
	case "area", "mobile":
		srgNum, err := sp.gridRef.GetSrgCode(r.SCC, r.Country, r.FIPS, sp.matchFullSCC)
		if err != nil {
			return err
		}
		srgSpec, err := sp.srgSpecs.GetByCode(r.Country, srgNum)
		if err != nil {
			return err
		}

		r.GridSrgs = make([]*sparse.SparseArray, len(sp.grids))
		for i, grid := range sp.grids {
			r.GridSrgs[i], r.coveredByGrid[i], err = sp.surrogate(srgSpec, grid, r.FIPS)
			if err != nil {
				return err
			}
			if r.GridSrgs[i] != nil {
				r.inGrid[i] = true
			}
		}
	default:
		return fmt.Errorf("unknown sectorType %v", sectorType)
	}
	return sp.addEmisToReport(r, sectorType)
}

func (sp *SpatialProcessor) addEmisToReport(r *ParsedRecord, sectorType string) error {
	// Add emissions to reports
	for p, periodEmis := range r.ANN_EMIS {
		if _, ok := sp.totals[p]; !ok {
			sp.totals[p] = newSpatialTotals()
		}
		for i, grid := range sp.grids {
			gridEmis, units, err := r.GriddedEmissions(sp, sectorType, i, p)
			if err != nil {
				return err
			}
			if _, ok := sp.totalGrid[grid]; !ok {
				sp.totalGrid[grid] =
					make(map[Period]map[string]*sparse.SparseArray)
			}
			if _, ok := sp.totalGrid[grid][p]; !ok {
				sp.totalGrid[grid][p] =
					make(map[string]*sparse.SparseArray)
			}
			for pol, polGridEmis := range gridEmis {
				if _, ok := sp.totalGrid[grid][p][pol]; !ok {
					sp.totalGrid[grid][p][pol] =
						sparse.ZerosSparse(grid.Ny, grid.Nx)
				}
				sp.totalGrid[grid][p][pol].AddSparse(polGridEmis)
				sp.totals[p].add(pol, grid.Name, periodEmis[pol].Val,
					polGridEmis, units[pol])
			}
		}
	}
	return nil
}

// Report returns summary information on the records that have been processed
// by sp.
func (sp *SpatialProcessor) Report() (totals map[Period]*SpatialTotals,
	totalGrid map[*GridDef]map[Period]map[string]*sparse.SparseArray) {
	return sp.totals, sp.totalGrid
}

// memCache implements an in-memory cache for spatial surrogates.
func (sp *SpatialProcessor) memCache(inChan chan *srgRequest) (outChan chan *srgRequest) {
	outChan = make(chan *srgRequest)
	cache := lru.New(sp.MemCacheSize)
	// Spawn multiple cachers to deals with merged surrogates.
	for i := 0; i < sp.MaxMergeDepth; i++ {
		go func() {
			for request := range inChan {
				key := request.key()
				v, ok := cache.Get(key)
				if ok { // the data we want is in the cache.
					request.data = v.(*griddingSurrogate)
					request.returnChan <- request
				} else { // the data we want is not in the cache.
					newRequest := request.repeat()
					outChan <- newRequest
					result := <-newRequest.returnChan
					cache.Add(key, result.data)
					request.returnChan <- result
				}
			}
		}()
	}
	return outChan
}

// diskCache implements a disk cache for spatial surrogates.
func (sp *SpatialProcessor) diskCache(inChan chan *srgRequest) (outChan chan *srgRequest) {
	outChan = make(chan *srgRequest)
	// Spawn multiple cachers to deals with merged surrogates.
	for i := 0; i < sp.MaxMergeDepth; i++ {
		go func() {
			for r := range inChan {
				var ok bool
				r.data, ok = sp.getSrgFromDisk(r)
				if ok {
					// Send result and continue: we're done here.
					r.returnChan <- r
					continue
				}
				if !r.waitInQueue {
					// Skip the queue for surrogates that are being created to merge into
					// other surrogates to avoid a channel lock.
					r.data, r.err = sp.createSurrogate(r.srgSpec, r.grid)
					if r.err != nil {
						r.returnChan <- r
					}
					r.err = sp.writeSrgToDisk(r)
					r.returnChan <- r
					continue
				}
				// Send the request to the queue and wait for it to return.
				newRequest := r.repeat()
				outChan <- newRequest
				newRequest = <-newRequest.returnChan
				if newRequest.err != nil {
					r.returnChan <- newRequest
				}
				newRequest.err = sp.writeSrgToDisk(newRequest)
				r.returnChan <- newRequest
			}
		}()
	}
	return outChan
}

// getSrgFromDisk attempts to get a gridding surrogate from a disk cache, returnin
// the surrogate and a boolean indicating whether the retrieval was successful.
func (sp *SpatialProcessor) getSrgFromDisk(r *srgRequest) (*griddingSurrogate, bool) {
	if sp.DiskCachePath == "" {
		return nil, false
	}

	f, err := os.Open(filepath.Join(sp.DiskCachePath, r.key()+".gob"))
	if err != nil {
		return nil, false
	}

	var data griddingSurrogate
	e := gob.NewDecoder(f)
	err = e.Decode(&data)
	if err != nil {
		// Ignore the error and just regenerate the file.
		return nil, false
	}
	err = f.Close()
	if err != nil {
		// Ignore the error and just regenerate the file.
		return nil, false
	}
	return &data, true
}

// writeSrgToDisk writes a gridding surrogate to a disk cache.
func (sp *SpatialProcessor) writeSrgToDisk(r *srgRequest) error {
	if sp.DiskCachePath == "" || r.data == nil || r.err != nil {
		return r.err
	}
	f, err := os.Create(filepath.Join(sp.DiskCachePath, r.key()+".gob"))
	if err != nil {
		return err
	}
	e := gob.NewEncoder(f)
	err = e.Encode(r.data)
	if err != nil {
		return err
	}
	err = f.Close()
	if err != nil {
		return err
	}
	return nil
}

// surrogate gets the specified spatial surrogate.
// It is important not to edit the returned surrogate in place, because the
// same copy is used over and over again. The second return value indicates
// whether the shape corresponding to fips is completely covered by the grid.
func (sp *SpatialProcessor) surrogate(srgSpec *SrgSpec, grid *GridDef, fips string) (*sparse.SparseArray, bool, error) {

	r := newSrgRequest(srgSpec, grid)
	sp.surrogateGeneratorChan <- r
	r = <-r.returnChan
	if r.err != nil {
		return nil, false, r.err
	}
	srg, coveredByGrid := r.data.ToGrid(fips)
	if srg != nil {
		return srg, coveredByGrid, r.err
	}
	// if srg was nil, try backup surrogates.
	for _, newName := range srgSpec.BackupSurrogateNames {
		newSrgSpec, err := sp.srgSpecs.GetByName(srgSpec.Region, newName)
		if err != nil {
			return nil, false, err
		}
		r := newSrgRequest(newSrgSpec, grid)
		sp.surrogateGeneratorChan <- r
		r = <-r.returnChan
		srg, coveredByGrid := r.data.ToGrid(fips)
		if srg != nil {
			return srg, coveredByGrid, r.err
		}
	}
	return nil, false, nil
}

type srgRequest struct {
	srgSpec    *SrgSpec
	grid       *GridDef
	data       *griddingSurrogate
	err        error
	returnChan chan *srgRequest

	// Usually we use channel to make sure only one surrogate is getting created
	// at a time. This avoids duplicate work if 2 records request the same surrogate
	// at the same time. However, surrogates that are being created to merge with
	// other surrogates need to skip the queue to avoid a channel lock.
	waitInQueue bool
}

// key returns a unique key for this surrogate request.
func (s *srgRequest) key() string {
	return fmt.Sprintf("%s_%s_%s", s.srgSpec.Region, s.srgSpec.Code,
		s.grid.Name)
}

// repeat copies the request with a new return chan
func (s *srgRequest) repeat() *srgRequest {
	ss := newSrgRequest(s.srgSpec, s.grid)
	ss.waitInQueue = s.waitInQueue
	return ss
}

func newSrgRequest(srgSpec *SrgSpec, grid *GridDef) *srgRequest {
	d := new(srgRequest)
	d.srgSpec = srgSpec
	d.grid = grid
	d.returnChan = make(chan *srgRequest)
	d.waitInQueue = true
	return d
}

// Generate spatial surrogates
func (sp *SpatialProcessor) surrogateGenerator(inChan chan *srgRequest) {
	for r := range inChan {
		r.data, r.err = sp.createSurrogate(r.srgSpec, r.grid)
		r.returnChan <- r
	}
}

// SrgSpecs holds a group of surrogate specifications
type SrgSpecs struct {
	byName map[Country]map[string]*SrgSpec
	byCode map[Country]map[string]*SrgSpec
}

// NewSrgSpecs initializes a new SrgSpecs object.
func NewSrgSpecs() *SrgSpecs {
	s := new(SrgSpecs)
	s.byName = make(map[Country]map[string]*SrgSpec)
	s.byCode = make(map[Country]map[string]*SrgSpec)
	return s
}

// Add adds a new SrgSpec to s.
func (s *SrgSpecs) Add(ss *SrgSpec) {
	if _, ok := s.byName[ss.Region]; !ok {
		s.byName[ss.Region] = make(map[string]*SrgSpec)
		s.byCode[ss.Region] = make(map[string]*SrgSpec)
	}
	s.byName[ss.Region][ss.Name] = ss
	s.byCode[ss.Region][ss.Code] = ss
}

// GetByName gets the surrogate matching the given region and name.
func (s *SrgSpecs) GetByName(region Country, name string) (*SrgSpec, error) {
	ss, ok := s.byName[region][name]
	if ok {
		return ss, nil
	}
	return nil, fmt.Errorf("can't find surrogate for region=%s, name=%s", region, name)
}

// GetByCode gets the surrogate matching the given region and code.
func (s *SrgSpecs) GetByCode(region Country, code string) (*SrgSpec, error) {
	ss, ok := s.byCode[region][code]
	if ok {
		return ss, nil
	}
	return nil, fmt.Errorf("can't find surrogate for region=%s, code=%s", region, code)
}

// SrgSpec holds spatial surrogate specification information.
type SrgSpec struct {
	Region          Country
	Name            string
	Code            string
	DATASHAPEFILE   string
	DATAATTRIBUTE   string
	WEIGHTSHAPEFILE string
	Details         string

	// BackupSurrogateNames specifies names of surrogates to use if this
	// one doesn't have data for the desired location.
	BackupSurrogateNames []string

	// WeightColumns specify the fields of the surogate shapefile that
	// should be used to weight the output locations.
	WeightColumns []string

	// WeightFactors are factors by which each of the WeightColumns should
	// be multiplied.
	WeightFactors []float64

	// FilterFunction specifies which rows in the surrogate shapefile should
	// be used to create this surrogate.
	FilterFunction *SurrogateFilter

	// MergeNames specify names of other surrogates that should be combined
	// to create this surrogate.
	MergeNames []string
	// MergeMultipliers specifies multipliers associated with the surrogates
	// in MergeNames.
	MergeMultipliers []float64

	// progress specifies the progress in generating the surrogate.
	progress     float64
	progressLock sync.Mutex
	// status specifies what the surrogate generator is currently doing.
	status string
}

// ReadSrgSpec reads a SMOKE formatted spatial surrogate specification file.
// Results are returned as a map of surrogate specifications as indexed by
// their unique ID, which is Region+SurrogateCode. shapefileDir specifies the
// location of all the required shapefiles, and checkShapeFiles specifies whether
// to check if the required shapefiles actually exist. If checkShapeFiles is
// true, then it is okay for the shapefiles to be in any subdirectory of
// shapefileDir, otherwise all shapefiles must be in shapefileDir itself and
// not a subdirectory.
func ReadSrgSpec(fid io.Reader, shapefileDir string, checkShapefiles bool) (*SrgSpecs, error) {
	srgs := NewSrgSpecs()
	reader := csv.NewReader(fid)
	reader.Comment = '#'
	reader.TrailingComma = true
	records, err := reader.ReadAll()
	if err != nil {
		return nil, fmt.Errorf("in ReadSrgSpec: %v", err)
	}
	for i := 1; i < len(records); i++ {
		record := records[i]
		srg := new(SrgSpec)
		srg.Region = getCountryFromName(record[0])
		srg.Name = strings.TrimSpace(record[1])
		srg.Code = record[2]
		srg.DATASHAPEFILE = record[3]
		srg.DATAATTRIBUTE = strings.TrimSpace(record[4])
		srg.WEIGHTSHAPEFILE = record[5]
		WEIGHTATTRIBUTE := record[6]
		WEIGHTFUNCTION := record[7]
		FILTERFUNCTION := record[8]
		MERGEFUNCTION := record[9]
		for i := 10; i <= 12; i++ {
			if len(record[i]) != 0 {
				srg.BackupSurrogateNames = append(srg.BackupSurrogateNames, record[i])
			}
		}
		srg.Details = record[13]

		// Parse weight function
		if WEIGHTATTRIBUTE != "NONE" && WEIGHTATTRIBUTE != "" {
			srg.WeightColumns = append(srg.WeightColumns,
				strings.TrimSpace(WEIGHTATTRIBUTE))
			srg.WeightFactors = append(srg.WeightFactors, 1.)
		}
		if WEIGHTFUNCTION != "" {
			weightfunction := strings.Split(WEIGHTFUNCTION, "+")
			for _, wf := range weightfunction {
				mulFunc := strings.Split(wf, "*")
				if len(mulFunc) == 1 {
					srg.WeightColumns = append(srg.WeightColumns,
						strings.TrimSpace(mulFunc[0]))
					srg.WeightFactors = append(srg.WeightFactors, 1.)
				} else if len(mulFunc) == 2 {
					v, err := strconv.ParseFloat(mulFunc[0], 64)
					if err != nil {
						return nil, fmt.Errorf("srgspec weight function: %v", err)
					}
					srg.WeightColumns = append(srg.WeightColumns,
						strings.TrimSpace(mulFunc[1]))
					srg.WeightFactors = append(srg.WeightFactors, v)
				} else {
					return nil, fmt.Errorf("invalid value %s in srgspec "+
						"weighting function", wf)
				}
			}
		}

		// Parse filter function
		srg.FilterFunction = ParseSurrogateFilter(FILTERFUNCTION)

		// Parse merge function
		if MERGEFUNCTION != "NONE" && MERGEFUNCTION != "" {
			s := strings.Split(MERGEFUNCTION, "+")
			for _, s2 := range s {
				s3 := strings.Split(s2, "*")
				srg.MergeNames = append(srg.MergeNames, strings.TrimSpace(s3[1]))
				val, err := strconv.ParseFloat(strings.TrimSpace(s3[0]), 64)
				if err != nil {
					return nil, err
				}
				srg.MergeMultipliers = append(srg.MergeMultipliers, val)
			}
		}
		if len(srg.MergeNames) == 0 {
			// If this is not a merged surrogate, setup the shapefile paths and
			// optionally check to make sure the shapefiles exist.
			if checkShapefiles {
				srg.DATASHAPEFILE, err = findFile(shapefileDir, srg.DATASHAPEFILE+".shp")
				if err != nil {
					return nil, err
				}
				srg.WEIGHTSHAPEFILE, err = findFile(shapefileDir, srg.WEIGHTSHAPEFILE+".shp")
				if err != nil {
					return nil, err
				}
			} else {
				srg.DATASHAPEFILE = filepath.Join(
					shapefileDir, srg.DATASHAPEFILE+".shp")
				srg.WEIGHTSHAPEFILE = filepath.Join(
					shapefileDir, srg.WEIGHTSHAPEFILE+".shp")
			}

			if checkShapefiles {
				shpf, err := shp.NewDecoder(srg.DATASHAPEFILE)
				if err != nil {
					return nil, err
				}
				shpf.Close()
				shpf, err = shp.NewDecoder(srg.WEIGHTSHAPEFILE)
				if err != nil {
					return nil, err
				}
				shpf.Close()
			}
		}
		srgs.Add(srg)
	}
	return srgs, nil
}

// findFile finds a file in dir or any of its subdirectories.
func findFile(dir, file string) (string, error) {
	var fullPath string
	var found bool
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() || found {
			return nil
		}
		if info.Name() == file {
			fullPath = path
			found = true
		}
		return nil
	})
	if err != nil {
		return "", err
	}
	if !found {
		return "", fmt.Errorf("could not find file %s within directory %s", file, dir)
	}
	return fullPath, nil
}

// GridRef specifies the grid surrogates the correspond with combinations of
// country (first map), SCC (second map), and FIPS or spatial ID (third map).
type GridRef map[Country]map[string]map[string]interface{}

// ReadGridRef reads the SMOKE gref file, which maps FIPS and SCC codes to grid surrogates
func ReadGridRef(f io.Reader) (*GridRef, error) {
	gr := make(GridRef)
	buf := bufio.NewReader(f)
	for {
		record, err := buf.ReadString('\n')
		if err != nil {
			if err.Error() == "EOF" {
				break
			} else {
				return nil, fmt.Errorf("in ReadGridRef: %v \nrecord= %s",
					err.Error(), record)
			}
		}
		// Get rid of comments at end of line.
		if i := strings.Index(record, "!"); i != -1 {
			record = record[0:i]
		}

		if record[0] != '#' && record[0] != '\n' {
			splitLine := strings.Split(record, ";")
			SCC := splitLine[1]
			if len(SCC) == 8 {
				// TODO: make this work with different types of codes; i.e. some sort of
				// fuzzy matching instead of just adding 2 zeros.
				SCC = "00" + SCC
			}
			country := getCountryFromID(splitLine[0][0:1])
			FIPS := splitLine[0][1:]
			srg := strings.Trim(splitLine[2], "\"\n ")

			if _, ok := gr[country]; !ok {
				gr[country] = make(map[string]map[string]interface{})
			}
			if _, ok := gr[country][SCC]; !ok {
				gr[country][SCC] = make(map[string]interface{})
			}
			gr[country][SCC][FIPS] = srg
		}
	}
	return &gr, nil
}

// GetSrgCode returns the surrogate code appropriate for the given SCC code,
// country and FIPS.
func (gr GridRef) GetSrgCode(SCC string, c Country, FIPS string, matchFullSCC bool) (string, error) {
	var err error
	var matchedVal interface{}
	if !matchFullSCC {
		_, _, matchedVal, err = MatchCodeDouble(
			SCC, FIPS, gr[c])
	} else {
		_, matchedVal, err = MatchCode(FIPS,
			gr[c][SCC])
	}
	if err != nil {
		return "", fmt.Errorf("in GetSrg: %v. (SCC=%v, Country=%v, FIPS=%v)",
			err.Error(), SCC, c, FIPS)
	}
	return matchedVal.(string), nil
}

// Merge combines values in gr2 into gr. If gr2 combines any values that
// conflict with values already in gr, an error is returned.
func (gr *GridRef) Merge(gr2 GridRef) error {
	grx := *gr
	for country, d1 := range gr2 {
		if _, ok := grx[country]; !ok {
			grx[country] = make(map[string]map[string]interface{})
		}
		for SCC, d2 := range d1 {
			if _, ok := grx[country][SCC]; !ok {
				grx[country][SCC] = make(map[string]interface{})
			}
			for FIPS, code := range d2 {
				if existingCode, ok := grx[country][SCC][FIPS]; ok && existingCode != code {
					return fmt.Errorf("GridRef already has code of %s for country=%s, "+
						"SCC=%s, FIPS=%s. Cannot replace with code %s.", country, SCC, FIPS)
				}
				grx[country][SCC][FIPS] = code
			}
		}
	}
	gr = &grx
	return nil
}
