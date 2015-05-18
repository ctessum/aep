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
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"

	"bitbucket.org/ctessum/sparse"
	"github.com/ctessum/geom/encoding/shp"
	"github.com/ctessum/geom/proj"
)

type SpatialProcessor struct {
	SrgSpec                map[string]*SrgSpecHolder
	srgCodes               map[string]string
	Grids                  []*GridDef
	gridRef                map[Country]map[string]map[string]interface{}
	SurrogateGeneratorChan chan *SrgGenData
	srgCache               map[string]*sparse.SparseArray
	srgCacheMx             sync.Mutex
	c                      *Context
}

func setupCommon(c *Context, e *ErrCat) (sp *SpatialProcessor, sr proj.SR) {
	c.Log("Setting up spatial environment...", 1)

	sp = new(SpatialProcessor)
	sp.SrgSpec = make(map[string]*SrgSpecHolder)
	sp.srgCodes = make(map[string]string)
	sp.Grids = make([]*GridDef, 0)
	sp.gridRef = make(map[Country]map[string]map[string]interface{})
	sp.SurrogateGeneratorChan = make(chan *SrgGenData)
	sp.srgCache = make(map[string]*sparse.SparseArray)
	sp.c = c

	// Check the surrogate ShapefileDir to make sure the shapefiles are
	// present and can be opened.
	e.Add(sp.SurrogateSpecification())
	if c.CheckSrgs {
		for _, srg := range sp.SrgSpec {
			file := filepath.Join(
				sp.c.ShapefileDir, srg.DATASHAPEFILE+".shp")
			shpf, err := shp.NewDecoder(file)
			e.Add(err)
			shpf.Close()
			if srg.WEIGHTSHAPEFILE != "" {
				file := filepath.Join(
					sp.c.ShapefileDir, srg.WEIGHTSHAPEFILE+".shp")
				shpf, err := shp.NewDecoder(file)
				e.Add(err)
				shpf.Close()
			}
		}
	}

	var err error

	c.wrfData, err = ParseWRFConfig(c.WPSnamelist, c.WRFnamelist)
	if err != nil {
		e.Add(err)
	}

	x := c.wrfData
	if x == nil {
		panic("wrfData needs to be initialized")
	}
	reportMx.Lock()
	Report.GridNames = x.DomainNames
	reportMx.Unlock()
	var mapProj string
	switch x.Map_proj {
	case "lambert":
		mapProj = "lcc"
	case "lat-lon":
		mapProj = "longlat"
	case "merc":
		mapProj = "merc"
	default:
		e.Add(fmt.Errorf("ERROR: `lambert', `lat-lon', and `merc' "+
			"are the only map projections"+
			" that are currently supported (your projection is `%v').",
			x.Map_proj))
		return
	}
	projInfo := new(proj.ParsedProj4)
	projInfo.Proj = mapProj
	projInfo.Lat_1 = x.Truelat1
	projInfo.Lat_2 = x.Truelat2
	projInfo.Lat_0 = x.Ref_lat
	projInfo.Lon_0 = x.Ref_lon
	projInfo.EarthRadius_a = c.EarthRadius
	projInfo.EarthRadius_b = c.EarthRadius
	projInfo.To_meter = 1.
	msg := fmt.Sprintf("Output projection is:\n%v", projInfo.ToString())
	c.Log(msg, 0)
	sr, err = proj.FromProj4(projInfo.ToString())
	e.Add(err)
	e.Add(sp.GridRef())
	return
}

func (sp *SpatialProcessor) setupSrgs(e *ErrCat) {
	sp.c.Log("Setting up surrogate generation...", 1)
	if sp.c.RegenerateSpatialData {
		// delete spatial surrogates
		e.Add(filepath.Walk(sp.c.GriddedSrgs,
			func(path string, info os.FileInfo, err error) error {
				if strings.HasSuffix(path, ".shp") ||
					strings.HasSuffix(path, ".shx") ||
					strings.HasSuffix(path, ".dbf") ||
					strings.HasSuffix(path, ".prj") ||
					strings.HasSuffix(path, ".sqlite") {
					os.Remove(path)
				}
				return err
			}))
		//} else {
		//	// Add spatial surrogates to the cache.
		//	e.Add(filepath.Walk(c.GriddedSrgs,
		//		func(path string, info os.FileInfo, err error) error {
		//			if strings.HasSuffix(path, ".dbf") &&
		//				strings.Contains(info.Name(), "_") {
		//				fname := info.Name()
		//				e.Add(spatialsrg.AddSurrogateToCache(path,
		//					fname[0:len(fname)-4], grids))
		//			}
		//			return err
		//		}))
	}
	for _, grid := range sp.Grids {
		e.Add(grid.SetupSrgMapCache(sp.c.GriddedSrgs))
	}
	go sp.SurrogateGenerator()
	return
}

func SpatialSetupRegularGrid(c *Context, e *ErrCat) *SpatialProcessor {
	c.Log("Setting up spatial projection and database connection...", 5)
	sp, sr := setupCommon(c, e)
	x := c.wrfData
	for i := 0; i < x.Max_dom; i++ {
		c.Log(fmt.Sprintf("Setting up grid %v...", i+1), 0)
		grid := NewGridRegular(x.DomainNames[i],
			x.Nx[i], x.Ny[i], x.Dx[i], x.Dy[i], x.W[i], x.S[i], sr)
		if c.RunTemporal {
			e.Add(grid.GetTimeZones(
				filepath.Join(c.ShapefileDir, "world_timezones.shp"), "TZID"))
		}
		e.Add(grid.WriteToShp(c.GriddedSrgs))
		sp.Grids = append(sp.Grids, grid)
	}
	sp.setupSrgs(e)
	return sp
}

// Use a shapefile as the grid.
func SpatialSetupIrregularGrid(c *Context, name, shapeFilePath string,
	columnsToKeep []string, e *ErrCat) *SpatialProcessor {
	sp, sr := setupCommon(c, e)
	if shapeFilePath == "" {
		e.Add(fmt.Errorf("In SpatialSetupIrregularGrid, shapeFilePath is empty"))
	}
	grid, err := NewGridIrregular(name, shapeFilePath,
		columnsToKeep, sr)
	e.Add(err)
	grid.IrregularGrid = true
	if c.RunTemporal {
		e.Add(grid.GetTimeZones(
			filepath.Join(c.ShapefileDir, "world_timezones.shp"), "TZID"))
	}
	sp.Grids = append(sp.Grids, grid)
	sp.setupSrgs(e)
	return sp
}

type SpatialTotals struct {
	InsideDomainTotals  map[string]map[string]*SpecValUnits
	OutsideDomainTotals map[string]map[string]*SpecValUnits
}

func newSpatialTotalHolder() *SpatialTotals {
	out := new(SpatialTotals)
	out.InsideDomainTotals = make(map[string]map[string]*SpecValUnits)
	out.OutsideDomainTotals = make(map[string]map[string]*SpecValUnits)
	return out
}

func (h *SpatialTotals) Add(pol, grid string, emis float64,
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

// EmisToGrid returns gridded emissions for a given grid index and period.
func (r *ParsedRecord) EmisToGrid(gi int, p Period) (
	emis map[string]*sparse.SparseArray, units map[string]string) {
	emis = make(map[string]*sparse.SparseArray)
	units = make(map[string]string)
	if r.gridSrg[gi] == nil {
		return
	}
	for pol, data := range r.ANN_EMIS[p] {
		emis[pol] = r.gridSrg[gi].ScaleCopy(data.Val)
		units[pol] = data.Units
	}
	return
}

// Spatialize spatializes a record.
func (sp *SpatialProcessor) Spatialize(record *ParsedRecord) (emisInRecord bool) {
	var err error
	switch sp.c.SectorType {
	case "point":
		var ct *proj.CoordinateTransform
		ct, err = proj.NewCoordinateTransform(sp.c.inputSr, sp.Grids[0].Sr)
		emisInRecord = false

		record.gridSrg = make([]*sparse.SparseArray, len(sp.Grids))
		for i, grid := range sp.Grids {
			record.gridSrg[i] = sparse.ZerosSparse(grid.Ny,
				grid.Nx)
			var row, col int
			var withinGrid bool
			record.PointXcoord, record.PointYcoord, row, col,
				withinGrid, err = grid.GetIndex(record.XLOC,
				record.YLOC, sp.c.inputSr, ct)
			if err != nil {
				panic(err)
			}
			if withinGrid {
				emisInRecord = true
				// For points, all emissions are in the same cell.
				record.gridSrg[i].Set(1., row, col)
			}
		}
	case "area", "mobile":
		var matchedVal interface{}
		if !sp.c.MatchFullSCC {
			_, _, matchedVal, err = MatchCodeDouble(
				record.SCC, record.FIPS, sp.gridRef[record.Country])
		} else {
			_, matchedVal, err = MatchCode(record.FIPS,
				sp.gridRef[record.Country][record.SCC])
		}
		if err != nil {
			err = fmt.Errorf("In spatial reference file: %v. (SCC=%v, FIPS=%v).",
				err.Error(), record.SCC, record.FIPS)
			panic(err)
		}
		srgNum := matchedVal.(string)

		record.gridSrg = make([]*sparse.SparseArray, len(sp.Grids))
		for i, grid := range sp.Grids {
			record.gridSrg[i] = sp.getSurrogate(srgNum, record.FIPS, grid,
				make([]string, 0))
		}
		emisInRecord = true
	default:
		err = fmt.Errorf("Unknown sectorType %v", sp.c.SectorType)
		panic(err)
	}
	return
}

// SpawnSpatializer spawns an ansychronous processor for spatializing records.
func (sp *SpatialProcessor) SpawnSpatializer(InputChan chan *ParsedRecord) (
	OutputChan chan *ParsedRecord) {
	OutputChan = make(chan *ParsedRecord)
	go func() {
		defer sp.c.ErrorRecoverCloseChan(InputChan)

		sp.c.Log("Spatializing "+sp.c.Sector+"...", 1)

		totals := make(map[string]*SpatialTotals)
		for _, p := range sp.c.runPeriods {
			totals[p.String()] = newSpatialTotalHolder()
		}
		TotalGrid := make(map[*GridDef]map[Period]map[string]*sparse.SparseArray) // map[grid][period][pol]data

		for record := range InputChan {
			emisInRecord := sp.Spatialize(record)
			sp.addEmisToReport(record, totals, TotalGrid)
			if emisInRecord {
				OutputChan <- record
			}

		}
		close(OutputChan)
		reportMx.Lock()
		for p, t := range totals {
			Report.SectorResults[sp.c.Sector][p].SpatialResults = t
		}
		reportMx.Unlock()
		sp.c.ResultMaps(totals, TotalGrid)
		sp.c.msgchan <- "Finished spatializing " + sp.c.Sector
		return
	}()
	return
}

func (sp *SpatialProcessor) addEmisToReport(r *ParsedRecord, totals map[string]*SpatialTotals,
	TotalGrid map[*GridDef]map[Period]map[string]*sparse.SparseArray) {
	// Add emissions to reports
	for p, periodEmis := range r.ANN_EMIS {
		for i, grid := range sp.Grids {
			gridEmis, units := r.EmisToGrid(i, p)
			if _, ok := TotalGrid[grid]; !ok {
				TotalGrid[grid] =
					make(map[Period]map[string]*sparse.SparseArray)
			}
			if _, ok := TotalGrid[grid][p]; !ok {
				TotalGrid[grid][p] =
					make(map[string]*sparse.SparseArray)
			}
			for pol, polGridEmis := range gridEmis {
				if _, ok := TotalGrid[grid][p][pol]; !ok {
					TotalGrid[grid][p][pol] =
						sparse.ZerosSparse(grid.Ny, grid.Nx)
				}
				TotalGrid[grid][p][pol].AddSparse(polGridEmis)
				totals[p.String()].Add(pol, grid.Name, periodEmis[pol].Val,
					polGridEmis, units[pol])
			}
		}
	}
}

func (sp *SpatialProcessor) getSurrogate(srgNum, FIPS string, grid *GridDef,
	upstreamSrgs []string) (srg *sparse.SparseArray) {

	tableName := grid.Name + "___" + srgNum
	status := Status.GetSrgStatus(tableName,
		filepath.Join(sp.c.GriddedSrgs, tableName+".shp"))
	switch {
	case status == "Generating" || status == "Waiting to generate" ||
		status == "Empty":
		if status == "Empty" {
			//Status.Lock.Lock()
			Status.Surrogates[tableName] = "Waiting to generate"
			//Status.Lock.Unlock()
		}
		srgGenData := NewSrgGenData(srgNum, grid)
		sp.SurrogateGeneratorChan <- srgGenData
		err := <-srgGenData.finishedChan
		if err != nil {
			panic(err)
		}
	case status == "Ready":
	default:
		panic(fmt.Sprintf("Unknown status \"%v\"", status))
	}
	srg = sp.retrieveSurrogate(srgNum, FIPS, grid, upstreamSrgs)
	return
}

// It is important not to edit the returned surrogate in place, because the
// same copy is used over and over again.
func (sp *SpatialProcessor) retrieveSurrogate(srgNum, FIPS string, grid *GridDef,
	upstreamSrgs []string) *sparse.SparseArray {

	var err error
	cacheKey := grid.Name + srgNum + FIPS

	// Check if surrogate is already in the cache.
	sp.srgCacheMx.Lock()
	if srg, found := sp.srgCache[cacheKey]; found {
		sp.srgCacheMx.Unlock()
		return srg
	}
	sp.srgCacheMx.Unlock()

	var srg *sparse.SparseArray
	secondarySrg := sp.SrgSpec[srgNum].SECONDARYSURROGATE
	tertiarySrg := sp.SrgSpec[srgNum].TERTIARYSURROGATE
	quarternarySrg := sp.SrgSpec[srgNum].QUARTERNARYSURROGATE
	MergeFunction := sp.SrgSpec[srgNum].MergeFunction
	if MergeFunction == nil {
		srg, err = RetrieveGriddingSurrogate(
			srgNum, FIPS, grid)
		if err != nil {
			panic(err)
		}

		// Try backup surrogates if the surrogate sums to zero.
		if srg == nil || srg.Sum() == 0. {
			backupNames := []string{secondarySrg, tertiarySrg, quarternarySrg}
			for _, backupName := range backupNames {
				if backupName != "" {
					newSrgNum := sp.srgCodes[backupName]
					if !IsStringInArray(upstreamSrgs, newSrgNum) {
						srg = sp.getSurrogate(newSrgNum, FIPS, grid,
							append(upstreamSrgs, newSrgNum))
					}
				}
				if srg != nil && srg.Sum() > 0. {
					break
				}
			}
		}
	} else {
		srg = sparse.ZerosSparse(grid.Ny, grid.Nx)
		for _, mrgval := range MergeFunction {
			newSrgNum, ok := sp.srgCodes[mrgval.name]
			if !ok {
				panic("No match for surrogate named " + mrgval.name)
			}
			weight := mrgval.val
			tempSrg := sp.retrieveSurrogate(newSrgNum, FIPS, grid,
				append(upstreamSrgs, newSrgNum))
			if tempSrg != nil {
				srg.AddSparse(tempSrg.ScaleCopy(weight))
			}
		}
	}
	//srgSum := srg.Sum()
	//if srgSum > 1.001 || math.IsNaN(srgSum) {
	//	if c.testMode {
	//		err = fmt.Errorf("Sum for surrogate !<= 1.0: %v", srgSum)
	//		panic(err)
	//	} else {
	//		srg.Scale(1. / srgSum)
	//	}
	//}
	// store surrogate in cache for faster access.
	sp.srgCacheMx.Lock()
	sp.srgCache[cacheKey] = srg
	sp.srgCacheMx.Unlock()
	return srg
}

type SrgGenData struct {
	srgNum       string
	grid         *GridDef
	finishedChan chan error
}

func NewSrgGenData(srgNum string, grid *GridDef) (
	d *SrgGenData) {
	d = new(SrgGenData)
	d.srgNum = srgNum
	d.grid = grid
	d.finishedChan = make(chan error)
	return
}

// Generate spatial surrogates
func (sp *SpatialProcessor) SurrogateGenerator() {
	for srgData := range sp.SurrogateGeneratorChan {
		var err error
		srgNum := srgData.srgNum
		sp.c.Log(sp.SrgSpec[srgNum], 2)
		if _, ok := sp.SrgSpec[srgNum]; !ok {
			err := fmt.Errorf("There is no surrogate specification for surrogate "+
				"number %v. This needs to be fixed in %v.", srgNum, sp.c.SrgSpecFile)
			//Status.Lock.Lock()
			Status.Surrogates[srgData.grid.Name+"___"+srgNum] = "Failed!"
			//Status.Lock.Unlock()
			srgData.finishedChan <- err
			continue
		}
		MergeFunction := sp.SrgSpec[srgNum].MergeFunction
		if MergeFunction == nil {
			err = sp.genSrgNoMerge(srgData)
		} else {
			err = sp.genSrgMerge(srgData)
		}
		srgData.finishedChan <- err
	}
}

// Generate a surrogate that doesn't require merging
func (sp *SpatialProcessor) genSrgNoMerge(srgData *SrgGenData) (err error) {
	srgNum := srgData.srgNum
	grid := srgData.grid
	inputMap := sp.SrgSpec[srgNum].DATASHAPEFILE
	inputColumn := sp.SrgSpec[srgNum].DATAATTRIBUTE
	surrogateMap := sp.SrgSpec[srgNum].WEIGHTSHAPEFILE
	WeightColumns := sp.SrgSpec[srgNum].WeightColumns
	FilterFunction := sp.SrgSpec[srgNum].FilterFunction
	//Status.Lock.Lock()
	Status.Surrogates[grid.Name+"___"+srgNum] = "Generating"
	//Status.Lock.Unlock()
	inputFilePath := filepath.Join(sp.c.ShapefileDir, inputMap+".shp")
	surrogateFilePath := filepath.Join(sp.c.ShapefileDir, surrogateMap+".shp")
	err = CreateGriddingSurrogate(srgNum, inputFilePath,
		inputColumn, surrogateFilePath, WeightColumns, FilterFunction,
		grid, sp.c.GriddedSrgs, sp.c.slaves)
	if err == nil {
		//Status.Lock.Lock()
		Status.Surrogates[grid.Name+"___"+srgNum] = "Ready"
		//Status.Lock.Unlock()
		return
	} else {
		//Status.Lock.Lock()
		Status.Surrogates[grid.Name+"___"+srgNum] = "Failed!"
		//Status.Lock.Unlock()
		err = fmt.Errorf("Surrogate %v_%v Failed!\n%v",
			grid.Name, srgNum, err.Error())
		return
	}
}

// surrogate merging: create a surrogate from other surrogates
// Here, we just create weight surrogate tables if they don't exist.
// The actual merging happens elsewhere.
func (sp *SpatialProcessor) genSrgMerge(srgData *SrgGenData) (err error) {
	srgNum := srgData.srgNum
	grid := srgData.grid
	sp.c.Log(sp.SrgSpec[srgNum], 2)
	MergeFunction := sp.SrgSpec[srgNum].MergeFunction
	//Status.Lock.Lock()
	Status.Surrogates[grid.Name+"___"+srgNum] = "Generating"
	//Status.Lock.Unlock()
	for _, mrgval := range MergeFunction {
		newSrgNum, ok := sp.srgCodes[mrgval.name]
		if !ok {
			err = fmt.Errorf("No match for surrogate named %v.", mrgval.name)
			return
		}
		tableName := grid.Name + "___" + newSrgNum
		filename := filepath.Join(sp.c.GriddedSrgs, tableName+".shp")
		status := Status.GetSrgStatus(tableName, filename)
		switch {
		case status == "Generating" || status == "Waiting to generate" ||
			status == "Empty":
			newSrgData := NewSrgGenData(newSrgNum, grid)
			newMergeFunction := sp.SrgSpec[newSrgNum].MergeFunction
			if newMergeFunction == nil {
				err = sp.genSrgNoMerge(newSrgData)
			} else {
				err = sp.genSrgMerge(newSrgData)
			}
			if err != nil {
				return
			}
		case status == "Ready":
		default:
			err = fmt.Errorf("Unknown status \"%v\"", status)
			return
		}
	}
	//Status.Lock.Lock()
	Status.Surrogates[grid.Name+"___"+srgNum] = "Ready"
	//Status.Lock.Unlock()
	return
}

type SrgSpecHolder struct {
	REGION               string
	SURROGATE            string
	SURROGATECODE        string
	DATASHAPEFILE        string
	DATAATTRIBUTE        string
	WEIGHTSHAPEFILE      string
	WEIGHTATTRIBUTE      string
	WEIGHTFUNCTION       string
	FILTERFUNCTION       string
	MERGEFUNCTION        string
	SECONDARYSURROGATE   string
	TERTIARYSURROGATE    string
	QUARTERNARYSURROGATE string
	DETAILS              string
	WeightColumns        []string
	FilterFunction       *SurrogateFilter
	MergeFunction        []*SrgMerge
}

type SrgMerge struct {
	name string
	val  float64
}

func (sp *SpatialProcessor) SurrogateSpecification() (err error) {
	var record []string
	fid, err := os.Open(sp.c.SrgSpecFile)
	if err != nil {
		return
	}
	defer fid.Close()
	reader := csv.NewReader(fid)
	reader.Comment = '#'
	reader.TrailingComma = true
	firstLine := true
	for {
		record, err = reader.Read()
		if err != nil {
			if err == io.EOF {
				err = nil
				break
			} else {
				err = fmt.Errorf("SurrogateSpecification: %v \nFile= %v\nRecord= ",
					err.Error(), sp.c.SrgSpecFile, record)
				return
			}
		}
		if firstLine {
			firstLine = false
			continue
		}
		srg := new(SrgSpecHolder)
		srg.REGION = record[0]
		srg.SURROGATE = strings.TrimSpace(record[1])
		srg.SURROGATECODE = record[2]
		srg.DATASHAPEFILE = record[3]
		srg.DATAATTRIBUTE = record[4]
		srg.WEIGHTSHAPEFILE = record[5]
		srg.WEIGHTATTRIBUTE = record[6]
		srg.WEIGHTFUNCTION = record[7]
		srg.FILTERFUNCTION = record[8]
		srg.MERGEFUNCTION = record[9]
		if len(record[10]) != 0 {
			srg.SECONDARYSURROGATE = srg.REGION + record[10]
		}
		if len(record[11]) != 0 {
			srg.TERTIARYSURROGATE = srg.REGION + record[11]
		}
		if len(record[12]) != 0 {
			srg.QUARTERNARYSURROGATE = srg.REGION + record[12]
		}
		srg.DETAILS = record[13]

		// Parse weight function
		if srg.WEIGHTATTRIBUTE != "NONE" && srg.WEIGHTATTRIBUTE != "" {
			srg.WeightColumns = strings.Split(srg.WEIGHTATTRIBUTE, "+")
			for i := 0; i < len(srg.WeightColumns); i++ {
				srg.WeightColumns[i] = strings.TrimSpace(srg.WeightColumns[i])
			}
		}

		// Parse filter function
		if srg.FILTERFUNCTION != "NONE" && srg.FILTERFUNCTION != "" {
			srg.FilterFunction = NewSurrogateFilter()
			s := make([]string, 0)
			if strings.Index(srg.FILTERFUNCTION, "!=") != -1 {
				srg.FilterFunction.EqualNotEqual = "NotEqual"
				s = strings.Split(srg.FILTERFUNCTION, "!=")
			} else {
				srg.FilterFunction.EqualNotEqual = "Equal"
				s = strings.Split(srg.FILTERFUNCTION, "=")
			}
			srg.FilterFunction.Column = strings.TrimSpace(s[0])
			splitstr := strings.Split(s[1], ",")
			for _, val := range splitstr {
				srg.FilterFunction.Values = append(srg.FilterFunction.Values,
					strings.TrimSpace(val))
			}
		}

		// Parse merge function
		if srg.MERGEFUNCTION != "NONE" && srg.MERGEFUNCTION != "" {
			srg.MergeFunction = make([]*SrgMerge, 0)
			s := strings.Split(srg.MERGEFUNCTION, "+")
			for _, s2 := range s {
				v := new(SrgMerge)
				s3 := strings.Split(s2, "*")
				v.name = srg.REGION + strings.TrimSpace(s3[1])
				v.val, err = strconv.ParseFloat(strings.TrimSpace(s3[0]), 64)
				if err != nil {
					return
				}
				srg.MergeFunction = append(srg.MergeFunction, v)
			}
		}

		sp.SrgSpec[srg.REGION+srg.SURROGATECODE] = srg
		sp.srgCodes[srg.REGION+srg.SURROGATE] = srg.REGION + srg.SURROGATECODE
	}
	return
}

// GridRef reads the SMOKE gref file, which maps FIPS and SCC codes to grid surrogates
func (sp *SpatialProcessor) GridRef() (err error) {
	var record string
	fid, err := os.Open(sp.c.GridRefFile)
	if err != nil {
		err = fmt.Errorf("GridRef: %v \nFile= %v\nRecord= ",
			err.Error(), sp.c.GridRefFile, record)
		return
	} else {
		defer fid.Close()
	}
	buf := bufio.NewReader(fid)
	for {
		record, err = buf.ReadString('\n')
		if err != nil {
			if err.Error() == "EOF" {
				err = nil
				break
			} else {
				err = fmt.Errorf("GridRef: %v \nFile= %v\nRecord= ",
					err.Error(), sp.c.GridRefFile, record)
				return
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
				SCC = "00" + SCC
			}
			country := getCountryFromID(splitLine[0][0:1])
			FIPS := splitLine[0][1:]
			srg := strings.Trim(splitLine[2], "\"\n")

			if _, ok := sp.gridRef[country]; !ok {
				sp.gridRef[country] = make(map[string]map[string]interface{})
			}
			if _, ok := sp.gridRef[country][SCC]; !ok {
				sp.gridRef[country][SCC] = make(map[string]interface{})
			}
			sp.gridRef[country][SCC][FIPS] = country.String() + srg
		}
	}
	return
}
