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
	"bitbucket.org/ctessum/aep/spatialsrg"
	"bitbucket.org/ctessum/gis"
	"bitbucket.org/ctessum/gisconversions"
	"bitbucket.org/ctessum/sparse"
	"bufio"
	"encoding/csv"
	"fmt"
	"github.com/lukeroth/gdal"
	"github.com/pmylund/go-cache"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

var (
	srgSpec                = make(map[string]*srgSpecHolder)
	srgCodes               = make(map[string]string)
	grids                  = make([]*spatialsrg.GridDef, 0)
	gridRef                = make(map[string]map[string]map[string]interface{})
	SurrogateGeneratorChan = make(chan *SrgGenData)
	srgCache               *cache.Cache
)

func (c *RunData) setupCommon(e *ErrCat) (sr gdal.SpatialReference) {
	c.Log("Setting up spatial environment...", 1)
	spatialsrg.DebugLevel = c.DebugLevel
	var err error
	x := c.wrfData
	Report.GridNames = x.DomainNames
	var proj string
	switch x.Map_proj {
	case "lambert":
		proj = "lcc"
	case "lat-lon":
		proj = "longlat"
	case "merc":
		proj = "merc"
	default:
		e.Add(fmt.Errorf("ERROR: `lambert', `lat-lon', and `merc' "+
			"are the only map projections"+
			" that are currently supported (your projection is `%v').",
			x.Map_proj))
		return
	}
	projInfo := new(gis.ParsedProj4)
	projInfo.Proj = proj
	projInfo.Lat_1 = x.Truelat1
	projInfo.Lat_2 = x.Truelat2
	projInfo.Lat_0 = x.Ref_lat
	projInfo.Lon_0 = x.Ref_lon
	projInfo.EarthRadius_a = c.EarthRadius
	projInfo.EarthRadius_b = c.EarthRadius
	projInfo.To_meter = 1.
	msg := fmt.Sprintf("Output projection is:\n%v", projInfo.ToString())
	c.Log(msg, 0)
	sr, err = gis.CreateSpatialReference(projInfo.ToString())
	e.Add(err)
	e.Add(c.GridRef())
	return
}

func (c *RunData) setupSrgs(e *ErrCat) {
	c.Log("Setting up surrogate generation...", 1)
	if c.RegenerateSpatialData {
		// delete spatial surrogates
		e.Add(filepath.Walk(c.griddedSrgs,
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
		//	e.Add(filepath.Walk(c.griddedSrgs,
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
	for _, grid := range grids {
		e.Add(grid.SetupSrgMapCache(c.griddedSrgs))
	}
	t := c.SrgCacheExpirationTime
	srgCache = cache.New(t*time.Minute, t*time.Minute)
	go c.SurrogateGenerator()
	return
}

func (c *RunData) SpatialSetupRegularGrid(e *ErrCat) {
	c.Log("Setting up spatial projection and database connection...", 5)
	sr := c.setupCommon(e)
	x := c.wrfData
	for i := 0; i < x.Max_dom; i++ {
		c.Log(fmt.Sprintf("Setting up grid %v...", i+1), 0)
		grid := spatialsrg.NewGridRegular(x.DomainNames[i],
			x.Nx[i], x.Ny[i], x.Dx[i], x.Dy[i], x.W[i], x.S[i], sr)
		if c.RunTemporal {
			e.Add(grid.GetTimeZones(
				filepath.Join(c.shapefiles, "timezone.shp"), "zone"))
		}
		e.Add(grid.WriteToShp(c.griddedSrgs))
		grids = append(grids, grid)
	}
	c.setupSrgs(e)
}

// Use a spatial table (which needs to be already loaded into PostGIS
// as the grid. The table to needs to have an ID column called gid
func (c *RunData) SpatialSetupIrregularGrid(name, shapeFilePath string,
	columnsToKeep []string, e *ErrCat) {
	sr := c.setupCommon(e)
	grid, err := spatialsrg.NewGridIrregular(name, shapeFilePath,
		columnsToKeep, sr)
	e.Add(err)
	grid.IrregularGrid = true
	if c.RunTemporal {
		e.Add(grid.GetTimeZones(
			filepath.Join(c.shapefiles, "timezone"), "zone"))
	}
	grids = append(grids, grid)
	c.setupSrgs(e)
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

func (h *SpatialTotals) Add(pol, grid string, data *SpecValUnits, i int) {
	t := *h
	if _, ok := t.InsideDomainTotals[grid]; !ok {
		t.InsideDomainTotals[grid] = make(map[string]*SpecValUnits)
		t.OutsideDomainTotals[grid] = make(map[string]*SpecValUnits)
	}
	if _, ok := t.InsideDomainTotals[grid][pol]; !ok {
		t.InsideDomainTotals[grid][pol] = new(SpecValUnits)
		t.InsideDomainTotals[grid][pol].Units = data.Units
		t.OutsideDomainTotals[grid][pol] = new(SpecValUnits)
		t.OutsideDomainTotals[grid][pol].Units = data.Units
	} else {
		if t.InsideDomainTotals[grid][pol].Units != data.Units {
			err := fmt.Errorf("Units problem: %v! = %v",
				t.InsideDomainTotals[grid][pol].Units, data.Units)
			panic(err)
		}
	}
	gridTotal := data.Gridded[i].Sum()
	t.InsideDomainTotals[grid][pol].Val += gridTotal
	t.OutsideDomainTotals[grid][pol].Val += data.Val - gridTotal
	*h = t
}

func (c *RunData) Spatialize(InputChan chan *ParsedRecord,
	OutputChan chan *ParsedRecord, period string) {
	defer c.ErrorRecoverCloseChan(InputChan)
	var err error

	c.Log("Spatializing "+period+" "+c.Sector+"...", 1)

	totals := newSpatialTotalHolder()
	TotalGrid := make(map[*spatialsrg.GridDef]map[string]*sparse.SparseArray) // map[grid][pol]data

	switch c.SectorType {
	case "point":
		var ct *gisconversions.CoordinateTransform
		ct, err = gisconversions.NewCoordinateTransform(c.inputSr, grids[0].Sr)
		for record := range InputChan {
			emisInRecord := false
			for pol, data := range record.ANN_EMIS {
				data.Gridded = make([]*sparse.SparseArray, len(grids))
				for i, grid := range grids {
					if _, ok := TotalGrid[grid]; !ok {
						TotalGrid[grid] = make(map[string]*sparse.SparseArray)
					}
					if _, ok := TotalGrid[grid][pol]; !ok {
						TotalGrid[grid][pol] =
							sparse.ZerosSparse(grid.Ny, grid.Nx)
					}
					data.Gridded[i] = sparse.ZerosSparse(grid.Ny,
						grid.Nx)
					var row, col int
					var withinGrid bool
					record.PointXcoord, record.PointYcoord, row, col,
						withinGrid, err = grid.GetIndex(record.XLOC,
						record.YLOC, &c.inputSr, ct)
					if err != nil {
						panic(err)
					}
					if withinGrid {
						if data.Val != 0. {
							emisInRecord = true
						}
						data.Gridded[i].Set(data.Val, row, col)
						TotalGrid[grid][pol].AddVal(data.Val, row, col)
					}
					totals.Add(pol, grid.Name, data, i)
				}
			}
			if emisInRecord {
				OutputChan <- record
			}
		}
	case "area", "mobile":
		for record := range InputChan {
			var matchedVal interface{}
			if !c.MatchFullSCC {
				_, _, matchedVal, err = MatchCodeDouble(
					record.SCC, record.FIPS, gridRef[record.Country])
			} else {
				_, matchedVal, err = MatchCode(record.FIPS,
					gridRef[record.Country][record.SCC])
			}
			if err != nil {
				err = fmt.Errorf("In spatial reference file: %v. (SCC=%v, FIPS=%v).",
					err.Error(), record.SCC, record.FIPS)
				panic(err)
			}
			srgNum := matchedVal.(string)

			for pol, val := range record.ANN_EMIS {
				val.Gridded = make([]*sparse.SparseArray,
					len(grids))
				for i, grid := range grids {
					if _, ok := TotalGrid[grid]; !ok {
						TotalGrid[grid] = make(map[string]*sparse.SparseArray)
					}
					if _, ok := TotalGrid[grid][pol]; !ok {
						TotalGrid[grid][pol] = sparse.ZerosSparse(grid.Ny, grid.Nx)
					}

					srg := c.getSurrogate(srgNum, record.FIPS, grid,
						make([]string, 0))
					val.Gridded[i] = sparse.ZerosSparse(grid.Ny,
						grid.Nx)
					if srg != nil {
						val.Gridded[i] = srg.ScaleCopy(val.Val)
						TotalGrid[grid][pol].AddSparse(val.Gridded[i])
					}
					totals.Add(pol, grid.Name, val, i)
				}
			}
			OutputChan <- record
		}
	default:
		err = fmt.Errorf("Unknown sectorType %v", c.SectorType)
		panic(err)
	}
	close(OutputChan)
	Report.SectorResults[c.Sector][period].SpatialResults = totals
	c.ResultMaps(totals, TotalGrid, period)
	c.msgchan <- "Finished spatializing " + period + " " + c.Sector
	return
}

func (c *RunData) getSurrogate(srgNum, FIPS string, grid *spatialsrg.GridDef,
	upstreamSrgs []string) (srg *sparse.SparseArray) {

	tableName := grid.Name + "___" + srgNum
	status := Status.GetSrgStatus(tableName,
		filepath.Join(c.griddedSrgs, tableName+".shp"))
	switch {
	case status == "Generating" || status == "Waiting to generate" ||
		status == "Empty":
		if status == "Empty" {
			Status.Surrogates[tableName] = "Waiting to generate"
		}
		srgGenData := NewSrgGenData(srgNum, grid)
		SurrogateGeneratorChan <- srgGenData
		err := <-srgGenData.finishedChan
		if err != nil {
			panic(err)
		}
	case status == "Ready":
	default:
		panic(fmt.Sprintf("Unknown status \"%v\"", status))
	}
	srg = c.retrieveSurrogate(srgNum, FIPS, grid, upstreamSrgs)
	return
}

// It is important not to edit the returned surrogate in place, because the
// same copy is used over and over again.
func (c *RunData) retrieveSurrogate(srgNum, FIPS string, grid *spatialsrg.GridDef,
	upstreamSrgs []string) *sparse.SparseArray {

	// Start the surrogate cacher, but only start it once.
	//go srgCacheStart.Do(srgCacher)

	var err error
	cacheKey := grid.Name + srgNum + FIPS

	// Check if surrogate is already in the cache.
	if srg, found := srgCache.Get(cacheKey); found {
		return srg.(*sparse.SparseArray)
	}

	var srg *sparse.SparseArray
	secondarySrg := srgSpec[srgNum].SECONDARYSURROGATE
	tertiarySrg := srgSpec[srgNum].TERTIARYSURROGATE
	quarternarySrg := srgSpec[srgNum].QUARTERNARYSURROGATE
	MergeFunction := srgSpec[srgNum].MergeFunction
	if MergeFunction == nil {
		srg, err = spatialsrg.RetrieveGriddingSurrogate(
			srgNum, FIPS, grid)
		if err != nil {
			panic(err)
		}

		// Try backup surrogates if the surrogate sums to zero.
		if srg == nil || srg.Sum() == 0. {
			backupNames := []string{secondarySrg, tertiarySrg, quarternarySrg}
			for _, backupName := range backupNames {
				if backupName != "" {
					newSrgNum := srgCodes[backupName]
					if !IsStringInArray(upstreamSrgs, newSrgNum) {
						srg = c.getSurrogate(newSrgNum, FIPS, grid,
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
			newSrgNum, ok := srgCodes[mrgval.name]
			if !ok {
				panic("No match for surrogate named " + mrgval.name)
			}
			weight := mrgval.val
			tempSrg := c.retrieveSurrogate(newSrgNum, FIPS, grid,
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
	srgCache.Set(cacheKey, srg, 0)
	return srg
}

type SrgGenData struct {
	srgNum       string
	grid         *spatialsrg.GridDef
	finishedChan chan error
}

func NewSrgGenData(srgNum string, grid *spatialsrg.GridDef) (
	d *SrgGenData) {
	d = new(SrgGenData)
	d.srgNum = srgNum
	d.grid = grid
	d.finishedChan = make(chan error)
	return
}

// Generate spatial surrogates
func (c *RunData) SurrogateGenerator() {
	for srgData := range SurrogateGeneratorChan {
		var err error
		srgNum := srgData.srgNum
		c.Log(srgSpec[srgNum], 2)
		if _, ok := srgSpec[srgNum]; !ok {
			err := fmt.Errorf("There is no surrogate specification for surrogate "+
				"number %v. This needs to be fixed in %v.", srgNum, c.SrgSpecFile)
			Status.Surrogates[srgData.grid.Name+"___"+srgNum] = "Failed!"
			srgData.finishedChan <- err
			continue
		}
		MergeFunction := srgSpec[srgNum].MergeFunction
		if MergeFunction == nil {
			err = c.genSrgNoMerge(srgData)
		} else {
			err = c.genSrgMerge(srgData)
		}
		srgData.finishedChan <- err
	}
}

// Generate a surrogate that doesn't require merging
func (c *RunData) genSrgNoMerge(srgData *SrgGenData) (err error) {
	srgNum := srgData.srgNum
	grid := srgData.grid
	inputMap := srgSpec[srgNum].DATASHAPEFILE
	inputColumn := srgSpec[srgNum].DATAATTRIBUTE
	surrogateMap := srgSpec[srgNum].WEIGHTSHAPEFILE
	WeightColumns := srgSpec[srgNum].WeightColumns
	FilterFunction := srgSpec[srgNum].FilterFunction
	Status.Surrogates[grid.Name+"___"+srgNum] = "Generating"
	inputFilePath := filepath.Join(c.shapefiles, inputMap+".shp")
	surrogateFilePath := filepath.Join(c.shapefiles, surrogateMap+".shp")
	err = spatialsrg.CreateGriddingSurrogate(srgNum, inputFilePath,
		inputColumn, surrogateFilePath, WeightColumns, FilterFunction,
		grid, c.griddedSrgs, c.slaves)
	if err == nil {
		Status.Surrogates[grid.Name+"___"+srgNum] = "Ready"
		return
	} else {
		Status.Surrogates[grid.Name+"___"+srgNum] = "Failed!"
		err = fmt.Errorf("Surrogate %v_%v Failed!\n%v",
			grid.Name, srgNum, err.Error())
		return
	}
}

// surrogate merging: create a surrogate from other surrogates
// Here, we just create weight surrogate tables if they don't exist.
// The actual merging happens elsewhere.
func (c *RunData) genSrgMerge(srgData *SrgGenData) (err error) {
	srgNum := srgData.srgNum
	grid := srgData.grid
	c.Log(srgSpec[srgNum], 2)
	MergeFunction := srgSpec[srgNum].MergeFunction
	Status.Surrogates[grid.Name+"___"+srgNum] = "Generating"
	for _, mrgval := range MergeFunction {
		newSrgNum, ok := srgCodes[mrgval.name]
		if !ok {
			err = fmt.Errorf("No match for surrogate named %v.", mrgval.name)
			return
		}
		tableName := grid.Name + "___" + newSrgNum
		filename := filepath.Join(c.griddedSrgs, tableName+".shp")
		status := Status.GetSrgStatus(tableName, filename)
		switch {
		case status == "Generating" || status == "Waiting to generate" ||
			status == "Empty":
			newSrgData := NewSrgGenData(newSrgNum, grid)
			newMergeFunction := srgSpec[newSrgNum].MergeFunction
			if newMergeFunction == nil {
				err = c.genSrgNoMerge(newSrgData)
			} else {
				err = c.genSrgMerge(newSrgData)
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
	Status.Surrogates[grid.Name+"___"+srgNum] = "Ready"
	return
}

type srgSpecHolder struct {
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
	FilterFunction       *spatialsrg.SurrogateFilter
	MergeFunction        []*SrgMerge
}

type SrgMerge struct {
	name string
	val  float64
}

func (c *RunData) SurrogateSpecification() (err error) {
	var record []string
	fid, err := os.Open(c.SrgSpecFile)
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
					err.Error(), c.SrgSpecFile, record)
				return
			}
		}
		if firstLine {
			firstLine = false
			continue
		}
		srg := new(srgSpecHolder)
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
			srg.FilterFunction = spatialsrg.NewSurrogateFilter()
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

		srgSpec[srg.REGION+srg.SURROGATECODE] = srg
		srgCodes[srg.REGION+srg.SURROGATE] = srg.REGION + srg.SURROGATECODE
	}
	return
}

// GridRef reads the SMOKE gref file, which maps FIPS and SCC codes to grid surrogates
func (c *RunData) GridRef() (err error) {
	var record string
	fid, err := os.Open(c.GridRefFile)
	if err != nil {
		err = fmt.Errorf("GridRef: %v \nFile= %v\nRecord= ",
			err.Error(), c.GridRefFile, record)
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
					err.Error(), c.GridRefFile, record)
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
			country := getCountryName(splitLine[0][0:1])
			FIPS := splitLine[0][1:]
			srg := strings.Trim(splitLine[2], "\"\n")

			if _, ok := gridRef[country]; !ok {
				gridRef[country] = make(map[string]map[string]interface{})
			}
			if _, ok := gridRef[country][SCC]; !ok {
				gridRef[country][SCC] = make(map[string]interface{})
			}
			gridRef[country][SCC][FIPS] = country + srg
		}
	}
	return
}
