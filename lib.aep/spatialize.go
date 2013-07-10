package aep

import (
	"bitbucket.org/ctessum/aep/gis"
	"bitbucket.org/ctessum/aep/sparse"
	"bufio"
	"encoding/csv"
	"fmt"
	"github.com/pmylund/go-cache"
	"io"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
	"time"
)

var (
	srgSpec                = make(map[string]*srgSpecHolder)
	srgCodes               = make(map[string]string)
	grids                  = make([]*gis.GridDef, 0)
	gridRef                = make(map[string]map[string]map[string]string)
	SurrogateGeneratorChan = make(chan *SrgGenData)
	//	srgCacheStart          sync.Once
	//	srgCacheRequestChan    = make(chan *srgCacheRequest)
	srgCache *cache.Cache
)

func (c *RunData) PGconnect() (pg *gis.PostGis, err error) {
	pg, err = gis.Connect(c.PostGISuser, c.PostGISdatabase,
		c.PostGISpassword, c.OtherLibpqConnectionParameters)
	return
}

func (c *RunData) SpatialSetup(e *ErrCat) {
	c.Log("Setting up spatial environment...", 1)
	gis.DebugLevel = c.DebugLevel
	pg, err := c.PGconnect()
	defer pg.Disconnect()
	e.Add(err)
	x := c.wrfData
	var proj string
	switch x.Map_proj {
	case "lambert":
		proj = "lcc"
	default:
		e.Add("ERROR: \"lambert\" is the only map projection" +
			" that is currently supported (your projection is " +
			x.Map_proj + ").")
	}
	projInfo := new(gis.ParsedProj4)
	projInfo.SRID = c.SRID
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
	e.Add(pg.NewProjection(projInfo))
	if c.RegenerateSpatialData {
		pg.DropSchema(c.SimulationName)
	}
	pg.CreateSchema(c.SimulationName)

	for i := 0; i < x.Max_dom; i++ {
		grid := gis.NewGrid(x.DomainNames[i], x.Nx[i], x.Ny[i],
			x.Dx[i], x.Dy[i], x.W[i], x.S[i], c.SRID, c.SimulationName)

		if !pg.TableExists(c.SimulationName, grid.Name) {
			e.Add(pg.CreateGrid(grid, c.ShapefileSchema))
		}
		e.Add(pg.GridAddTimeZones(grid, c.ShapefileSchema))
		grids = append(grids, grid)
	}
	t := c.SrgCacheExpirationTime
	srgCache = cache.New(t*time.Minute, t*time.Minute)
	e.Add(c.SurrogateSpecification())
	e.Add(c.GridRef())
	go c.SurrogateGenerator()
	return
}

type SpatialTotals struct {
	InsideDomainTotals  map[string]map[string]*specValUnits
	OutsideDomainTotals map[string]map[string]*specValUnits
}

func newSpatialTotalHolder() *SpatialTotals {
	out := new(SpatialTotals)
	out.InsideDomainTotals = make(map[string]map[string]*specValUnits)
	out.OutsideDomainTotals = make(map[string]map[string]*specValUnits)
	return out
}

func (h *SpatialTotals) Add(pol, grid string, data *specValUnits, i int) {
	t := *h
	if _, ok := t.InsideDomainTotals[grid]; !ok {
		t.InsideDomainTotals[grid] = make(map[string]*specValUnits)
		t.OutsideDomainTotals[grid] = make(map[string]*specValUnits)
	}
	if _, ok := t.InsideDomainTotals[grid][pol]; !ok {
		t.InsideDomainTotals[grid][pol] = new(specValUnits)
		t.InsideDomainTotals[grid][pol].Units = data.Units
		t.OutsideDomainTotals[grid][pol] = new(specValUnits)
		t.OutsideDomainTotals[grid][pol].Units = data.Units
	} else {
		if t.InsideDomainTotals[grid][pol].Units != data.Units {
			err := fmt.Errorf("Units problem: %v! = %v",
				t.InsideDomainTotals[grid][pol].Units, data.Units)
			panic(err)
		}
	}
	gridTotal := data.gridded[i].Sum()
	t.InsideDomainTotals[grid][pol].Val += gridTotal
	t.OutsideDomainTotals[grid][pol].Val += data.Val - gridTotal
	*h = t
}

func (c *RunData) Spatialize(InputChan chan *ParsedRecord,
	OutputChan chan *ParsedRecord, period string) {
	defer c.ErrorRecoverCloseChan(InputChan)
	var err error

	c.Log("Spatializing "+period+" "+c.Sector+"...", 0)

	totals := newSpatialTotalHolder()
	TotalGrid := make(map[*gis.GridDef]map[string]*sparse.SparseArray) // map[grid][pol]data

	pg, err := c.PGconnect()
	defer pg.Disconnect()

	switch c.SectorType {
	case "point":
		for record := range InputChan {
			// Convert Lat/Lon to projection
			record.PointXcoord, record.PointYcoord, err =
				pg.ProjectPoint(record.YLOC, record.XLOC, c.SRID)
			if err != nil {
				panic(err)
			}

			emisInRecord := false
			for pol, data := range record.ANN_EMIS {
				data.gridded = make([]*sparse.SparseArray, len(grids))
				for i, grid := range grids {
					if _, ok := TotalGrid[grid]; !ok {
						TotalGrid[grid] = make(map[string]*sparse.SparseArray)
					}
					if _, ok := TotalGrid[grid][pol]; !ok {
						TotalGrid[grid][pol] =
							sparse.ZerosSparse(grid.Ny, grid.Nx)
					}

					data.gridded[i] = sparse.ZerosSparse(grid.Ny,
						grid.Nx)
					row := int((record.PointYcoord - grid.Y0) /
						grid.Dy)
					col := int((record.PointXcoord - grid.X0) /
						grid.Dx)
					if row > 0 && row < grid.Ny &&
						col > 0 && col < grid.Nx {
						if data.Val != 0. {
							emisInRecord = true
						}
						data.gridded[i].Set(data.Val, row, col)
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
			var matchedSCC string
			if !c.MatchFullSCC {
				matchedSCC, err = MatchCode2(record.SCC, gridRef[record.Country])
				if err != nil {
					panic(err)
				}
			} else {
				matchedSCC = record.SCC
			}
			matchedFIPS, err := MatchCode3(record.FIPS,
				gridRef[record.Country][matchedSCC])
			if err != nil {
				panic(err)
			}
			srgNum := gridRef[record.Country][matchedSCC][matchedFIPS]

			for pol, val := range record.ANN_EMIS {
				val.gridded = make([]*sparse.SparseArray,
					len(grids))
				for i, grid := range grids {
					if _, ok := TotalGrid[grid]; !ok {
						TotalGrid[grid] = make(map[string]*sparse.SparseArray)
					}
					if _, ok := TotalGrid[grid][pol]; !ok {
						TotalGrid[grid][pol] = sparse.ZerosSparse(grid.Ny, grid.Nx)
					}

					val.gridded[i] = c.getSurrogate(srgNum, record.FIPS, grid, pg,
						make([]string, 0))
					val.gridded[i].Scale(val.Val)
					TotalGrid[grid][pol].AddSparse(val.gridded[i])
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
	c.ResultMaps(TotalGrid, period, pg)
	c.msgchan <- "Finished spatializing " + period + " " + c.Sector
	return
}

func (c *RunData) getSurrogate(srgNum, FIPS string, grid *gis.GridDef,
	pg *gis.PostGis, upstreamSrgs []string) (srg *sparse.SparseArray) {

	tableName := grid.Name + "_" + srgNum
	status := Status.GetSrgStatus(tableName, grid.Schema, pg)
	switch {
	case status == "Generating" || status == "Waiting to generate" ||
		status == "Empty":
		if status == "Empty" {
			Status.Surrogates[tableName] = "Waiting to generate"
		}
		srgGenData := NewSrgGenData(srgNum, grid, pg)
		SurrogateGeneratorChan <- srgGenData
		err := <-srgGenData.finishedChan
		if err != nil {
			panic(err)
		}
	case status == "Ready":
	default:
		panic(fmt.Sprintf("Unknown status \"%v\"", status))
	}
	srg = c.retrieveSurrogate(srgNum, FIPS, grid, pg, upstreamSrgs)
	return
}

//func srgCacher() {
//	srgCache := make(map[string]map[string]map[string]*sparse.SparseArray)
//	for r := range srgCacheRequestChan {
//		switch {
//		case r.srg == nil && r.returnChan != nil: // retrieve from cache
//			srg, ok := srgCache[r.grid][r.srgNum][r.FIPS]
//			if ok {
//				r.returnChan <- srg
//			} else {
//				r.returnChan <- nil
//			}
//		case r.srg != nil && r.returnChan == nil: // add to cache
//			if _, ok := srgCache[r.grid]; !ok {
//				srgCache[r.grid] = make(map[string]map[string]*sparse.SparseArray)
//			}
//			if _, ok := srgCache[r.grid][r.srgNum]; !ok {
//				srgCache[r.grid][r.srgNum] = make(map[string]*sparse.SparseArray)
//			}
//			srgCache[r.grid][r.srgNum][r.FIPS] = r.srg
//		}
//	}
//}
//
//type srgCacheRequest struct {
//	grid       string
//	srgNum     string
//	FIPS       string
//	srg        *sparse.SparseArray
//	returnChan chan *sparse.SparseArray
//}

func (c *RunData) retrieveSurrogate(srgNum, FIPS string, grid *gis.GridDef,
	pg *gis.PostGis, upstreamSrgs []string) *sparse.SparseArray {

	// Start the surrogate cacher, but only start it once.
	//go srgCacheStart.Do(srgCacher)

	var err error
	cacheKey := grid.Name + srgNum + FIPS

	// Check if surrogate is already in the cache.
	if srg, found := srgCache.Get(cacheKey); found {
		// send copy so original is not altered.
		return srg.(*sparse.SparseArray).Copy()
	}
	//srgRequest := srgCacheRequest{grid.Name, srgNum, FIPS, nil,
	//	make(chan *sparse.SparseArray)}
	//srgCacheRequestChan <- &srgRequest
	//srg := <-srgRequest.returnChan
	//if srg != nil {

	var srg *sparse.SparseArray
	secondarySrg := srgSpec[srgNum].SECONDARYSURROGATE
	tertiarySrg := srgSpec[srgNum].TERTIARYSURROGATE
	quarternarySrg := srgSpec[srgNum].QUARTERNARYSURROGATE
	MergeFunction := srgSpec[srgNum].MergeFunction
	if MergeFunction == nil {

		srg, err = pg.RetrieveGriddingSurrogate(srgNum, FIPS,
			c.SimulationName, grid)
		if err != nil {
			panic(err)
		}

		// Try backup surrogates if the surrogate sums to zero.
		if srg.Sum() == 0. {
			backupNames := []string{secondarySrg, tertiarySrg, quarternarySrg}
			for _, backupName := range backupNames {
				if backupName != "" {
					newSrgNum := srgCodes[backupName]
					if !IsStringInArray(upstreamSrgs, newSrgNum) {
						srg = c.getSurrogate(newSrgNum, FIPS, grid, pg,
							append(upstreamSrgs, newSrgNum))
					}
				}
				if srg.Sum() > 0. {
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
			tempSrg := c.retrieveSurrogate(newSrgNum, FIPS, grid, pg,
				append(upstreamSrgs, newSrgNum))
			tempSrg.Scale(weight)
			srg.AddSparse(tempSrg)

		}
	}
	srgSum := srg.Sum()
	if srgSum > 1.001 || math.IsNaN(srgSum) {
		err = fmt.Errorf("Sum for surrogate !<= 1.0: %v", srgSum)
		panic(err)
	}
	// store surrogate in cache for faster access.
	srgCache.Set(cacheKey, srg, 0)
	//srgSendRequest := srgCacheRequest{grid.Name, srgNum, FIPS, srg, nil}
	//srgCacheRequestChan <- &srgSendRequest
	return srg.Copy()
}

type SrgGenData struct {
	srgNum       string
	grid         *gis.GridDef
	pg           *gis.PostGis
	finishedChan chan error
}

func NewSrgGenData(srgNum string, grid *gis.GridDef, pg *gis.PostGis) (
	d *SrgGenData) {
	d = new(SrgGenData)
	d.srgNum = srgNum
	d.grid = grid
	d.pg = pg
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
			Status.Surrogates[srgData.grid.Name+"_"+srgNum] = "Failed!"
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
	Status.Surrogates[grid.Name+"_"+srgNum] = "Generating"
	err = gis.CreateGriddingSurrogate(c, srgNum, inputMap,
		inputColumn, surrogateMap, WeightColumns, FilterFunction,
		grid, c.ShapefileSchema)
	if err == nil {
		Status.Surrogates[grid.Name+"_"+srgNum] = "Ready"
		return
	} else {
		Status.Surrogates[grid.Name+"_"+srgNum] = "Failed!"
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
	pg := srgData.pg
	c.Log(srgSpec[srgNum], 2)
	MergeFunction := srgSpec[srgNum].MergeFunction
	Status.Surrogates[grid.Name+"_"+srgNum] = "Generating"
	for _, mrgval := range MergeFunction {
		newSrgNum, ok := srgCodes[mrgval.name]
		if !ok {
			err = fmt.Errorf("No match for surrogate named %v.", mrgval.name)
			return
		}
		tableName := grid.Name + "_" + newSrgNum
		status := Status.GetSrgStatus(tableName, grid.Schema, pg)
		switch {
		case status == "Generating" || status == "Waiting to generate" ||
			status == "Empty":
			newSrgData := NewSrgGenData(newSrgNum, grid, pg)
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
	Status.Surrogates[grid.Name+"_"+srgNum] = "Ready"
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
	WeightColumns        string
	FilterFunction       *gis.SurrogateFilter
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
			srg.WeightColumns = srg.WEIGHTATTRIBUTE
		} else if srg.WEIGHTFUNCTION != "NONE" && srg.WEIGHTFUNCTION != "" {
			srg.WeightColumns = srg.WEIGHTFUNCTION
		}

		// Parse filter function
		if srg.FILTERFUNCTION != "NONE" && srg.FILTERFUNCTION != "" {
			srg.FilterFunction = gis.NewSurrogateFilter()
			s := make([]string, 0)
			if strings.Index(srg.FILTERFUNCTION, "!=") != -1 {
				srg.FilterFunction.EqualNotEqual = "NotEqual"
				s = strings.Split(srg.FILTERFUNCTION, "!=")
			} else {
				srg.FilterFunction.EqualNotEqual = "Equal"
				s = strings.Split(srg.FILTERFUNCTION, "=")
			}
			srg.FilterFunction.Column = s[0]
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
			country := GetCountryName(splitLine[0][0:1])
			FIPS := splitLine[0][1:]
			srg := strings.Trim(splitLine[2], "\"\n")

			if _, ok := gridRef[country]; !ok {
				gridRef[country] = make(map[string]map[string]string)
			}
			if _, ok := gridRef[country][SCC]; !ok {
				gridRef[country][SCC] = make(map[string]string)
			}
			gridRef[country][SCC][FIPS] = country + srg
		}
	}
	return
}
