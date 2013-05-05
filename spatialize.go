package main

import (
	"bitbucket.org/ctessum/aep/gis"
	"bufio"
	"encoding/csv"
	"fmt"
	"github.com/skelterjohn/go.matrix"
	"io"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
	"time"
)

var (
	srgSpec             = make(map[string]*srgSpecHolder)
	srgCodes            = make(map[string]string)
	pendingSurrogates   = make(map[string]string)
	grids               = make([]*gis.GridDef, 0)
	gridRef             = make(map[string]map[string]map[string]string)
	SrgsMissingCoverage = make(map[string]map[string][]string) // map[grid][srgNum]{FIPS}
)

func (c *RunData) SpatialSetup(e *ErrCat) {
	c.Log("Setting up spatial environment...", 0)
	gis.DebugLevel = c.DebugLevel
	pg, err := gis.Connect(c.PostGISuser, c.PostGISdatabase,
		c.PostGISpassword)
	defer pg.Disconnect()
	if err != nil {
		e.Add(err)
		return
	}
	x := c.wpsData
	var proj string
	switch x.map_proj {
	case "lambert":
		proj = "lcc"
	default:
		log.Fatalln("ERROR: \"lambert\" is the only map projection" +
			" that is currently supported (your projection is " +
			x.map_proj + ").")
	}
	err = pg.NewProjection(c.SRID, proj, x.truelat1,
		x.truelat2, x.ref_lat, x.ref_lon, c.EarthRadius)
	if err != nil {
		e.Add(err)
		return
	}
	if c.RegenerateSpatialData {
		pg.DropSchema(c.SimulationName)
	}
	pg.CreateSchema(c.SimulationName)

	for i := 0; i < x.max_dom; i++ {
		grid := gis.NewGrid(x.domainName[i], x.nx[i], x.ny[i],
			x.dx[i], x.dy[i], x.W[i], x.S[i], c.SRID, c.SimulationName)

		if !pg.TableExists(c.SimulationName, grid.Name) {
			err = pg.CreateGrid(grid)
			e.Add(err)
		}
		grids = append(grids, grid)
	}
	return
}

// If this function fails during surrogate creation, the program may
// hang as other instances of this function wait for the surrogate
// to be completed.
func (c *RunData) spatialize(MesgChan chan string, InputChan chan *ParsedRecord,
	OutputChan chan *ParsedRecord, period string) {
	defer c.ErrorRecoverCloseChan(MesgChan, InputChan)
	var err error

	c.Log("Spatializing "+period+" "+c.Sector+"...", 0)

	TotalGrid := make(map[*gis.GridDef]map[string]*matrix.SparseMatrix) // map[grid][pol]data
	DroppedTotals := make(map[string]map[string]float64)

	pg, err := gis.Connect(c.PostGISuser, c.PostGISdatabase,
		c.PostGISpassword)
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

			for pol, data := range record.ANN_EMIS {
				data.gridded = make([]*matrix.SparseMatrix, len(grids))
				for i, grid := range grids {
					if _, ok := TotalGrid[grid]; !ok {
						TotalGrid[grid] = make(map[string]*matrix.SparseMatrix)
						DroppedTotals[grid.Name] = make(map[string]float64)
					}
					if _, ok := TotalGrid[grid][pol]; !ok {
						TotalGrid[grid][pol] =
							matrix.ZerosSparse(grid.Ny, grid.Nx)
					}

					data.gridded[i] = matrix.ZerosSparse(grid.Ny,
						grid.Nx)
					row := int((record.PointYcoord - grid.Y0) /
						grid.Dy)
					col := int((record.PointXcoord - grid.X0) /
						grid.Dx)
					if row > 0 && row < grid.Ny &&
						col > 0 && col < grid.Nx {
						data.gridded[i].Set(row, col, data.val)
						TotalGrid[grid][pol].Add(data.gridded[i])
					} else {
						DroppedTotals[grid.Name][pol] += MatrixSum(data.gridded[i])
					}
				}
			}
			OutputChan <- record
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
				val.gridded = make([]*matrix.SparseMatrix,
					len(grids))
				for i, grid := range grids {
					if _, ok := TotalGrid[grid]; !ok {
						TotalGrid[grid] = make(map[string]*matrix.SparseMatrix)
						DroppedTotals[grid.Name] = make(map[string]float64)
					}
					if _, ok := TotalGrid[grid][pol]; !ok {
						TotalGrid[grid][pol] = matrix.ZerosSparse(grid.Ny, grid.Nx)
					}

					val.gridded[i] = c.getSurrogate(srgNum, record.FIPS, grid, pg)
					if MatrixSum(val.gridded[i]) != 0. {
						val.gridded[i].Scale(val.val)
						TotalGrid[grid][pol].Add(val.gridded[i])
					} else {
						DroppedTotals[grid.Name][pol] += val.val
					}
				}
			}
			OutputChan <- record
		}
	default:
		err = fmt.Errorf("Unknown sectorType %v", c.SectorType)
		panic(err)
	}
	if OutputChan != TotalReportChan {
		close(OutputChan)
	}
	c.SpatialReport(TotalGrid, DroppedTotals, period, pg)
	MesgChan <- "Finished spatializing " + period + " " + c.Sector
	return
}

func (c *RunData) getSurrogate(srgNum, FIPS string, grid *gis.GridDef,
	pg *gis.PostGis) (srg *matrix.SparseMatrix) {

	tableName := grid.Name + "_" + srgNum
	if !pg.TableExists(c.SimulationName, tableName) {
		// Make sure this surrogate isn't being generated by another instance
		// of this function.
		if _, ok := pendingSurrogates[srgNum]; ok {
			WaitSrgToFinish(srgNum)
		} else {
			pendingSurrogates[srgNum] = ""
			c.generateSurrogate(srgNum, grid, pg)
			delete(pendingSurrogates, srgNum)
		}
	}
	srg = c.retrieveSurrogate(srgNum, FIPS, grid, pg)
	return
}

func (c *RunData) retrieveSurrogate(srgNum, FIPS string, grid *gis.GridDef,
	pg *gis.PostGis) (srg *matrix.SparseMatrix) {

	if _, ok := SrgsMissingCoverage[grid.Name][srgNum]; ok {
		if IsStringInArray(SrgsMissingCoverage[grid.Name][srgNum], FIPS) {
			srg = matrix.ZerosSparse(grid.Ny, grid.Nx)
			return
		}
	}

	var err error
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
		if MatrixSum(srg) == 0 {
			AddToSrgsMissingCoverage(grid.Name, srgNum, FIPS)
			backupNames := []string{secondarySrg, tertiarySrg, quarternarySrg}
			for _, backupName := range backupNames {
				if backupName != "" {
					newSrgNum := srgCodes[backupName]
					srg = c.getSurrogate(newSrgNum, FIPS, grid, pg)
					if MatrixSum(srg) == 0 {
						AddToSrgsMissingCoverage(grid.Name, newSrgNum, FIPS)
					} else {
						break
					}
				}
			}
		}
	} else {
		srg = matrix.ZerosSparse(grid.Ny, grid.Nx)
		for _, mrgval := range MergeFunction {
			newSrgNum, ok := srgCodes[mrgval.name]
			if !ok {
				panic("No match for surrogate named " + mrgval.name)
			}
			weight := mrgval.val
			tempSrg := c.retrieveSurrogate(newSrgNum, FIPS, grid, pg)
			tempSrg.Scale(weight)
			srg.AddSparse(tempSrg)

		}
	}
	srgSum := MatrixSum(srg)
	if srgSum > 1.000001 || math.IsNaN(srgSum) {
		err = fmt.Errorf("Sum for surrogate !<= 1.0: %v", srgSum)
		panic(err)
	}
	return
}

func AddToSrgsMissingCoverage(gridName, srgNum, FIPS string) {
	if _, ok := SrgsMissingCoverage[gridName]; !ok {
		SrgsMissingCoverage[gridName] = make(map[string][]string)
	}
	if _, ok := SrgsMissingCoverage[gridName][srgNum]; !ok {
		SrgsMissingCoverage[gridName][srgNum] = make([]string, 0)
	}
	SrgsMissingCoverage[gridName][srgNum] =
		append(SrgsMissingCoverage[gridName][srgNum], FIPS)
}

// Generate spatial surrogates
func (c *RunData) generateSurrogate(srgNum string, grid *gis.GridDef,
	pg *gis.PostGis) {
	var err error
	c.Log(srgSpec[srgNum], 2)
	inputMap := srgSpec[srgNum].DATASHAPEFILE
	inputColumn := srgSpec[srgNum].DATAATTRIBUTE
	surrogateMap := srgSpec[srgNum].WEIGHTSHAPEFILE
	WeightColumns := srgSpec[srgNum].WeightColumns
	FilterFunction := srgSpec[srgNum].FilterFunction
	MergeFunction := srgSpec[srgNum].MergeFunction
	if MergeFunction == nil {
		err = pg.CreateGriddingSurrogate(srgNum, inputMap,
			inputColumn, surrogateMap, WeightColumns, FilterFunction,
			grid, c.ShapefileSchema)
		if err != nil {
			delete(pendingSurrogates, srgNum)
			panic(err)
		}
	} else { // surrogate merging: create a surrogate from other surrogates
		// Here, we just create weight surrogate tables if they don't exist.
		// The actual merging happens elsewhere.
		for _, mrgval := range MergeFunction {
			newSrgNum, ok := srgCodes[mrgval.name]
			if !ok {
				panic("No match for surrogate named " + mrgval.name)
			}
			tableName := grid.Name + "_" + newSrgNum
			if !pg.TableExists(c.SimulationName, tableName) {
				// Make sure this surrogate isn't being generated by another instance
				// of this function.
				if _, ok := pendingSurrogates[newSrgNum]; ok {
					WaitSrgToFinish(newSrgNum)
				} else {
					pendingSurrogates[newSrgNum] = ""
					c.generateSurrogate(newSrgNum, grid, pg)
					delete(pendingSurrogates, newSrgNum)
				}
			}
		}
	}
}

func WaitSrgToFinish(srgNum string) {
	for {
		if _, ok := pendingSurrogates[srgNum]; ok {
			time.Sleep(100 * time.Millisecond)
			fmt.Println("x")
		} else {
			break
		}
	}
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
	defer fid.Close()
	if err != nil {
		return
	}
	reader := csv.NewReader(fid)
	reader.Comment = '#'
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

		if record[0] != '#' {
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
