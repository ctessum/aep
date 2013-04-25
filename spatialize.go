package main

import (
	"bitbucket.org/ctessum/gonetcdf"
	"bitbucket.org/ctessum/grassutils"
	"bufio"
	"encoding/csv"
	"fmt"
	"github.com/skelterjohn/go.matrix"
	"io"
	"log"
	"math"
	"os"
	"path"
	"strconv"
	"strings"
	"time"
)

var (
	surrogateChan      = make(chan string)
	srgs               = make(map[string]map[string][]*matrix.SparseMatrix)
	srgSpec            = make(map[string]*srgSpecHolder)
	srgCodes           = make(map[string]string)
	pendingSurrogates  = make(map[string]string)
	finishedSurrogates = make(map[string]string)
	regions            = make([]*grassutils.Region, 0)
	gridRef            = make(map[string]map[string]string)
	savedSurrogates    = make([]string, 0)
)

func (c *RunData) SpatialSetup(e *ErrCat) {
	c.Log("Setting up GRASS spatial environment...", 0)
	grassutils.DebugLevel = c.DebugLevel
	err := grassutils.CheckGrassEnv()
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
	err = grassutils.NewLocation(c.SimulationName, proj, x.truelat1, x.truelat2, x.ref_lat,
		x.ref_lon, c.EarthRadius)
	if err != nil {
		e.Add(err)
		return
	}
	if c.RegenerateSpatialData {
		err = grassutils.DeleteAll()
		if err != nil {
			e.Add(err)
			return
		}
	}
	//	if !grassutils.IsMap(c.LineMap, "vect") {
	//		err = grassutils.Vproj(c.LineMap, c.SpatialDataLoc, c.SpatialDataMapset)
	//		if err != nil {
	//			e.Add(err)
	//			return
	//		}
	//	}
	x.S = make([]float64, x.max_dom)
	x.W = make([]float64, x.max_dom)
	x.S[0] = 0 - (x.ref_y-0.5)*x.dy0
	x.W[0] = 0 - (x.ref_x-0.5)*x.dx0
	x.dx = make([]float64, x.max_dom)
	x.dy = make([]float64, x.max_dom)
	x.dx[0] = x.dx0
	x.dy[0] = x.dy0
	x.nx = make([]int, x.max_dom)
	x.ny = make([]int, x.max_dom)
	x.nx[0] = x.e_we[0] - 1
	x.ny[0] = x.e_sn[0] - 1
	x.mapsetName = make([]string, x.max_dom)
	for i := 1; i < x.max_dom; i++ {
		x.S[i] = x.S[x.parent_id[i]-1] + float64(x.j_parent_start[i]-1)*x.dy0
		x.W[i] = x.W[x.parent_id[i]-1] + float64(x.i_parent_start[i]-1)*x.dx0
		x.dx[i] = x.dx[x.parent_id[i]-1] / x.parent_grid_ratio[i]
		x.dy[i] = x.dy[x.parent_id[i]-1] / x.parent_grid_ratio[i]
		x.nx[i] = x.e_we[i] - 1
		x.ny[i] = x.e_sn[i] - 1
	}
	for i := 0; i < x.max_dom; i++ {
		x.mapsetName[i] = fmt.Sprintf("d%02v", i+1)
		region, err := grassutils.NewRegion(x.S[i], x.W[i],
			x.dx[i], x.dy[i], x.nx[i], x.ny[i])
		if !grassutils.IsMap(x.mapsetName[i], "vect") {
			region.CreateGrid(x.mapsetName[i])
			if err != nil {
				e.Add(err)
				return
			}
		}
		regions = append(regions, region)
	}
	// Load surrogates from netCDF file, or create file to save surrogates to.
	for i, region := range regions {
		surrogateSaveLoc := path.Join(grassutils.GrassDB, c.SimulationName,
			"surrogates_"+x.mapsetName[i]+".nc")
		if c.RegenerateSpatialData {
			c.Log("Creating new surrogate file "+surrogateSaveLoc, 0)
			CreateSavedSurrogates(surrogateSaveLoc, region)
		} else {
			c.Log("Loading surrogate file "+surrogateSaveLoc, 0)
			LoadSavedSurrogates(surrogateSaveLoc, region, i)
		}
	}
	return
}

func (c *RunData) spatialize(MesgChan chan string, InputChan chan *ParsedRecord,
	OutputChan chan *ParsedRecord, period string) {
	defer c.ErrorRecoverCloseChan(MesgChan, InputChan)
	var err error
	reportChan := make(chan *ParsedRecord)
	finishedChan := make(chan int)
	go c.SpatialReport(reportChan, finishedChan, period)

	c.Log("Spatializing "+period+" "+c.Sector+"...", 0)

	switch c.SectorType {
	case "point":
		for record := range InputChan {
			// Convert Lat/Lon to projection
			record.PointYcoord, record.PointXcoord, err =
				grassutils.LatLonToProj(
					record.YLOC, record.XLOC)
			if err != nil {
				panic(err)
			}

			for _, data := range record.ANN_EMIS {
				data.gridded = make([]*matrix.SparseMatrix, len(regions))
				for i, region := range regions {
					data.gridded[i] = matrix.ZerosSparse(region.NY,
						region.NX)
					row := int((record.PointYcoord - region.South) /
						region.DY)
					col := int((record.PointXcoord - region.West) /
						region.DX)
					if row > 0 && row < region.NY &&
						col > 0 && col < region.NX {
						data.gridded[i].Set(row, col, data.val)
					}
				}
			}
			OutputChan <- record
			reportChan <- record
		}
	case "area":
		for record := range InputChan {
			var matchedSCC string
			if !c.MatchFullSCC {
				matchedSCC, err = MatchCode2(record.SCC, gridRef)
				if err != nil {
					panic(err)
				}
			} else {
				matchedSCC = record.SCC
			}
			matchedFIPS, err := MatchCode3(record.FIPS,
				gridRef[matchedSCC])
			if err != nil {
				panic(err)
			}
			srgNum := gridRef[matchedSCC][matchedFIPS]
			if _, ok := finishedSurrogates[srgNum]; !ok {
				surrogateChan <- srgNum
				WaitSrgToFinish(srgNum)
			}
			for _, val := range record.ANN_EMIS {
				val.gridded = make([]*matrix.SparseMatrix,
					len(regions))
				for i, mat := range srgs[srgNum][record.FIPS] {
					val.gridded[i] = mat.Copy()
					val.gridded[i].Scale(val.val)
				}
			}
			OutputChan <- record
			reportChan <- record
		}
	default:
		err = fmt.Errorf("Unknown sectorType %v", c.SectorType)
	}
	close(reportChan)
	if OutputChan != TotalReportChan {
		close(OutputChan)
	}
	<-finishedChan
	MesgChan <- "Finished spatializing " + period + " " + c.Sector
	return
}

// Generate spatial surrogates
func (c *RunData) surrogateGenerator() {
	var err error
	for srgNum := range surrogateChan {
		// Make sure this surrogate isn't being generated by another instance
		// of this function.
		if _, ok := pendingSurrogates[srgNum]; !ok {
			pendingSurrogates[srgNum] = ""
			c.Log(srgSpec[srgNum], 2)
			inputMap := srgSpec[srgNum].DATASHAPEFILE
			inputColumn := srgSpec[srgNum].DATAATTRIBUTE
			surrogateMap := srgSpec[srgNum].WEIGHTSHAPEFILE
			WeightColumns := srgSpec[srgNum].WeightColumns
			FilterFunction := srgSpec[srgNum].FilterFunction
			MergeFunction := srgSpec[srgNum].MergeFunction
			secondarySrg := srgSpec[srgNum].SECONDARYSURROGATE
			tertiarySrg := srgSpec[srgNum].TERTIARYSURROGATE
			quarternarySrg := srgSpec[srgNum].QUARTERNARYSURROGATE
			if MergeFunction == nil {
				c.gensrg(srgNum, inputMap, inputColumn, surrogateMap,
					WeightColumns, FilterFunction)
				// Try backup surrogates if the surrogate 
				// exists but is equal to zero.
				for inputID, _ := range srgs[srgNum] {
					for i, srg := range srgs[srgNum][inputID] {
						if MatrixSum(srg) == 0 {
							newSrgNum := srgCodes[secondarySrg]
							success := SubstituteBackupSurrogate(srgNum,
								newSrgNum, inputID, i)
							if !success {
								newSrgNum = srgCodes[tertiarySrg]
								success = SubstituteBackupSurrogate(srgNum,
									newSrgNum, inputID, i)
							}
							if !success {
								newSrgNum = srgCodes[quarternarySrg]
								success = SubstituteBackupSurrogate(srgNum,
									newSrgNum, inputID, i)
							} else {
								err = fmt.Errorf("No spatial coverage "+
									"for surrogate %v, input shape %v",
									srgNum, inputID)
								panic(err)
							}
						}
					}
				}
			} else { // surrogate merging: create a surrogate from other surrogates
				for j, mrgval := range MergeFunction {
					if j == 0 {
						srgs[srgNum] = make(map[string][]*matrix.SparseMatrix)
					}
					newSrgNum := srgCodes[mrgval.name]
					weight := mrgval.val
					// Create weight surrogate if it doesn't exist
					if _, ok := finishedSurrogates[newSrgNum]; !ok {
						surrogateChan <- newSrgNum
						WaitSrgToFinish(newSrgNum)
					}
					for inputID, _ := range srgs[newSrgNum] {
						if j == 0 {
							srgs[srgNum][inputID] = make(
								[]*matrix.SparseMatrix, len(regions))
						}
						for i, region := range regions {
							srgs[srgNum][inputID][i] =
								matrix.ZerosSparse(int(region.NY), int(region.NX))
							tempMat := srgs[srgNum][inputID][i].Copy()
							tempMat.Scale(weight)
							srgs[srgNum][inputID][i].
								AddSparse(tempMat)

							srgSum := MatrixSum(srgs[srgNum][inputID][i])
							if srgSum > 1.001 || srgSum < 0.999 || math.IsNaN(srgSum) {
								err = fmt.Errorf("Sum for surrogate != 1.0: %v", srgSum)
								panic(err)
							}
						}
					}
				}
			}
			delete(pendingSurrogates, srgNum)
			finishedSurrogates[srgNum] = ""
		}
	}
}

func WaitSrgToFinish(srgNum string) {
	for {
		if _, ok := finishedSurrogates[srgNum]; !ok {
			time.Sleep(100 * time.Millisecond)
		} else {
			break
		}
	}
}

func SubstituteBackupSurrogate(srgNum, newSrgNum, inputID string, i int) (
	success bool) {
	if _, ok := finishedSurrogates[newSrgNum]; !ok {
		surrogateChan <- newSrgNum
		WaitSrgToFinish(newSrgNum)
	}
	if MatrixSum(srgs[newSrgNum][inputID][i]) != 0 {
		srgs[srgNum][inputID][i] = srgs[newSrgNum][inputID][i]
		success = true
	} else {
		success = false
	}
	return
}

func (c *RunData) gensrg(srgNum, inputMap, inputColumn,
	surrogateMap string, WeightColumns []string,
	FilterFunction *grassutils.SurrogateFilter) {

	var err error
	if _, ok := srgs[srgNum]; !ok {
		srgs[srgNum] = make(map[string][]*matrix.SparseMatrix)
	}

	if !grassutils.IsMap(inputMap, "vect") {
		err = grassutils.Vproj(inputMap, c.SpatialDataLoc,
			c.SpatialDataMapset)
		if err != nil {
			panic(err)
		}
	}
	if !grassutils.IsMap(surrogateMap, "vect") {
		err = grassutils.Vproj(surrogateMap, c.SpatialDataLoc,
			c.SpatialDataMapset)
		if err != nil {
			panic(err)
		}
	}
	for i, region := range regions {
		tempSrgs, err := grassutils.Surrogate(
			inputMap, inputColumn, surrogateMap,
			WeightColumns, FilterFunction,
			c.wpsData.mapsetName[i], region)
		if err != nil {
			panic(err)
		}
		for inputRow, srg := range tempSrgs {
			if i == 0 {
				srgs[srgNum][inputRow] = make([]*matrix.SparseMatrix,
					len(regions))
			}
			srgs[srgNum][inputRow][i] = srg
			//err = Sparse2Nc(savedSurrogates[i], srgNum+
			//	"__"+inputRow,
			//	srgs[srgNum][inputRow][i], region)
			//if err != nil {
			//	panic(err)
			//}
		}
	}
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
	FilterFunction       *grassutils.SurrogateFilter
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
		srg.SURROGATE = record[1]
		srg.SURROGATECODE = record[2]
		srg.DATASHAPEFILE = record[3]
		srg.DATAATTRIBUTE = record[4]
		srg.WEIGHTSHAPEFILE = record[5]
		srg.WEIGHTATTRIBUTE = record[6]
		srg.WEIGHTFUNCTION = record[7]
		srg.FILTERFUNCTION = record[8]
		srg.MERGEFUNCTION = record[9]
		srg.SECONDARYSURROGATE = record[10]
		srg.TERTIARYSURROGATE = record[11]
		srg.QUARTERNARYSURROGATE = record[12]
		srg.DETAILS = record[13]

		// Parse weight function
		srg.WeightColumns = make([]string, 0)
		if srg.WEIGHTATTRIBUTE != "NONE" && srg.WEIGHTATTRIBUTE != "" {
			srg.WeightColumns = append(srg.WeightColumns, srg.WEIGHTATTRIBUTE)
		} else if srg.WEIGHTFUNCTION != "NONE" && srg.WEIGHTFUNCTION != "" {
			srg.WeightColumns = strings.Split(srg.WEIGHTFUNCTION, "+")
		}
		for i := 0; i < len(srg.WeightColumns); i++ {
			srg.WeightColumns[i] = strings.TrimSpace(srg.WeightColumns[i])
		}

		// Parse filter function
		if srg.FILTERFUNCTION != "NONE" && srg.FILTERFUNCTION != "" {
			srg.FilterFunction = grassutils.NewSurrogateFilter()
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
				srg.FilterFunction.Values[strings.TrimSpace(val)] = 0
			}
		}

		// Parse merge function
		if srg.MERGEFUNCTION != "NONE" && srg.MERGEFUNCTION != "" {
			srg.MergeFunction = make([]*SrgMerge, 0)
			s := strings.Split(srg.MERGEFUNCTION, "+")
			for _, s2 := range s {
				v := new(SrgMerge)
				s3 := strings.Split(s2, "*")
				v.name = strings.TrimSpace(s3[1])
				v.val, err = strconv.ParseFloat(strings.TrimSpace(s3[0]), 64)
				if err != nil {
					return
				}
				srg.MergeFunction = append(srg.MergeFunction, v)
			}
		}

		srgSpec[srg.SURROGATECODE] = srg
		srgCodes[srg.SURROGATE] = srg.SURROGATECODE
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
			FIPS := splitLine[0][1:]
			srg := strings.Trim(splitLine[2], "\"\n")

			if _, ok := gridRef[SCC]; !ok {
				gridRef[SCC] = make(map[string]string)
			}
			gridRef[SCC][FIPS] = srg
		}
	}
	return
}

func CreateSavedSurrogates(surrogateSaveLoc string, region *grassutils.Region) {
	nc, err := gonetcdf.Create(surrogateSaveLoc, "64bitoffset")
	if err != nil {
		panic(err)
	}
	err = nc.DefDim("nx", region.NX)
	if err != nil {
		panic(err)
	}
	err = nc.DefDim("ny", region.NY)
	if err != nil {
		panic(err)
	}
	err = nc.EndDef()
	if err != nil {
		panic(err)
	}
	err = nc.Close()
	if err != nil {
		panic(err)
	}
	savedSurrogates = append(savedSurrogates, surrogateSaveLoc)
}

func LoadSavedSurrogates(surrogateSaveLoc string, region *grassutils.Region, i int) {
	nc, err := gonetcdf.Open(surrogateSaveLoc, "write")
	if err != nil {
		CreateSavedSurrogates(surrogateSaveLoc, region)
		return
	}
	for varname, _ := range nc.VarNames {
		splitvar := strings.Split(varname, "__")
		srgNum := splitvar[0]
		FIPS := splitvar[1]
		if _, ok := srgs[srgNum]; !ok {
			srgs[srgNum] = make(map[string][]*matrix.SparseMatrix)
		}
		if _, ok := srgs[srgNum][FIPS]; !ok {
			srgs[srgNum][FIPS] = make([]*matrix.SparseMatrix, len(regions))
		}
		srgs[srgNum][FIPS][i], err = Nc2sparse(nc, varname, region)
		finishedSurrogates[srgNum] = ""
	}
	err = nc.Close()
	if err != nil {
		panic(err)
	}
	savedSurrogates = append(savedSurrogates, surrogateSaveLoc)
}
