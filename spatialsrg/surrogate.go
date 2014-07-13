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

package spatialsrg

import (
	"bytes"
	"database/sql"
	"encoding/gob"
	"fmt"
	"io"
	"log"
	"math"
	"net/rpc"
	"os"
	"path/filepath"
	"runtime"
	"sync"

	"bitbucket.org/ctessum/gis"
	"bitbucket.org/ctessum/gisconversions"
	"bitbucket.org/ctessum/sparse"
	"github.com/ctessum/geomop"
	"github.com/ctessum/shapefile"
	"github.com/dhconnelly/rtreego"
	"github.com/lukeroth/gdal"
	_ "github.com/mattn/go-sqlite3"
	"github.com/twpayne/gogeom/geom"
)

var (
	SrgProgress     float64
	SrgProgressLock sync.RWMutex
)

var (
	DebugLevel = 3 // amount of output to print to the screen. A higher number means more messages.
)

func Log(msg string, debug int) {
	if debug <= DebugLevel {
		log.Println(msg)
	}
}

type SrgGenWorker struct {
	surrogates *rtreego.Rtree
	GridCells  *GridDef
}

type SrgGenWorkerInitData struct {
	Surrogates *rtreego.Rtree
	GridCells  *GridDef
}
type GriddedSrgData struct {
	InputID              string
	InputGeom            geom.T
	GridCellGeom         []geom.T
	Cells                []GridCell
	SingleShapeSrgWeight float64
	CoveredByGrid        bool
}

type SrgHolder struct {
	Weight float64
	Geom   geom.T
	Extent *rtreego.Rect
}

func (s *SrgHolder) Bounds() *rtreego.Rect {
	return s.Extent
}

type Empty struct{} // dummy argument for RPC

type SurrogateFilter struct {
	Column        string
	EqualNotEqual string
	Values        []string
}

func NewSurrogateFilter() (srgflt *SurrogateFilter) {
	srgflt = new(SurrogateFilter)
	srgflt.Values = make([]string, 0)
	return
}

func CreateGriddingSurrogate(srgCode, inputShapeFile,
	ShapeColumn, surrogateFile string, WeightColumns []string,
	FilterFunction *SurrogateFilter, gridData *GridDef,
	outputDir string, slaves []string) (err error) {

	SrgProgress = 0.

	OutName := fmt.Sprintf("%v___%v", gridData.Name, srgCode)
	OutFile := filepath.Join(outputDir, OutName)
	// if this surrogate was requested by more than one sector, make sure we
	// don't create it twice.
	if _, err = os.Stat(OutFile + ".shp"); err == nil {
		// file already exists
		return
	}

	// Make sure required files do exist
	for _, file := range []string{inputShapeFile, surrogateFile} {
		if _, err = os.Stat(file); os.IsNotExist(err) {
			err = fmt.Errorf("File %v doesn't exist", file)
			return
		}
	}

	Log("Creating gridding surrogate "+OutName+"...", 0)

	var inputData map[string]geom.T
	inputData, err = getInputData(inputShapeFile, ShapeColumn, gridData)
	if err != nil {
		return
	}
	var srgData *rtreego.Rtree
	srgData, err = getSrgData(surrogateFile, WeightColumns,
		FilterFunction, gridData)
	if err != nil {
		return
	}

	// Start workers
	nprocs := runtime.GOMAXPROCS(0)
	singleShapeChan := make(chan *GriddedSrgData, nprocs*2)
	griddedSrgChan := make(chan *GriddedSrgData, nprocs*2)
	errchan := make(chan error, nprocs*2)
	workersRunning := 0
	if len(slaves) == 0 { // not distributed computing
		for i := 0; i < nprocs; i++ {
			go srgGenWorkerLocal(singleShapeChan, griddedSrgChan, errchan,
				gridData, srgData)
			workersRunning++
		}
	} else { // distributed computing
		for i := 0; i < len(slaves); i++ {
			go srgGenWorkerDistributed(singleShapeChan, griddedSrgChan,
				errchan, slaves[i], gridData, srgData)
			workersRunning++
		}
	}

	srgsFinished := 0
	griddedSrgs := make([]*GriddedSrgData, len(inputData))
	for inputID, geom := range inputData {
		singleShapeData := &GriddedSrgData{InputID: inputID, InputGeom: geom}
		select {
		case err = <-errchan:
			if err != nil {
				return
			}
			workersRunning--
			singleShapeChan <- singleShapeData
		default:
			select {
			case griddedSrgs[srgsFinished] = <-griddedSrgChan:
				SrgProgress += 100. / float64(len(inputData))
				srgsFinished++
				singleShapeChan <- singleShapeData
			default:
				singleShapeChan <- singleShapeData
			}
		}
	}
	close(singleShapeChan)
	// wait for remaining results
	for i := srgsFinished; i < len(inputData); i++ {
		griddedSrgs[i] = <-griddedSrgChan
		SrgProgress += 100. / float64(len(inputData))
		srgsFinished++
	}
	// wait for workers to finish
	for i := 0; i < workersRunning; i++ {
		err = <-errchan
		if err != nil {
			return
		}
	}
	// Write data to files (shapefile and srgMapCache)
	var outShp *gis.Shapefile
	outShp, err = gis.CreateShapefile(outputDir, OutName, gridData.Sr,
		gdal.GT_Polygon,
		[]string{"row", "col", "inputID", "shapeFrac", "allCovered"},
		0, 0, "", float64(0), true)
	defer outShp.Close()
	for fid := 0; fid < len(griddedSrgs); fid++ {
		err = griddedSrgs[fid].WriteToShp(outShp, fid)
		if err != nil {
			return
		}
		// store surrogate map in cache for faster access.
		srg := griddedSrgs[fid]
		srgOut := sparse.ZerosSparse(gridData.Ny, gridData.Nx)
		for _, cell := range srg.Cells {
			srgOut.Set(cell.Weight, cell.Row, cell.Col)
		}
		// normalize so sum = 1 if the input shape is completely covered by the
		// grid.
		if srg.CoveredByGrid {
			sum := srgOut.Sum()
			if sum != 0. {
				srgOut.Scale(1. / sum)
			}
		}
		buf := new(bytes.Buffer)
		e := gob.NewEncoder(buf)
		err = e.Encode(srgOut)
		if err != nil {
			return
		}
		_, err = gridData.srgMapCache.db.Exec("INSERT INTO srgs "+
			"(code, inputid, val) VALUES (?, ?, ?)",
			srgCode, srg.InputID, buf.Bytes())
		if err != nil {
			return
		}
	}

	Log(fmt.Sprintf("Finished creating gridding surrogate %v.",
		OutName), 0)
	SrgProgress = 0.
	return
}

func (g *GriddedSrgData) WriteToShp(s *gis.Shapefile, fid int) error {
	for _, cell := range g.Cells {
		err := s.WriteFeature(fid, cell.Geom, []int{0, 1, 2, 3, 4},
			cell.Row, cell.Col, g.InputID, cell.Weight, g.CoveredByGrid)
		if err != nil {
			return err
		}
	}
	return nil
}

// Get input shapes
func getInputData(inputShapeFile, ShapeColumn string,
	gridData *GridDef) (inputData map[string]geom.T,
	err error) {
	Log("Getting input shape data...", 1)
	var inputShp *gis.Shapefile
	inputShp, err = gis.OpenShapefile(inputShapeFile, true)
	defer inputShp.Close()
	if err != nil {
		return
	}
	var ct *gisconversions.CoordinateTransform
	ct, err = gisconversions.NewCoordinateTransform(inputShp.Sr, gridData.Sr)
	if err != nil {
		return
	}
	var id int
	id, err = inputShp.GetColumnIndex(ShapeColumn)
	if err != nil {
		return
	}
	inputData = make(map[string]geom.T)
	var ggeom geom.T
	var inputIDtemp []interface{}
	var intersects bool
	gridBounds := gridData.Extent.Bounds(nil)
	for {
		ggeom, inputIDtemp, err = inputShp.ReadNextFeature(id)
		if err != nil {
			if err == io.EOF {
				err = nil
				break
			}
			return
		}
		ggeom, err = ct.Reproject(ggeom)
		if err != nil {
			return
		}
		intersects = ggeom.Bounds(nil).Overlaps(gridBounds)
		if intersects {
			inputID := inputIDtemp[0].(string)
			// Extend existing polygon if one already exists for this InputID
			if _, ok := inputData[inputID]; !ok {
				inputData[inputID] = ggeom
			} else {
				inputData[inputID] = geomop.Construct(ggeom,
					inputData[inputID], geomop.UNION)
			}
		}
	}
	return
}

// get surrogate shapes and weights
func getSrgData(surrogateShapeFile string, WeightColumns []string,
	FilterFunction *SurrogateFilter, gridData *GridDef) (
	srgData *rtreego.Rtree, err error) {
	Log("Getting surrogate data...", 1)
	SrgProgressLock.Lock()
	SrgProgress = 0.
	SrgProgressLock.Unlock()
	var srgShp *gis.Shapefile
	srgShp, err = gis.OpenShapefile(surrogateShapeFile, true)
	if err != nil {
		return
	}
	defer srgShp.Close()

	var ct *gisconversions.CoordinateTransform
	ct, err = gisconversions.NewCoordinateTransform(srgShp.Sr, gridData.Sr)
	if err != nil {
		return
	}
	var filterId int
	if FilterFunction != nil {
		filterId, err = srgShp.GetColumnIndex(FilterFunction.Column)
		if err != nil {
			return
		}
	}
	var weightIds []int
	if WeightColumns != nil {
		weightIds = make([]int, len(WeightColumns))
		for i, col := range WeightColumns {
			weightIds[i], err = srgShp.GetColumnIndex(col)
			if err != nil {
				return
			}
		}
	}
	srgData = rtreego.NewTree(2, 25, 50)
	var data []interface{}
	var rec *shapefile.ShapefileRecord
	var intersects bool
	var keepFeature bool
	var featureVal string
	var size float64
	gridBounds := gridData.Extent.Bounds(nil)
	for {
		rec, err = srgShp.Shp2.NextRecord()
		if err != nil {
			if err == io.EOF {
				err = nil
				break
			}
			return
		}
		data, err = srgShp.Dbf2.NextRecord()
		if err != nil {
			return
		}
		SrgProgressLock.Lock()
		SrgProgress += 100. / float64(srgShp.NumFeatures)
		SrgProgressLock.Unlock()

		if FilterFunction == nil {
			keepFeature = true
		} else {
			keepFeature = false
			featureVal = fmt.Sprintf("%v", data[filterId])
			for _, filterVal := range FilterFunction.Values {
				switch FilterFunction.EqualNotEqual {
				case "NotEqual":
					if featureVal != filterVal {
						keepFeature = true
					}
				default:
					if featureVal == filterVal {
						keepFeature = true
					}
				}
			}
		}
		if keepFeature && rec.Geometry != nil {
			srg := new(SrgHolder)
			srg.Geom, err = ct.Reproject(rec.Geometry)
			if err != nil {
				return
			}
			intersects = srg.Geom.Bounds(nil).Overlaps(gridBounds)
			if intersects {
				if weightIds != nil {
					weightval := 0.
					for _, id := range weightIds {
						switch t := data[id].(type) {
						case float64:
							weightval += data[id].(float64)
						case int:
							weightval += float64(data[id].(int))
						case error: // can't parse value
							Log(data[id].(error).Error(), 1)
							//weightval += 0.
						default:
							err = fmt.Errorf("Can't deal with data type %t", t)
							return
						}
					}
					switch srg.Geom.(type) {
					case geom.Polygon, geom.MultiPolygon:
						size = geomop.Area(srg.Geom)
						if size == 0. {
							err = fmt.Errorf("Area should not equal "+
								"zero in %v", surrogateShapeFile)
							return
						}
						srg.Weight = weightval / size
					case geom.LineString, geom.MultiLineString:
						size = geomop.Length(srg.Geom)
						if size == 0. {
							err = fmt.Errorf("Length should not equal "+
								"zero in %v", surrogateShapeFile)
							return
						}
						srg.Weight = weightval / size
					case geom.Point:
						srg.Weight = weightval
					default:
						err = geomop.NewError(srg.Geom)
						return
					}
				} else {
					srg.Weight = 1.
				}
				if srg.Weight < 0. || math.IsInf(srg.Weight, 0) ||
					math.IsNaN(srg.Weight) {
					err = fmt.Errorf("Surrogate weight is %v, which "+
						"is not acceptable.", srg.Weight)
					return
				} else if srg.Weight != 0. {
					srg.Extent, err = gisconversions.GeomToRect(srg.Geom)
					if err != nil {
						return
					}
					srgData.Insert(srg)
				}
			}
		}
	}
	SrgProgressLock.Lock()
	SrgProgress = 0.
	SrgProgressLock.Unlock()
	return
}

func srgGenWorkerLocal(singleShapeChan, griddedSrgChan chan *GriddedSrgData,
	errchan chan error, gridData *GridDef, srgData *rtreego.Rtree) {
	var err error

	s := new(SrgGenWorker)

	var data *GriddedSrgData
	first := true
	for data = range singleShapeChan {
		if first {
			Log("Initializing SrgGenWorkerLocal", 3)
			d := &SrgGenWorkerInitData{srgData, gridData}
			e := new(Empty)
			err = s.Initialize(d, e) // Load data (only do once)
			if err != nil {
				errchan <- err
				return
			}
			first = false
		}
		result := new(GriddedSrgData)
		err = s.Calculate(data, result)
		if err != nil {
			errchan <- err
		}
		griddedSrgChan <- result
	}
	errchan <- err
}

func srgGenWorkerDistributed(singleShapeChan, griddedSrgChan chan *GriddedSrgData,
	errchan chan error, slaveAddress string, gridData *GridDef,
	srgData *rtreego.Rtree) {

	client, err := rpc.DialHTTP("tcp", slaveAddress+":"+RPCport)
	if err != nil {
		errchan <- handle(err, "")
		return
	}

	e := new(Empty)
	var data *GriddedSrgData
	first := true
	for data = range singleShapeChan {
		if first { // Load data (only  once)
			d := SrgGenWorkerInitData{srgData, gridData}
			err = client.Call("SrgGenWorker.Initialize", d, e)
			if err != nil {
				errchan <- handle(err, "")
				return
			}
			first = false
		}
		result := new(GriddedSrgData)
		err := client.Call("SrgGenWorker.Calculate", data, result)
		if err != nil {
			errchan <- handle(err, "")
			return
		}
		griddedSrgChan <- result
	}
	errchan <- err
}

func (s *SrgGenWorker) Initialize(data *SrgGenWorkerInitData, _ *Empty) error {
	s.surrogates = data.Surrogates
	s.GridCells = data.GridCells
	return nil
}

// Set up to allow distributed computing through RPC
func (s *SrgGenWorker) Calculate(data, result *GriddedSrgData) (
	err error) {
	result.InputID = data.InputID

	Log(fmt.Sprintf("Working on shape %v.", data.InputID), 1)

	// Figure out if inputShape is completely within the grid
	result.CoveredByGrid = geomop.Within(data.InputGeom, s.GridCells.Extent)

	var inputBounds *rtreego.Rect
	inputBounds, err = gisconversions.GeomToRect(data.InputGeom)
	if err != nil {
		return
	}
	srgsWithinBounds := s.surrogates.SearchIntersect(inputBounds)

	GridCellChan := make(chan []*GridCell)
	srgChan := make(chan []*SrgHolder)
	weightChan := make(chan float64)
	errChan := make(chan error)
	nprocs := runtime.GOMAXPROCS(0)
	thingsWaitingFor := 0
	// run subroutines for concurrent processing
	for p := 0; p < nprocs; p++ {
		go s.intersections1(p, nprocs, data, srgsWithinBounds,
			GridCellChan, srgChan, weightChan, errChan)
		thingsWaitingFor += 4
	}

	GridCells := make([]*GridCell, 0, 30)
	InputShapeSrgs := make([]*SrgHolder, 0, 500)
	data.SingleShapeSrgWeight = 0.
	// Wait for resulting data.
	for {
		select {
		case cells := <-GridCellChan:
			GridCells = append(GridCells, cells...)
		case srgs := <-srgChan:
			InputShapeSrgs = append(InputShapeSrgs, srgs...)
		case srgWeight := <-weightChan:
			data.SingleShapeSrgWeight += srgWeight
		case err = <-errChan:
			if err != nil {
				return
			}
		}
		thingsWaitingFor--
		if thingsWaitingFor == 0 {
			break
		}
	}
	if data.SingleShapeSrgWeight != 0. {
		for p := 0; p < nprocs; p++ {
			go s.intersections2(p, nprocs, data, InputShapeSrgs, GridCells, errChan)
		}
		for p := 0; p < nprocs; p++ {
			err = <-errChan
			if err != nil {
				return
			}
		}
		result.Cells = make([]GridCell, 0, len(GridCells))
		for _, cell := range GridCells {
			if cell.Weight > 0. {
				result.Cells = append(result.Cells, *cell)
			}
		}
	}
	return
}

var RPCport = "6061" // Port for RPC communications for distributed computing

// Calculate the intersections between the grid cells and the input shape,
// and between the surrogate shapes and the input shape
func (s *SrgGenWorker) intersections1(procnum, nprocs int,
	data *GriddedSrgData, srgsWithinBounds []rtreego.Spatial,
	GridCellChan chan []*GridCell, srgChan chan []*SrgHolder,
	weightChan chan float64, errChan chan error) {
	var err error

	// Figure out which grid cells might intersect with the input shape
	inputBounds := data.InputGeom.Bounds(nil)
	GridCells := make([]*GridCell, 0, 30)
	var intersects bool
	for i := procnum; i < len(s.GridCells.Cells); i += nprocs {
		cell := s.GridCells.Cells[i]
		intersects = cell.Geom.Bounds(nil).Overlaps(inputBounds)
		if intersects {
			GridCells = append(GridCells, cell)
		}
	}
	GridCellChan <- GridCells

	// get all of the surrogates which intersect with the input
	// shape, and save only the intersecting parts.
	var size, singleShapeSrgWeight float64
	var intersection geom.T
	srgs := make([]*SrgHolder, 0, 500)
	for i := procnum; i < len(srgsWithinBounds); i += nprocs {
		srg := srgsWithinBounds[i].(*SrgHolder)
		Log(fmt.Sprintf("intersections1 surrogate shape %v out of %v", i,
			len(srgsWithinBounds)), 4)
		switch srg.Geom.(type) {
		case geom.Point:
			if geomop.PointInPolygon(srg.Geom.(geom.Point), data.InputGeom) {
				intersection = srg.Geom
			} else {
				continue
			}
		default:
			intersection = geomop.Construct(srg.Geom,
				data.InputGeom, geomop.INTERSECTION)
			if intersection == nil {
				continue
			}
		}
		srgs = append(srgs, &SrgHolder{Weight: srg.Weight, Geom: intersection})
		// Add the individual surrogate weight to the total
		// weight for the input shape.
		switch srg.Geom.(type) {
		case geom.Polygon, geom.MultiPolygon:
			size = geomop.Area(intersection)
			singleShapeSrgWeight += srg.Weight * size
		case geom.LineString, geom.MultiLineString:
			size = geomop.Length(intersection)
			if err != nil {
				errChan <- handle(err, "")
				return
			}
			singleShapeSrgWeight += srg.Weight * size
		case geom.Point:
			singleShapeSrgWeight += srg.Weight
		default:
			panic(geomop.NewError(intersection))
		}
	}
	srgChan <- srgs
	weightChan <- singleShapeSrgWeight
	errChan <- nil
}

// Given the surrogate shapes that are within an input shape,
// find the surrogate shapes that are within an individual grid
// cell. This function updates the values in `GridCells`.
func (s *SrgGenWorker) intersections2(procnum, nprocs int, data *GriddedSrgData,
	InputShapeSrgs []*SrgHolder, GridCells []*GridCell,
	errChan chan error) {
	for i := procnum; i < len(GridCells); i += nprocs {
		Log(fmt.Sprintf("intersections2 grid cell %v out of %v", i,
			len(GridCells)), 4)
		cell := GridCells[i]
		var size, weight float64
		var intersection geom.T
		for _, srg := range InputShapeSrgs {
			switch srg.Geom.(type) {
			case geom.Point:
				if geomop.PointInPolygon(srg.Geom.(geom.Point), cell.Geom) {
					intersection = srg.Geom
				} else {
					continue
				}
			default:
				intersection = geomop.Construct(srg.Geom,
					cell.Geom, geomop.INTERSECTION)
				if intersection == nil {
					continue
				}
			}
			switch srg.Geom.(type) {
			case geom.Polygon, geom.MultiPolygon:
				size = geomop.Area(intersection)
				weight += srg.Weight * size / data.SingleShapeSrgWeight
			case geom.LineString, geom.MultiLineString:
				size = geomop.Length(intersection)
				weight += srg.Weight * size / data.SingleShapeSrgWeight
			case geom.Point:
				weight += srg.Weight / data.SingleShapeSrgWeight
			default:
				panic(geomop.NewError(intersection))
			}
		}
		GridCells[i].Weight = weight
	}
	errChan <- nil
}

func handle(err error, cmd string) error {
	err2 := err.Error()
	buf := make([]byte, 5000)
	runtime.Stack(buf, false)
	err2 += "\n" + cmd + "\n" + string(buf)
	err3 := fmt.Errorf(err2)
	return err3
}

type cacheDB struct {
	db *sql.DB
}

func (g *GridDef) SetupSrgMapCache(srgDir string) (err error) {
	fname := filepath.Join(srgDir, "srgMapCache_"+g.Name+".sqlite")
	g.srgMapCache.db, err = sql.Open("sqlite3", fname)
	if err != nil {
		panic(err)
	}
	_, err = g.srgMapCache.db.Exec("CREATE TABLE srgs (code TEXT, " +
		"inputid TEXT, val BLOB)")
	return
}

// Returned srg may be nil
func RetrieveGriddingSurrogate(srgCode string, inputID string,
	grid *GridDef) (srg *sparse.SparseArray, err error) {

	var tempRow *sql.Row
	tempRow = grid.srgMapCache.db.QueryRow(
		"SELECT val FROM srgs WHERE "+
			"code=? AND inputid=?", srgCode, inputID)
	var tempResult interface{}
	err = tempRow.Scan(tempResult)
	if err != nil {
		return
	}
	srgByte := tempResult.([]byte)
	if srgByte != nil {
		buf := bytes.NewReader(srgByte)
		d := gob.NewDecoder(buf)
		err = d.Decode(&srg)
		if err != nil {
			return
		}
		srg.Fix()
	}
	return
}

//func AddSurrogateToCache(dataFileName, srgName string, grids []*GridDef) (
//	err error) {
//	var f *os.File
//	f, err = os.Open(dataFileName)
//	defer f.Close()
//	if err != nil {
//		return
//	}
//	var data *shapefile.DBFFile
//	data, err = shapefile.OpenDBFFile(f)
//	if err != nil {
//		return
//	}
//	columnIDs := make([]int, 5)
//	for i, column := range []string{"row", "col", "inputID", "shapeFrac",
//		"allCovered"} {
//		var ok bool
//		columnIDs[i], ok = data.FieldIndicies[column]
//		if !ok {
//			err = fmt.Errorf("Column %v not found in %v. Column options are %v",
//				column, dataFileName, data.FieldIndicies)
//			return
//		}
//	}
//
//	//Figure out which grid this is.
//	s := strings.Split(srgName, "_")
//	gridName := s[0]
//	srgCode := s[1]
//	var grid *GridDef
//	for _, g := range grids {
//		if gridName == g.Name {
//			grid = g
//		}
//	}
//
//	srgMap := make(map[string]*sparse.SparseArray)
//	allCovered := make(map[string]int)
//	var tempVals []interface{}
//	for {
//		tempVals, err = data.NextRecord()
//		if err != nil {
//			if err == io.EOF {
//				err = nil
//				break
//			}
//			return
//		}
//		row := tempVals[columnIDs[0]].(int)
//		col := tempVals[columnIDs[1]].(int)
//		inputID := tempVals[columnIDs[2]].(string)
//		shapeFraction := tempVals[columnIDs[3]].(float64)
//		allCovered[inputID] = tempVals[columnIDs[4]].(int)
//		if _, ok := srgMap[inputID]; !ok {
//			srgMap[inputID] = sparse.ZerosSparse(grid.Ny, grid.Nx)
//		}
//		srgMap[inputID].Set(shapeFraction, row, col)
//	}
//	for inputID, data := range srgMap {
//		// normalize so sum = 1 if the input shape is completely covered by the
//		// grid.
//		if allCovered[inputID] == 1 {
//			sum := data.Sum()
//			if sum != 0. {
//				data.Scale(1. / sum)
//			}
//		}
//		//cacheKey := []byte(fmt.Sprintf("%v%v%v", gridName, srgCode, inputID))
//		buf := new(bytes.Buffer)
//		e := gob.NewEncoder(buf)
//		err = e.Encode(data)
//		if err != nil {
//			return
//		}
//		//err = srgMapCache.Set(cacheKey, buf.Bytes())
//		if err != nil {
//			return
//		}
//	}
//	return
//}
