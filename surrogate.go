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
	"bytes"
	"database/sql"
	"encoding/gob"
	"fmt"
	"log"
	"math"
	"net/rpc"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"sync"

	"bitbucket.org/ctessum/sparse"
	"github.com/ctessum/geom"
	"github.com/ctessum/geom/encoding/shp"
	"github.com/ctessum/geom/index/rtree"
	"github.com/ctessum/geom/op"
	"github.com/ctessum/geom/proj"
	goshp "github.com/jonas-p/go-shp"
	_ "github.com/mattn/go-sqlite3"
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
	surrogates *rtree.Rtree
	GridCells  *GridDef
}

type SrgGenWorkerInitData struct {
	Surrogates *rtree.Rtree
	GridCells  *GridDef
}
type GriddedSrgData struct {
	InputID              string
	InputGeom            geom.T
	GridCellGeom         []geom.T
	Cells                []*GridCell
	SingleShapeSrgWeight float64
	CoveredByGrid        bool
}

type SrgHolder struct {
	Weight float64
	geom.T
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
			err = fmt.Errorf("Input shapefile %v doesn't exist", file)
			return
		}
	}

	Log("Creating gridding surrogate "+OutName+"...", 0)

	var inputData map[string]geom.T
	inputData, err = getInputData(inputShapeFile, ShapeColumn, gridData)
	if err != nil {
		return
	}
	var srgData *rtree.Rtree
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
	GriddedSrgs := make([]*GriddedSrgData, len(inputData))
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
			case GriddedSrgs[srgsFinished] = <-griddedSrgChan:
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
		GriddedSrgs[i] = <-griddedSrgChan
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
	// Write data to files (shapefile and srgMapCache sql db)
	fields := make([]goshp.Field, 5)
	fields[0] = goshp.NumberField("row", 10)
	fields[1] = goshp.NumberField("col", 10)
	fields[2] = goshp.StringField("inputID", 50)
	fields[3] = goshp.FloatField("shapeFrac", 20, 10)
	fields[4] = goshp.StringField("allCovered", 1)
	var outShp *shp.Encoder
	outShp, err = shp.NewEncoderFromFields(
		filepath.Join(outputDir, OutName+".shp"),
		goshp.POLYGON, fields...)
	defer outShp.Close()
	var tx *sql.Tx
	gridData.srgMapCache.mutex.Lock()
	tx, err = gridData.srgMapCache.db.Begin()
	if err != nil {
		gridData.srgMapCache.mutex.Unlock()
		return
	}
	var stmt *sql.Stmt
	stmt, err = tx.Prepare("INSERT INTO srgs " +
		"(code, inputid, val) VALUES (?, ?, ?)")
	if err != nil {
		gridData.srgMapCache.mutex.Unlock()
		return
	}
	for fid := 0; fid < len(GriddedSrgs); fid++ {
		err = GriddedSrgs[fid].WriteToShp(outShp)
		if err != nil {
			gridData.srgMapCache.mutex.Unlock()
			return
		}
		// store surrogate map in sqlite cache for faster access.
		srg := GriddedSrgs[fid]
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
			gridData.srgMapCache.mutex.Unlock()
			return
		}
		_, err = stmt.Exec(srgCode, srg.InputID, buf.Bytes())
		if err != nil {
			gridData.srgMapCache.mutex.Unlock()
			return
		}
	}
	err = tx.Commit()
	if err != nil {
		gridData.srgMapCache.mutex.Unlock()
		return
	}
	gridData.srgMapCache.mutex.Unlock()

	Log(fmt.Sprintf("Finished creating gridding surrogate %v.",
		OutName), 0)
	SrgProgress = 0.
	return
}

func (g *GriddedSrgData) WriteToShp(s *shp.Encoder) error {
	for _, cell := range g.Cells {
		var covered string
		if g.CoveredByGrid {
			covered = "T"
		} else {
			covered = "F"
		}
		err := s.EncodeFields(cell.T,
			cell.Row, cell.Col, g.InputID, cell.Weight, covered)
		if err != nil {
			return err
		}
	}
	return nil
}

// Get input shapes
func getInputData(inputShapeFile, ShapeColumn string,
	gridData *GridDef) (map[string]geom.T, error) {
	Log("Getting input shape data...", 1)
	inputShp, err := shp.NewDecoder(inputShapeFile)
	defer inputShp.Close()
	if err != nil {
		return nil, err
	}
	prjf, err := os.Open(strings.TrimSuffix(inputShapeFile, ".shp") + ".prj")
	if err != nil {
		return nil, err
	}
	inputSR, err := proj.ReadPrj(prjf)
	if err != nil {
		return nil, err
	}
	ct, err := proj.NewCoordinateTransform(inputSR, gridData.Sr)
	if err != nil {
		return nil, err
	}
	inputData := make(map[string]geom.T)
	var ggeom geom.T
	var fields map[string]string
	var intersects bool
	var more bool
	gridBounds := gridData.Extent.Bounds(nil)
	for {
		ggeom, fields, more = inputShp.DecodeRowFields(ShapeColumn)
		if !more {
			break
		}
		ggeom, err = ct.Reproject(ggeom)
		if err != nil {
			return inputData, err
		}
		intersects = ggeom.Bounds(nil).Overlaps(gridBounds)
		if intersects {
			inputID := fields[ShapeColumn]
			// Extend existing polygon if one already exists for this InputID
			if _, ok := inputData[inputID]; !ok {
				inputData[inputID] = ggeom
			} else {
				inputData[inputID], err = op.Construct(ggeom,
					inputData[inputID], op.UNION)
				if err != nil {
					return inputData, err
				}
			}
		}
	}
	return inputData, inputShp.Error()
}

// get surrogate shapes and weights
func getSrgData(surrogateShapeFile string, WeightColumns []string,
	FilterFunction *SurrogateFilter, gridData *GridDef) (
	*rtree.Rtree, error) {
	Log("Getting surrogate data...", 1)
	SrgProgressLock.Lock()
	SrgProgress = 0.
	SrgProgressLock.Unlock()
	srgShp, err := shp.NewDecoder(surrogateShapeFile)
	if err != nil {
		return nil, err
	}
	defer srgShp.Close()

	prjf, err := os.Open(strings.TrimSuffix(surrogateShapeFile, ".shp") + ".prj")
	if err != nil {
		return nil, err
	}
	srgSR, err := proj.ReadPrj(prjf)
	ct, err := proj.NewCoordinateTransform(srgSR, gridData.Sr)
	if err != nil {
		return nil, err
	}
	var fieldNames []string
	if FilterFunction != nil {
		fieldNames = append(fieldNames, FilterFunction.Column)
	}
	if WeightColumns != nil {
		fieldNames = append(fieldNames, WeightColumns...)
	}
	srgData := rtree.NewTree(25, 50)
	var recGeom geom.T
	var data map[string]string
	var intersects bool
	var keepFeature bool
	var featureVal string
	var size float64
	var more bool
	gridBounds := gridData.Extent.Bounds(nil)
	for {
		recGeom, data, more = srgShp.DecodeRowFields(fieldNames...)
		if !more {
			break
		}
		SrgProgressLock.Lock()
		SrgProgress += 100. / float64(srgShp.AttributeCount())
		SrgProgressLock.Unlock()

		if FilterFunction == nil {
			keepFeature = true
		} else {
			keepFeature = false
			featureVal = fmt.Sprintf("%v", data[FilterFunction.Column])
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
		if keepFeature && recGeom != nil {
			srg := new(SrgHolder)
			srg.T, err = ct.Reproject(recGeom)
			if err != nil {
				return srgData, err
			}
			intersects = srg.T.Bounds(nil).Overlaps(gridBounds)
			if intersects {
				if WeightColumns != nil {
					weightval := 0.
					for _, name := range WeightColumns {
						v, err := strconv.ParseFloat(data[name], 64)
						if err != nil {
							return srgData, err
						}
						weightval += v
					}
					switch srg.T.(type) {
					case geom.Polygon, geom.MultiPolygon:
						size = op.Area(srg.T)
						if size == 0. {
							err = fmt.Errorf("Area should not equal "+
								"zero in %v", surrogateShapeFile)
							return srgData, err
						}
						srg.Weight = weightval / size
					case geom.LineString, geom.MultiLineString:
						size = op.Length(srg.T)
						if size == 0. {
							err = fmt.Errorf("Length should not equal "+
								"zero in %v", surrogateShapeFile)
							return srgData, err
						}
						srg.Weight = weightval / size
					case geom.Point:
						srg.Weight = weightval
					default:
						err = op.UnsupportedGeometryError{srg.T}
						return srgData, err
					}
				} else {
					srg.Weight = 1.
				}
				if srg.Weight < 0. || math.IsInf(srg.Weight, 0) ||
					math.IsNaN(srg.Weight) {
					err = fmt.Errorf("Surrogate weight is %v, which "+
						"is not acceptable.", srg.Weight)
					return srgData, err
				} else if srg.Weight != 0. {
					srgData.Insert(srg)
				}
			}
		}
	}
	SrgProgressLock.Lock()
	SrgProgress = 0.
	SrgProgressLock.Unlock()
	return srgData, srgShp.Error()
}

func srgGenWorkerLocal(singleShapeChan, griddedSrgChan chan *GriddedSrgData,
	errchan chan error, gridData *GridDef, srgData *rtree.Rtree) {
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
	srgData *rtree.Rtree) {

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
	result.CoveredByGrid, err = op.Within(data.InputGeom, s.GridCells.Extent)
	if err != nil {
		return
	}

	var GridCells []*GridCell
	var InputShapeSrgs []*SrgHolder
	GridCells, InputShapeSrgs, data.SingleShapeSrgWeight, err =
		s.intersections1(data, s.surrogates)
	if err != nil {
		return
	}

	if data.SingleShapeSrgWeight != 0. {
		result.Cells, err = s.intersections2(data, InputShapeSrgs, GridCells)
		if err != nil {
			return
		}
	}
	return
}

// Calculate the intersections between the grid cells and the input shape,
// and between the surrogate shapes and the input shape
func (s *SrgGenWorker) intersections1(
	data *GriddedSrgData, surrogates *rtree.Rtree) (
	GridCells []*GridCell, srgs []*SrgHolder,
	singleShapeSrgWeight float64, err error) {

	nprocs := runtime.GOMAXPROCS(0)
	var mu sync.Mutex
	var wg sync.WaitGroup

	// Figure out which grid cells might intersect with the input shape
	inputBounds := data.InputGeom.Bounds(nil)
	GridCells = make([]*GridCell, 0, 30)
	wg.Add(nprocs)
	for procnum := 0; procnum < nprocs; procnum++ {
		go func(procnum int) {
			defer wg.Done()
			var intersects bool
			for i := procnum; i < len(s.GridCells.Cells); i += nprocs {
				cell := s.GridCells.Cells[i]
				intersects = cell.T.Bounds(nil).Overlaps(inputBounds)
				if intersects {
					mu.Lock()
					GridCells = append(GridCells, cell)
					mu.Unlock()
				}
			}
		}(procnum)
	}
	wg.Wait()

	// get all of the surrogates which intersect with the input
	// shape, and save only the intersecting parts.
	singleShapeSrgWeight = 0.
	srgs = make([]*SrgHolder, 0, 500)
	wg.Add(nprocs)
	srgsWithinBounds := s.surrogates.SearchIntersect(inputBounds)
	errChan := make(chan error)
	for procnum := 0; procnum < nprocs; procnum++ {
		go func(procnum int) {
			var err2 error
			for i := procnum; i < len(srgsWithinBounds); i += nprocs {
				var intersection geom.T
				srg := srgsWithinBounds[i].(*SrgHolder)
				Log(fmt.Sprintf("intersections1 surrogate shape %v out of %v", i,
					len(srgsWithinBounds)), 4)
				switch srg.T.(type) {
				case geom.Point:
					var in bool
					in, err2 = op.Within(srg.T, data.InputGeom)
					if err2 != nil {
						errChan <- err2
						return
					}
					if in {
						intersection = srg.T
					} else {
						continue
					}
				default:
					intersection, err2 = op.Construct(srg.T,
						data.InputGeom, op.INTERSECTION)
					if err2 != nil {
						errChan <- err2
						return
					}
					if intersection == nil {
						continue
					}
				}
				mu.Lock()
				srgs = append(srgs, &SrgHolder{Weight: srg.Weight,
					T: intersection})
				// Add the individual surrogate weight to the total
				// weight for the input shape.
				switch srg.T.(type) {
				case geom.Polygon, geom.MultiPolygon:
					singleShapeSrgWeight += srg.Weight *
						op.Area(intersection)
				case geom.LineString, geom.MultiLineString:
					singleShapeSrgWeight += srg.Weight *
						op.Length(intersection)
				case geom.Point:
					singleShapeSrgWeight += srg.Weight
				default:
					panic(op.UnsupportedGeometryError{intersection})
				}
				mu.Unlock()
			}
			errChan <- nil
		}(procnum)
	}
	for procnum := 0; procnum < nprocs; procnum++ {
		if err = <-errChan; err != nil {
			return
		}
	}
	return
}

// Given the surrogate shapes that are within an input shape,
// find the surrogate shapes that are within an individual grid
// cell. This function updates the values in `GridCells`.
func (s *SrgGenWorker) intersections2(data *GriddedSrgData,
	InputShapeSrgs []*SrgHolder, GridCells []*GridCell) (
	result []*GridCell, err error) {

	nprocs := runtime.GOMAXPROCS(0)
	var mu sync.Mutex
	result = make([]*GridCell, 0, len(GridCells))

	errChan := make(chan error)
	for procnum := 0; procnum < nprocs; procnum++ {
		go func(procnum int) {
			var err2 error
			for i := procnum; i < len(GridCells); i += nprocs {
				Log(fmt.Sprintf("intersections2 grid cell %v out of %v", i,
					len(GridCells)), 4)
				cell := GridCells[i].Copy()
				var intersection geom.T
				for _, srg := range InputShapeSrgs {
					switch srg.T.(type) {
					case geom.Point:
						var in bool
						in, err2 = op.Within(srg.T, cell.T)
						if err2 != nil {
							errChan <- err2
							return
						}
						if in {
							intersection = srg.T
						} else {
							continue
						}
					default:
						intersection, err2 = op.Construct(srg.T,
							cell.T, op.INTERSECTION)
						if err2 != nil {
							errChan <- err2
							return
						}
						if intersection == nil {
							continue
						}
					}
					switch srg.T.(type) {
					case geom.Polygon, geom.MultiPolygon:
						cell.Weight += srg.Weight * op.Area(intersection) /
							data.SingleShapeSrgWeight
					case geom.LineString, geom.MultiLineString:
						cell.Weight += srg.Weight * op.Length(intersection) /
							data.SingleShapeSrgWeight
					case geom.Point:
						cell.Weight += srg.Weight / data.SingleShapeSrgWeight
					default:
						panic(op.UnsupportedGeometryError{intersection})
					}
				}
				mu.Lock()
				if cell.Weight > 0. {
					result = append(result, cell)
				}
				mu.Unlock()
			}
			errChan <- nil
		}(procnum)
	}
	for procnum := 0; procnum < nprocs; procnum++ {
		if err = <-errChan; err != nil {
			return
		}
	}
	return
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
	db    *sql.DB
	mutex sync.Mutex
	cache map[string]map[string]*sparse.SparseArray
}

func (g *GridDef) SetupSrgMapCache(srgDir string) (err error) {
	g.srgMapCache.mutex.Lock()
	g.srgMapCache.cache = make(map[string]map[string]*sparse.SparseArray)
	fname := filepath.Join(srgDir, "srgMapCache_"+g.Name+".sqlite")
	err = os.MkdirAll(srgDir, os.ModePerm)
	if err != nil {
		return
	}
	g.srgMapCache.db, err = sql.Open("sqlite3", fname)
	if err != nil {
		g.srgMapCache.mutex.Unlock()
		panic(err)
	}
	err = g.srgMapCache.db.Ping()
	if err != nil {
		g.srgMapCache.mutex.Unlock()
		return
	}
	g.srgMapCache.db.Exec("CREATE TABLE srgs (code TEXT, " +
		"inputid TEXT, val BLOB)")
	g.srgMapCache.mutex.Unlock()
	return
}

// Returned srg may be nil
func RetrieveGriddingSurrogate(srgCode string, inputID string,
	grid *GridDef) (*sparse.SparseArray, error) {

	var err error
	grid.srgMapCache.mutex.Lock()
	defer grid.srgMapCache.mutex.Unlock()

	if _, ok := grid.srgMapCache.cache[srgCode]; !ok { // get srg from sql cache.
		grid.srgMapCache.cache[srgCode] = make(map[string]*sparse.SparseArray)
		var rows *sql.Rows
		rows, err = grid.srgMapCache.db.Query(
			"SELECT val, inputid FROM srgs WHERE "+
				"code=?", srgCode)
		if err != nil {
			return nil, err
		}
		defer rows.Close()
		for rows.Next() {
			var srgByte []byte
			var inputid string
			err = rows.Scan(&srgByte, &inputid)
			if err != nil {
				// No result found, return nil
				if err == sql.ErrNoRows {
					err = nil
				}
				return nil, err
			}
			buf := bytes.NewReader(srgByte)
			d := gob.NewDecoder(buf)
			var srg *sparse.SparseArray
			err = d.Decode(&srg)
			if err != nil {
				return nil, err
			}
			srg.Fix()
			grid.srgMapCache.cache[srgCode][inputid] = srg
		}
	}
	return grid.srgMapCache.cache[srgCode][inputID], nil
}
