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
	"fmt"
	"log"
	"math"
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
)

type srgGenWorker struct {
	surrogates *rtree.Rtree
	GridCells  *GridDef
}

type srgGenWorkerInitData struct {
	Surrogates *rtree.Rtree
	GridCells  *GridDef
}

// GriddingSurrogate holds generated gridding surrogate data, and can be
// used to allocate emissions attributed to a relatively large area, such as
// a county, to the grid cells within that area.
type GriddingSurrogate struct {
	// Srg holds surrogate data associated with individual input locations.
	Srg map[string]*GriddedSrgData

	// Nx and Ny are the number of columns and rows in the grid
	Nx, Ny int
}

// ToGrid allocates the 1 unit of emissions associated with shapeID to a grid
// based on gs. It will return nil if there is no surrogate for the specified
// shapeID or if the sum of the surrogate is zero. The second returned value
// indicates whether the shape corresponding to shapeID is completely covered
// by the grid.
func (gs *GriddingSurrogate) ToGrid(shapeID string) (*sparse.SparseArray, bool) {
	srg, ok := gs.Srg[shapeID]
	if !ok {
		return nil, false
	}
	srgOut := sparse.ZerosSparse(gs.Ny, gs.Nx)
	for _, cell := range srg.Cells {
		srgOut.AddVal(cell.Weight, cell.Row, cell.Col)
	}
	sum := srgOut.Sum()
	if sum == 0 {
		return nil, false
	}
	// normalize so sum = 1 if the input shape is completely covered by the
	// grid.
	if srg.CoveredByGrid {
		srgOut.Scale(1. / sum)
	}
	return srgOut, srg.CoveredByGrid
}

// mergeSrgs merges a number of surrogates, multiplying each of them by the
// corresponding factor.
func mergeSrgs(srgs []*GriddingSurrogate, factors []float64) *GriddingSurrogate {
	o := new(GriddingSurrogate)
	o.Nx, o.Ny = srgs[0].Nx, srgs[0].Ny
	o.Srg = make(map[string]*GriddedSrgData)
	for i, g := range srgs {
		fac := factors[i]
		for id, gsd := range g.Srg {
			if _, ok := o.Srg[id]; !ok {
				o.Srg[id] = &GriddedSrgData{
					InputID:       gsd.InputID,
					InputGeom:     gsd.InputGeom,
					CoveredByGrid: gsd.CoveredByGrid,
				}
			}
			for _, cell := range gsd.Cells {
				o.Srg[id].Cells = append(o.Srg[id].Cells, &GridCell{
					Row:    cell.Row,
					Col:    cell.Col,
					Weight: cell.Weight * fac,
					T:      cell.T,
				})
			}
		}
	}
	return o
}

// GriddedSrgData holds the data for a single input shape of a gridding surrogate.
type GriddedSrgData struct {
	InputID              string
	InputGeom            geom.T
	Cells                []*GridCell
	SingleShapeSrgWeight float64
	CoveredByGrid        bool
}

type srgHolder struct {
	Weight float64
	geom.T
}

// SurrogateFilter can be used to limit which rows in a shapefile are
// used to create a gridding surrogate.
type SurrogateFilter struct {
	Column        string
	EqualNotEqual string
	Values        []string
}

// ParseSurrogateFilter creates a new surrogate filter object from a
// SMOKE-format spatial surrogate filter definition.
func ParseSurrogateFilter(filterFunction string) *SurrogateFilter {
	if filterFunction != "NONE" && filterFunction != "" {
		srgflt := new(SurrogateFilter)
		srgflt.Values = make([]string, 0)
		var s []string
		if strings.Index(filterFunction, "!=") != -1 {
			srgflt.EqualNotEqual = "NotEqual"
			s = strings.Split(filterFunction, "!=")
		} else {
			srgflt.EqualNotEqual = "Equal"
			s = strings.Split(filterFunction, "=")
		}
		srgflt.Column = strings.TrimSpace(s[0])
		splitstr := strings.Split(s[1], ",")
		for _, val := range splitstr {
			srgflt.Values = append(srgflt.Values,
				strings.TrimSpace(val))
		}
		return srgflt
	}
	return nil
}

// createMerged creates a surrogate by creating and merging other surrogates.
func (sp *SpatialProcessor) createMerged(srg *SrgSpec, gridData *GridDef) (*GriddingSurrogate, error) {
	mrgSrgs := make([]*GriddingSurrogate, len(srg.MergeNames))
	for i, mrgName := range srg.MergeNames {
		newSrg, err := sp.srgSpecs.GetByName(srg.Region, mrgName)
		if err != nil {
			return nil, err
		}
		// First, send a srg request to see if it's already in the cache.
		// We need to cancel the request if it's not in the cache to avoid a
		// channel lock.
		r := newSrgRequest(newSrg, gridData)
		r.waitInQueue = false
		sp.surrogateGeneratorChan <- r
		r = <-r.returnChan
		if r.err != nil {
			return nil, r.err
		}
		if r.data == nil {
			// If it's not in the cache, directly create it.
			r.data, r.err = sp.CreateSurrogate(newSrg, gridData)
			if r.err != nil {
				return nil, err
			}
		}
		mrgSrgs[i] = r.data
	}
	return mergeSrgs(mrgSrgs, srg.MergeMultipliers), nil
}

// CreateSurrogate creates a new gridding surrogate based on a
// surrogate specification and grid definition.
func (sp SpatialProcessor) CreateSurrogate(srg *SrgSpec, gridData *GridDef) (*GriddingSurrogate, error) {
	log.Println("Creating", srg.Code, srg.Name)
	if len(srg.MergeNames) != 0 {
		return sp.createMerged(srg, gridData)
	}

	inputData, err := srg.getInputData(gridData, sp.SimplifyTolerance)
	if err != nil {
		return nil, err
	}
	srgData, err := srg.getSrgData(gridData, sp.SimplifyTolerance)
	if err != nil {
		return nil, err
	}

	// Start workers
	nprocs := runtime.GOMAXPROCS(0)
	singleShapeChan := make(chan *GriddedSrgData, nprocs*2)
	griddedSrgChan := make(chan *GriddedSrgData, nprocs*2)
	errchan := make(chan error, nprocs*2)
	workersRunning := 0
	for i := 0; i < nprocs; i++ {
		go genSrgWorker(singleShapeChan, griddedSrgChan, errchan, gridData, srgData)
		workersRunning++
	}

	srg.progressLock.Lock()
	srg.progress = 0.
	srg.status = "overlaying shapes"
	srg.progressLock.Unlock()

	srgsFinished := 0
	GriddedSrgs := make(map[string]*GriddedSrgData)
	for inputID, geom := range inputData {
		singleShapeData := &GriddedSrgData{InputID: inputID, InputGeom: geom}
		select {
		case err = <-errchan:
			if err != nil {
				return nil, err
			}
			workersRunning--
			singleShapeChan <- singleShapeData
		default:
			select {
			case grdsrg := <-griddedSrgChan:
				GriddedSrgs[grdsrg.InputID] = grdsrg
				srg.progressLock.Lock()
				srg.progress += 100. / float64(len(inputData))
				srg.progressLock.Unlock()
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
		grdsrg := <-griddedSrgChan
		GriddedSrgs[grdsrg.InputID] = grdsrg
		srg.progressLock.Lock()
		srg.progress += 100. / float64(len(inputData))
		srg.progressLock.Unlock()
		srgsFinished++
	}
	// wait for workers to finish
	for i := 0; i < workersRunning; i++ {
		err = <-errchan
		if err != nil {
			return nil, err
		}
	}
	o := &GriddingSurrogate{
		Srg: GriddedSrgs,
		Nx:  gridData.Nx,
		Ny:  gridData.Ny,
	}
	return o, nil
}

// WriteToShp write an individual gridding surrogate to a shapefile.
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

// Get input shapes. tol is simplifcation tolerance.
func (srg *SrgSpec) getInputData(gridData *GridDef, tol float64) (map[string]geom.T, error) {
	srg.progressLock.Lock()
	srg.status = "getting surrogate input shape data"
	srg.progress = 0.
	srg.progressLock.Unlock()

	inputShp, err := shp.NewDecoder(srg.DATASHAPEFILE)
	defer inputShp.Close()
	if err != nil {
		return nil, err
	}
	//prjf, err := os.Open(strings.TrimSuffix(srg.DATASHAPEFILE, ".shp") + ".prj")
	//if err != nil {
	//	return nil, err
	//}
	// TODO: For now, assume lambert projection. This needs to be fixed.
	inputSR, err := proj.FromProj4("+proj=lcc +lat_1=33.000000 +lat_2=45.000000 +lat_0=40.000000 +lon_0=-97.000000 +x_0=0 +y_0=0 +a=6370997.000000 +b=6370997.000000 +to_meter=1")
	//inputSR, err := proj.ReadPrj(prjf)
	if err != nil {
		return nil, err
	}
	ct, err := proj.NewCoordinateTransform(inputSR, gridData.SR)
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
		ggeom, fields, more = inputShp.DecodeRowFields(srg.DATAATTRIBUTE)
		if !more {
			break
		}
		srg.progressLock.Lock()
		srg.progress += 100. / float64(inputShp.AttributeCount())
		srg.progressLock.Unlock()

		if err := op.FixOrientation(ggeom); err != nil {
			// op.Within requires correct orientation.
			return inputData, err
		}

		ggeom, err = ct.Reproject(ggeom)
		if err != nil {
			return inputData, err
		}
		if tol > 0 {
			ggeom, err = op.Simplify(ggeom, tol)
			if err != nil {
				// TODO: Fix this upstream
				log.Println(err, "Problem simplifying")
				continue
				//return inputData, err
			}
		}

		intersects = ggeom.Bounds(nil).Overlaps(gridBounds)
		if intersects {
			inputID := fields[srg.DATAATTRIBUTE]
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
	if inputShp.Error() != nil {
		return nil, fmt.Errorf("in file %s, %v", srg.DATASHAPEFILE, inputShp.Error())
	}
	return inputData, nil
}

// get surrogate shapes and weights. tol is a geometry simplification tolerance.
func (srg *SrgSpec) getSrgData(gridData *GridDef, tol float64) (*rtree.Rtree, error) {
	srg.progressLock.Lock()
	srg.progress = 0.
	srg.status = "getting surrogate weight data"
	srg.progressLock.Unlock()
	srgShp, err := shp.NewDecoder(srg.WEIGHTSHAPEFILE)
	if err != nil {
		return nil, err
	}
	defer srgShp.Close()

	// TODO: For now, assume lambert projection. This needs to be fixed.
	//prjf, err := os.Open(strings.TrimSuffix(srg.WEIGHTSHAPEFILE, ".shp") + ".prj")
	//if err != nil {
	//	return nil, err
	//}
	//srgSR, err := proj.ReadPrj(prjf)
	srgSR, err := proj.FromProj4("+proj=lcc +lat_1=33.000000 +lat_2=45.000000 +lat_0=40.000000 +lon_0=-97.000000 +x_0=0 +y_0=0 +a=6370997.000000 +b=6370997.000000 +to_meter=1")
	if err != nil {
		return nil, err
	}
	ct, err := proj.NewCoordinateTransform(srgSR, gridData.SR)
	if err != nil {
		return nil, err
	}
	var fieldNames []string
	if srg.FilterFunction != nil {
		fieldNames = append(fieldNames, srg.FilterFunction.Column)
	}
	if srg.WeightColumns != nil {
		fieldNames = append(fieldNames, srg.WeightColumns...)
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
		srg.progressLock.Lock()
		srg.progress += 100. / float64(srgShp.AttributeCount())
		srg.progressLock.Unlock()

		if srg.FilterFunction == nil {
			keepFeature = true
		} else {
			// Determine whether this feature should be kept according to
			// the filter function.
			keepFeature = false
			featureVal = fmt.Sprintf("%v", data[srg.FilterFunction.Column])
			for _, filterVal := range srg.FilterFunction.Values {
				switch srg.FilterFunction.EqualNotEqual {
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
			srgH := new(srgHolder)
			srgH.T, err = ct.Reproject(recGeom)
			if err != nil {
				return srgData, err
			}
			if tol > 0 {
				srgH.T, err = op.Simplify(srgH.T, tol)
				if err != nil {
					// TODO: Fix this upstream
					log.Println(err, "Problem simplifying")
					continue
					//return srgData, err
				}
			}
			intersects = srgH.T.Bounds(nil).Overlaps(gridBounds)
			if intersects {
				if len(srg.WeightColumns) != 0 {
					weightval := 0.
					for i, name := range srg.WeightColumns {
						var v float64
						vstring := data[name]
						if vstring == "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00" || vstring == "****************" {
							// null value
							v = 0.
						} else {
							v, err = strconv.ParseFloat(data[name], 64)
							if err != nil {
								fmt.Println("xxxxxxxxxxxxxxxxxxxxxxxxxx")
								return srgData, err
							}
						}
						weightval += v * srg.WeightFactors[i]
					}
					switch srgH.T.(type) {
					case geom.Polygon, geom.MultiPolygon:
						size = op.Area(srgH.T)
						if size == 0. {
							if tol > 0 {
								// We probably simplified the shape down to zero area.
								continue
							} else {
								// TODO: Is it okay for input shapes to have zero area? Probably....
								continue
								//err = fmt.Errorf("Area should not equal "+
								//	"zero in %v", srg.WEIGHTSHAPEFILE)
								//return srgData, err
							}
						}
						srgH.Weight = weightval / size
					case geom.LineString, geom.MultiLineString:
						size = op.Length(srgH.T)
						if size == 0. {
							err = fmt.Errorf("Length should not equal "+
								"zero in %v", srg.WEIGHTSHAPEFILE)
							return srgData, err
						}
						srgH.Weight = weightval / size
					case geom.Point:
						srgH.Weight = weightval
					default:
						err = op.UnsupportedGeometryError{G: srgH.T}
						return srgData, err
					}
				} else {
					srgH.Weight = 1.
				}
				if srgH.Weight < 0. || math.IsInf(srgH.Weight, 0) ||
					math.IsNaN(srgH.Weight) {
					err = fmt.Errorf("Surrogate weight is %v, which "+
						"is not acceptable.", srgH.Weight)
					return srgData, err
				} else if srgH.Weight != 0. {
					srgData.Insert(srgH)
				}
			}
		}
	}
	if srgShp.Error() != nil {
		return nil, fmt.Errorf("in file %s, %v", srg.WEIGHTSHAPEFILE, srgShp.Error())
	}
	return srgData, nil
}

type empty struct{}

func genSrgWorker(singleShapeChan, griddedSrgChan chan *GriddedSrgData,
	errchan chan error, gridData *GridDef, srgData *rtree.Rtree) {
	var err error

	s := new(srgGenWorker)

	var data *GriddedSrgData
	first := true
	for data = range singleShapeChan {
		if first {
			d := &srgGenWorkerInitData{srgData, gridData}
			e := new(empty)
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

func (s *srgGenWorker) Initialize(data *srgGenWorkerInitData, _ *empty) error {
	s.surrogates = data.Surrogates
	s.GridCells = data.GridCells
	return nil
}

// Set up to allow distributed computing through RPC
func (s *srgGenWorker) Calculate(data, result *GriddedSrgData) (
	err error) {
	result.InputID = data.InputID

	// Figure out if inputShape is completely within the grid
	result.CoveredByGrid, err = op.Within(data.InputGeom, s.GridCells.Extent)
	if err != nil {
		return
	}

	var GridCells []*GridCell
	var InputShapeSrgs []*srgHolder
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
func (s *srgGenWorker) intersections1(
	data *GriddedSrgData, surrogates *rtree.Rtree) (
	GridCells []*GridCell, srgs []*srgHolder,
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
	srgs = make([]*srgHolder, 0, 500)
	wg.Add(nprocs)
	srgsWithinBounds := s.surrogates.SearchIntersect(inputBounds)
	errChan := make(chan error)
	for procnum := 0; procnum < nprocs; procnum++ {
		go func(procnum int) {
			var err2 error
			for i := procnum; i < len(srgsWithinBounds); i += nprocs {
				var intersection geom.T
				srg := srgsWithinBounds[i].(*srgHolder)
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
						log.Println("error intersecting shapes; continuing without this shape.") // error:", err2)
						continue
					}
					if intersection == nil {
						continue
					}
				}
				mu.Lock()
				srgs = append(srgs, &srgHolder{Weight: srg.Weight,
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
					panic(op.UnsupportedGeometryError{G: intersection})
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
func (s *srgGenWorker) intersections2(data *GriddedSrgData,
	InputShapeSrgs []*srgHolder, GridCells []*GridCell) (
	result []*GridCell, err error) {

	nprocs := runtime.GOMAXPROCS(0)
	var mu sync.Mutex
	result = make([]*GridCell, 0, len(GridCells))

	errChan := make(chan error)
	for procnum := 0; procnum < nprocs; procnum++ {
		go func(procnum int) {
			var err2 error
			for i := procnum; i < len(GridCells); i += nprocs {
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
							log.Println("error intersecting shapes; continuing without this shape.") // error:", err2)
							continue
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
						panic(op.UnsupportedGeometryError{G: intersection})
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
