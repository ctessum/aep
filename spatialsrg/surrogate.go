package spatialsrg

import (
	"bitbucket.org/ctessum/gis"
	"bitbucket.org/ctessum/sparse"
	"fmt"
	"github.com/ctessum/shapefile"
	"github.com/dhconnelly/rtreego"
	"github.com/lukeroth/gdal"
	"github.com/paulsmith/gogeos/geos"
	"github.com/pmylund/go-cache"
	"github.com/twpayne/gogeom/geom"
	"io"
	"log"
	"math"
	"net/rpc"
	"os"
	"path/filepath"
	"runtime"
	"time"
)

var SrgProgress float64

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
	srgType    gdal.GeometryType
}

type SrgGenWorkerInitData struct {
	Surrogates []*SrgHolder
	GridCells  *GridDef
	srgType    gdal.GeometryType
}
type GriddedSrgData struct {
	InputID              string
	inputGeom            *geos.Geometry
	InputGeomWKT         []string
	GridCellGeom         []*geos.Geometry
	Cells                []*GridCell
	SingleShapeSrgWeight float64
	CoveredByGrid        bool
}

type SrgHolder struct {
	Weight float64
	geom   *geos.Geometry
	WKT    string
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

	OutName := fmt.Sprintf("%v_%v", gridData.Name, srgCode)
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

	var inputData map[string][]string
	inputData, err = getInputData(inputShapeFile, ShapeColumn, gridData)
	if err != nil {
		return
	}
	var srgData []*SrgHolder
	var srgType gdal.GeometryType
	srgData, srgType, err = getSrgData(surrogateFile, WeightColumns,
		FilterFunction, gridData)
	if err != nil {
		return
	}

	// Start workers
	nprocs := runtime.GOMAXPROCS(0)
	singleShapeChan := make(chan *GriddedSrgData, nprocs*2)
	griddedSrgChan := make(chan *GriddedSrgData, nprocs*2)
	errchan := make(chan error)
	workersRunning := 0
	if len(slaves) == 0 { // not distributed computing
		for i := 0; i < nprocs; i++ {
			go srgGenWorkerLocal(singleShapeChan, griddedSrgChan, errchan,
				gridData, srgData, srgType)
			workersRunning++
		}
	} else { // distributed computing
		for i := 0; i < len(slaves); i++ {
			go srgGenWorkerDistributed(singleShapeChan, griddedSrgChan,
				errchan, slaves[i], gridData, srgData, srgType)
			workersRunning++
		}
	}

	srgsFinished := 0
	griddedSrgs := make([]*GriddedSrgData, len(inputData))
	for inputID, geomWKT := range inputData {
		singleShapeData := &GriddedSrgData{InputID: inputID, InputGeomWKT: geomWKT}
		singleShapeChan <- singleShapeData
		select {
		case griddedSrgs[srgsFinished] = <-griddedSrgChan:
			SrgProgress += 100. / float64(len(inputData))
			srgsFinished++
		case err = <-errchan:
			if err != nil {
				return
			}
			workersRunning--
		default:
			continue
		}
	}
	close(singleShapeChan)
	// wait for remaining results
	for i := srgsFinished; i < len(inputData); i++ {
		griddedSrgs[i] = <-griddedSrgChan
		SrgProgress = 100. / float64(len(inputData))
		srgsFinished++
	}
	// wait for workers to finish
	for i := 0; i < workersRunning; i++ {
		err = <-errchan
		if err != nil {
			return
		}
	}
	// Write data to file
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
	}

	Log(fmt.Sprintf("Finished creating gridding surrogate %v.",
		OutName), 0)
	SrgProgress = 0.
	return
}

func (g *GriddedSrgData) WriteToShp(s *gis.Shapefile, fid int) error {
	for _, cell := range g.Cells {
		err := s.WriteFeature(fid, cell.geom, []int{0, 1, 2, 3, 4},
			cell.Row, cell.Col, g.InputID, cell.Weight, g.CoveredByGrid)
		if err != nil {
			return err
		}
	}
	return nil
}

// Get input shapes
func getInputData(inputShapeFile, ShapeColumn string,
	gridData *GridDef) (inputData map[string][]string,
	err error) {
	Log("Getting input shape data...", 1)
	var inputShp *gis.Shapefile
	inputShp, err = gis.OpenShapefile(inputShapeFile, true)
	defer inputShp.Close()
	if err != nil {
		return
	}
	var ct *gis.CoordinateTransform
	ct, err = gis.NewCoordinateTransform(inputShp.Sr, gridData.Sr)
	if err != nil {
		return
	}
	var id int
	id, err = inputShp.GetColumnIndex(ShapeColumn)
	if err != nil {
		return
	}
	inputData = make(map[string][]string)
	var ggeom geom.T
	var ggeos *geos.Geometry
	var inputIDtemp []interface{}
	var intersects bool
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
		ggeos, err = gis.GeomToGEOS(ggeom)
		if err != nil {
			return
		}
		intersects, err = ggeos.Intersects(gridData.extent)
		if err != nil {
			return
		}
		if intersects {
			inputID := inputIDtemp[0].(string)
			if _, ok := inputData[inputID]; !ok {
				inputData[inputID] = make([]string, 0, 1)
			}
			var wkt string
			wkt, err = ggeos.ToWKT()
			if err != nil {
				return
			}
			inputData[inputID] = append(inputData[inputID], wkt)
		}
	}
	return
}

// get surrogate shapes and weights
func getSrgData(surrogateShapeFile string, WeightColumns []string,
	FilterFunction *SurrogateFilter, gridData *GridDef) (
	srgData []*SrgHolder, srgType gdal.GeometryType, err error) {
	Log("Getting surrogate data...", 1)
	var srgShp *gis.Shapefile
	srgShp, err = gis.OpenShapefile(surrogateShapeFile, true)
	if err != nil {
		return
	}
	defer srgShp.Close()
	srgType = srgShp.Type

	var ct *gis.CoordinateTransform
	ct, err = gis.NewCoordinateTransform(srgShp.Sr, gridData.Sr)
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
	srgData = make([]*SrgHolder, 0, srgShp.NumFeatures/4)
	var data []interface{}
	var rec *shapefile.ShapefileRecord
	var ggeom geom.T
	var ggeos *geos.Geometry
	var intersects bool
	var keepFeature bool
	var featureVal string
	var size float64
	//var g gdal.Geometry
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
		if keepFeature {
			ggeom, err = ct.Reproject(rec.Geometry)
			if err != nil {
				return
			}
			ggeos, err = gis.GeomToGEOS(ggeom)
			if err != nil {
				return
			}
			intersects, err = ggeos.Intersects(gridData.extent)
			if err != nil {
				return
			}
			if intersects {
				srg := new(SrgHolder)
				srg.WKT, err = ggeos.ToWKT()
				if err != nil {
					return
				}
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
					switch srgShp.Type {
					case gdal.GT_Polygon, gdal.GT_MultiPolygon:
						size, err = ggeos.Area()
						if err != nil {
							return
						}
						if size == 0. {
							err = fmt.Errorf("Area should not equal "+
								"zero in %v", surrogateShapeFile)
							return
						}
						srg.Weight = weightval / size
					case gdal.GT_LineString, gdal.GT_MultiLineString:
						size, err = ggeos.Length()
						if err != nil {
							return
						}
						if size == 0. {
							err = fmt.Errorf("Length should not equal "+
								"zero in %v", surrogateShapeFile)
							return
						}
						srg.Weight = weightval / size
					case gdal.GT_Point:
						srg.Weight = weightval
					default:
						err = fmt.Errorf("Can't handle shape type %v in %v",
							srgShp.Type, surrogateShapeFile)
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
					srgData = append(srgData, srg)
				}
			}
		}
	}
	return
}

func srgGenWorkerLocal(singleShapeChan, griddedSrgChan chan *GriddedSrgData,
	errchan chan error, gridData *GridDef, srgData []*SrgHolder,
	srgType gdal.GeometryType) {
	var err error

	s := new(SrgGenWorker)

	var data *GriddedSrgData
	first := true
	for data = range singleShapeChan {
		if first {
			d := &SrgGenWorkerInitData{srgData, gridData, srgType}
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
			return
		}
		griddedSrgChan <- result
	}
	errchan <- err
}

func srgGenWorkerDistributed(singleShapeChan, griddedSrgChan chan *GriddedSrgData,
	errchan chan error, slaveAddress string, gridData *GridDef,
	srgData []*SrgHolder, srgType gdal.GeometryType) {

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
			d := SrgGenWorkerInitData{srgData, gridData, srgType}
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
	s.surrogates = rtreego.NewTree(2, 50, 100)
	var err error
	for _, srg := range data.Surrogates {
		srg.geom, err = geos.FromWKT(srg.WKT)
		if err != nil {
			return err
		}
		srg.Extent, err = gis.GeosToRect(srg.geom)
		if err != nil {
			return err
		}
		s.surrogates.Insert(srg)
	}
	s.GridCells = data.GridCells
	var g *geos.Geometry
	for i := 0; i < len(s.GridCells.Cells); i++ {
		g, err = geos.FromWKT(s.GridCells.Cells[i].WKT)
		if err != nil {
			return err
		}
		s.GridCells.Cells[i].geom = g
	}
	s.GridCells.extent, err = geos.FromWKT(s.GridCells.ExtentWKT)
	if err != nil {
		return err
	}
	s.srgType = data.srgType
	return nil
}

// Set up to allow distributed computing through RPC
func (s *SrgGenWorker) Calculate(data, result *GriddedSrgData) (
	err error) {
	result.InputID = data.InputID

	Log(fmt.Sprintf("Working on shape %v.", data.InputID), 1)

	// convert input shape to geos (there can be more than one geometry
	// per input ID
	data.inputGeom, err = geos.FromWKT(data.InputGeomWKT[0])
	if err != nil {
		err = handle(err, "")
		return
	}
	if len(data.InputGeomWKT) > 1 {
		var g *geos.Geometry
		for _, wkt := range data.InputGeomWKT[1:] {
			g, err = geos.FromWKT(wkt)
			if err != nil {
				err = handle(err, "")
				return
			}
			data.inputGeom, err = data.inputGeom.Union(g)
			if err != nil {
				err = handle(err, "")
				return
			}
		}
	}

	// Figure out if inputShape is completely within the grid
	result.CoveredByGrid, err = data.inputGeom.Within(s.GridCells.extent)
	if err != nil {
		err = handle(err, "")
		return
	}

	GridCellChan := make(chan []*GridCell)
	srgChan := make(chan []*SrgHolder)
	weightChan := make(chan float64)
	errChan := make(chan error)
	nprocs := runtime.GOMAXPROCS(0)
	thingsWaitingFor := 0
	// run subroutines for concurrent processing
	for p := 0; p < nprocs; p++ {
		go s.intersections1(p, nprocs, data,
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
			for _, cell := range cells {
				GridCells = append(GridCells, cell)
			}
		case srgs := <-srgChan:
			for _, srg := range srgs {
				InputShapeSrgs = append(InputShapeSrgs, srg)
			}
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
	for p := 0; p < nprocs; p++ {
		go s.intersections2(p, nprocs, data, InputShapeSrgs, GridCells, errChan)
	}
	for p := 0; p < nprocs; p++ {
		err = <-errChan
		if err != nil {
			return
		}
	}
	result.Cells = make([]*GridCell, 0, len(GridCells))
	for _, cell := range GridCells {
		if cell.Weight > 0. {
			result.Cells = append(result.Cells, cell)
		}
	}
	return
}

var RPCport = "6061" // Port for RPC communications for distributed computing

// Calculate the intersections between the grid cells and the input shape,
// and between the surrogate shapes and the input shape
func (s *SrgGenWorker) intersections1(procnum, nprocs int,
	data *GriddedSrgData, GridCellChan chan []*GridCell,
	srgChan chan []*SrgHolder, weightChan chan float64, errChan chan error) {

	inputGeomP := geos.PrepareGeometry(data.inputGeom)
	inputBounds, err := gis.GeosToRect(data.inputGeom)
	if err != nil {
		return
	}

	// Figure out which grid cells intersect with the input shape
	GridCells := make([]*GridCell, 0, 30)
	var intersects bool
	for i := procnum; i < len(s.GridCells.Cells); i += nprocs {
		cell := s.GridCells.Cells[i]
		intersects, err = inputGeomP.Intersects(s.GridCells.Cells[i].geom)
		if err != nil {
			errChan <- handle(err, "")
			return
		}
		if intersects {
			GridCells = append(GridCells, cell)
		}
	}
	GridCellChan <- GridCells

	// get all of the surrogates which intersect with the input
	// shape, and save only the intersecting parts.
	var size, singleShapeSrgWeight float64
	var intersection *geos.Geometry
	srgs := make([]*SrgHolder, 0, 500)
	for _, srgI := range s.surrogates.SearchIntersect(inputBounds) {
		srg := srgI.(*SrgHolder)
		intersects, err = inputGeomP.Intersects(srg.geom)
		if err != nil {
			errChan <- handle(err, "")
			return
		}
		if intersects {
			intersection, err = IntersectionFaultTolerant(
				data.inputGeom, srg.geom)
			if err != nil {
				errChan <- handle(err, "")
				return
			}
			srgs = append(srgs, srg)
			// Add the individual surrogate weight to the total
			// weight for the input shape.
			switch s.srgType {
			case gdal.GT_Polygon, gdal.GT_MultiPolygon:
				size, err = intersection.Area()
				if err != nil {
					errChan <- handle(err, "")
					return
				}
				singleShapeSrgWeight += srg.Weight * size
			case gdal.GT_LineString, gdal.GT_MultiLineString:
				size, err = intersection.Length()
				if err != nil {
					errChan <- handle(err, "")
					return
				}
				singleShapeSrgWeight += srg.Weight * size
			case gdal.GT_Point:
				singleShapeSrgWeight += srg.Weight
			default:
				panic(fmt.Sprintf("Unknown shape type %v.", s.srgType))
			}
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
	//var GridCellP *geos.PGeometry
	var err error
	for i := procnum; i < len(GridCells); i += nprocs {
		cell := GridCells[i]
		//GridCellP = geos.PrepareGeometry(cell.geom)
		var intersects bool
		var intersection *geos.Geometry
		var size, weight float64
		for _, srg := range InputShapeSrgs {
			//intersects, err = GridCellP.Intersects(srg.geom)
			intersects, err = cell.geom.Intersects(srg.geom)
			if err != nil {
				errChan <- handle(err, "")
				return
			}
			if intersects {
				intersection, err = IntersectionFaultTolerant(
					cell.geom, srg.geom)
				if err != nil {
					errChan <- handle(err, "")
					return
				}
				// Add the individual surrogate weight to the total
				// weight fraction for the grid cell.
				switch s.srgType {
				case gdal.GT_Polygon, gdal.GT_MultiPolygon:
					size, err = intersection.Area()
					if err != nil {
						errChan <- handle(err, "")
						return
					}
					weight += srg.Weight * size / data.SingleShapeSrgWeight
				case gdal.GT_LineString, gdal.GT_MultiLineString:
					size, err = intersection.Length()
					if err != nil {
						errChan <- handle(err, "")
						return
					}
					weight += srg.Weight * size / data.SingleShapeSrgWeight
				case gdal.GT_Point:
					weight += srg.Weight / data.SingleShapeSrgWeight
				default:
					panic(fmt.Sprintf("Unknown shape type %v.", s.srgType))
				}
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

var srgMapCache *cache.Cache

func SetupSrgMapCache(t time.Duration) {
	srgMapCache = cache.New(t*time.Minute, t*time.Minute)
}

func RetrieveGriddingSurrogate(shapefileName string, inputID string,
	grid *GridDef) (srg *sparse.SparseArray, err error) {

	cacheKey := shapefileName + inputID
	// Check if surrogate is already in the cache.
	if srgMap, found := srgMapCache.Get(cacheKey); found {
		srg = srgMap.(map[string]*sparse.SparseArray)[inputID] // srg may be nil
	} else {
		var srgMap map[string]*sparse.SparseArray
		srgMap, err = readGriddingSurrogate(shapefileName, grid)
		// store surrogate map in cache for faster access.
		srgMapCache.Set(cacheKey, srgMap, 0)
		srg = srgMap[inputID] // srg may be nil
	}
	return
}

func readGriddingSurrogate(shapefileName string, grid *GridDef) (
	srgMap map[string]*sparse.SparseArray, err error) {
	srgMap = make(map[string]*sparse.SparseArray)
	var shp *gis.Shapefile
	shp, err = gis.OpenShapefile(shapefileName, true)
	defer shp.Close()
	if err != nil {
		return
	}
	columnIDs := make([]int, 4)
	for i, column := range []string{"row", "col", "inputID", "shapeFrac"} {
		columnIDs[i], err = shp.GetColumnIndex(column)
		if err != nil {
			return
		}
	}
	var tempVals []interface{}
	for {
		_, tempVals, err = shp.ReadNextFeature(columnIDs...)
		if err != nil {
			if err == io.EOF {
				err = nil
				break
			}
			return
		}
		row := tempVals[0].(int)
		col := tempVals[1].(int)
		inputID := tempVals[2].(string)
		shapeFraction := tempVals[3].(float64)
		if _, ok := srgMap[inputID]; !ok {
			srgMap[inputID] = sparse.ZerosSparse(grid.Ny, grid.Nx)
		}
		srgMap[inputID].Set(shapeFraction, row, col)
	}
	return
}

func IntersectionFaultTolerant(g1, g2 *geos.Geometry) (g3 *geos.Geometry,
	err error) {
	var buf1, buf2 *geos.Geometry
	g3, err = g1.Intersection(g2)
	if err != nil { // If there is a problem, try a 0 buffer
		err = handle(err, "")
		Log(err.Error(), 3)
		buf1, err = g1.Buffer(0.)
		if err != nil {
			return
		}
		buf2, err = g2.Buffer(0.)
		if err != nil {
			return
		}
		g3, err = buf1.Intersection(buf2)
		if err != nil {
			return
		}
	}
	return
}
