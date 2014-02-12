package spatialsrg

import (
	"bitbucket.org/ctessum/geos2rtree"
	"bitbucket.org/ctessum/gis"
	"bitbucket.org/ctessum/sparse"
	"fmt"
	"github.com/dhconnelly/rtreego"
	"github.com/lukeroth/gdal"
	"github.com/paulsmith/gogeos/geos"
	"io/ioutil"
	"log"
	"math"
	"net/rpc"
	"os"
	"path/filepath"
	"runtime"
	"strings"
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
	ShapeColumn, surrogateFile, WeightColumns string,
	FilterFunction *SurrogateFilter, gridDir, gridName, gridIDcol string,
	outputDir string, slaves []string) (err error) {

	srgCompletion := 0
	SrgProgress = 0.

	OutName := fmt.Sprintf("%v_%v", gridName, srgCode)
	OutFile := filepath.Join(outputDir, OutName)
	// if this surrogate was requested by more than one sector, make sure we
	// don't create it twice.
	if _, err = os.Stat(OutFile); err == nil {
		// file already exists
		return
	}

	gridFile := filepath.Join(gridDir, gridName+".shp")
	// Make sure required files do exist
	for _, file := range []string{gridFile, inputShapeFile, surrogateFile} {
		if _, err = os.Stat(file); os.IsNotExist(err) {
			err = fmt.Errorf("File %v doesn't exist", file)
			return
		}
	}

	Log("Creating gridding surrogate "+OutName+"...", 0)

	var gridData *GridDef
	gridData, err = getGridData(gridFile, gridIDcol)
	defer gridData.Close()
	if err != nil {
		return
	}
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
	singleShapeChan := make(chan *GriddedSrgData)
	griddedSrgChan := make(chan *GriddedSrgData)
	errchan := make(chan error)
	workersRunning := 0
	if len(slaves) == 0 { // not distributed computing
		go srgGenWorkerLocal(singleShapeChan, griddedSrgChan, errchan,
			gridData, srgData, srgType)
		workersRunning++
	} else { // distributed computing
		for i := 0; i < len(slaves); i++ {
			go srgGenWorkerDistributed(singleShapeChan, griddedSrgChan,
				errchan, slaves[i], gridData, srgData, srgType)
			workersRunning++
		}
	}

	// first create a temporary file for the shapefile, and then copy it to the
	// final location once it's done. Otherwise if there was an error before
	// the file finished creating it would cause a problem during the next
	// program run.
	var tempdir, tempfilename string
	var outShp *gis.Shapefile
	tempdir, err = ioutil.TempDir("", "aep")
	if err != nil {
		return
	}
	tempfilename = filepath.Join(tempdir, OutName)
	outShp, err = gis.CreateShapefile(tempfilename, gridData.shp.Sr,
		gdal.GT_Polygon,
		[]string{"gridID", "inputID", "shapeFraction", "coveredByGrid"},
		[]gdal.FieldType{gdal.FT_String, gdal.FT_String, gdal.FT_Real,
			gdal.FT_Binary})

	fid := 0 // index for output shapefile
	for inputID, geomWKT := range inputData {
		SrgProgress = float64(srgCompletion) / float64(len(inputData)) * 100.
		srgCompletion++
		singleShapeData := &GriddedSrgData{InputID: inputID, InputGeomWKT: geomWKT}
		select {
		case singleShapeChan <- singleShapeData:
		case griddedSrg := <-griddedSrgChan:
			err = griddedSrg.WriteToShp(outShp, fid)
			if err != nil {
				return
			}
			fid++
		case err = <-errchan:
			if err != nil {
				return
			}
			workersRunning--
			singleShapeChan <- singleShapeData
		}
	}
	close(singleShapeChan)
	// wait for workers to finish
	for i := 0; i < workersRunning; i++ {
		err = <-errchan
		if err != nil {
			return
		}
	}

	outShp.Close()
	// move files to final directory.
	for _, ext := range []string{".shp", ".dbf", ".prj", ".shx"} {
		err = os.Rename(tempfilename+ext, OutFile+ext)
		if err != nil {
			return
		}
	}
	os.RemoveAll(tempdir)

	Log(fmt.Sprintf("Finished creating gridding surrogate %v.",
		OutName), 0)
	SrgProgress = 0.
	return
}

func (g *GriddedSrgData) WriteToShp(s *gis.Shapefile, fid int) error {
	for _, cell := range g.Cells {
		geom, err := gdal.CreateFromWKT(cell.WKT, s.Sr)
		if err != nil {
			return err
		}
		err = s.WriteFeature(fid, geom, []int{0, 1, 2, 3},
			[]interface{}{cell.Id, g.InputID, cell.Weight, g.CoveredByGrid})
		if err != nil {
			return err
		}
		geom.Destroy()
	}
	return nil
}

func newGridDataHolder(shp *gis.Shapefile) (g *GridDef, err error) {
	var numShapes int
	g = new(GridDef)
	g.shp = shp
	numShapes, err = shp.GetNumShapes()
	if err != nil {
		return
	}
	g.Cells = make([]*GridCell, numShapes)
	return
}

func (g *GridDef) Close() {
	g.shp.Close()
}

// Get grid shapes
func getGridData(gridFile, gridIDcol string) (gridData *GridDef,
	err error) {
	var gridShp *gis.Shapefile
	gridShp, err = gis.OpenShapefile(gridFile, 0)
	if err != nil {
		return
	}
	var id int
	id, err = gridShp.GetColumnIndex(gridIDcol)
	if err != nil {
		return
	}
	gridData, err = newGridDataHolder(gridShp)
	if err != nil {
		return
	}
	for i := 0; i < len(gridData.Cells); i++ {
		gridData.Cells[i] = new(GridCell)
		feature := gridData.shp.Layer.NextFeature()
		gridData.Cells[i].Id = feature.FieldAsInteger(id)
		g := feature.Geometry()
		gridData.Cells[i].WKT, err = g.ToWKT()
		if err != nil {
			return
		}
		g.Destroy()
		feature.Destroy()
	}
	return
}

// Get input shapes
func getInputData(inputShapeFile, ShapeColumn string,
	gridData *GridDef) (inputData map[string][]string,
	err error) {
	var inputShp *gis.Shapefile
	inputShp, err = gis.OpenShapefile(inputShapeFile, 0)
	defer inputShp.Close()
	ct := gdal.CreateCoordinateTransform(inputShp.Sr, gridData.shp.Sr)
	var id int
	id, err = inputShp.GetColumnIndex(ShapeColumn)
	if err != nil {
		return
	}
	inputData = make(map[string][]string)
	var numShapes int
	numShapes, err = inputShp.GetNumShapes()
	if err != nil {
		return
	}
	var wkt string
	for i := 0; i < numShapes; i++ {
		feature := inputShp.Layer.NextFeature()
		g := feature.Geometry()
		err = g.Transform(ct)
		if err.Error() != "No Error" {
			return
		}
		if g.Envelope().Intersects(gridData.shp.Extent) {
			inputID := feature.FieldAsString(id)
			if _, ok := inputData[inputID]; !ok {
				inputData[inputID] = make([]string, 0, 1)
			}
			wkt, err = g.ToWKT()
			if err.Error() != "No Error" {
				return
			}
			inputData[inputID] = append(inputData[inputID], wkt)
		}
		g.Destroy()
		feature.Destroy()
	}
	return
}

// get surrogate shapes and weights
func getSrgData(surrogateShapeFile string, WeightColumns string,
	FilterFunction *SurrogateFilter, gridData *GridDef) (
	srgData []*SrgHolder, srgType gdal.GeometryType, err error) {
	var srgShp *gis.Shapefile
	srgShp, err = gis.OpenShapefile(surrogateShapeFile, 0)
	if err != nil {
		return
	}
	defer srgShp.Close()
	srgType = srgShp.Type

	ct := gdal.CreateCoordinateTransform(srgShp.Sr, gridData.shp.Sr)
	var filterId, weightId int
	filterId, err = srgShp.GetColumnIndex(FilterFunction.Column)
	if err != nil {
		return
	}
	if WeightColumns != "" {
		weightId, err = srgShp.GetColumnIndex(WeightColumns)
		if err != nil {
			return
		}
	}
	var numShapes int
	numShapes, err = srgShp.GetNumShapes()
	if err != nil {
		return
	}
	srgData = make([]*SrgHolder, 0, numShapes/4)
	var wkt string
	for i := 0; i < numShapes; i++ {
		feature := srgShp.Layer.NextFeature()

		var keepFeature bool
		if FilterFunction == nil {
			keepFeature = true
		} else {
			keepFeature = false
			featureVal := feature.FieldAsString(filterId)
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
			g := feature.Geometry()
			err = g.Transform(ct)
			if err.Error() != "No Error" {
				return
			}
			if g.Envelope().Intersects(gridData.shp.Extent) {
				wkt, err = g.ToWKT()
				if err.Error() != "No Error" {
					return
				}
				srg := new(SrgHolder)
				srg.WKT = wkt
				if WeightColumns != "" {
					weightval := feature.FieldAsFloat64(weightId)
					switch srgShp.Type {
					case gdal.GT_Polygon, gdal.GT_MultiPolygon:
						area := g.Area()
						if area == 0. {
							err = fmt.Errorf("Area should not equal "+
								"zero in %v", surrogateShapeFile)
							return
						}
						srg.Weight = weightval / area
					case gdal.GT_LineString, gdal.GT_MultiLineString:
						length := g.Length()
						if length == 0. {
							err = fmt.Errorf("Length should not equal "+
								"zero in %v", surrogateShapeFile)
							return
						}
						srg.Weight = weightval / length
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
			g.Destroy()
		}
		feature.Destroy()
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
		srg.Extent, err = geos2rtree.GeosToRect(srg.geom)
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
		var w bool
		w, err = g.Within(s.GridCells.extent)
		if err != nil {
			return err
		}
		if !w {
			s.GridCells.extent, err = s.GridCells.extent.Union(g)
			if err != nil {
				return err
			}
		}
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
	inputBounds, err := geos2rtree.GeosToRect(data.inputGeom)
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
				panic("problem!")
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
	var GridCellP *geos.PGeometry
	var err error
	for i := procnum; i < len(GridCells); i += nprocs {
		cell := GridCells[i]
		GridCellP = geos.PrepareGeometry(cell.geom)
		var intersects bool
		var intersection *geos.Geometry
		var size, weight float64
		for _, srg := range InputShapeSrgs {
			intersects, err = GridCellP.Intersects(srg.geom)
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

func RetrieveGriddingSurrogate(pg *gis.PostGis, srgNum, inputID, schema string,
	grid *gis.GridDef) (srg *sparse.SparseArray, err error) {

	srg = sparse.ZerosSparse(grid.Ny, grid.Nx)

	cmd := fmt.Sprintf("SELECT row,col,shapeFraction,coveredByGrid "+
		"FROM %v.%v_%v WHERE inputID='%v';", strings.ToLower(schema),
		strings.ToLower(grid.Name), srgNum, inputID)

	Log(cmd, 3)
	rows, err := pg.Db.Query(cmd)
	if err != nil {
		return
	}
	var coveredByGrid bool // this should be the same for all rows
	for rows.Next() {
		var row, col int
		var fraction float64
		rows.Scan(&row, &col, &fraction, &coveredByGrid)
		srg.Set(fraction, row, col)
	}
	rows.Close()
	// If this input shape is completely within the boundaries of the grid,
	// renormalize it so the surrogate sum = 1. This shouldn't be necessary
	// but sometimes there are GIS problems that cause the sum != 1.
	// In cases where part of the input shape is outside of the grid boundary,
	// the sum of the surrogate should be less than 1.
	sum := srg.Sum()
	if coveredByGrid && sum > 0. {
		srg.Scale(1. / sum)
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
