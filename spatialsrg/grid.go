package spatialsrg

import (
	"bitbucket.org/ctessum/gdal2rtree"
	"bitbucket.org/ctessum/geos2rtree"
	"bitbucket.org/ctessum/gis"
	"bitbucket.org/ctessum/sparse"
	"fmt"
	"github.com/dhconnelly/rtreego"
	"github.com/lukeroth/gdal"
	"github.com/paulsmith/gogeos/geos"
	"path/filepath"
)

type GridDef struct {
	Name            string
	Nx, Ny          int
	Dx, Dy          float64
	X0, Y0          float64
	TimeZones       map[int]*sparse.SparseArray
	Cells           []*GridCell
	Sr              gdal.SpatialReference
	extent          *geos.Geometry
	ExtentWKT       string
	IrregularGrid   bool     // whether the grid is a regular grid
	OtherFieldNames []string // names of other columns we want to keep
	rtree           *rtreego.Rtree
}

type GridCell struct {
	geom           *geos.Geometry
	WKT            string
	Row, Col       int
	OtherFieldData []interface{} // data for the other columns we want to keep
	Weight         float64
	rtreebounds    *rtreego.Rect
}

func (g *GridCell) Bounds() *rtreego.Rect {
	return g.rtreebounds
}

func (g *GridDef) AddOtherFieldNames(names ...string) {
	if g.OtherFieldNames == nil {
		g.OtherFieldNames = make([]string, 0)
	}
	for _, name := range names {
		g.OtherFieldNames = append(g.OtherFieldNames, name)
	}
}

func (c *GridCell) AddOtherFieldData(data ...interface{}) {
	if c.OtherFieldData == nil {
		c.OtherFieldData = make([]interface{}, 0)
	}
	for _, val := range data {
		c.OtherFieldData = append(c.OtherFieldData, val)
	}
}

func NewGridRegular(Name string, Nx, Ny int, Dx, Dy, X0, Y0 float64,
	sr gdal.SpatialReference) (grid *GridDef, err error) {
	grid = new(GridDef)
	grid.Name = Name
	grid.Nx, grid.Ny = Nx, Ny
	grid.Dx, grid.Dy = Dx, Dy
	grid.X0, grid.Y0 = X0, Y0
	grid.Sr = sr
	grid.rtree = rtreego.NewTree(2, 25, 50)
	// Create geometry
	grid.Cells = make([]*GridCell, grid.Nx*grid.Ny)
	for ix := 0; ix < grid.Nx; ix++ {
		for iy := 0; iy < grid.Ny; iy++ {
			cell := new(GridCell)
			x := grid.X0 + float64(ix)*grid.Dx
			y := grid.Y0 + float64(iy)*grid.Dy
			cell.WKT = fmt.Sprintf("POINT ((%v %v, %v %v, %v %v, %v %v, %v %v))",
				x, y, x+grid.Dx, y, x+grid.Dx, y+grid.Dy,
				x, y+grid.Dy, x, y)
			cell.Row, cell.Col = ix, iy
			cell.geom, err = geos.FromWKT(cell.WKT)
			if err != nil {
				return
			}
			cell.rtreebounds, err = geos2rtree.GeosToRect(cell.geom)
			if err != nil {
				return
			}
			grid.rtree.Insert(cell)
		}
	}
	grid.ExtentWKT = fmt.Sprintf("POINT ((%v %v, %v %v, %v %v, %v %v, %v %v))",
		X0, Y0, X0+Dx*float64(Nx), Y0, X0+Dx*float64(Nx), Y0+Dy*float64(Ny),
		X0, Y0+Dy*float64(Ny), X0, Y0)
	return
}

// Irregular grids have 1 column and n rows, where n is the number of
// shapes in the shapefile.
func NewGridIrregular(Name, shapefilePath string, columnsToKeep []string,
	outputSr gdal.SpatialReference) (grid *GridDef, err error) {
	grid = new(GridDef)
	grid.Name = Name
	grid.Sr = outputSr
	grid.IrregularGrid = true
	var shp *gis.Shapefile
	shp, err = gis.OpenShapefile(shapefilePath, 0)
	if err != nil {
		return
	}
	grid.Cells = make([]*GridCell, shp.NumFeatures)
	grid.Ny = shp.NumFeatures
	grid.Nx = 1
	grid.OtherFieldNames = columnsToKeep
	columnIndicies := make([]int, len(columnsToKeep))
	for i, name := range columnsToKeep {
		columnIndicies[i], err = shp.GetColumnIndex(name)
		if err != nil {
			return
		}
	}
	ct := gdal.CreateCoordinateTransform(shp.Sr, grid.Sr)
	grid.rtree = rtreego.NewTree(2, 25, 50)
	var g gdal.Geometry
	var w bool
	for i := 0; i < shp.NumFeatures; i++ {
		cell := new(GridCell)
		g, cell.OtherFieldData, err = shp.ReadFeature(i, columnIndicies...)
		err = g.Transform(ct)
		if err != nil {
			return
		}
		cell.geom, err = gdal2rtree.GDALtoGEOS(g)
		if err != nil {
			return
		}
		g.Destroy()
		cell.WKT, err = cell.geom.ToWKT()
		if err != nil {
			return
		}
		cell.Row = i
		grid.Cells[i] = cell

		w, err = cell.geom.Within(grid.extent)
		if err != nil {
			return
		}
		if !w {
			grid.extent, err = grid.extent.Union(cell.geom)
			if err != nil {
				return
			}
		}
		cell.rtreebounds, err = geos2rtree.GeosToRect(cell.geom)
		if err != nil {
			return
		}
		grid.rtree.Insert(cell)
	}
	grid.ExtentWKT, err = grid.extent.ToWKT()
	if err != nil {
		return
	}
	return
}

// Get time zones.
func (grid *GridDef) GetTimeZones(tzFile, tzColumn string) (err error) {
	var timezones []*tzHolder
	var tzSr gdal.SpatialReference
	timezones, tzSr, err = getTimeZones(tzFile, tzColumn)
	if err != nil {
		return
	}
	grid.AddOtherFieldNames("timezone")
	grid.TimeZones = make(map[int]*sparse.SparseArray)

	ct := gdal.CreateCoordinateTransform(grid.Sr, tzSr)

	var g, cellCenter gdal.Geometry
	for _, cell := range grid.Cells {
		// find timezone nearest to the center of the cell.
		// Need to project grid to timezone projection rather than the
		// other way around because the timezones include the north
		// and south poles which don't convert well to other projections.
		g, err = gdal.CreateFromWKT(cell.WKT, grid.Sr)
		if err != nil {
			return
		}
		cellCenter = g.Centroid()
		err = cellCenter.Transform(ct)
		var tz float64
		var foundtz, intersects bool
		for _, tzData := range timezones {
			intersects = tzData.geom.Intersects(cellCenter)
			if intersects {
				if foundtz {
					panic("Cell matches more than one timezone")
				}
				tz = tzData.tz
				foundtz = true
			}
		}
		if !foundtz {
			panic("Cell doesn't match any timezones")
		}
		cell.AddOtherFieldData(tz)
		tzSeconds := int(tz * 3600.)
		if _, ok := grid.TimeZones[tzSeconds]; !ok {
			grid.TimeZones[tzSeconds] = sparse.ZerosSparse(grid.Ny, grid.Nx)
		}
		grid.TimeZones[tzSeconds].Set(1., cell.Row, cell.Col)
	}
	return
}

type tzHolder struct {
	tz   float64
	geom gdal.Geometry
}

func getTimeZones(tzFile, tzColumn string) (
	timezones []*tzHolder, tzSr gdal.SpatialReference, err error) {
	timezones = make([]*tzHolder, 0, 50)

	var shp *gis.Shapefile
	shp, err = gis.OpenShapefile(tzFile, 0)
	if err != nil {
		return
	}
	tzSr = shp.Sr
	var tzIndex int
	tzIndex, err = shp.GetColumnIndex(tzColumn)
	if err != nil {
		return
	}
	for i := 0; i < shp.NumFeatures; i++ {
		tzData := new(tzHolder)
		var tzTemp []interface{}
		tzData.geom, tzTemp, err = shp.ReadFeature(i, tzIndex)
		if err != nil {
			return
		}
		tzData.tz = tzTemp[0].(float64)
		timezones = append(timezones, tzData)
	}
	shp.Close()
	return
}

func (grid *GridDef) GetIndex(x, y float64, inputSr *gdal.SpatialReference,
	ct *gdal.CoordinateTransform) (
	row, col int, withinGrid bool, err error) {
	var g gdal.Geometry
	g, err = gdal.CreateFromWKT(fmt.Sprintf("POINT (%v %v)", x, y), *inputSr)
	if err != nil {
		return
	}
	err = g.Transform(*ct)
	if err != nil {
		return
	}
	X, Y, _ := g.Point(0) // coordinates transformed to output projection
	var geosPoint *geos.Geometry
	geosPoint, err = geos.NewPoint(geos.NewCoord(X, Y))
	if err != nil {
		return
	}
	withinGrid, err = geosPoint.Within(grid.extent)
	if !withinGrid {
		return
	}
	rtreepoint := rtreego.Point([]float64{X, Y})
	gridCellTemp := grid.rtree.NearestNeighbor(rtreepoint)
	cell := gridCellTemp.(*GridCell)
	row = cell.Row
	col = cell.Col
	g.Destroy()
	return
}

func (g *GridDef) WriteToShp(outdir string) error {
	var err error
	colNames := []string{"row", "col"}
	exampleData := []interface{}{0, 0}
	for i, name := range g.OtherFieldNames {
		colNames = append(colNames, name)
		exampleData = append(exampleData, g.Cells[0].OtherFieldData[i])
	}
	var shp *gis.Shapefile
	shp, err = gis.CreateShapefile(filepath.Join(outdir, g.Name),
		g.Sr, gdal.GT_Polygon, colNames, exampleData...)
	if err != nil {
		return err
	}
	for fid, cell := range g.Cells {
		geom, err := gdal.CreateFromWKT(cell.WKT, g.Sr)
		if err != nil {
			return err
		}
		data := []interface{}{cell.Row, cell.Col}
		index := []int{0, 1}
		for i, d := range cell.OtherFieldData {
			data = append(data, d)
			index = append(index, i+2)
		}
		err = shp.WriteFeature(fid, geom, index, data...)
		if err != nil {
			return err
		}
		geom.Destroy()
	}
	shp.Close()
	return nil
}
