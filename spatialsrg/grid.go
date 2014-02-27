package spatialsrg

import (
	"bitbucket.org/ctessum/gis"
	"bitbucket.org/ctessum/sparse"
	"fmt"
	"github.com/dhconnelly/rtreego"
	"github.com/lukeroth/gdal"
	"github.com/paulsmith/gogeos/geos"
	"github.com/twpayne/gogeom/geom"
	"io"
	"os"
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
	ggeom          geom.T
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
	sr gdal.SpatialReference) (grid *GridDef) {
	var err error
	grid = new(GridDef)
	grid.Name = Name
	grid.Nx, grid.Ny = Nx, Ny
	grid.Dx, grid.Dy = Dx, Dy
	grid.X0, grid.Y0 = X0, Y0
	grid.Sr = sr
	grid.rtree = rtreego.NewTree(2, 25, 50)
	// Create geometry
	grid.Cells = make([]*GridCell, grid.Nx*grid.Ny)
	i := 0
	for ix := 0; ix < grid.Nx; ix++ {
		for iy := 0; iy < grid.Ny; iy++ {
			cell := new(GridCell)
			x := grid.X0 + float64(ix)*grid.Dx
			y := grid.Y0 + float64(iy)*grid.Dy
			cell.WKT = fmt.Sprintf("POLYGON ((%v %v, %v %v, %v %v, %v %v, %v %v))",
				x, y, x+grid.Dx, y, x+grid.Dx, y+grid.Dy,
				x, y+grid.Dy, x, y)
			cell.Row, cell.Col = iy, ix
			cell.geom, err = geos.FromWKT(cell.WKT)
			if err != nil {
				panic(err)
			}
			var tempGeom geom.Polygon
			tempGeom.Rings = make([][]geom.Point, 1)
			tempGeom.Rings[0] = []geom.Point{{x, y}, {x + grid.Dx, y},
				{x + grid.Dx, y + grid.Dy}, {x, y + grid.Dy}, {x, y}}
			cell.ggeom = tempGeom
			cell.rtreebounds, err = gis.GeosToRect(cell.geom)
			if err != nil {
				panic(err)
			}
			grid.rtree.Insert(cell)
			grid.Cells[i] = cell
			i++
		}
	}
	grid.ExtentWKT = fmt.Sprintf("POLYGON ((%v %v, %v %v, %v %v, %v %v, %v %v))",
		X0, Y0, X0+Dx*float64(Nx), Y0, X0+Dx*float64(Nx), Y0+Dy*float64(Ny),
		X0, Y0+Dy*float64(Ny), X0, Y0)
	grid.extent, err = geos.FromWKT(grid.ExtentWKT)
	if err != nil {
		panic(err)
	}
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
	shp, err = gis.OpenShapefile(shapefilePath, true)
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
	var ct *gis.CoordinateTransform
	ct, err = gis.NewCoordinateTransform(shp.Sr, grid.Sr)
	if err != nil {
		return
	}
	grid.rtree = rtreego.NewTree(2, 25, 50)
	var w bool
	i := 0
	for {
		cell := new(GridCell)
		cell.ggeom, cell.OtherFieldData, err =
			shp.ReadNextFeature(columnIndicies...)
		if err != nil {
			if err == io.EOF {
				err = nil
				break
			}
			return
		}
		cell.ggeom, err = ct.Reproject(cell.ggeom)
		if err != nil {
			return
		}
		cell.geom, err = gis.GeomToGEOS(cell.ggeom)
		if err != nil {
			return
		}
		cell.WKT, err = cell.geom.ToWKT()
		if err != nil {
			return
		}
		cell.Row = i
		grid.Cells[i] = cell

		if grid.extent == nil {
			grid.extent = cell.geom
		}
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
		cell.rtreebounds, err = gis.GeosToRect(cell.geom)
		if err != nil {
			return
		}
		grid.rtree.Insert(cell)
		i++
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
	var tzShp *gis.Shapefile
	timezones, tzShp, err = getTimeZones(tzFile, tzColumn)
	if err != nil {
		return
	}
	grid.AddOtherFieldNames("timezone")
	grid.TimeZones = make(map[int]*sparse.SparseArray)

	ct := gdal.CreateCoordinateTransform(grid.Sr, tzShp.Sr)

	var cellCenter *geos.Geometry
	for _, cell := range grid.Cells {
		// find timezone nearest to the center of the cell.
		// Need to project grid to timezone projection rather than the
		// other way around because the timezones include the north
		// and south poles which don't convert well to other projections.
		cellCenter, err = cell.geom.Centroid()
		if err != nil {
			return
		}
		cellCenter, err = gis.GeosTransform(cellCenter, grid.Sr, ct)
		if err != nil {
			return
		}
		var tz float64
		var foundtz, intersects bool
		for _, tzData := range timezones {
			intersects, err = tzData.geom.Intersects(cellCenter)
			if err != nil {
				return
			}
			if intersects {
				if foundtz {
					err = fmt.Errorf("In spatialsrg.GetTimeZones, there is a " +
						"grid cell that overlaps with more than one timezone")
					return
				}
				tz = tzData.tz
				foundtz = true
			}
		}
		if !foundtz {
			err = fmt.Errorf("In spatialsrg.GetTimeZones, there is a " +
				"grid cell that doesn't match any timezones")
			return
		}
		cell.AddOtherFieldData(tz)
		tzSeconds := int(tz * 3600.)
		if _, ok := grid.TimeZones[tzSeconds]; !ok {
			grid.TimeZones[tzSeconds] = sparse.ZerosSparse(grid.Ny, grid.Nx)
		}
		grid.TimeZones[tzSeconds].Set(1., cell.Row, cell.Col)
	}
	tzShp.Close()
	return
}

type tzHolder struct {
	tz   float64
	geom *geos.Geometry
}

func getTimeZones(tzFile, tzColumn string) (
	timezones []*tzHolder, tzShp *gis.Shapefile, err error) {
	timezones = make([]*tzHolder, 0, 50)

	tzShp, err = gis.OpenShapefile(tzFile, true)
	if err != nil {
		return
	}
	var tzIndex int
	tzIndex, err = tzShp.GetColumnIndex(tzColumn)
	if err != nil {
		return
	}
	for {
		tzData := new(tzHolder)
		var tzTemp []interface{}
		var ggeom geom.T
		ggeom, tzTemp, err = tzShp.ReadNextFeature(tzIndex)
		if err != nil {
			if err == io.EOF {
				err = nil
				break
			}
			return
		}
		tzData.geom, err = gis.GeomToGEOS(ggeom)
		if err != nil {
			return
		}
		tzData.tz = tzTemp[0].(float64)
		timezones = append(timezones, tzData)
	}
	return
}

func (grid *GridDef) GetIndex(x, y float64, inputSr *gdal.SpatialReference,
	ct *gdal.CoordinateTransform) (
	X, Y float64, row, col int, withinGrid bool, err error) {
	var g gdal.Geometry
	g, err = gdal.CreateFromWKT(fmt.Sprintf("POINT (%v %v)", x, y), *inputSr)
	if err.Error() != "No Error" {
		return
	}
	err = g.Transform(*ct)
	if err.Error() != "No Error" {
		return
	}
	X, Y, _ = g.Point(0) // coordinates transformed to output projection
	var geosPoint *geos.Geometry
	geosPoint, err = geos.NewPoint(geos.NewCoord(X, Y))
	if err != nil {
		return
	}
	withinGrid, err = geosPoint.Within(grid.extent)
	if err != nil {
		return
	}
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
	for _, ext := range []string{".shp", ".prj", ".dbf", ".shx"} {
		os.Remove(filepath.Join(outdir, g.Name+ext))
	}
	colNames := []string{"row", "col"}
	exampleData := []interface{}{0, 0}
	if len(g.OtherFieldNames) != len(g.Cells[0].OtherFieldData) {
		return fmt.Errorf("OtherFieldNames (%v) is not the same length "+
			"as OtherFieldData (%v)", g.OtherFieldNames,
			g.Cells[0].OtherFieldData)
	}
	for i, name := range g.OtherFieldNames {
		colNames = append(colNames, name)
		exampleData = append(exampleData, g.Cells[0].OtherFieldData[i])
	}
	var shp *gis.Shapefile
	shp, err = gis.CreateShapefile(outdir, g.Name,
		g.Sr, gdal.GT_Polygon, colNames, exampleData...)
	if err != nil {
		return err
	}
	for fid, cell := range g.Cells {
		data := []interface{}{cell.Row, cell.Col}
		index := []int{0, 1}
		for i, d := range cell.OtherFieldData {
			data = append(data, d)
			index = append(index, i+2)
		}
		err = shp.WriteFeature(fid, cell.geom, index, data...)
		if err != nil {
			return err
		}
	}
	shp.Close()
	return nil
}
