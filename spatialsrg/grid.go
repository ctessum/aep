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
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"bitbucket.org/ctessum/gis"
	"bitbucket.org/ctessum/sparse"
	"github.com/ctessum/geomconv"
	"github.com/ctessum/geomop"
	"github.com/ctessum/projgeom"
	"github.com/ctessum/shapefile"
	"github.com/lukeroth/gdal"
	"github.com/patrick-higgins/rtreego"
	"github.com/twpayne/gogeom/geom"
)

type GridDef struct {
	Name            string
	Nx, Ny          int
	Dx, Dy          float64
	X0, Y0          float64
	TimeZones       map[string]*sparse.SparseArray
	Cells           []*GridCell
	Sr              gdal.SpatialReference
	Extent          geom.T
	IrregularGrid   bool     // whether the grid is a regular grid
	OtherFieldNames []string // names of other columns we want to keep
	rtree           *rtreego.Rtree
	srgMapCache     cacheDB
}

type GridCell struct {
	Geom           geom.T
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
	grid.rtree = rtreego.NewTree(25, 50)
	// Create geometry
	grid.Cells = make([]*GridCell, grid.Nx*grid.Ny)
	i := 0
	for ix := 0; ix < grid.Nx; ix++ {
		for iy := 0; iy < grid.Ny; iy++ {
			cell := new(GridCell)
			x := grid.X0 + float64(ix)*grid.Dx
			y := grid.Y0 + float64(iy)*grid.Dy
			cell.Row, cell.Col = iy, ix
			cell.Geom = geom.T(geom.Polygon{[][]geom.Point{{
				{x, y}, {x + grid.Dx, y},
				{x + grid.Dx, y + grid.Dy}, {x, y + grid.Dy}, {x, y}}}})
			cell.rtreebounds, err = geomconv.GeomToRect(cell.Geom)
			if err != nil {
				panic(err)
			}
			grid.rtree.Insert(cell)
			grid.Cells[i] = cell
			i++
		}
	}
	grid.Extent = geom.T(geom.Polygon{[][]geom.Point{{{X0, Y0},
		{X0 + Dx*float64(Nx), Y0},
		{X0 + Dx*float64(Nx), Y0 + Dy*float64(Ny)},
		{X0, Y0 + Dy*float64(Ny)}, {X0, Y0}}}})
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
	var ct *projgeom.CoordinateTransform
	ct, err = projgeom.NewCoordinateTransform(shp.Sr, grid.Sr)
	if err != nil {
		return
	}
	grid.rtree = rtreego.NewTree(25, 50)
	i := 0
	for {
		cell := new(GridCell)
		cell.Geom, cell.OtherFieldData, err =
			shp.ReadNextFeature(columnIndicies...)
		if err != nil {
			if err == io.EOF {
				err = nil
				break
			}
			return
		}
		cell.Geom, err = ct.Reproject(cell.Geom)
		if err != nil {
			return
		}
		cell.Row = i
		grid.Cells[i] = cell

		if grid.Extent == nil {
			grid.Extent = cell.Geom
		} else {
			grid.Extent = geomop.Construct(grid.Extent, cell.Geom, geomop.UNION)
		}
		cell.rtreebounds, err = geomconv.GeomToRect(cell.Geom)
		if err != nil {
			return
		}
		grid.rtree.Insert(cell)
		i++
	}
	return
}

// Get time zones.
func (grid *GridDef) GetTimeZones(tzFile, tzColumn string) (err error) {

	var timezones []*tzHolder
	timezones, err = getTimeZones(tzFile, tzColumn)
	if err != nil {
		return
	}
	grid.AddOtherFieldNames("timezone")
	grid.TimeZones = make(map[string]*sparse.SparseArray)

	f, err := os.Open(strings.Replace(tzFile, ".shp", ".prj", -1))
	if err != nil {
		return
	}
	tzsr, err := projgeom.ReadPrj(f)
	if err != nil {
		return
	}
	var ct *projgeom.CoordinateTransform
	ct, err = projgeom.NewCoordinateTransform(grid.Sr, tzsr)
	if err != nil {
		return
	}

	var cellCenter geom.T
	for _, cell := range grid.Cells {
		// find timezone nearest to the center of the cell.
		// Need to project grid to timezone projection rather than the
		// other way around because the timezones can include the north
		// and south poles which don't convert well to other projections.
		cellCenter = geomop.Centroid(cell.Geom)
		cellCenter, err = ct.Reproject(cellCenter)
		if err != nil {
			return
		}
		var tz string
		var foundtz, intersects bool
		for _, tzData := range timezones {
			intersects = geomop.Within(cellCenter, tzData.Geom)
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
			//err = fmt.Errorf("In spatialsrg.GetTimeZones, there is a " +
			//	"grid cell that doesn't match any timezones")
			//return
			tz = "UTC" // The timezone shapefile doesn't include timezones
			// over the ocean, so we assume all timezones that don't have
			// tz info are UTC.
		}
		cell.AddOtherFieldData(tz)
		if _, ok := grid.TimeZones[tz]; !ok {
			grid.TimeZones[tz] = sparse.ZerosSparse(grid.Ny, grid.Nx)
		}
		grid.TimeZones[tz].Set(1., cell.Row, cell.Col)
	}
	return
}

type tzHolder struct {
	tz   string
	Geom geom.T
}

func getTimeZones(tzFile, tzColumn string) ([]*tzHolder, error) {
	timezones := make([]*tzHolder, 0, 50)

	f1, err := os.Open(tzFile)
	if err != nil {
		return nil, err
	}
	defer f1.Close()
	tzShp, err := shapefile.OpenShapefile(f1)
	if err != nil {
		return nil, err
	}
	f2, err := os.Open(strings.Replace(tzFile, ".shp", ".dbf", -1))
	if err != nil {
		return nil, err
	}
	defer f2.Close()
	tzDBF, err := shapefile.OpenDBFFile(f2)
	if err != nil {
		return nil, err
	}
	tzIndex, ok := tzDBF.FieldIndicies[tzColumn]
	if !ok {
		err = fmt.Errorf("TZ shapefile doesn't contain column %v", tzColumn)
	}
	for {
		rec, err := tzShp.NextRecord()
		if err != nil {
			if err == io.EOF {
				break
			}
			return nil, err
		}
		tzData := new(tzHolder)
		tzData.Geom = rec.Geometry
		fields, err := tzDBF.NextRecord()
		if err != nil {
			return nil, err
		}
		tzData.tz = fields[tzIndex].(string)
		timezones = append(timezones, tzData)
	}
	return timezones, nil
}
func stringToFloat(s string) float64 {
	f, err := strconv.ParseFloat(trimString(s), 64)
	if err != nil {
		panic(err)
	} else {
		return f
	}
}

func trimString(s string) string {
	return strings.Trim(s, "\" ")
}

func (grid *GridDef) GetIndex(x, y float64, inputSr *gdal.SpatialReference,
	ct *projgeom.CoordinateTransform) (
	X, Y float64, row, col int, withinGrid bool, err error) {
	g := geom.T(geom.Point{x, y})
	g, err = ct.Reproject(g)
	if err != nil {
		return
	}
	gp := g.(geom.Point)
	X, Y = gp.X, gp.Y // coordinates transformed to output projection
	withinGrid = geomop.Within(g, grid.Extent)
	if !withinGrid {
		return
	}
	rtreepoint := rtreego.Point([3]float64{X, Y, 0.})
	gridCellTemp := grid.rtree.NearestNeighbor(rtreepoint)
	cell := gridCellTemp.(*GridCell)
	row = cell.Row
	col = cell.Col
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
		err = shp.WriteFeature(fid, cell.Geom, index, data...)
		if err != nil {
			return err
		}
	}
	shp.Close()
	return nil
}
