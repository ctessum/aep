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
	"encoding/gob"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"sync"

	"bitbucket.org/ctessum/sparse"
	"github.com/ctessum/geom"
	"github.com/ctessum/geom/encoding/shp"
	"github.com/ctessum/geom/index/rtree"
	"github.com/ctessum/geom/op"
	"github.com/ctessum/geom/proj"
	goshp "github.com/jonas-p/go-shp"
)

func init() {
	gob.Register(geom.Polygon{})
}

type GridDef struct {
	Name            string
	Nx, Ny          int
	Dx, Dy          float64
	X0, Y0          float64
	TimeZones       map[string]*sparse.SparseArray
	Cells           []*GridCell
	Sr              proj.SR
	Extent          geom.T
	IrregularGrid   bool     // whether the grid is a regular grid
	OtherFieldNames []string // names of other columns we want to keep
	rtree           *rtree.Rtree
	srgMapCache     cacheDB
}

type GridCell struct {
	geom.T
	Row, Col       int
	OtherFieldData []interface{} // data for the other columns we want to keep
	Weight         float64
}

func (c *GridCell) Copy() *GridCell {
	o := new(GridCell)
	o.T = c.T
	o.Row, o.Col = c.Row, c.Col
	return o
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
	sr proj.SR) (grid *GridDef) {
	grid = new(GridDef)
	grid.Name = Name
	grid.Nx, grid.Ny = Nx, Ny
	grid.Dx, grid.Dy = Dx, Dy
	grid.X0, grid.Y0 = X0, Y0
	grid.Sr = sr
	grid.rtree = rtree.NewTree(25, 50)
	// Create geometry
	grid.Cells = make([]*GridCell, grid.Nx*grid.Ny)
	i := 0
	for ix := 0; ix < grid.Nx; ix++ {
		for iy := 0; iy < grid.Ny; iy++ {
			cell := new(GridCell)
			x := grid.X0 + float64(ix)*grid.Dx
			y := grid.Y0 + float64(iy)*grid.Dy
			cell.Row, cell.Col = iy, ix
			cell.T = geom.T(geom.Polygon([][]geom.Point{{
				{x, y}, {x + grid.Dx, y},
				{x + grid.Dx, y + grid.Dy}, {x, y + grid.Dy}, {x, y}}}))
			grid.rtree.Insert(cell)
			grid.Cells[i] = cell
			i++
		}
	}
	grid.Extent = geom.T(geom.Polygon([][]geom.Point{{{X0, Y0},
		{X0 + Dx*float64(Nx), Y0},
		{X0 + Dx*float64(Nx), Y0 + Dy*float64(Ny)},
		{X0, Y0 + Dy*float64(Ny)}, {X0, Y0}}}))
	return
}

// Irregular grids have 1 column and n rows, where n is the number of
// shapes in the shapefile.
func NewGridIrregular(Name, shapefilePath string, columnsToKeep []string,
	outputSr proj.SR) (grid *GridDef, err error) {
	grid = new(GridDef)
	grid.Name = Name
	grid.Sr = outputSr
	grid.IrregularGrid = true
	var shpf *shp.Decoder
	shpf, err = shp.NewDecoder(shapefilePath)
	if err != nil {
		return
	}
	var srf *os.File
	srf, err = os.Open(strings.TrimSuffix(shapefilePath, ".shp") + ".prj")
	if err != nil {
		return
	}
	var shpSR proj.SR
	shpSR, err = proj.ReadPrj(srf)
	if err != nil {
		return
	}
	grid.Cells = make([]*GridCell, shpf.AttributeCount())
	grid.Ny = shpf.AttributeCount()
	grid.Nx = 1
	grid.OtherFieldNames = columnsToKeep
	var ct *proj.CoordinateTransform
	ct, err = proj.NewCoordinateTransform(shpSR, grid.Sr)
	if err != nil {
		return
	}
	grid.rtree = rtree.NewTree(25, 50)
	i := 0
	for {
		cell := new(GridCell)
		g, fields, more := shpf.DecodeRowFields(columnsToKeep...)
		if !more {
			break
		}
		for _, col := range columnsToKeep {
			cell.OtherFieldData = append(cell.OtherFieldData, fields[col])
		}
		cell.T, err = ct.Reproject(g)
		if err != nil {
			return
		}
		cell.Row = i
		grid.Cells[i] = cell

		if grid.Extent == nil {
			grid.Extent = cell.T
		} else {
			grid.Extent, err = op.Construct(grid.Extent, cell.T, op.UNION)
			if err != nil {
				return
			}
			grid.Extent, err = op.Simplify(grid.Extent, 1.e-8)
			if err != nil {
				return
			}
		}
		grid.rtree.Insert(cell)
		i++
	}
	return
}

// Get time zones.
func (grid *GridDef) GetTimeZones(tzFile, tzColumn string) error {

	var err error
	var timezones *rtree.Rtree
	timezones, err = getTimeZones(tzFile, tzColumn)
	if err != nil {
		return err
	}
	grid.AddOtherFieldNames("timezone")
	grid.TimeZones = make(map[string]*sparse.SparseArray)

	f, err := os.Open(strings.Replace(tzFile, ".shp", ".prj", -1))
	if err != nil {
		return err
	}
	tzsr, err := proj.ReadPrj(f)
	if err != nil {
		return err
	}
	var ct *proj.CoordinateTransform
	ct, err = proj.NewCoordinateTransform(grid.Sr, tzsr)
	if err != nil {
		return err
	}

	var lock sync.Mutex
	errChan := make(chan error)
	nprocs := runtime.GOMAXPROCS(-1)
	for proc := 0; proc < nprocs; proc++ {
		go func(proc int) {
			var err2 error
			var cellCenter geom.T
			for ii := proc; ii < len(grid.Cells); ii += nprocs {
				cell := grid.Cells[ii]
				// find timezone nearest to the center of the cell.
				// Need to project grid to timezone projection rather than the
				// other way around because the timezones can include the north
				// and south poles which don't convert well to other projections.
				cellCenter, err2 = op.Centroid(cell.T)
				if err2 != nil {
					errChan <- err2
					return
				}
				cellCenter, err2 = ct.Reproject(cellCenter)
				if err2 != nil {
					errChan <- err2
					return
				}
				var tz string
				var foundtz, intersects bool
				for _, tzDataI := range timezones.SearchIntersect(cellCenter.Bounds(nil)) {
					tzData := tzDataI.(*tzHolder)
					intersects, err2 = op.Within(cellCenter, tzData.T)
					if err2 != nil {
						errChan <- err2
						return
					}
					if intersects {
						if foundtz {
							panic("In spatialsrg.GetTimeZones, there is a " +
								"grid cell that overlaps with more than one timezone." +
								" This probably shouldn't be happening.")
							break
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
				lock.Lock()
				if _, ok := grid.TimeZones[tz]; !ok {
					grid.TimeZones[tz] = sparse.ZerosSparse(grid.Ny, grid.Nx)
				}
				grid.TimeZones[tz].Set(1., cell.Row, cell.Col)
				lock.Unlock()
			}
			errChan <- nil
		}(proc)
	}
	for procnum := 0; procnum < nprocs; procnum++ {
		if err = <-errChan; err != nil {
			return err
		}
	}
	return nil
}

type tzHolder struct {
	tz string
	geom.T
}

func getTimeZones(tzFile, tzColumn string) (*rtree.Rtree, error) {
	timezones := rtree.NewTree(25, 50)

	tzShp, err := shp.NewDecoder(tzFile)
	if err != nil {
		return nil, err
	}
	defer tzShp.Close()
	for {
		g, fields, more := tzShp.DecodeRowFields(tzColumn)
		if !more {
			break
		}

		tzData := new(tzHolder)
		tzData.T = g
		tzData.tz = fields[tzColumn]
		timezones.Insert(tzData)
	}
	return timezones, tzShp.Error()
}

func (grid *GridDef) GetIndex(x, y float64, inputSr proj.SR,
	ct *proj.CoordinateTransform) (
	X, Y float64, row, col int, withinGrid bool, err error) {
	g := geom.T(geom.Point{x, y})
	g, err = ct.Reproject(g)
	if err != nil {
		return
	}
	gp := g.(geom.Point)
	X, Y = gp.X, gp.Y // coordinates transformed to output projection
	withinGrid, err = op.Within(g, grid.Extent)
	if err != nil || !withinGrid {
		return
	}
	gridCellsTemp := grid.rtree.SearchIntersect(g.Bounds(nil))
	cell := gridCellsTemp[0].(*GridCell)
	row = cell.Row
	col = cell.Col
	return
}

func (g *GridDef) WriteToShp(outdir string) error {
	var err error
	for _, ext := range []string{".shp", ".prj", ".dbf", ".shx"} {
		os.Remove(filepath.Join(outdir, g.Name+ext))
	}
	if len(g.OtherFieldNames) != len(g.Cells[0].OtherFieldData) {
		return fmt.Errorf("OtherFieldNames (%v) is not the same length "+
			"as OtherFieldData (%v)", g.OtherFieldNames,
			g.Cells[0].OtherFieldData)
	}
	fields := make([]goshp.Field, 2+len(g.OtherFieldNames))
	fields[0] = goshp.NumberField("row", 10)
	fields[1] = goshp.NumberField("col", 10)
	for i, name := range g.OtherFieldNames {
		switch g.Cells[0].OtherFieldData[i].(type) {
		case float64:
			fields[i+2] = goshp.FloatField(name, 20, 10)
		case int:
			fields[i+2] = goshp.NumberField(name, 10)
		case string:
			fields[i+2] = goshp.StringField(name, 20)
		}
	}
	var shpf *shp.Encoder
	shpf, err = shp.NewEncoderFromFields(filepath.Join(outdir, g.Name),
		goshp.POLYGON, fields...)
	if err != nil {
		return err
	}
	for _, cell := range g.Cells {
		data := []interface{}{cell.Row, cell.Col}
		for _, d := range cell.OtherFieldData {
			data = append(data, d)
		}
		err = shpf.EncodeFields(cell.T, data...)
		if err != nil {
			return err
		}
	}
	shpf.Close()
	return nil
}
