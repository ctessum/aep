package spatialsrg

import (
	"bitbucket.org/ctessum/gis"
	"database/sql"
	"fmt"
	_ "github.com/lib/pq"
	"github.com/paulsmith/gogeos/geos"
	"github.com/pebbe/go-proj-4/proj"
)

type GridDef struct {
	Name          string
	Nx, Ny        int
	Dx, Dy        float64
	X0, Y0        float64
	TimeZones     map[int]*sparse.SparseArray
	Cells         []*gridCell
	Sr            gdal.SpatialReference
	ExtentWKT     string
	IrregularGrid bool // whether the grid is a regular grid
}

type GridCell struct {
	geom      *geos.Geometry
	WKT       string
	Row, Col  int
	otherInfo []interface{}
	Weight    float64
}

func NewGridRegular(Name string, Nx, Ny int, Dx, Dy, X0, Y0 float64,
	sr gdal.SpatialReference) (grid *GridDef, err error) {
	grid = new(GridDef)
	grid.Name = Name
	grid.Nx, grid.Ny = Nx, Ny
	grid.Dx, grid.Dy = Dx, Dy
	grid.X0, grid.Y0 = X0, Y0
	grid.Sr = Sr
	grid.TimeZones = make(map[int]*sparse.SparseArray)
	// Create geometry
	grid.Cells = make([]*GidCell, grid.Nx*grid.Ny)
	for ix := 0; ix < grid.Nx; ix++ {
		for iy := 0; iy < grid.Ny; iy++ {
			cell := new(gridCell)
			x := grid.X0 + float64(ix)*grid.Dx
			y := grid.Y0 + float64(iy)*grid.Dy
			cell.WKT = fmt.Sprintf("POINT ((%v %v, %v %v, %v %v, %v %v, %v %v))",
				x, y, x+grid.Dx, y, x+grid.Dx, y+grid.Dy,
				x, y+grid.Dy, x, y)
			cell.Id, cell.Row, cell.Col = id, ix, iy
			cell.geom, err = geos.FromWKT(cell.WKT)
			if err != nil {
				return
			}
		}
	}
	grid.ExtentWKT = fmt.Sprintf("POINT ((%v %v, %v %v, %v %v, %v %v, %v %v))",
		X0, Y0, X0+Dx*Nx, Y0, X0+Dx*Nx, Y0+Dy*Ny,
		X0, Y0+Dy*Ny, X0, Y0)
	return
}

// Create grid geometry, get time zones, and save to PostGIS database.
func (grid *GridDef) GridGeomWithTimeZone(
	tzFile, tzColumn string) (err error) {
	var timezones []*tzHolder
	var cell *geos.Geometry
	var cmd string
	timezones, err = getTimeZones(pg, tzSchema, tzTable, tzColumn)
	if err != nil {
		return
	}

	pg.DropTable(grid.Schema, grid.Name)
	tx, err := pg.Db.Begin()
	if err != nil {
		return
	}
	cmd = fmt.Sprintf("CREATE TABLE %v.\"%v\" (row int, col int, "+
		"timezone double precision, geom geometry);",
		grid.Schema, grid.Name)
	Log(cmd, 3)
	_, err = tx.Exec(cmd)
	if err != nil {
		err = handle(err, cmd)
		tx.Rollback()
		return
	}

	var tzProj, gridProj *proj.Proj
	tzSRID := pg.GetSRID(tzSchema, tzTable)
	tzProj, err = proj.NewProj(pg.GetSRIDProjString(tzSRID))
	if err != nil {
		err = handle(err, "converting timezone SRID to proj")
		tx.Rollback()
		return
	}
	gridProj, err = proj.NewProj(pg.GetSRIDProjString(grid.SRID))
	if err != nil {
		err = handle(err, "converting grid SRID to proj")
		tx.Rollback()
		return
	}
	defer tzProj.Close()
	defer gridProj.Close()

	for ix := 0; ix < grid.Nx; ix++ {
		for iy := 0; iy < grid.Ny; iy++ {
			x := grid.X0 + float64(ix)*grid.Dx
			y := grid.Y0 + float64(iy)*grid.Dy
			cell, err = geos.NewPolygon([]geos.Coord{
				geos.NewCoord(x, y),
				geos.NewCoord(x+grid.Dx, y),
				geos.NewCoord(x+grid.Dx, y+grid.Dy),
				geos.NewCoord(x, y+grid.Dy),
				geos.NewCoord(x, y)})
			if err != nil {
				tx.Rollback()
				return
			}
			// find timezone nearest to the center of the cell.
			xcenter := x + grid.Dx/2.
			ycenter := y + grid.Dy/2.
			if tzSRID != grid.SRID {
				// Need to project grid to timezone projection rather than the
				// other way around because the timezones include the north
				// and south poles which don't convert well to other projections.
				xcenter, ycenter, err = proj.Transform2(gridProj, tzProj,
					xcenter, ycenter)
				if err != nil {
					err = handle(err, "Reprojecting grid cell center")
					tx.Rollback()
					return
				}
				xcenter = proj.RadToDeg(xcenter)
				ycenter = proj.RadToDeg(ycenter)
			}
			var cellCenter *geos.Geometry
			cellCenter, err = geos.NewPoint(geos.NewCoord(xcenter, ycenter))
			if err != nil {
				err = handle(err, "Creating cell center")
				tx.Rollback()
				return
			}
			var tz float64
			var foundtz, intersects bool
			for _, tzData := range timezones {
				intersects, err = tzData.geom.Intersects(cellCenter)
				if err != nil {
					err = handle(err, "checking intersections")
					tx.Rollback()
					return
				}
				if intersects {
					if foundtz {
						tx.Rollback()
						panic("Cell matches more than one timezone")
					}
					tz = tzData.tz
					foundtz = true
				}
			}
			if !foundtz {
				tx.Rollback()
				panic("Cell doesn't match any timezones")
			}

			var wkt string
			wkt, err = cell.ToWKT()
			if err != nil {
				tx.Rollback()
				return
			}
			cmd = fmt.Sprintf("INSERT INTO %v.\"%v\" "+
				"(row, col, timezone, geom) "+
				"VALUES (%v,%v,%v,ST_GeomFromEWKT('%v'));",
				grid.Schema, grid.Name, iy, ix, tz, wkt)
			Log(cmd, 4)
			_, err = tx.Exec(cmd)
			if err != nil {
				tx.Rollback()
				err = handle(err, cmd)
				return
			}
		}
	}
	err = tx.Commit()
	if err != nil {
		return
	}
	pg.AddGid(grid.Schema, grid.Name)
	pg.AddGix(grid.Schema, grid.Name)
	pg.UpdateSRID(grid.Schema, grid.Name, grid.SRID)
	pg.VacuumAnalyze(grid.Schema, grid.Name)
	return
}

type tzHolder struct {
	tz   float64
	geom *geos.Geometry
}

func getTimeZones(pg *gis.PostGis, tzSchema, tzTable, tzColumn string) (
	timezones []*tzHolder, err error) {
	timezones = make([]*tzHolder, 0, 50)

	if !pg.TableExists(tzSchema, tzTable) {
		err = fmt.Errorf("Shapefile \"%v\" is not loaded in PostGIS "+
			"schema \"%v\". See file test/loadshp2.sh for an example of how to "+
			"get and load this file.", tzTable, tzSchema)
		return
	}

	tzQuery := fmt.Sprintf("SELECT %v, ST_AsText(geom) FROM %v.\"%v\"",
		tzColumn, tzSchema, tzTable)
	Log(tzQuery, 3)
	var tzRows *sql.Rows
	tzRows, err = pg.Db.Query(tzQuery)
	if err != nil {
		err = handle(err, tzQuery)
		return
	}
	var geomWKT string
	var geom *geos.Geometry
	for tzRows.Next() {
		var tz sql.NullFloat64
		err = tzRows.Scan(&tz, &geomWKT)
		if err != nil {
			err = handle(err, "scanning timezones")
			return
		}
		if !tz.Valid {
			continue
		}
		geom, err = geos.FromWKT(geomWKT)
		if err != nil {
			err = handle(err, "parsing timezone geometry")
			return
		}
		if geom != nil {
			tzData := &tzHolder{tz: tz.Float64, geom: geom}
			timezones = append(timezones, tzData)
		}
	}
	tzRows.Close()
	err = tzRows.Err()
	if err != nil {
		err = handle(err, "getting timezones")
		return
	}
	return
}
