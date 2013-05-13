package gis

import (
	"bytes"
	"database/sql"
	"encoding/json"
	"fmt"
	_ "github.com/bmizerany/pq"
	"github.com/skelterjohn/go.matrix"
	"log"
	"os"
	"strings"
	"text/template"
)

var (
	DebugLevel = 3 // amout of output to print to the screen. A higher number means more messages.
)

func Log(msg string, debug int) {
	if debug <= DebugLevel {
		log.Println(msg)
	}
}

type PostGis struct {
	db *sql.DB
}

func Connect(user, dbname, password string) (db *PostGis, err error) {
	db = new(PostGis)
	db.db, err = sql.Open("postgres", fmt.Sprintf(
		"user=%v dbname=%v password=%v", user, dbname, password))
	return
}

func (pg *PostGis) Disconnect() error {
	err := pg.db.Close()
	return err
}

func (pg *PostGis) NewProjection(SRID int, proj string, lat_1 float64, lat_2 float64,
	lat_0 float64, lon_0 float64, EarthRadius float64) (err error) {

	ProjString := fmt.Sprintf("+proj=%s +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +a=%f +b=%f +to_meter=1",
		proj, lat_1, lat_2, lat_0, lon_0, EarthRadius, EarthRadius)

	var currentProj4 string
	cmd := fmt.Sprintf("SELECT proj4text from spatial_ref_sys "+
		"where SRID=%v", SRID)
	Log(cmd, 3)
	// See if projection already exists
	err = pg.db.QueryRow(cmd).Scan(&currentProj4)
	if err != nil {
		if err.Error() == "sql: no rows in result set" {
			err = nil
			// if it doesn't exist, create it
			cmd := fmt.Sprintf("INSERT INTO spatial_ref_sys "+
				"(srid, auth_name, auth_srid, proj4text) "+
				"VALUES (%v,'aep',%v,'%v');", SRID, SRID, ProjString)
			Log(cmd, 3)
			_, err = pg.db.Exec(cmd)
			return
		} else {
			err = fmt.Errorf("In gis.NewProjection:\n%v", err.Error())
			return
		}
	} else {
		if currentProj4 != ProjString {
			// if it does exist but is different from our projection,
			// throw an error.
			err = fmt.Errorf("Projection already exists (and does not match"+
				" new projection) for SRID %v.\nold proj=%v\nnew proj=%v\n"+
				"Try a different SRID code in configuration file (it "+
				"doesn't matter what number is chosen as long as it's "+
				"not already in use).",
				SRID, currentProj4, ProjString)
			return
		}
	}
	return
}

type GridDef struct {
	Name   string
	Nx     int
	Ny     int
	Dx     float64
	Dy     float64
	X0     float64
	Y0     float64
	SRID   int    // PostGIS projection type (Spatial reference ID)
	Schema string // name of the Postgres schema it will be saved in
}

func NewGrid(Name string, Nx, Ny int, Dx, Dy, X0, Y0 float64,
	srid int, schema string) (
	grid *GridDef) {
	grid = new(GridDef)
	grid.Name = Name
	grid.Nx = Nx
	grid.Ny = Ny
	grid.Dx = Dx
	grid.Dy = Dy
	grid.X0 = X0
	grid.Y0 = Y0
	grid.SRID = srid
	grid.Schema = schema
	return
}

func (pg *PostGis) CreateGrid(grid *GridDef) error {

	const t = `
CREATE TABLE {{.Schema}}.{{.Name}}  AS
SELECT i + 1 AS row, j + 1 AS col,
ST_Translate(cell, j * {{.Dx}} + {{.X0}}, i * {{.Dy}} + {{.Y0}}) AS geom
FROM generate_series(0, {{.Ny}} - 1) AS i,generate_series(0, {{.Nx}} - 1) AS j,
(SELECT ('POLYGON((0 0, 0 {{.Dy}}, {{.Dx}} {{.Dy}}, {{.Dx}} 0,0 0))')::geometry AS cell) AS foo;`

	pg.DropTable(grid.Schema, grid.Name)
	t2 := template.Must(template.New("CreateGrid").Parse(t))
	var b bytes.Buffer
	err := t2.Execute(&b, grid)
	if err != nil {
		return err
	}
	cmd := b.String()
	Log(cmd, 3)
	_, err = pg.db.Exec(cmd)
	if err != nil {
		return err
	}
	err = pg.AddGid(grid.Schema, grid.Name)
	if err != nil {
		return err
	}
	err = pg.AddGix(grid.Schema, grid.Name)
	if err != nil {
		return err
	}
	err = pg.UpdateSRID(grid.Schema, grid.Name, grid.SRID)
	if err != nil {
		return err
	}
	err = pg.VacuumAnalyze(grid.Schema, grid.Name)
	return err
}

func (pg *PostGis) DropTable(schema, name string) {
	pg.db.Exec("DROP TABLE " + schema + "." + name + ";")
	pg.db.Exec("DROP INDEX " + schema + "." + name + "_gix;")
}

func (pg *PostGis) DropSchema(name string) {
	pg.db.Exec("DROP SCHEMA " + name + " CASCADE;")
}

func (pg *PostGis) CreateSchema(name string) {
	pg.db.Exec("CREATE SCHEMA " + name + ";")
}

// Add ID column to table
func (pg *PostGis) AddGid(schema, name string) error {
	cmd := "alter table " + schema + "." + name + " add gid serial not null;"
	Log(cmd, 3)
	_, err := pg.db.Exec(cmd)
	return err
}

// Add spatial index to table
func (pg *PostGis) AddGix(schema, name string) error {
	cmd := "CREATE INDEX " + name + "_gix ON " + schema + "." + name +
		" USING GIST (geom);"
	Log(cmd, 3)
	_, err := pg.db.Exec(cmd)
	return err
}

func (pg *PostGis) TableExists(schema, name string) (exists bool) {
	cmd := fmt.Sprintf("SELECT EXISTS(SELECT 1 FROM information_schema.tables "+
		"WHERE table_schema='%v' AND table_name='%v');",
		strings.ToLower(schema), strings.ToLower(name))
	Log(cmd, 3)
	//	var exists string
	err := pg.db.QueryRow(cmd).Scan(&exists)
	if err != nil {
		panic(err)
	}
	return
}

// Change projection definition (without reprojecting)
func (pg *PostGis) UpdateSRID(schema, name string, SRID int) error {
	cmd := fmt.Sprintf("SELECT UpdateGeometrySRID('%v','%v', 'geom', %v);",
		strings.ToLower(schema), strings.ToLower(name), SRID)
	Log(cmd, 3)
	_, err := pg.db.Query(cmd)
	return err
}

func (pg *PostGis) VacuumAnalyze(schema, name string) error {
	cmd := "VACUUM ANALYZE " + schema + "." + name + ";"
	Log(cmd, 3)
	_, err := pg.db.Exec(cmd)
	return err
}

func (pg *PostGis) GetSRID(schema, name string) (SRID int) {
	cmd := fmt.Sprintf("SELECT ST_SRID(geom) FROM %v.\"%v\" LIMIT 1;", schema, name)
	Log(cmd, 3)
	err := pg.db.QueryRow(cmd).Scan(&SRID)
	if err != nil {
		panic(err)
	}
	return
}

func (pg *PostGis) GetNumPolygons(schema, name string) (polygons int) {
	cmd := fmt.Sprintf("SELECT Count(geom) from %v.\"%v\" "+
		"WHERE Geometrytype(geom)='POLYGON' "+
		"OR Geometrytype(geom)='MULTIPOLYGON';",
		strings.ToLower(schema), strings.ToLower(name))
	Log(cmd, 3)
	err := pg.db.QueryRow(cmd).Scan(&polygons)
	if err != nil {
		panic(err)
	}
	return
}
func (pg *PostGis) GetNumLines(schema, name string) (lines int) {
	cmd := fmt.Sprintf("SELECT Count(geom) from %v.\"%v\" "+
		"WHERE Geometrytype(geom)='LINESTRING' "+
		"OR Geometrytype(geom)='MULTILINESTRING';",
		strings.ToLower(schema), strings.ToLower(name))
	Log(cmd, 3)
	err := pg.db.QueryRow(cmd).Scan(&lines)
	if err != nil {
		panic(err)
	}
	return
}
func (pg *PostGis) GetNumPoints(schema, name string) (points int) {
	cmd := fmt.Sprintf("SELECT Count(geom) from %v.\"%v\" "+
		"WHERE Geometrytype(geom)='POINT';",
		strings.ToLower(schema), strings.ToLower(name))
	Log(cmd, 3)
	err := pg.db.QueryRow(cmd).Scan(&points)
	if err != nil {
		panic(err)
	}
	return
}
func (pg *PostGis) GetNumShapes(schema, name string) (shapes int) {
	cmd := fmt.Sprintf("SELECT Count(geom) from %v.\"%v\";",
		strings.ToLower(schema), strings.ToLower(name))
	Log(cmd, 3)
	err := pg.db.QueryRow(cmd).Scan(&shapes)
	if err != nil {
		panic(err)
	}
	return
}

func (pg *PostGis) CreateGriddingSurrogate(srgCode, shapeTable,
	ShapeColumn, surrogateTable, WeightColumns string,
	FilterFunction *SurrogateFilter, grid *GridDef,
	shapefileSchema string) (
	err error) {

	if !pg.TableExists(shapefileSchema, surrogateTable) {
		err = fmt.Errorf("Table %v.%v doesn't exist", shapefileSchema,
			surrogateTable)
		return
	}
	if !pg.TableExists(shapefileSchema, shapeTable) {
		err = fmt.Errorf("Table %v.%v doesn't exist", shapefileSchema,
			shapeTable)
		return
	}

	var srgType string
	numShapes := pg.GetNumShapes(shapefileSchema, surrogateTable)
	if pg.GetNumPolygons(shapefileSchema, surrogateTable) == numShapes {
		srgType = "polygon"
	} else if pg.GetNumLines(shapefileSchema, surrogateTable) == numShapes {
		srgType = "line"
	} else if pg.GetNumPoints(shapefileSchema, surrogateTable) == numShapes {
		srgType = "point"
	} else {
		err = fmt.Errorf("Surrogate shapefiles need to contain shapes that are "+
			"all either points, lines, or polygons (the same file cannot contain more "+
			"than one type of shapes). Shapefile %v does not meet this requirement.",
			surrogateTable)
		return
	}

	FilterString := " AND ("
	if FilterFunction != nil {
		for i, val := range FilterFunction.Values {
			if i != 0 {
				FilterString += " OR a." + FilterFunction.Column
			} else {
				FilterString += "a." + FilterFunction.Column
			}
			if FilterFunction.EqualNotEqual == "NotEqual" {
				FilterString += "!="
			} else {
				FilterString += "="
			}
			FilterString += "'" + val + "'"
		}
		FilterString += ")"
	}

	snapDistance := 1. // increase this to avoid "found non-noded intersection" errors

	type holder struct {
		ShapeTbl          string
		ShapeColumn       string
		ShapeSRID         int
		SrgTbl            string
		SrgSRID           int
		Grid              *GridDef
		OutName           string
		FilterString      string
		ShapefileSchema   string
		WeightColumns     string
		PolygonWithWeight bool
		LineWithWeight    bool
		PointWithWeight   bool
		WithWeight        bool
		Polygon           bool
		Line              bool
		Point             bool
		UseFilter         bool
		SnapDistance      float64
	}
	shapeSRID := pg.GetSRID(shapefileSchema, shapeTable)
	srgSRID := pg.GetSRID(shapefileSchema, surrogateTable)
	OutName := grid.Name + "_" + srgCode
	data := holder{shapeTable, ShapeColumn, shapeSRID, surrogateTable,
		srgSRID, grid, OutName, FilterString, shapefileSchema, WeightColumns,
		(srgType == "polygon" && WeightColumns != ""),
		(srgType == "line" && WeightColumns != ""),
		(srgType == "point" && WeightColumns != ""),
		(WeightColumns != ""),
		(srgType == "polygon"),
		(srgType == "line"),
		(srgType == "point"),
		(FilterFunction != nil),
		snapDistance}

	Log("Creating gridding surrogate "+grid.Schema+"."+OutName+"...", 0)

	const t = `
BEGIN;
CREATE TEMP TABLE shapeSelect ON COMMIT DROP AS
WITH 
gridbdy AS (select ST_BuildArea(ST_Boundary(ST_Union(geom))) 
	AS geom FROM {{.Grid.Schema}}.{{.Grid.Name}})
SELECT a.{{.ShapeColumn}},ST_SnapToGrid(
	ST_Transform(a.geom,{{.Grid.SRID}}),{{.SnapDistance}}) as geom 
	FROM {{.ShapefileSchema}}."{{.ShapeTbl}}" a, gridbdy b
WHERE ST_Intersects(a.geom, ST_Transform(b.geom,{{.ShapeSRID}}));
CREATE INDEX shapeSelect_gix ON shapeSelect USING GIST (geom);
ANALYZE shapeSelect;

CREATE TEMP TABLE srgSelect ON COMMIT DROP AS
WITH
shapebdy AS (select ST_Transform(ST_BuildArea(ST_Boundary(ST_Union(geom))),
	{{.SrgSRID}}) AS geom FROM shapeSelect)
SELECT a.gid, ST_SnapToGrid(ST_Transform(a.geom,{{.Grid.SRID}}),
	{{.SnapDistance}}) as geom,
{{if .PolygonWithWeight}} ({{.WeightColumns}})/ST_Area(a.geom) AS weight {{else}}
{{if .LineWithWeight}} ({{.WeightColumns}})/ST_Length(a.geom) AS weight {{else}}
{{if .PointWithWeight}} {{.WeightColumns}} AS weight {{else}}
	1.0 as weight
{{end}} {{end}} {{end}}
	FROM {{.ShapefileSchema}}."{{.SrgTbl}}" a, shapebdy b
WHERE ST_Intersects(a.geom,b.geom){{if .UseFilter}} {{.FilterString}}{{end}}{{if .WithWeight}} AND ({{.WeightColumns}})!=0.{{end}};
CREATE INDEX srgSelect_gix ON srgSelect USING GIST (geom);
ANALYZE srgSelect;
  
{{if .Polygon}}
CREATE TEMP TABLE new_shapes ON COMMIT DROP AS 
WITH
all_lines AS( 
SELECT St_ExteriorRing((ST_Dump(geom)).geom) AS geom
FROM shapeSelect
UNION ALL
SELECT St_ExteriorRing((ST_Dump(geom)).geom) AS geom
FROM srgSelect
UNION ALL
SELECT St_ExteriorRing((ST_Dump(geom)).geom) AS geom
FROM {{.Grid.Schema}}.{{.Grid.Name}}),
noded_lines AS (
SELECT St_Union(geom) AS geom
FROM all_lines) 
SELECT geom AS geom, ST_PointOnSurface(geom) AS pip
FROM St_Dump((
SELECT St_Polygonize(geom) AS geom
FROM noded_lines));
{{end}}{{if .Line}}
CREATE TEMP TABLE new_shapes ON COMMIT DROP AS 
WITH
all_lines AS( 
SELECT St_ExteriorRing((ST_Dump(geom)).geom) AS geom
FROM shapeSelect
UNION ALL
SELECT (ST_Dump(geom)).geom AS geom
FROM srgSelect
UNION ALL
SELECT St_ExteriorRing((ST_Dump(geom)).geom) AS geom
FROM {{.Grid.Schema}}.{{.Grid.Name}})
SELECT geom AS geom, ST_Line_Interpolate_Point(geom,0.5) AS pip
FROM St_Dump((SELECT St_Union(ST_snaptogrid(geom,
	{{.SnapDistance}})) AS geom FROM all_lines));
{{end}}{{if .Point}}
CREATE TEMP TABLE new_shapes ON COMMIT DROP AS
SELECT geom as geom, geom as pip FROM srgSelect;
{{end}}
CREATE INDEX new_shapes_gix ON new_shapes USING GIST (geom);
ANALYZE new_shapes;

CREATE TABLE {{.Grid.Schema}}.{{.OutName}} AS
WITH
polyWithAttributes AS (
SELECT c.gid AS grid_gid, b.{{.ShapeColumn}}, a.gid AS srg_gid, 
{{if .Polygon}} a.weight * ST_Area(p.geom) AS weight {{end}}
{{if .Line}} a.weight * ST_Length(p.geom) AS weight {{end}}
{{if .Point}} a.weight AS weight {{end}}
FROM new_shapes p
RIGHT JOIN srgSelect a ON {{if .Line}}p.geom && a.geom AND ST_distance(p.pip,a.geom) < 1.e-9{{else}}St_Within(p.pip, a.geom){{end}}
RIGHT JOIN shapeSelect b ON St_Within(p.pip, b.geom)
LEFT JOIN {{.Grid.Schema}}.{{.Grid.Name}} c ON St_Within(p.pip, c.geom)),
shapeTotals AS (select {{.ShapeColumn}},sum(weight) AS weight FROM polyWithAttributes
GROUP BY {{.ShapeColumn}}),
gridTotals AS (select {{.ShapeColumn}}, grid_gid, sum(weight) 
	AS weight FROM polyWithAttributes
GROUP BY grid_gid, {{.ShapeColumn}})
SELECT c.row,c.col,a.{{.ShapeColumn}} as inputID,
	a.weight/b.weight AS shapeFraction, c.geom
FROM gridTotals a 
INNER JOIN shapeTotals b ON a.{{.ShapeColumn}}=b.{{.ShapeColumn}}
INNER JOIN {{.Grid.Schema}}.{{.Grid.Name}} c ON c.gid=a.grid_gid;
COMMIT;`

	pg.DropTable(grid.Schema, OutName)
	t2 := template.Must(template.New("CreateSurrogate").Parse(t))
	var b bytes.Buffer
	err = t2.Execute(&b, data)
	if err != nil {
		return
	}
	cmd := b.String()
	Log(cmd, 3)
	_, err = pg.db.Exec(cmd)
	if err != nil {
		return
	}
	err = pg.VacuumAnalyze(grid.Schema, OutName)

	Log(fmt.Sprintf("Finished creating gridding surrogate %v.%v.", grid.Schema,
		OutName), 0)
	return
}

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

func (pg *PostGis) RetrieveGriddingSurrogate(srgNum, inputID, schema string,
	grid *GridDef) (srg *matrix.SparseMatrix, err error) {

	srg = matrix.ZerosSparse(grid.Ny, grid.Nx)

	cmd := fmt.Sprintf("SELECT row,col,shapeFraction FROM %v.%v_%v "+
		"WHERE inputID='%v';", strings.ToLower(schema),
		strings.ToLower(grid.Name), srgNum, inputID)

	Log(cmd, 3)
	rows, err := pg.db.Query(cmd)
	if err != nil {
		return
	}
	for rows.Next() {
		var row, col int
		var fraction float64
		rows.Scan(&row, &col, &fraction)
		srg.Set(row-1, col-1, fraction)
	}
	rows.Close()
	return
}

func (pg *PostGis) ProjectPoint(lat, lon float64, SRID int) (
	x, y float64, err error) {

	cmd := fmt.Sprintf("SELECT ST_AsGeoJson(ST_Transform(ST_SetSRID("+
		"ST_Point(%v, %v), 4030), %v))", lon, lat, SRID)

	Log(cmd, 3)
	var jsonout []byte
	err = pg.db.QueryRow(cmd).Scan(&jsonout)
	if err != nil {
		return
	}

	type xyHolder struct{ Coordinates []float64 }
	var xy xyHolder
	err = json.Unmarshal(jsonout, &xy)
	if err != nil {
		return
	}
	x = xy.Coordinates[0]
	y = xy.Coordinates[1]

	return
}

func (pg *PostGis) MakeRasterTable(schema, name string) {
	pg.DropTable(schema, name)
	cmd := fmt.Sprintf("CREATE TABLE %v.%v(rid serial primary key, "+
		"name varchar, rast raster);", schema, name)
	Log(cmd, 3)
	_, err := pg.db.Exec(cmd)
	if err != nil {
		panic(err)
	}
}

func (pg *PostGis) AddDataRowToRasterTable(schema, tableName, rasterRowName string,
	grid *GridDef, data *matrix.SparseMatrix) {
	var err error

	type holder struct {
		Nx, Ny, SRID               int
		Dx, Dy, X0, Y0             float64
		TableName, Schema, RowName string
	}

	cmdData := holder{grid.Nx, grid.Ny, grid.SRID,
		grid.Dx, grid.Dy, grid.X0, grid.Y0,
		tableName, schema, rasterRowName}

	const t = `
BEGIN;
INSERT INTO {{.Schema}}.{{.TableName}}(name,rast) 
VALUES 
('{{.RowName}}',ST_MakeEmptyRaster({{.Nx}}, {{.Ny}}, {{.X0}}, {{.Y0}}, 
	{{.Dx}}, {{.Dy}}, 0., 0., {{.SRID}}));

UPDATE {{.Schema}}.{{.TableName}}
SET rast=ST_AddBand(rast,'64BF'::text,0.)  
WHERE name='{{.RowName}}';
COMMIT;`

	t2 := template.Must(template.New("rasterRow").Parse(t))
	var b bytes.Buffer
	err = t2.Execute(&b, cmdData)
	if err != nil {
		panic(err)
	}
	cmd := b.String()
	Log(cmd, 3)
	_, err = pg.db.Exec(cmd)
	if err != nil {
		panic(err)
	}

	var val float64
	for j := 0; j < grid.Ny; j++ {
		for i := 0; i < grid.Nx; i++ {
			val = data.Get(j, i)
			if val != 0 {
				cmd := fmt.Sprintf("UPDATE %v.%v SET rast=ST_SetValue(rast,%v,%v,%v)"+
					"where name='%v';", schema, tableName, i+1, j+1, val, rasterRowName)
				_, err = pg.db.Exec(cmd)
				if err != nil {
					panic(err)
				}
			}
		}
	}
}

//func (pg *PostGis) AddDataRowToRasterTable(schema, tableName, rasterRowName string,
//	grid *GridDef, data *matrix.SparseMatrix) {
//	var err error
//
//	dataStr1 := ""
//	dataStr2 := ""
//	var val float64
//	for j := 0; j < grid.Ny; j++ {
//		for i := 0; i < grid.Nx; i++ {
//			val = data.Get(j, i)
//			if val != 0 {
//				dataStr1 += "ST_SetValue("
//				dataStr2 += fmt.Sprintf(",%v,%v,%v)", i+1, j+1, val)
//			}
//		}
//	}
//	dataCmd := dataStr1 + "rast" + dataStr2
//
//	type holder struct {
//		Nx, Ny, SRID               int
//		Dx, Dy, X0, Y0             float64
//		TableName, Schema, RowName string
//		DataCmd                    string
//	}
//
//	cmdData := holder{grid.Nx, grid.Ny, grid.SRID,
//		grid.Dx, grid.Dy, grid.X0, grid.Y0,
//		tableName, schema, rasterRowName, dataCmd}
//
//	const t = `
//BEGIN;
//INSERT INTO {{.Schema}}.{{.TableName}}(name,rast) 
//VALUES 
//('{{.RowName}}',ST_MakeEmptyRaster({{.Nx}}, {{.Ny}}, {{.X0}}, {{.Y0}}, 
//	{{.Dx}}, {{.Dy}}, 0., 0., {{.SRID}}));
//
//UPDATE {{.Schema}}.{{.TableName}}
//SET rast=ST_AddBand(rast,'64BF'::text,0.)  
//WHERE name='{{.RowName}}';
//
//UPDATE {{.Schema}}.{{.TableName}}
//SET rast={{.DataCmd}}
//where name='{{.RowName}}';
//COMMIT;`
//
//	t2 := template.Must(template.New("rasterRow").Parse(t))
//	var b bytes.Buffer
//	err = t2.Execute(&b, cmdData)
//	if err != nil {
//		panic(err)
//	}
//	cmd := b.String()
//	Log(cmd, 3)
//	_, err = pg.db.Exec(cmd)
//	if err != nil {
//		panic(err)
//	}
//}

func (pg *PostGis) WriteOutRaster(schema, tableName, rasterRowName,
	filename string) {
	cmd := fmt.Sprintf("SELECT ST_AsTIFF(rast, 'PACKBITS') "+
		"FROM %v.%v WHERE name='%v';", schema, tableName, rasterRowName)
	var data []byte
	Log(cmd, 3)
	err := pg.db.QueryRow(cmd).Scan(&data)
	if err != nil {
		panic(err)
	}
	f, err := os.Create(filename)
	if err != nil {
		panic(err)
	}
	_, err = f.Write(data)
	if err != nil {
		panic(err)
	}
	f.Close()
}
