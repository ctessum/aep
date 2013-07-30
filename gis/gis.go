package gis

import (
	"bitbucket.org/ctessum/aep/sparse"
	"bytes"
	"database/sql"
	"encoding/json"
	"fmt"
	_ "github.com/bmizerany/pq"
	"log"
	"os"
	"runtime"
	"strconv"
	"strings"
	"text/template"
)

var (
	DebugLevel  = 3 // amout of output to print to the screen. A higher number means more messages.
	SrgProgress float64
)

func Log(msg string, debug int) {
	if debug <= DebugLevel {
		log.Println(msg)
	}
}

type PostGis struct {
	Db *sql.DB
}

func Connect(user, dbname, password, OtherLibpqConnectionParameters string) (
	db *PostGis, err error) {
	db = new(PostGis)
	options := fmt.Sprintf(
		"user=%v dbname=%v password=%v", user, dbname, password)
	if OtherLibpqConnectionParameters != "" {
		options += " " + OtherLibpqConnectionParameters
	}
	db.Db, err = sql.Open("postgres", options)
	if err != nil {
		return
	}
	err = db.Db.Ping()
	return
}

func (pg *PostGis) Disconnect() error {
	err := pg.Db.Close()
	return err
}

type ParsedProj4 struct {
	SRID          int
	Proj          string
	Lat_1         float64
	Lat_2         float64
	Lat_0         float64
	Lon_0         float64
	EarthRadius_a float64
	EarthRadius_b float64
	To_meter      float64
}

func (p *ParsedProj4) Equals(p2 *ParsedProj4) bool {
	switch p.Proj {
	case "lcc":
		return (p.Proj == p2.Proj &&
			p.Lat_1 == p2.Lat_1 &&
			p.Lat_2 == p2.Lat_2 &&
			p.Lat_0 == p2.Lat_0 &&
			p.Lon_0 == p2.Lon_0 &&
			p.EarthRadius_a == p2.EarthRadius_a &&
			p.EarthRadius_b == p2.EarthRadius_b &&
			p.To_meter == p2.To_meter)
	case "longlat":
		return (p.Proj == p2.Proj)
	case "merc":
		return (p.Proj == p2.Proj &&
			p.Lon_0 == p2.Lon_0 &&
			p.EarthRadius_a == p2.EarthRadius_a &&
			p.EarthRadius_b == p2.EarthRadius_b)
	}
	panic("Unsupported projection type " + p.Proj)
	return false
}

func parseHelper(s1, s2 string) string {
	return strings.Split(strings.Split(strings.ToLower(s1),
		"+"+strings.ToLower(s2)+"=")[1], " ")[0]
}
func parseHelperFloat(s1, s2 string) float64 {
	f, _ := strconv.ParseFloat(parseHelper(s1, s2), 64)
	return f
}

func ParseProj4(proj4 string) *ParsedProj4 {
	p := new(ParsedProj4)
	p.Proj = parseHelper(proj4, "proj")
	switch p.Proj {
	case "lcc":
		p.Lat_1 = parseHelperFloat(proj4, "lat_1")
		p.Lat_2 = parseHelperFloat(proj4, "lat_2")
		p.Lat_0 = parseHelperFloat(proj4, "lat_0")
		p.Lon_0 = parseHelperFloat(proj4, "lon_0")
		p.To_meter = parseHelperFloat(proj4, "to_meter")
		p.EarthRadius_a = parseHelperFloat(proj4, "a")
		p.EarthRadius_b = parseHelperFloat(proj4, "b")
	case "merc":
		p.Lon_0 = parseHelperFloat(proj4, "lon_0")
		p.EarthRadius_a = parseHelperFloat(proj4, "a")
		p.EarthRadius_b = parseHelperFloat(proj4, "b")
	}
	return p
}

func (p *ParsedProj4) ToString() string {
	var s string
	switch p.Proj {
	case "lcc":
		s = fmt.Sprintf("+proj=lcc +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f "+
			"+x_0=0 +y_0=0 +a=%f +b=%f +to_meter=%v",
			p.Lat_1, p.Lat_2, p.Lat_0, p.Lon_0,
			p.EarthRadius_a, p.EarthRadius_b, p.To_meter)
	case "longlat":
		s = "+proj=longlat +datum=WGS84 +no_defs"
	case "merc":
		s = fmt.Sprintf("+proj=merc +a=%f +b=%f +lat_ts=0.0 +lon_0=%f "+
			"+x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs",
			p.EarthRadius_a, p.EarthRadius_b, p.Lon_0)
	default:
		panic(fmt.Errorf("Unknown proj4 projection `%v'.", p.Proj))
	}
	return s
}

// Create a new PostGIS projection. Note: if you have two SRID numbers for the
// same projection, it will cause errors in the surrogate generator.
func (pg *PostGis) NewProjection(p *ParsedProj4) (err error) {

	var currentProj4 string
	cmd := fmt.Sprintf("SELECT proj4text from spatial_ref_sys "+
		"where SRID=%v", p.SRID)
	Log(cmd, 3)
	// See if projection already exists
	err = pg.Db.QueryRow(cmd).Scan(&currentProj4)
	if err != nil {
		if err.Error() == "sql: no rows in result set" {
			err = nil
			// if it doesn't exist, create it
			cmd := fmt.Sprintf("INSERT INTO spatial_ref_sys "+
				"(srid, auth_name, auth_srid, proj4text) "+
				"VALUES (%v,'aep',%v,'%v');", p.SRID, p.SRID, p.ToString())
			Log(cmd, 3)
			_, err = pg.Db.Exec(cmd)
			return
		} else {
			err = fmt.Errorf("In gis.NewProjection:\n%v", err.Error())
			return
		}
	} else {
		if !p.Equals(ParseProj4(currentProj4)) {
			// if it does exist but is different from our projection,
			// throw an error.
			err = fmt.Errorf("Projection already exists (and does not match"+
				" new projection) for SRID %v.\nold proj=%v\nnew proj=%v\n"+
				"Try a different SRID code in configuration file (it "+
				"doesn't matter what number is chosen as long as it's "+
				"not already in use).",
				p.SRID, currentProj4, p.ToString())
			return
		}
	}
	return
}

type GridDef struct {
	Name          string
	Nx            int
	Ny            int
	Dx            float64
	Dy            float64
	X0            float64
	Y0            float64
	SRID          int    // PostGIS projection type (Spatial reference ID)
	Schema        string // name of the Postgres schema it will be saved in
	TimeZones     map[int]*sparse.SparseArray
	IrregularGrid bool // whether the grid is a regular grid
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
	grid.TimeZones = make(map[int]*sparse.SparseArray)
	return
}

func (pg *PostGis) CreateGrid(grid *GridDef, timeZoneSchema string) error {

	const t = `
CREATE TABLE {{.Schema}}.{{.Name}}  AS
SELECT i AS row, j AS col,
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
	_, err = pg.Db.Exec(cmd)
	if err != nil {
		return err
	}
	pg.AddGid(grid.Schema, grid.Name)
	pg.AddGix(grid.Schema, grid.Name)
	pg.UpdateSRID(grid.Schema, grid.Name, grid.SRID)

	// Add timezone info
	if !pg.TableExists(timeZoneSchema, "timezone") {
		return fmt.Errorf("Shapefile \"timezone\" is not loaded in PostGIS "+
			"schema \"%v\". See file test/loadshp2.sh for an example of how to "+
			"get and load this file.", timeZoneSchema)
	}
	tzSRID := pg.GetSRID(timeZoneSchema, "timezone")
	cmd = fmt.Sprintf(
		"ALTER TABLE %v.%v ADD timezone double precision;\n"+
			"UPDATE %v.%v set timezone=b.zone FROM %v.%v a\n"+
			"LEFT JOIN %v.timezone b\n"+
			"ON ST_Within(ST_PointOnSurface("+
			"ST_Transform(a.geom,%v)),b.geom);",
		grid.Schema, grid.Name, grid.Schema, grid.Name, grid.Schema, grid.Name,
		timeZoneSchema, tzSRID)
	Log(cmd, 3)
	_, err = pg.Db.Exec(cmd)
	if err != nil {
		pg.DropTable(grid.Schema, grid.Name)
		return err
	}

	pg.VacuumAnalyze(grid.Schema, grid.Name)

	return err
}

func (pg *PostGis) CreateIrregularGrid(grid *GridDef, inputSchema string) error {
	if !pg.TableExists(inputSchema, grid.Name) {
		return (fmt.Errorf("Grid table %v does not exist", grid.Name))
	}
	var transcmd string
	if pg.GetSRID(inputSchema, grid.Name) == grid.SRID {
		transcmd = "b.geom"
	} else {
		transcmd = fmt.Sprintf("ST_Transform(b.geom,%v)", grid.SRID)
	}
	cmd := fmt.Sprintf("CREATE TABLE %v.%v AS SELECT a.row, 0 as col, "+
		"ST_SimplifyPreserveTopology(%v,%v) as geom "+
		"FROM (SELECT generate_series(0,(SELECT count(geom)-1 "+
		"FROM %v.%v)) as row) as a "+
		"LEFT JOIN %v.%v b "+
		"ON a.row=b.gid-1;", grid.Schema, grid.Name, transcmd, grid.Dx,
		inputSchema, grid.Name, inputSchema, grid.Name)
	Log(cmd, 3)
	_, err := pg.Db.Exec(cmd)
	if err != nil {
		pg.DropTable(grid.Schema, grid.Name)
		return err
	}

	pg.VacuumAnalyze(grid.Schema, grid.Name)

	return err
}

// Store timezones from PostGIS table into memory.
func (pg *PostGis) GridAddTimeZones(gridIn *GridDef, timeZoneSchema string) error {
	grid := *gridIn
	// Store time zones
	gridQuery := fmt.Sprintf("SELECT row,col,timezone FROM %v.%v;",
		grid.Schema, grid.Name)
	Log(gridQuery, 3)
	gridRows, err := pg.Db.Query(gridQuery)
	if err != nil {
		return err
	}
	for gridRows.Next() {
		var tz float64
		var row, col int
		err = gridRows.Scan(&row, &col, &tz)
		if err != nil {
			return err
		}
		tzSeconds := int(tz * 3600.)
		if _, ok := grid.TimeZones[tzSeconds]; !ok {
			grid.TimeZones[tzSeconds] = sparse.ZerosSparse(grid.Ny, grid.Nx)
		}
		grid.TimeZones[tzSeconds].Set(1., row, col)
	}
	gridRows.Close()
	*gridIn = grid
	return gridRows.Err()
}

func (pg *PostGis) DropTable(schema, name string) {
	_, err := pg.Db.Exec("DROP TABLE IF EXISTS " + schema + "." + name + ";")
	if err != nil {
		panic(err)
	}
	_, err = pg.Db.Exec("DROP INDEX IF EXISTS " + schema + "." + name + "_gix;")
	if err != nil {
		panic(err)
	}
}

func (pg *PostGis) DropSchema(name string) {
	pg.Db.Exec("DROP SCHEMA " + name + " CASCADE;")
}

func (pg *PostGis) CreateSchema(name string) {
	pg.Db.Exec("CREATE SCHEMA " + name + ";")
}

// Add ID column to table
func (pg *PostGis) AddGid(schema, name string) {
	cmd := "alter table " + schema + "." + name + " add gid serial not null;"
	Log(cmd, 3)
	_, err := pg.Db.Exec(cmd)
	if err != nil {
		panic(err)
	}
}

// Add spatial index to table
func (pg *PostGis) AddGix(schema, name string) {
	cmd := "CREATE INDEX " + name + "_gix ON " + schema + "." + name +
		" USING GIST (geom);"
	Log(cmd, 3)
	_, err := pg.Db.Exec(cmd)
	if err != nil {
		panic(err)
	}
}

func (pg *PostGis) TableExists(schema, name string) (exists bool) {
	cmd := fmt.Sprintf("SELECT EXISTS(SELECT 1 FROM information_schema.tables "+
		"WHERE table_schema='%v' AND table_name='%v');",
		strings.ToLower(schema), strings.ToLower(name))
	Log(cmd, 3)
	//	var exists string
	err := pg.Db.QueryRow(cmd).Scan(&exists)
	if err != nil {
		panic(err)
	}
	return
}

// Change projection definition (without reprojecting)
func (pg *PostGis) UpdateSRID(schema, name string, SRID int) {
	cmd := fmt.Sprintf("SELECT UpdateGeometrySRID('%v','%v', 'geom', %v);",
		strings.ToLower(schema), strings.ToLower(name), SRID)
	Log(cmd, 3)
	_, err := pg.Db.Query(cmd)
	if err != nil {
		panic(err)
	}
}

func (pg *PostGis) VacuumAnalyze(schema, name string) {
	cmd := "VACUUM ANALYZE " + schema + "." + name + ";"
	Log(cmd, 3)
	_, err := pg.Db.Exec(cmd)
	if err != nil {
		panic(err)
	}
}

func (pg *PostGis) Vacuum() {
	cmd := "VACUUM;"
	Log(cmd, 3)
	_, err := pg.Db.Exec(cmd)
	if err != nil {
		panic(err)
	}
}

func (pg *PostGis) Rollback() {
	cmd := "ROLLBACK;"
	Log(cmd, 3)
	_, err := pg.Db.Exec(cmd)
	if err != nil {
		panic(err)
	}
}

func (pg *PostGis) GetSRID(schema, name string) (SRID int) {
	cmd := fmt.Sprintf("SELECT ST_SRID(geom) FROM %v.\"%v\" LIMIT 1;", schema, name)
	Log(cmd, 3)
	err := pg.Db.QueryRow(cmd).Scan(&SRID)
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
	err := pg.Db.QueryRow(cmd).Scan(&polygons)
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
	err := pg.Db.QueryRow(cmd).Scan(&lines)
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
	err := pg.Db.QueryRow(cmd).Scan(&points)
	if err != nil {
		panic(err)
	}
	return
}
func (pg *PostGis) GetNumShapes(schema, name string) (shapes int) {
	cmd := fmt.Sprintf("SELECT Count(geom) from %v.\"%v\";",
		strings.ToLower(schema), strings.ToLower(name))
	Log(cmd, 3)
	err := pg.Db.QueryRow(cmd).Scan(&shapes)
	if err != nil {
		panic(err)
	}
	return
}

type PostGISconnecter interface {
	PGconnect() (*PostGis, error)
}

type srgdataholder struct {
	ShapeTbl          string
	ShapeColumn       string
	ShapeSRID         int
	SrgTbl            string
	SrgSRID           int
	Grid              *GridDef
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
	ShapeSameSRID     bool
	SrgSameSRID       bool
	SnapDistance      string
	Geom              string
	InputID           string
	WeightAndFilter   bool
	WeightOrFilter    bool
}

func newsrgdataholder(ShapeTbl, ShapeColumn string, ShapeSRID int,
	SrgTbl string, SrgSRID int, Grid *GridDef, FilterString,
	ShapefileSchema, WeightColumns, srgType, SnapDistance string) *srgdataholder {
	h := new(srgdataholder)
	h.ShapeTbl = ShapeTbl
	h.ShapeColumn = ShapeColumn
	h.ShapeSRID = ShapeSRID
	h.SrgTbl = SrgTbl
	h.SrgSRID = SrgSRID
	h.Grid = Grid
	h.FilterString = FilterString
	h.ShapefileSchema = ShapefileSchema
	h.WeightColumns = WeightColumns
	h.PolygonWithWeight = (srgType == "polygon" && WeightColumns != "")
	h.LineWithWeight = (srgType == "line" && WeightColumns != "")
	h.PointWithWeight = (srgType == "point" && WeightColumns != "")
	h.WithWeight = (WeightColumns != "")
	h.Polygon = (srgType == "polygon")
	h.Line = (srgType == "line")
	h.Point = (srgType == "point")
	h.UseFilter = (FilterString != "")
	h.ShapeSameSRID = (Grid.SRID == ShapeSRID)
	h.SrgSameSRID = (Grid.SRID == SrgSRID)
	h.SnapDistance = SnapDistance
	h.WeightAndFilter = h.UseFilter && h.WithWeight
	h.WeightOrFilter = h.UseFilter || h.WithWeight
	return h
}

func CreateGriddingSurrogate(PGc PostGISconnecter, srgCode, shapeTable,
	ShapeColumn, surrogateTable, WeightColumns string,
	FilterFunction *SurrogateFilter, grid *GridDef,
	shapefileSchema string) (err error) {

	srgCompletion := 0
	SrgProgress = 0.
	pg, err := PGc.PGconnect()
	if err != nil {
		err = handle(err, "")
		return
	}
	defer pg.Disconnect()

	OutName := grid.Name + "_" + srgCode
	// if this surrogate was requested by more than one sector, make sure we
	// don't create it twice.
	if pg.TableExists(grid.Schema, OutName) {
		return
	}

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

	FilterString := ""
	if FilterFunction != nil {
		FilterString = "("
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

	shapeSRID := pg.GetSRID(shapefileSchema, shapeTable)
	srgSRID := pg.GetSRID(shapefileSchema, surrogateTable)
	SnapDistance := "1.e-9"
	data := newsrgdataholder(shapeTable, ShapeColumn, shapeSRID, surrogateTable,
		srgSRID, grid, FilterString, shapefileSchema, WeightColumns, srgType,
		SnapDistance)

	Log("Creating gridding surrogate "+grid.Schema+"."+OutName+"...", 0)
	pg.DropTable(grid.Schema, OutName)

	inputIDchan := make(chan string)
	errchan := make(chan error)
	workersRunning := 0
	for i := 0; i < runtime.GOMAXPROCS(-1); i++ {
		data := newsrgdataholder(shapeTable, ShapeColumn, shapeSRID, surrogateTable,
			srgSRID, grid, FilterString, shapefileSchema, WeightColumns, srgType,
			SnapDistance)
		go srgGenWorker(inputIDchan, data, PGc, errchan)
		workersRunning++
	}

	pg.DropTable("public", "shapeSelect")
	pg.DropTable("public", "srgSelect")
	pg.DropTable("public", "gridBoundary")
	const t = `
BEGIN;
CREATE TABLE gridBoundary AS
SELECT ST_SetSRID(ST_BuildArea(ST_Extent(geom)),{{.Grid.SRID}})
	AS geom FROM {{.Grid.Schema}}.{{.Grid.Name}};
CREATE TABLE shapeSelect AS
WITH a AS(SELECT {{.ShapeColumn}} as inputID,
	ST_SimplifyPreserveTopology({{if .ShapeSameSRID}}
		ST_Buffer(geom,0){{else}}
		ST_Transform(ST_Buffer(geom,0),{{.Grid.SRID}}){{end}},{{.Grid.Dx}}) as geom 
	FROM {{.ShapefileSchema}}."{{.ShapeTbl}}" a)
SELECT a.inputID, a.geom from a, gridBoundary b
WHERE ST_Intersects(ST_Buffer(a.geom,0), b.geom);
CREATE INDEX shapeSelect_gix ON shapeSelect USING GIST (geom);
ANALYZE shapeSelect;

CREATE TABLE srgSelect AS
SELECT a.gid, ST_SimplifyPreserveTopology({{if .SrgSameSRID}}
	a.geom{{else}}
	ST_Transform(a.geom,{{.Grid.SRID}})
	{{end}},{{.Grid.Dx}}) as geom,{{if .PolygonWithWeight}} 
({{.WeightColumns}})/ST_Area(ST_Buffer(a.geom,0)) AS weight {{else}}{{if .LineWithWeight}} 
({{.WeightColumns}})/ST_Length(a.geom) AS weight {{else}}{{if .PointWithWeight}} 
{{.WeightColumns}} AS weight {{else}}1.0 as weight{{end}} {{end}} {{end}}
	FROM {{.ShapefileSchema}}."{{.SrgTbl}}" a
{{if .WeightOrFilter}}WHERE {{end}}{{if .WithWeight}}({{.WeightColumns}})!=0.{{end}}{{if .WeightAndFilter}} AND {{end}}{{if .UseFilter}}{{.FilterString}}{{end}};
CREATE INDEX srgSelect_gix ON srgSelect USING GIST (geom);
ANALYZE srgSelect;
COMMIT;`
	t2 := template.Must(template.New("temptables").Parse(t))
	var b bytes.Buffer
	err = t2.Execute(&b, data)
	if err != nil {
		err = handle(err, "")
		return
	}
	cmd := b.String()
	Log(cmd, 3)
	_, err = pg.Db.Exec(cmd)
	if err != nil {
		err = handle(err, cmd)
		return
	}
	pg.VacuumAnalyze("public", "shapeSelect")
	pg.VacuumAnalyze("public", "srgSelect")

	pg.DropTable(grid.Schema, "tempsrg")
	cmd = fmt.Sprintf("CREATE TABLE %v.tempsrg (row int, col int, "+
		"inputID char(5), shapeFraction double precision, geom geometry, "+
		"coveredByGrid boolean);",
		grid.Schema)
	Log(cmd, 3)
	_, err = pg.Db.Exec(cmd)
	if err != nil {
		err = handle(err, cmd)
		return
	}

	var numInputShapes int
	err = pg.Db.QueryRow("SELECT Count(inputID) FROM shapeSelect;").
		Scan(&numInputShapes)
	if err != nil {
		errchan <- handle(err, cmd)
		return
	}

	shapeRows, err := pg.Db.Query("SELECT DISTINCT inputID FROM shapeSelect;")
	if err != nil {
		err = handle(err, cmd)
		return
	}
	for shapeRows.Next() {
		SrgProgress = float64(srgCompletion) / float64(numInputShapes) * 100.
		srgCompletion++
		var inputID string
		err = shapeRows.Scan(&inputID)
		if err != nil {
			err = handle(err, "")
			return
		}
		select {
		case inputIDchan <- inputID:
		case err = <-errchan:
			if err != nil {
				return
			}
			workersRunning--
			inputIDchan <- inputID
		}
	}
	close(inputIDchan)
	shapeRows.Close()
	err = shapeRows.Err()
	if err != nil {
		return
	}
	// wait for workers to finish
	for i := 0; i < workersRunning; i++ {
		err = <-errchan
		if err != nil {
			return
		}
	}

	cmd = fmt.Sprintf("ALTER TABLE %v.tempsrg RENAME TO %v",
		grid.Schema, OutName)
	Log(cmd, 3)
	_, err = pg.Db.Exec(cmd)
	if err != nil {
		err = handle(err, cmd)
		return
	}
	pg.VacuumAnalyze(grid.Schema, OutName)

	Log(fmt.Sprintf("Finished creating gridding surrogate %v.%v.", grid.Schema,
		OutName), 0)
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

func srgGenWorker(inputIDchan chan string, data *srgdataholder,
	PGc PostGISconnecter, errchan chan error) {

	pg, err := PGc.PGconnect()
	if err != nil {
		errchan <- handle(err, "")
		return
	}
	defer pg.Disconnect()

	templates := template.New("srgGen")
	templates, err = templates.New("shapeSrg").Parse(`
CREATE TABLE singleShapeSrgs_{{.InputID}} AS
WITH 
singleShape AS(
SELECT geom from shapeSelect where inputID='{{.InputID}}'),
splitSrgs AS(
	SELECT a.weight, (ST_Dump(ST_Split(a.geom,b.geom))).geom AS geom 
	FROM 
		(SELECT x.weight, x.geom from srgSelect x, singleShape y
		WHERE ST_Overlaps(x.geom,y.geom)) a, 
		(SELECT ST_ExteriorRing((ST_Dump(geom)).geom) as geom 
		FROM singleShape) b)
SELECT a.weight, a.geom from srgSelect a, singleShape b 
	WHERE ST_Within(a.geom,b.geom)
UNION ALL
SELECT a.weight, b.geom from srgSelect a, singleShape b 
	WHERE ST_Within(b.geom,a.geom)
UNION ALL
SELECT a.weight, a.geom from splitSrgs a, singleShape b 
	WHERE ST_Intersects(b.geom,a.geom) 
	AND ST_Touches(ST_Snap(b.geom,a.geom,{{.SnapDistance}}),a.geom) = false;
CREATE INDEX singleShapeSrgs_{{.InputID}}_gix ON singleShapeSrgs_{{.InputID}} 
	USING GIST (geom);
ANALYZE singleShapeSrgs_{{.InputID}};`)
	if err != nil {
		errchan <- handle(err, "")
		return
	}
	templates, err = templates.New("shapeSrgBackup").Parse(`
CREATE TABLE singleShapeSrgs_{{.InputID}} AS
WITH 
singleShape AS(
SELECT ST_Buffer(geom,0) as geom from shapeSelect where inputID='{{.InputID}}'),
splitSrgs AS(
	SELECT a.weight, (ST_Dump(ST_Split(ST_Buffer(a.geom,0),b.geom))).geom AS geom 
	FROM 
		(SELECT x.weight, x.geom from srgSelect x, singleShape y
		WHERE ST_Overlaps(ST_Buffer(x.geom,0),ST_Buffer(y.geom,0))) a, 
		(SELECT ST_ExteriorRing((ST_Dump(geom)).geom) as geom 
		FROM singleShape) b)
SELECT a.weight, a.geom from srgSelect a, singleShape b 
	WHERE ST_Within(ST_Buffer(a.geom,0),ST_Buffer(b.geom,0))
UNION ALL
SELECT a.weight, b.geom from srgSelect a, singleShape b 
	WHERE ST_Within(ST_Buffer(b.geom,0),ST_Buffer(a.geom,0))
UNION ALL
SELECT a.weight, a.geom from splitSrgs a, singleShape b 
	WHERE ST_Intersects(b.geom,a.geom) 
	AND ST_Touches(ST_Buffer(ST_Snap(ST_Buffer(b.geom,0),ST_Buffer(a.geom,0),{{.SnapDistance}}),0),ST_Buffer(a.geom,0)) = false;
CREATE INDEX singleShapeSrgs_{{.InputID}}_gix ON singleShapeSrgs_{{.InputID}} 
	USING GIST (geom);
ANALYZE singleShapeSrgs_{{.InputID}};`)
	if err != nil {
		errchan <- handle(err, "")
		return
	}

	templates, err = templates.New("gridSrg").Parse(`
CREATE TEMP TABLE srgsInGrid AS
WITH
gridcell AS(
	SELECT ST_GeomFromEWKT('{{.Geom}}') as geom),
splitSrgs AS(
	SELECT a.weight, (ST_Dump(ST_Split(a.geom,b.geom))).geom AS geom 
	FROM 
		(SELECT x.weight, x.geom from singleShapeSrgs_{{.InputID}} x, gridcell y
		WHERE {{if .Line}}ST_Crosses{{else}}ST_Overlaps{{end}}(x.geom,y.geom)) a,
		(SELECT ST_ExteriorRing((ST_Dump(geom)).geom) as geom FROM gridcell) b)
SELECT a.weight, a.geom from singleShapeSrgs_{{.InputID}} a, gridcell b
	WHERE ST_Within(a.geom,b.geom)
UNION ALL
SELECT a.weight, b.geom 
	FROM singleShapeSrgs_{{.InputID}} a, gridcell b
	WHERE ST_Within(b.geom,a.geom)
UNION ALL
SELECT a.weight, a.geom from splitSrgs a, gridcell b
	WHERE ST_Intersects(b.geom,a.geom) 
	AND ST_Touches(ST_Snap(b.geom,a.geom,1.e-9),a.geom) = false;`)
	if err != nil {
		errchan <- handle(err, "")
		return
	}
	templates, err = templates.New("gridSrgBackup").Parse(`
CREATE TEMP TABLE srgsInGrid AS
WITH
gridcell AS(
	SELECT ST_GeomFromEWKT('{{.Geom}}') as geom),
splitSrgs AS(
	SELECT a.weight, (ST_Dump(ST_Split(ST_Buffer(ST_SnapToGrid(a.geom,{{.SnapDistance}}),0),b.geom))).geom AS geom 
	FROM 
		(SELECT x.weight, x.geom from singleShapeSrgs_{{.InputID}} x, gridcell y
		WHERE {{if .Line}}ST_Crosses{{else}}ST_Overlaps{{end}}(ST_Buffer(x.geom,0),ST_Buffer(y.geom,0))) a,
		(SELECT ST_ExteriorRing((ST_Dump(geom)).geom) as geom FROM gridcell) b)
SELECT a.weight, a.geom from singleShapeSrgs_{{.InputID}} a, gridcell b
	WHERE ST_Within(ST_Buffer(a.geom,0),ST_Buffer(b.geom,0))
UNION ALL
SELECT a.weight, b.geom 
	FROM singleShapeSrgs_{{.InputID}} a, gridcell b
	WHERE ST_Within(ST_Buffer(b.geom,0),ST_Buffer(a.geom,0))
UNION ALL
SELECT a.weight, a.geom from splitSrgs a, gridcell b
	WHERE ST_Intersects(ST_Buffer(b.geom,0),ST_Buffer(a.geom,0)) 
	AND ST_Touches(ST_Buffer(ST_Snap(b.geom,a.geom,1.e-9),0),ST_Buffer(a.geom,0)) = false;`)
	if err != nil {
		errchan <- handle(err, "")
		return
	}

	for data.InputID = range inputIDchan {
		var cmd string
		pg.DropTable("public", "singleShapeSrgs_"+data.InputID)

		var coveredByGrid bool
		cmd = fmt.Sprintf("SELECT ST_Within(a.geom,b.geom) FROM "+
			"(SELECT geom FROM shapeSelect where inputID='%v') a, "+
			"gridBoundary b;", data.InputID)
		err = pg.Db.QueryRow(cmd).Scan(&coveredByGrid)
		if err != nil {
			errchan <- handle(err, cmd)
		}

		// First run the faster version of the sql query, and if there is
		// an error, run it again with the slower
		// (and more error tolerant) version.
		err = pg.RunTemplate(templates, "shapeSrg", data, 3)
		if err != nil {
			err = pg.RunTemplate(templates, "shapeSrgBackup", data, 3)
			if err != nil {
				errchan <- err
				return
			}
		}

		var singleShapeSrgWeightTemp sql.NullFloat64
		var singleShapeSrgWeight float64
		if data.Polygon {
			cmd = fmt.Sprintf("SELECT SUM(weight*ST_Area(geom)) "+
				"FROM singleShapeSrgs_%v;", data.InputID)
		} else if data.Line {
			cmd = fmt.Sprintf("SELECT SUM(weight*ST_Length(geom)) "+
				"FROM singleShapeSrgs_%v;", data.InputID)
		} else if data.Point {
			cmd = fmt.Sprintf("SELECT SUM(weight) "+
				"FROM singleShapeSrgs_%v;", data.InputID)
		}
		Log(cmd, 3)
		err = pg.Db.QueryRow(cmd).Scan(&singleShapeSrgWeightTemp)
		if err != nil {
			errchan <- handle(err, cmd)
			return
		}
		if singleShapeSrgWeightTemp.Valid {
			singleShapeSrgWeight = singleShapeSrgWeightTemp.Float64
		} else {
			continue
		}

		gridQuery := fmt.Sprintf("SELECT a.row,a.col,ST_AsEWKT(a.geom) "+
			"FROM %v.%v a, "+
			"(SELECT ST_MakeValid(geom) as geom "+
			"FROM shapeSelect WHERE inputID='%v') b "+
			"WHERE ST_Intersects(ST_Buffer(a.geom,0),ST_Buffer(b.geom,0));",
			data.Grid.Schema, data.Grid.Name, data.InputID)

		Log(gridQuery, 3)
		gridRows, err := pg.Db.Query(gridQuery)
		if err != nil {
			errchan <- handle(err, gridQuery)
			return
		}
		for gridRows.Next() {
			var geom string
			var row, col int
			err = gridRows.Scan(&row, &col, &geom)
			if err != nil {
				errchan <- handle(err, "")
				return
			}
			data.Geom = geom
			// First run the faster version of the sql query, and if there is
			// an error, run it again with the slower
			// (and more error tolerant) version.
			err = pg.RunTemplate(templates, "gridSrg", data, 4)
			if err != nil {
				err = pg.RunTemplate(templates, "gridSrgBackup", data, 4)
				if err != nil {
					errchan <- err
					return
				}
			}

			var gridCellSrgWeightTemp sql.NullFloat64
			var gridCellSrgWeight float64
			if data.Polygon {
				cmd = "SELECT SUM(weight*ST_Area(geom)) FROM srgsInGrid;"
			} else if data.Line {
				cmd = "SELECT SUM(weight*ST_Length(geom)) FROM srgsInGrid;"
			} else if data.Point {
				cmd = "SELECT SUM(weight) FROM srgsInGrid;"
			}
			Log(cmd, 4)
			err = pg.Db.QueryRow(cmd).Scan(&gridCellSrgWeightTemp)
			if err != nil {
				errchan <- handle(err, cmd)
				return
			}
			if gridCellSrgWeightTemp.Valid {
				gridCellSrgWeight = gridCellSrgWeightTemp.Float64
			}

			var frac float64
			if gridCellSrgWeight > 0 {
				frac = gridCellSrgWeight / singleShapeSrgWeight
				cmd = fmt.Sprintf("INSERT INTO %v.tempsrg "+
					"(row, col, inputID, shapeFraction, geom, coveredByGrid) "+
					"VALUES (%v,%v,'%v',%v,ST_GeomFromEWKT('%v'),%v);",
					data.Grid.Schema, row, col, data.InputID, frac, geom,
					coveredByGrid)
				Log(cmd, 4)
				_, err = pg.Db.Exec(cmd)
				if err != nil {
					errchan <- handle(err, cmd)
					return
				}
			}

			pg.Db.Exec("DROP TABLE srgsInGrid;")
		}
		gridRows.Close()
		err = gridRows.Err()
		if err != nil {
			errchan <- handle(err, cmd)
			return
		}
		pg.DropTable("public", "singleShapeSrgs_"+data.InputID)
	}
	errchan <- err
}

func (pg *PostGis) RunTemplate(t *template.Template, name string,
	data interface{}, lognum int) error {
	var b bytes.Buffer
	err := t.ExecuteTemplate(&b, name, data)
	if err != nil {
		return handle(err, "")
	}
	cmd := b.String()
	Log(cmd, lognum)
	_, err = pg.Db.Exec(cmd)
	if err != nil {
		return handle(err, cmd)
	}
	return nil
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
	grid *GridDef) (srg *sparse.SparseArray, err error) {

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

func (pg *PostGis) ProjectPoint(lat, lon float64, SRID int) (
	x, y float64, err error) {

	var jsonout []byte
	err = pg.Db.QueryRow("SELECT ST_AsGeoJson(ST_Transform("+
		"ST_SetSRID(ST_Point($1, $2), 4030), $3))",
		lon, lat, SRID).Scan(&jsonout)
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

func (pg *PostGis) IrregularGridIndex(y, x float64, grid *GridDef) (
	row, col int) {
	err := pg.Db.QueryRow(fmt.Sprintf("SELECT row, col FROM %v.%v "+
		"WHERE ST_Intersects(ST_Point(%f,%f),geom);",
		grid.Schema, grid.Name, x, y)).Scan(&row, &col)
	if err != nil {
		// in case point is in between shapes
		err = pg.Db.QueryRow(fmt.Sprintf("SELECT row, col FROM %v.%v "+
			"WHERE ST_Intersects(ST_Snap(ST_Point(%f,%f),geom,%v),geom);",
			grid.Schema, grid.Name, x, y, grid.Dx)).Scan(&row, &col)
		if err != nil {
			panic(err)
		}
	}
	return
}

type VectorShape struct {
	schema, name, Shapetype string
	X                       []float64
	Y                       []float64
	Xmin, Xmax, Ymin, Ymax  float64
}

func (pg *PostGis) GetVectorColumnFloat64(schema, name, column string) []float64 {
	var rows *sql.Rows
	numrows := pg.GetNumShapes(schema, name)
	out := make([]float64, numrows)
	rows, err := pg.Db.Query(fmt.Sprintf(
		"SELECT %v FROM \"%v\".\"%v\"", column, schema, name))
	if err != nil {
		panic(err)
	}
	i := 0
	for rows.Next() {
		err = rows.Scan(&out[i])
		if err != nil {
			panic(err)
		}
		i++
	}
	rows.Close()
	err = rows.Err()
	if err != nil {
		panic(err)
	}
	return out
}

func (pg *PostGis) GetVectorColumnString(schema, name, column string) []string {
	var rows *sql.Rows
	numrows := pg.GetNumShapes(schema, name)
	out := make([]string, numrows)
	rows, err := pg.Db.Query(fmt.Sprintf(
		"SELECT %v FROM \"%v\".\"%v\"", column, schema, name))
	if err != nil {
		panic(err)
	}
	i := 0
	for rows.Next() {
		err = rows.Scan(&out[i])
		if err != nil {
			panic(err)
		}
		i++
	}
	rows.Close()
	err = rows.Err()
	if err != nil {
		panic(err)
	}
	return out
}

func (pg *PostGis) GetVector(schema, name string, SRID int) (
	out []*VectorShape, err error) {

	numrows := pg.GetNumShapes(schema, name)
	out = make([]*VectorShape, numrows)

	vectorSRID := pg.GetSRID(schema, name)
	var rows *sql.Rows
	if vectorSRID == SRID {
		rows, err = pg.Db.Query(fmt.Sprintf(
			"SELECT ST_AsGeoJson(geom) FROM \"%v\".\"%v\"", schema, name))
	} else {
		rows, err = pg.Db.Query(fmt.Sprintf(
			"SELECT ST_AsGeoJson(ST_Transform(geom,%v)) FROM \"%v\".\"%v\"",
			SRID, schema, name))
	}
	if err != nil {
		return
	}

	var jsonout []byte
	i := 0
	for rows.Next() {
		err = rows.Scan(&jsonout)
		if err != nil {
			return
		}
		type holder struct {
			Type        string
			Coordinates []interface{}
		}
		var data holder
		err = json.Unmarshal(jsonout, &data)
		if err != nil {
			return
		}
		out[i] = new(VectorShape)
		out[i].Shapetype = data.Type
		out[i].name = name
		out[i].schema = schema
		switch data.Type {
		case "Point":
			out[i].X = make([]float64, 1)
			out[i].X[0] = data.Coordinates[0].(float64)
			out[i].Y = make([]float64, 1)
			out[i].Y[0] = data.Coordinates[1].(float64)
		case "LineString":
			out[i].X = make([]float64, len(data.Coordinates))
			out[i].Y = make([]float64, len(data.Coordinates))
			for j, point := range data.Coordinates {
				pointfloat := point.([]interface{})
				out[i].X[j] = pointfloat[0].(float64)
				out[i].Y[j] = pointfloat[1].(float64)
			}
		case "Polygon":
			// Inner rings and holes are not supported
			if len(data.Coordinates) > 1 {
				fmt.Println("WARNING: Ignoring inner rings of polygon")
			}
			points := data.Coordinates[0].([]interface{})
			out[i].X = make([]float64, len(points))
			out[i].Y = make([]float64, len(points))
			for j, point := range points {
				pointfloat := point.([]interface{})
				out[i].X[j] = pointfloat[0].(float64)
				out[i].Y[j] = pointfloat[1].(float64)
			}
		case "MultiPolygon":
			for j, polygon := range data.Coordinates {
				v := new(VectorShape)
				v.Shapetype = data.Type
				v.name = name
				v.schema = schema
				poly := polygon.([]interface{})
				// Inner rings and holes are not supported
				if len(poly) > 1 {
					fmt.Println("WARNING: Ignoring inner rings of polygon")
				}
				points := poly[0].([]interface{})
				v.X = make([]float64, len(points))
				v.Y = make([]float64, len(points))
				for k, point := range points {
					pointfloat := point.([]interface{})
					v.X[k] = pointfloat[0].(float64)
					v.Y[k] = pointfloat[1].(float64)
				}
				if j == 0 {
					out[i] = v
				} else {
					out = append(out, v)
				}
			}
		default:
			fmt.Println(data)
			panic(fmt.Sprintf("Type %v not supported.", data.Type))
		}
		for _, val := range out[i].X {
			if val < out[i].Xmin {
				out[i].Xmin = val
			}
			if val > out[i].Xmax {
				out[i].Xmax = val
			}
		}
		for _, val := range out[i].Y {
			if val < out[i].Ymin {
				out[i].Ymin = val
			}
			if val > out[i].Ymax {
				out[i].Ymax = val
			}
		}
		i++
	}
	rows.Close()
	err = rows.Err()
	return
}

func (pg *PostGis) MakeRasterTable(schema, name string) {
	pg.DropTable(schema, name)
	cmd := fmt.Sprintf("CREATE TABLE %v.%v(rid serial primary key, "+
		"name varchar, rast raster);", schema, name)
	Log(cmd, 3)
	_, err := pg.Db.Exec(cmd)
	if err != nil {
		panic(err)
	}
}

func (pg *PostGis) AddDataRowToRasterTable(schema, tableName, rasterRowName string,
	grid *GridDef, data *sparse.SparseArray) {
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
	_, err = pg.Db.Exec(cmd)
	if err != nil {
		panic(err)
	}

	var val float64
	for _, i := range data.Nonzero() {
		val = data.Get1d(i)
		index := data.IndexNd(i)
		_, err = pg.Db.Exec(fmt.Sprintf("UPDATE %v.%v SET rast="+
			"ST_SetValue(rast,%v,%v,%v) WHERE name='%v';",
			schema, tableName, index[1]+1, index[0]+1, val, rasterRowName))
		if err != nil {
			panic(err)
		}
	}
}

func (pg *PostGis) WriteOutRaster(schema, tableName, rasterRowName,
	filename string) {
	cmd := fmt.Sprintf("SELECT ST_AsTIFF(rast, 'PACKBITS') "+
		"FROM %v.%v WHERE name='%v';", schema, tableName, rasterRowName)
	var data []byte
	Log(cmd, 3)
	err := pg.Db.QueryRow(cmd).Scan(&data)
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
