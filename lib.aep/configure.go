package aep

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
	"time"
)

const (
	tons2g   = 907184.74
	tonnes2g = 1.0e6
	kg2g     = 1000.
	g2g      = 1.0
	lbs2g    = 453.59237
	months   = "jan feb mar apr may jun jul aug sep oct nov dec"
)

// Reads and parse a json configuration file.
// See below for the required variables.
func ReadConfigFile(filepath *string, testmode *bool, e *ErrCat) (config *configInput) {
	// Open the configuration file
	var (
		file  *os.File
		bytes []byte
		err   error
	)
	file, err = os.Open(*filepath)
	if err != nil {
		fmt.Printf("The configuration file you have specified, %v, does not "+
			"appear to exist. Please check the file name and location and "+
			"try again.\n", *filepath)
		os.Exit(1)
	}
	reader := bufio.NewReader(file)
	bytes, err = ioutil.ReadAll(reader)
	if err != nil {
		panic(err)
	}

	config = new(configInput)
	err = json.Unmarshal(bytes, config)
	if err != nil {
		fmt.Printf(
			"There has been an error parsing the configuration file.\n"+
				"Please ensure that the file is in valid JSON format\n"+
				"(you can check for errors at http://jsonlint.com/)\n"+
				"and try again!\n\n%v\n\n", err.Error())
		os.Exit(1)
	}

	config.DefaultSettings.testMode = *testmode
	// Replace variables in directories with full paths
	config.setup(e)

	return
}

type configInput struct {
	Dirs            *DirInfo
	DefaultSettings *RunData
	Sectors         map[string]*RunData
}

type DirInfo struct {
	Home       string // Home directory
	Input      string // Directory of input files
	Ancilliary string // Home directory of log files
	Output     string // Directory for ancilliary information
}

// type RunData is a container for the configuration and report info
type RunData struct {
	outputDir                      string // Output directory
	Sector                         string // Name of the sector
	SectorType                     string // Type of sector (point, area)
	RunSpeciate                    bool   // Whether to speciate data
	RunSpatialize                  bool   // Whether to spatialize data
	RunTemporal                    bool   // Whether to temporalize data
	StartDate                      string
	startDate                      time.Time
	EndDate                        string
	endDate                        time.Time
	Tstep                          string
	tStep                          time.Duration
	OutputType                     string
	SccDesc                        string // name of file with SCC code descriptions
	SicDesc                        string // name of file with SIC code descriptions
	NaicsDesc                      string // name of file with NAICS code descriptions
	SpecRefFile                    string
	SpecRefComboFile               string
	SpecProFile                    string
	SpecType                       string // Type of speciation to perform. Either "mol" or "mass".
	SpeciesGroupName               string // name for chemical species grouping scheme (needs to be present in SPECIATE database as columns with "_GROUP" and "_FACTOR" appended)
	specFrac                       map[string]map[string]map[string]specHolder
	PolsToKeep                     map[string]*PolHolder // List and characteristics of pollutants to extract from the inventory and process
	GridRefFile                    string
	SrgSpecFile                    string
	TemporalRefFile                string
	TemporalProFile                string
	HolidayFile                    string
	CaseName                       string
	InventoryFreq                  string
	MatchFullSCC                   bool
	DebugLevel                     int // Sets the volume of output printed to the screen. Set to 0 for least output, 3 for most output. Also, if DebugLevel > 0, any errors encountered will cause the entire program to crash with a stack trace, rather than just printing an error message and continuing.
	Ncpus                          int // Number of processors available for use
	InputUnits                     string
	InputConv                      float64
	InvFileNames                   []string
	EarthRadius                    float64       // in meters
	PostGISuser                    string        // should have been chosen when setting up PostGIS
	PostGISdatabase                string        // should have been previously created as a PostgreSQL database with the PostGIS additions
	PostGISpassword                string        // should have been chosen when setting up PostGIS
	OtherLibpqConnectionParameters string        // Other parameters for connecting to the database. See: https://github.com/bmizerany/pq
	ShapefileSchema                string        // PostGIS schema where surrogate shapefiles are stored
	SrgCacheExpirationTime         time.Duration // Time in minutes after which surrogates in memeory cache are purged. Decrease to reduce memory usage, increase for faster performance. Default is 5 minutes.
	WPSnamelist                    string        // Path to WPS namelist file
	WRFnamelist                    string        // Path to WPS namelist file
	OldWRFout                      string        // Path to old WRF output files for plume rise calculations. [DOMAIN] and [DATE] can be used as wildcards.
	wrfData                        *WRFconfigData
	SRID                           int    // PostGIS projection ID number. It should be a number not currently being used by PostGIS, unless the output projection is the same as the projection in any of the input shapefiles, in which case the SRID should be the same as the SRID in the matching input data (using more than one SRID for the same projection will cause errors).
	LineMap                        string // Name of map containing lines to be overlayed on output maps. Should be in the "PERMANENT" mapset of the "SpatialDataLoc" location
	RegenerateSpatialData          bool   // Whether or not to delete surrogates and shapefiles generated by previous run and start over. If the model has not been previously run with the current "SimuationName", this does nothing.
	SimulationName                 string // Name for the simulation: user choice
	currentTime                    time.Time
	ErrorFlag                      bool // whether this sector has already encountered an error
	testMode                       bool // testMode is set by a command line flag and is used for testing the program. In ensures that SpecType is set to "mass" and turns off the conversion from VOCs to TOGs so that speciated emissions totals can be compared to total emissions in the inventory
	msgchan                        chan string
}

type PolHolder struct {
	SpecType  string   // The type of speciation that will be applied. Options are "VOC","PM2.5","NOx", and "SOx". If empty, the pollutant will be carried through to the output without speciation, or grouped as if it were the pollutants in "SpecNames".
	SpecNames []string // Names of pollutants in the SPECIATE database which are equivalent to this pollutant. For records containing this pollutant, the pollutants included in "SpecNames" will be left out of any speciation that occurs to avoid double counting.
}

func (p *configInput) setup(e *ErrCat) {
	c := *p
	var err error
	if c.DefaultSettings.InventoryFreq == "" {
		c.DefaultSettings.InventoryFreq = "annual"
	}
	c.DefaultSettings.OutputType =
		strings.ToLower(c.DefaultSettings.OutputType)
	c.DefaultSettings.setup(e)
	c.DefaultSettings.catPaths(c.Dirs, e)
	c.DefaultSettings.wrfData, err = ParseWRFConfig(
		c.DefaultSettings.WPSnamelist, c.DefaultSettings.WRFnamelist)
	if err != nil {
		e.Add(err)
	}
	if c.DefaultSettings.testMode {
		c.DefaultSettings.SpecType = "mass"
	}
	for sector, _ := range c.Sectors {
		c.Sectors[sector].Sector = sector
		c.Sectors[sector].FillWithDefaults(c.DefaultSettings, e)
		c.Sectors[sector].setup(e)
		c.Sectors[sector].catPaths(c.Dirs, e)
	}
	*p = c
}

func (p *RunData) FillWithDefaults(d *RunData, e *ErrCat) {
	c := *p
	c.RunSpeciate = d.RunSpeciate
	c.RunSpatialize = d.RunSpatialize
	c.RunTemporal = d.RunTemporal
	c.StartDate = d.StartDate
	c.EndDate = d.EndDate
	c.Tstep = d.Tstep
	c.OutputType = d.OutputType
	c.TemporalRefFile = d.TemporalRefFile
	c.TemporalProFile = d.TemporalProFile
	c.HolidayFile = d.HolidayFile
	c.EarthRadius = d.EarthRadius
	c.WPSnamelist = d.WPSnamelist
	c.WRFnamelist = d.WRFnamelist
	c.OldWRFout = d.OldWRFout
	c.SRID = d.SRID
	c.wrfData = d.wrfData
	c.SimulationName = d.SimulationName
	c.PostGISuser = d.PostGISuser
	c.PostGISdatabase = d.PostGISdatabase
	c.PostGISpassword = d.PostGISpassword
	c.OtherLibpqConnectionParameters = d.OtherLibpqConnectionParameters
	c.ShapefileSchema = d.ShapefileSchema
	if d.SrgCacheExpirationTime == 0 {
		d.SrgCacheExpirationTime = 5
	}
	c.SrgCacheExpirationTime = d.SrgCacheExpirationTime
	c.testMode = d.testMode
	if c.SccDesc == "" {
		c.SccDesc = d.SccDesc
	}
	if c.SicDesc == "" {
		c.SicDesc = d.SicDesc
	}
	if c.NaicsDesc == "" {
		c.NaicsDesc = d.NaicsDesc
	}
	if c.SpecRefFile == "" {
		c.SpecRefFile = d.SpecRefFile
	}
	if c.SpecRefComboFile == "" {
		c.SpecRefComboFile = d.SpecRefComboFile
	}
	if c.SpecProFile == "" {
		c.SpecProFile = d.SpecProFile
	}
	if c.SpecType == "" {
		c.SpecType = d.SpecType
	}
	if c.testMode { // for testing do mass speciation so speciated totals match inventory totals.
		c.SpecType = "mass"
	}
	if c.SpeciesGroupName == "" {
		c.SpeciesGroupName = d.SpeciesGroupName
	}
	if c.PolsToKeep == nil {
		c.PolsToKeep = d.PolsToKeep
	}
	if c.GridRefFile == "" {
		c.GridRefFile = d.GridRefFile
	}
	if c.SrgSpecFile == "" {
		c.SrgSpecFile = d.SrgSpecFile
	}
	if c.InputUnits == "" {
		c.InputUnits = d.InputUnits
	}
	*p = c
}

func (p *RunData) setup(e *ErrCat) {
	c := *p
	var err error
	c.startDate, err = time.Parse("2006/01/02", c.StartDate)
	if err != nil {
		panic(err)
	}
	e.Add(err)
	c.currentTime = c.startDate
	c.endDate, err = time.Parse("2006/01/02", c.EndDate)
	e.Add(err)
	c.tStep, err = time.ParseDuration(c.Tstep)
	e.Add(err)
	if c.InventoryFreq != "annual" && c.InventoryFreq != "monthly" {
		e.Add(fmt.Errorf("In sector " + c.Sector + ", " + c.InventoryFreq +
			" is not a valid value for variable inventoryFreq. Please choose " +
			"either `annual' or `monthly'."))
	}
	switch c.InputUnits {
	case "tons/year":
		c.InputConv = tons2g
	case "tonnes/year":
		c.InputConv = tonnes2g
	case "kg/year":
		c.InputConv = kg2g
	case "g/year":
		c.InputConv = g2g
	case "lbs/year":
		c.InputConv = lbs2g
	default:
		e.Add(fmt.Errorf("In configuration file: unknown value " + c.InputUnits +
			" for variable InputUnits. Acceptable values are `tons/year', " +
			"`tonnes/year', `kg/year', `g/year', and `lbs/year'."))
	}
	// Fill in information about ending time (varies by output file format)
	if c.OutputType == "camx" || c.OutputType == "wrf" {
		d, err := time.ParseDuration("23h")
		e.Add(err)
		c.endDate = c.endDate.Add(d)
	} else if c.OutputType == "cmaq" {
		d, err := time.ParseDuration("24h")
		e.Add(err)
		c.endDate = c.endDate.Add(d)
	} else if c.OutputType != "none" {
		e.Add(fmt.Errorf("OutputType " + c.OutputType + " unknown"))
	}
	*p = c
}

// Replace directory and file names with full paths
func (p *RunData) catPaths(d *DirInfo, e *ErrCat) {
	c := *p

	d.Output = strings.Replace(strings.Replace(strings.Replace(
		d.Output, "[Home]", d.Home, -1),
		"[input]", d.Input, -1),
		"[Ancilliary]", d.Ancilliary, -1)
	d.Output = os.ExpandEnv(d.Output)
	err := os.MkdirAll(d.Output, os.ModePerm)
	if err != nil {
		e.Add(fmt.Errorf("Unable to make output directory `%v'.",
			d.Output))
	}
	c.outputDir = d.Output
	if c.OutputType != "none" {
		path := filepath.Join(d.Output, c.OutputType)
		err := os.MkdirAll(path, os.ModePerm)
		if err != nil {
			e.Add(fmt.Errorf("Unable to make AQM output directory `%v'.",
				d.Output))
		}
	}

	paths := []*string{&d.Home, &d.Input, &d.Ancilliary,
		&c.SpecRefFile, &c.SpecRefComboFile, &c.SpecProFile,
		&c.SccDesc, &c.SicDesc, &c.NaicsDesc,
		&c.GridRefFile, &c.TemporalRefFile, &c.TemporalProFile,
		&c.HolidayFile, &c.SrgSpecFile, &c.WPSnamelist, &c.WRFnamelist,
		&c.OldWRFout}
	varnames := []string{"Home", "Input", "Ancilliary",
		"SpecRefFile", "SpecRefComboFile", "SpecProFile",
		"SccDesc", "SicDesc", "NaicsDesc",
		"GridRefFile", "TemporalRefFile", "TemporalProFile", "HolidayFile",
		"SrgSpecFile", "WPSnamelist", "WRFnamelist", "OldWRFout"}

	for i, path := range paths {
		*path = strings.Replace(*path, "[Home]", d.Home, -1)
		*path = strings.Replace(*path, "[Home]", d.Home, -1)
		*path = strings.Replace(*path, "[input]", d.Input, -1)
		*path = strings.Replace(*path, "[Ancilliary]", d.Ancilliary, -1)
		*path = os.ExpandEnv(*path)
		if strings.Index(*path, "[DOMAIN]") == -1 &&
			strings.Index(*path, "[DATE]") == -1 {
			e.statOS(*path, varnames[i])
		}
	}
	if c.Sector != "" {
		sectorPath := filepath.Join(d.Input, c.CaseName, c.Sector)
		e.statOS(sectorPath, "Sector "+c.Sector+" input")
		for i, name := range c.InvFileNames {
			if c.InventoryFreq == "annual" {
				c.InvFileNames[i] = filepath.Join(sectorPath, name)
				e.statOS(c.InvFileNames[i], c.Sector+" Inventory file")
			} else if c.InventoryFreq == "monthly" {
				for _, month := range strings.Split(months, " ") {
					c.InvFileNames[i] = filepath.Join(sectorPath,
						strings.Replace(name, "[month]", month, -1))
					e.statOS(c.InvFileNames[i], c.Sector+" Inventory file")
				}
			}
		}
	}
	*p = c
	return
}

func (c *RunData) MessageChan() chan string {
	c.msgchan = make(chan string, 1)
	return c.msgchan
}