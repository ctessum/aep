package main

import (
	"goconf.googlecode.com/hg"
	"fmt"
	"strings"
	"path/filepath"
	"os"
	"flag"
	"time"
)

const (
	tons2g   = 907184.74
	tonnes2g = 1.0e6
	kg2g     = 1000.
	g2g      = 1.0
	lbs2g    = 453.59237
)

// type RunData is a container for the configuration and report info
type RunData struct {
	sector         string
	sectorType     string
	SmkHome        string
	input          string
	logs           string
	sectorLogs     string
	ancilliary     string
	startDate      *time.Time
	endDate        *time.Time
	tStepSec       int
	inventoryMonth string
	outputType     string
	srgDesc        string
	specSynonyms   string
	specRef        string
	specConv       string
	specPro        string
	specType       string
	specFrac       map[string]map[string]map[string]specHolder
	PolsToDrop     string
	caseName       string
	inventoryFreq  string
	inputUnits     string
	inputConv      float64
	invFileNames   string
	invFilePaths   string
	currentTime    *time.Time
	// report files
	InvRep  *os.File
	SpecRep *os.File
}

func newRunData() (s RunData) {
	s.inventoryMonth = "None"
	return
}

func (c *RunData) ErrorReport(error interface{}) {
	err := "ERROR REPORT\n"
	err += "Sector: " + c.sector + "\n"
	err += "Message:\n"
	err += fmt.Sprintf("%v", error) + "\n"
	fmt.Print(err)
	return
}

// catPaths replaces directory names with full paths
func (p *RunData) catPaths() {
	c := *p
	c.input = strings.Replace(c.input, "[SmkHome]", c.SmkHome, -1)
	c.ancilliary = strings.Replace(c.ancilliary, "[input]", c.input, -1)
	c.ancilliary = strings.Replace(c.ancilliary, "[SmkHome]", c.SmkHome, -1)
	c.logs = strings.Replace(c.logs, "[input]", c.input, -1)
	c.logs = strings.Replace(c.logs, "[SmkHome]", c.SmkHome, -1)
	_, err := os.Stat(c.SmkHome)
	if err != nil {
		panic(err)
	}
	_, err = os.Stat(c.input)
	if err != nil {
		panic(err)
	}
	_, err = os.Stat(c.ancilliary)
	if err != nil {
		panic(err)
	}
	*p = c
}

type errCat struct {
	str string
}

func (e *errCat) getVariable(varname, sector string, c *conf.ConfigFile) string {
	result, err := c.GetString(sector, varname)
	if err != nil {
		result, err = c.GetString("default", varname)
		if err != nil {
			e.str += err.String() + "\n"
		}
	}
	return result
}
func (e *errCat) getSectorVariable(varname, sector string, c *conf.ConfigFile) string {
	result, err := c.GetString(sector, varname)
	if err != nil {
		e.str += err.String() + "\n"
	}
	return result
}
func (e *errCat) getVariableInt(varname, sector string, c *conf.ConfigFile) int {
	result, err := c.GetInt(sector, varname)
	if err != nil {
		result, err = c.GetInt("default", varname)
		if err != nil {
			e.str += err.String() + "\n"
		}
	}
	return result
}
func (e *errCat) getLowerCaseVar(varname, sector string, c *conf.ConfigFile) string {
	result, err := c.GetString(sector, varname)
	if err != nil {
		result, err = c.GetString("default", varname)
		if err != nil {
			e.str += err.String() + "\n"
		}
	}
	result = strings.ToLower(result)
	return result
}
func (e *errCat) createFilePath(varname, basePath, sector string, c *conf.ConfigFile) string {
	result, err := c.GetString(sector, varname)
	if err != nil {
		result, err = c.GetString("default", varname)
		if err != nil {
			e.str += "In createFilePath, sector=" + sector + ", "
			e.str += "While trying to read variable " + varname + " from configuration file, "
			e.str += err.String() + "\n"
		}
	}
	result = filepath.Join(basePath, result)
	_, err = os.Stat(result)
	if err != nil {
		e.str += "In createFilePath, sector=" + sector + ", "
		e.str += "While trying to read variable " + varname + " from configuration file, "
		e.str += err.String() + "\n"
	}
	return result
}

func main() {
	// Read from configuration file and prepare sectors for processing
	var sectorFlag *string = flag.String("sectors", "none", "List of sectors to process, in quotes, separated by spaces")
	var configFile *string = flag.String("config", "none", "Path to configuration file")
	fmt.Println("success")
	flag.Parse()
	if *sectorFlag == "none" || *configFile == "none" {
		fmt.Println("Please set `-sectors' and `-config' flags and run again: ie: GoSmoke -config=config_file -sectors=\"sector list\"")
		fmt.Println("For more information try typing `GoSmoke --help'")
		return
	}

	// create list of sectors
	sectors := strings.Split(*sectorFlag, " ")

	err := ReadConfigFile(*configFile)

	// parse configuration file
	c, err := conf.ReadConfigFile(*configFile)
	if err != nil {
		panic(err)
	}

	e := new(errCat)
	runChan := make(chan string, len(sectors))
	for _, sect := range sectors {
		config := newRunData()
		config.sector = sect

		// Read in settings and input file locations and make sure files exist.
		// First try to read in sector-specific settings, use defaults if they 
		// don't exist.
		config.SmkHome = e.getVariable("SmkHome", config.sector, c)
		config.input = e.getVariable("input", config.sector, c)
		config.ancilliary = e.getVariable("ancilliary", config.sector, c)
		config.logs = e.getVariable("logs", config.sector, c)
		// Replace variable names such as [input] in directory paths with full paths
		config.catPaths()

		dateTemp := e.getVariable("startDate", config.sector, c)
		config.startDate, err = time.Parse("2006/01/02", dateTemp)
		config.currentTime = config.startDate
		if err != nil {
			e.str += err.String() + "\n"
		}
		dateTemp = e.getVariable("endDate", config.sector, c)
		config.endDate, err = time.Parse("2006/01/02", dateTemp)
		if err != nil {
			e.str += err.String() + "\n"
		}

		config.outputType = e.getVariable("outputType", config.sector, c)
		if config.outputType == "CAMx" || config.outputType == "WRF" {
			config.endDate = time.SecondsToUTC(config.endDate.Seconds() + 23*60*60)
		} else if config.outputType == "CMAQ" {
			config.endDate = time.SecondsToUTC(config.endDate.Seconds() + 24*60*60)
		} else {
			e.str += "ouputType " + config.outputType + " unknown\n"
		}

		config.tStepSec = e.getVariableInt("tStepSec", config.sector, c)

		config.srgDesc = e.createFilePath("srgDesc", config.ancilliary, config.sector, c)
		config.specSynonyms = e.createFilePath("specSynonyms", config.ancilliary, config.sector, c)

		config.caseName = e.getVariable("caseName", config.sector, c)

		config.specRef = e.createFilePath("specRef", config.ancilliary, config.sector, c)
		config.specConv = e.createFilePath("specConv", config.ancilliary, config.sector, c)
		config.specPro = e.createFilePath("specPro", config.ancilliary, config.sector, c)
		config.specType = e.getLowerCaseVar("specType", config.sector, c)

		err = config.SpecFractions(config.specRef, config.specConv, config.specPro, config.specSynonyms, config.specType)
		if err != nil {
			e.str += err.String() + "\n"
		}

		config.PolsToDrop = e.getVariable("PolsToDrop", config.sector, c)
		config.sectorType = e.getLowerCaseVar("sectorType", config.sector, c)
		config.inventoryFreq = e.getLowerCaseVar("inventoryFreq", config.sector, c)
		if config.inventoryFreq != "annual" && config.inventoryFreq != "monthly" {
			e.str += "In sector " + config.sector + ", " + config.inventoryFreq + " is not a valid value for variable inventoryFreq. Please choose either `annual' or `monthly'."
		}
		config.invFileNames = e.getSectorVariable("invFileNames", config.sector, c)

		config.inputUnits = e.getLowerCaseVar("inputUnits", config.sector, c)
		if config.inputUnits == "tons/year" {
			config.inputConv = tons2g
		} else if config.inputUnits == "tonnes/year" {
			config.inputConv = tonnes2g
		} else if config.inputUnits == "kg/year" {
			config.inputConv = kg2g
		} else if config.inputUnits == "g/year" {
			config.inputConv = g2g
		} else if config.inputUnits == "lbs/year" {
			config.inputConv = lbs2g
		} else {
			e.str += "In configuration file: unknown value " + config.inputUnits + " for variable inputUnits. Acceptable values are `tons/year', `tonnes/year', `kg/year', `g/year', and `lbs/year'."
		}

		// Create sector-specific log directory path.
		config.sectorLogs = filepath.Join(config.logs, config.caseName, config.sector)

		// Convert file names to full file paths
		for _, file := range strings.Split(config.invFileNames, " ") {
			fileTest := filepath.Join(config.input, config.caseName, config.sector, file)
			if config.inventoryFreq == "annual" {
				_, err = os.Stat(fileTest)
				if err != nil {
					e.str += err.String() + "\n"
				}
			} else if config.inventoryFreq == "monthly" {
				for _, month := range strings.Split("jan feb mar apr may jun jul aug sep oct nov dec", " ") {
					_, err = os.Stat(strings.Replace(fileTest, "[month]", month, -1))
					if err != nil {
						e.str += err.String() + "\n"
					}
				}
			}
			config.invFilePaths += fileTest + " "
		}

		// run subroutines
		go config.Run(runChan)
	}
	if e.str != "" {
		fmt.Println("The following errors were found:\n" + e.str)
		os.Exit(1)
	}
	// wait for calculations to complete
	for i := 0; i < len(sectors); i++ {
		message := <-runChan
		fmt.Println(message)
	}
}

func (c *RunData) nextTime() {
	c.currentTime = time.SecondsToUTC(c.currentTime.Seconds() + int64(c.tStepSec))
}
func (c *RunData) CurrentMonth() (month string) {
	month = strings.ToLower(c.currentTime.Format("Jan"))
	return
}

func (c *RunData) Run(runChan chan string) {
	MesgChan := make(chan string, 100)
	if c.inventoryFreq == "annual" {
		InvSpecChan := make(chan ParsedRecord, 1)
		go c.Inventory(MesgChan, InvSpecChan, "annual")
		go c.Speciate(MesgChan, InvSpecChan, "annual")
		// wait for calculations to complete
		for i := 0; i < 2; i++ {
			message := <-MesgChan
			fmt.Println(message)
		}
	}
	for {
		month := c.CurrentMonth()
		if c.inventoryFreq == "monthly" && month != c.inventoryMonth {
			InvSpecChan := make(chan ParsedRecord, 1)
			go c.Inventory(MesgChan, InvSpecChan, month)
			go c.Speciate(MesgChan, InvSpecChan, month)
			// wait for calculations to complete
			for i := 0; i < 2; i++ {
				message := <-MesgChan
				fmt.Println(message)
			}
			c.inventoryMonth = month
		}
		// either advance to next date or end loop
		//fmt.Println(c.currentTime.Format("Mon Jan 2 15:04:05 2006"))
		if c.currentTime.Seconds() < c.endDate.Seconds() {
			c.nextTime()
		} else {
			break
		}
	}

	runChan <- "Finished processing " + c.sector
}
