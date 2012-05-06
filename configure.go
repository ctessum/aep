package main

import (
	"bufio"
	"encoding/json"
//	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
	"time"
"sort"
"errors"
)

const (
	tons2g   = 907184.74
	tonnes2g = 1.0e6
	kg2g     = 1000.
	g2g      = 1.0
	lbs2g    = 453.59237
	months ="jan feb mar apr may jun jul aug sep oct nov dec"
)

// func ReadConfigFile reads and parses a json configuration file.
// See below for the required variables.
func ReadConfigFile(filepath *string, sectors []string) (SectorConfig []RunData, err error) {
	// Open the configuration file
	var (
		file  *os.File
		bytes []byte
	)
	if file, err = os.Open(*filepath); err != nil {
		return SectorConfig, err
	}
	reader := bufio.NewReader(file)
	if bytes, err = ioutil.ReadAll(reader); err != nil {
		return SectorConfig, err
	}

	var temp interface{}
	if err = json.Unmarshal(bytes, &temp); err != nil {
		return SectorConfig, err
	}

	c := temp.(map[string]interface{}) // configuration variable

	e := new(ErrCat) // error report

	SectorConfig = make([]RunData, len(sectors))

	// First, fill in all the default values
	for krd, rd := range SectorConfig {
		rd.inventoryMonth = "None"
		for _, v := range c {
			switch vv := v.(type) {
			case map[string]interface{}:
				for i, u := range vv {
					rd.process(i, u, e)
				}
			}
		}
		rd.catPaths(e)
		SectorConfig[krd] = rd
	}
	// Now, fill in sector specific values
	for krd, rd := range SectorConfig {
		for k, v := range c {
			if k == "sectors" {
				switch vv := v.(type) {
				case map[string]interface{}:
					for i, u := range vv {
						if i == sectors[krd] {
							rd.sector = sectors[krd]

							switch uu := u.(type) {
							case map[string]interface{}:
								for q, z := range uu {
									rd.process(q, z, e)
								}
							}
						}
					}
				}
			}
		}
		// Replace variables in directories with full paths
		rd.catPaths(e)

		// Fill in information about ending time (varies by output file format)
		if rd.outputType == "CAMx" || rd.outputType == "WRF" {
			d, err := time.ParseDuration("23h")
			e.Add(err)
			rd.endDate = rd.endDate.Add(d)
		} else if rd.outputType == "CMAQ" {
			d, err := time.ParseDuration("24h")
			e.Add(err)
			rd.endDate = rd.endDate.Add(d)
		} else {
			e.Add(errors.New("OutputType " + rd.outputType + " unknown"))
		}


		SectorConfig[krd] = rd
	}

	e.Report() // Print errors, if any

	return SectorConfig, err
}

// type RunData is a container for the configuration and report info
type RunData struct {
	sector         string
	sectorType     string
	SmkHome        string
	input          string
	logs           string
	sectorLogs     string
	ancilliary     string
	startDate      time.Time
	endDate        time.Time
	tStep	   time.Duration
	inventoryMonth string
	outputType     string
	srgDesc        string
	specSynonyms   string
	specRef        string
	specConv       string
	specPro        string
	specType       string
	specFrac       map[string]map[string]map[string]specHolder
	PolsToDrop     []string
	caseName       string
	inventoryFreq  string
	inputUnits     string
	inputConv      float64
	invFileNames   []string
	invFilePaths   string
	currentTime    time.Time
	// report files
	InvRep  *os.File
	SpecRep *os.File
}

func (p *RunData) process(name string, value interface{}, e *ErrCat) {
	c := *p
	var err error
	switch vv := value.(type) {
	case string:
		switch name {
		case "SmkHome":
			c.SmkHome = vv
		case "input":
			c.input = vv
		case "logs":
			c.logs = vv
		case "ancilliary":
			c.ancilliary = vv
		case "startDate":
			c.startDate, err = time.Parse("2006/01/02", vv)
				e.Add(err)
			c.currentTime = c.startDate
		case "endDate":
			c.endDate, err = time.Parse("2006/01/02", vv)
				e.Add(err)
		case "tStep":
			c.tStep, err = time.ParseDuration(vv)
			e.Add(err)
		case "outputType":
			c.outputType = vv
		case "srgDesc":
			c.srgDesc = vv
		case "specSynonyms":
			c.specSynonyms = vv
		case "specRef":
			c.specRef = vv
		case "specConv":
			c.specConv = vv
		case "specPro":
			c.specPro = vv
		case "specType":
			c.specType = vv
		case "caseName":
			c.caseName = vv
		case "inventoryFreq":
			c.inventoryFreq = vv
			if vv != "annual" && vv != "monthly" {
				e.Add(errors.New("In sector " + c.sector + ", " + c.inventoryFreq + 
				" is not a valid value for variable inventoryFreq. Please choose " + 
				"either `annual' or `monthly'."))
}
		case "inputUnits":
			c.inputUnits = vv
			switch vv {
case "tons/year":
			c.inputConv = tons2g
		case "tonnes/year":
			c.inputConv = tonnes2g
		case "kg/year":
			c.inputConv = kg2g
		case "g/year":
			c.inputConv = g2g
		case "lbs/year":
			c.inputConv = lbs2g
		default:
			str := "In configuration file: unknown value "+ c.inputUnits + " for variable inputUnits. Acceptable values are `tons/year', `tonnes/year', `kg/year', `g/year', and `lbs/year'."
			e.Add(errors.New(str))
		}
		}
	case float64:
		switch name {
		case "inputConv":
			c.inputConv = vv
		}
	case []interface{}:
		tempslice := make([]string,len(vv))
		for i,u := range vv{
			switch uu := u.(type) {
			case string:
			tempslice[i] = uu
}}
		switch name {
		case "PolsToDrop":
			c.PolsToDrop = tempslice
			sort.Strings(c.PolsToDrop)
		case "invFileNames":
			c.invFileNames = tempslice
		}
	}
	*p = c
}

// catPaths replaces directory and file names with full paths
func (p *RunData) catPaths(e *ErrCat) {
	c := *p
	e.statOS(c.SmkHome)
	c.input = strings.Replace(c.input, "[SmkHome]", c.SmkHome, -1)
	e.statOS(c.input)
	c.ancilliary = strings.Replace(c.ancilliary, "[SmkHome]", c.SmkHome, -1)
	c.ancilliary = strings.Replace(c.ancilliary, "[input]", c.input, -1)
	e.statOS(c.ancilliary)	
	c.logs = strings.Replace(c.logs, "[SmkHome]", c.SmkHome, -1)
	c.logs = strings.Replace(c.logs, "[input]", c.input, -1)
	e.statOS(c.logs)
	c.specRef = e.createFilePath(c.ancilliary, c.specRef)
	c.specConv = e.createFilePath(c.ancilliary, c.specConv)
	c.specPro = e.createFilePath(c.ancilliary, c.specPro)
	c.specSynonyms = e.createFilePath(c.ancilliary, c.specSynonyms)
	if c.sector != "" {
	casePath := e.createFilePath(c.input,c.caseName)
	caseLogPath := e.createFilePath(c.logs,c.caseName)
	sectorPath := e.createFilePath(casePath,c.sector)
	c.sectorLogs = filepath.Join(caseLogPath, c.sector)
	for i,name := range(c.invFileNames) {
		if c.inventoryFreq == "annual" {
	c.invFileNames[i] = e.createFilePath(sectorPath,name)
} else if c.inventoryFreq == "monthly" {
				for _, month := range strings.Split(months, " ") {
					_, err := os.Stat(strings.Replace(name, "[month]", month, -1))
					e.Add(err)
					}
		
}
}
}

	return
}

func (e *ErrCat) createFilePath(basePath string, input string) string {
	result := filepath.Join(basePath, input)
	e.statOS(result)
	return result
}

func (e *ErrCat) statOS(path string) {
	_, err := os.Stat(path)
	e.Add(err)
return
}
