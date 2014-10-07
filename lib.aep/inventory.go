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
	"bitbucket.org/ctessum/sparse"
	"bufio"
	"fmt"
	"io"
	"os"
	"runtime"
	"strconv"
	"strings"
	"sync"
)

// type FileInfo holds information about each inventory file
type FileInfo struct {
	fname         string
	Format        string
	Ftype         string
	Year          string
	Country       string
	Totals        map[string]float64
	DroppedTotals map[string]float64
	Units         string  // Units for emissions values
	InputConv     float64 // factor for converting emissions to grams
	polid         string
	fid           *os.File
	buf           *bufio.Reader
}

func newFileInfo() (f *FileInfo) {
	f = new(FileInfo)
	f.Totals = make(map[string]float64)
	f.DroppedTotals = make(map[string]float64)
	return
}

// The ParsedRecord type is a container for all of the needed information in the ORL or IDA files.
// See the SMOKE manual for file format specification.
type ParsedRecord struct {
	FIPS    string //	Five digit FIPS code for state and county (required)
	PLANTID string //	Plant Identification Code (15 characters maximum) (required, this is the same as the State Facility Identifier in the NIF)
	POINTID string //	Point Identification Code (15 characters maximum) (required, this is the same as the Emission Unit ID in the NIF)
	STACKID string //	Stack Identification Code (15 characters maximum) (recommended, this is the same as the Emissions Release Point ID in the NIF)
	SEGMENT string //	DOE Plant ID (15 characters maximum) (recommended, this is the same as the Process ID in the NIF)
	PLANT   string //	Plant Name (40 characters maximum) (recommended)
	SCC     string //	Ten character Source Classification Code (required)
	SRCTYPE string // Source type (2 characters maximum), used by SMOKE in determining applicable MACT-based controls (required)
	// 01 = major
	// 02 = Section 12 area source
	// 03 = nonroad
	// 04 = onroad
	STKHGT  float64 //	Stack Height (ft) (required)
	STKDIAM float64 //	Stack Diameter (ft) (required)
	STKTEMP float64 //	Stack Gas Exit Temperature (°F) (required)
	STKFLOW float64 //	Stack Gas Flow Rate (ft3/sec) (optional, automatically calculated by Smkinven from velocity and diameter if not given in file)
	STKVEL  float64 //	Stack Gas Exit Velocity (ft/sec) (required)
	SIC     string  //		Standard Industrial Classification Code (recommended)
	MACT    string  // Maximum Available Control Technology Code (6 characters maximum) (optional)
	NAICS   string  //North American Industrial Classification System Code (6 characters maximum) (optional)
	CTYPE   string  //Coordinate system type (1 character maximum) (required)
	//U = Universal Transverse Mercator
	//  L = Latitude/longitude
	XLOC float64 //X location (required)
	//If CTYPE = U, Easting value (meters)
	//  If CTYPE = L, Longitude (decimal degrees)
	YLOC float64 //Y location (required)
	//If CTYPE = U, Northing value (meters)
	//  If CTYPE = L, Latitude (decimal degrees)
	UTMZ int //	UTM zone (required if CTYPE = U)
	//	CAS                string  // Pollutant CAS number or other code (16 characters maximum) (required, this is called the pollutant code in the NIF)
	ANN_EMIS           map[string]*SpecValUnits // Annual Emissions (tons/year) (required)
	CEFF               map[string]float64       //	Control Efficiency percentage (give value of 0-100) (recommended, if left blank, SMOKE default is 0)
	REFF               map[string]float64       //	Rule Effectiveness percentage (give value of 0-100) (recommended, if left blank, SMOKE default is 100)
	RPEN               map[string]float64       //	Rule Penetration percentage (give value of 0-100) (recommended, if left blank, SMOKE default is 100)
	NEI_UNIQUE_ID      string                   //Unique ID that ties together HAP and CAP emissions within a common facility ID and also ties together emissions obtained from muptiple data sources which may have different State facility identifiers but really belong to a single facility (optional, not currently used by SMOKE)
	ORIS_FACILITY_CODE string                   //DOE Plant ID (generally recommended, and required if matching to hour-specific CEM data)
	ORIS_BOILER_ID     string                   //Boiler Identification Code (recommended)
	PointXcoord        float64                  // Projected coordinate for point sources
	PointYcoord        float64                  // Projected coordinate for point sources
	InventoryFreq      string                   // inventory frequency from configuration file
	DoubleCountPols    []string                 // Pols that should not be included in the speciation of this record to avoid double counting
	Country            string
}

type SpecValUnits struct {
	Val     float64
	Units   string
	PolType *PolHolder
	Gridded []*sparse.SparseArray
}

func (c Context) newParsedRecord() (rec *ParsedRecord) {
	rec = new(ParsedRecord)
	rec.ANN_EMIS = make(map[string]*SpecValUnits)
	rec.CEFF = make(map[string]float64)
	rec.REFF = make(map[string]float64)
	rec.RPEN = make(map[string]float64)
	rec.InventoryFreq = c.InventoryFreq
	return
}

// remove commas from csv quotes as necessary
// This is done because otherwise commas within
// quoted strings cause problems.
func cleanRecordORL(record string) string {
	startindex := 0
	endindex := 0
	for {
		index := strings.Index(record[startindex:], "\"")
		if index == -1 {
			break
		}
		startindex += index
		endindex = startindex + strings.Index(record[startindex+1:], "\"") + 2
		if endindex == -1 {
			panic("ERROR: Start quotes don't match end quotes")
		}
		for {
			i := strings.Index(record[startindex:endindex], ",")
			if i == -1 {
				break
			}
			record = record[0:startindex+i] + " " + record[startindex+i+1:]
		}
		startindex = endindex + 1
	}
	return record
}

func (r *ParsedRecord) parseEmisHelper(pol, ann_emis, avd_emis string) string {
	pol = trimString(pol)
	ann := stringToFloat(ann_emis)
	// if annual emissions not present, fill with average day
	if ann <= 0. {
		avd := stringToFloat(avd_emis)
		if avd >= 0. {
			r.ANN_EMIS[pol] = new(SpecValUnits)
			r.ANN_EMIS[pol].Val = avd * 365.
		}
	} else if ann != 0. {
		r.ANN_EMIS[pol] = new(SpecValUnits)
		r.ANN_EMIS[pol].Val = ann
	}
	return pol
}

func (r *ParsedRecord) parsePointLocHelperORL(ctype, xloc, yloc, utmz string,
	c *Context) error {
	ctypeClean := trimString(ctype)
	if ctypeClean != "L" {
		return fmt.Errorf("ctype needs to equal `L'. It is instead `%v'.",
			ctypeClean)
	}
	x, err := strconv.ParseFloat(trimString(xloc), 64)
	if err != nil {
		return fmt.Errorf("Problem parsing latitude.")
	}
	y, err := strconv.ParseFloat(trimString(yloc), 64)
	if err != nil {
		return fmt.Errorf("Problem parsing longitude.")
	}
	if x > 0 && c.ForceWesternHemisphere {
		x = x * -1
	}
	r.CTYPE, r.XLOC, r.YLOC, r.UTMZ = ctypeClean, x, y, stringToInt(utmz)
	return nil
}

func (r *ParsedRecord) parsePointLocHelperIDA(xloc, yloc string, c *Context) error {
	x, err := strconv.ParseFloat(trimString(xloc), 64)
	if err != nil {
		return fmt.Errorf("Problem parsing latitude.")
	}
	y, err := strconv.ParseFloat(trimString(yloc), 64)
	if err != nil {
		return fmt.Errorf("Problem parsing longitude.")
	}
	if x > 0 && c.ForceWesternHemisphere {
		x = x * -1
	}
	r.XLOC, r.YLOC = x, y
	return nil
}

func parseFipsIDA(s string) string {
	return strings.Replace(strings.Trim(s, "\""), " ", "0", -1)
}

func stringToFloat(s string) float64 {
	f, err := strconv.ParseFloat(trimString(s), 64)
	if err != nil {
		return 0.
	} else {
		return f
	}
}
func stringToInt(s string) int {
	i, err := strconv.ParseInt(trimString(s), 0, 32)
	if err != nil {
		return 0.
	} else {
		return int(i)
	}
}
func stringToFloatDefault100(s string) float64 {
	f, err := strconv.ParseFloat(trimString(s), 64)
	if err != nil {
		return 100.
	} else {
		return f
	}
}

func trimString(s string) string {
	return strings.Trim(s, "\" ")
}

// clean up NAICS code so it either has 0 or 6 characters
func parseNAICS(s string) string {
	s2 := trimString(s)
	if s2 == "" || s2 == "-9" {
		return ""
	} else {
		return strings.Replace(fmt.Sprintf("%-6s", s2), " ", "0", -1)
	}
}

// clean up SIC code so it either has 0 or 4 characters
func parseSIC(s string) string {
	s2 := trimString(s)
	if s2 == "" || s2 == "-9" {
		return ""
	} else {
		return strings.Replace(fmt.Sprintf("%-4s", s2), " ", "0", -1)
	}
}

func (c *Context) parseRecordPointORL(record string,
	fInfo *FileInfo) *ParsedRecord {
	fields := c.newParsedRecord()
	record = cleanRecordORL(record)
	splitString := strings.Split(record, ",")
	var err error
	fields.FIPS = trimString(splitString[0])
	fields.PLANTID = trimString(splitString[1])
	fields.POINTID = trimString(splitString[2])
	fields.STACKID = trimString(splitString[3])
	fields.SEGMENT = trimString(splitString[4])
	fields.PLANT = trimString(splitString[5])
	fields.SCC = trimString(splitString[6])
	fields.SRCTYPE = splitString[8]
	fields.STKHGT = stringToFloat(splitString[9])
	fields.STKDIAM = stringToFloat(splitString[10])
	fields.STKTEMP = stringToFloat(splitString[11])
	fields.STKFLOW = stringToFloat(splitString[12])
	fields.STKVEL = stringToFloat(splitString[13])
	fields.SIC = parseSIC(splitString[14])
	fields.MACT = splitString[15]
	fields.NAICS = parseNAICS(splitString[16])
	err = fields.parsePointLocHelperORL(splitString[17], splitString[18],
		splitString[19], splitString[20], c)
	if err != nil {
		panic(fInfo.fname + "\n" + record + "\n" + err.Error())
	}
	pol := fields.parseEmisHelper(splitString[21], splitString[22],
		splitString[23])
	fields.CEFF[pol] = stringToFloat(splitString[24])
	fields.REFF[pol] = stringToFloatDefault100(splitString[25])
	fields.NEI_UNIQUE_ID = splitString[28]
	fields.ORIS_FACILITY_CODE = trimString(splitString[29])
	fields.ORIS_BOILER_ID = trimString(splitString[30])
	return fields
}

func (c *Context) parseRecordAreaORL(record string, fInfo *FileInfo) *ParsedRecord {
	fields := c.newParsedRecord()
	record = cleanRecordORL(record)
	splitString := strings.Split(record, ",")
	fields.FIPS = trimString(splitString[0])
	fields.SCC = trimString(splitString[1])
	fields.SIC = parseSIC(splitString[2])
	fields.MACT = splitString[3]
	fields.SRCTYPE = splitString[4]
	fields.NAICS = parseNAICS(splitString[5])
	pol := fields.parseEmisHelper(splitString[6], splitString[7], splitString[8])
	fields.CEFF[pol] = stringToFloat(splitString[9])
	fields.REFF[pol] = stringToFloatDefault100(splitString[10])
	fields.RPEN[pol] = stringToFloatDefault100(splitString[11])
	return fields
}

func (c *Context) parseRecordNonroadORL(record string,
	fInfo *FileInfo) *ParsedRecord {
	fields := c.newParsedRecord()
	record = cleanRecordORL(record)
	splitString := strings.Split(record, ",")
	fields.FIPS = trimString(splitString[0])
	fields.SCC = trimString(splitString[1])
	pol := fields.parseEmisHelper(splitString[2], splitString[3], splitString[4])
	fields.CEFF[pol] = stringToFloat(splitString[5])
	fields.REFF[pol] = stringToFloatDefault100(splitString[6])
	fields.RPEN[pol] = stringToFloatDefault100(splitString[7])
	fields.SRCTYPE = splitString[8]
	return fields
}

func (c *Context) parseRecordMobileORL(record string,
	fInfo *FileInfo) *ParsedRecord {
	fields := c.newParsedRecord()
	record = cleanRecordORL(record)
	splitString := strings.Split(record, ",")
	fields.FIPS = trimString(splitString[0])
	fields.SCC = trimString(splitString[1])
	fields.parseEmisHelper(splitString[2], splitString[3], splitString[4])
	fields.SRCTYPE = splitString[5]
	return fields
}

func checkRecordLengthIDA(record string, fInfo *FileInfo, start, length int) {
	pols := len(strings.Split(fInfo.polid, " "))
	if len(record) < start+length*pols {
		err := "In the file:\n" + fInfo.fname + "\n"
		err += "Format type: " + fInfo.Ftype + "\n"
		err += "The following IDA record:\n" + record + "\n"
		err += "is not the right length for the these pollutants:\n"
		err += fInfo.polid + "\nRequired length = "
		err += strconv.Itoa(start + length*pols)
		err += "\nRecord length = " + strconv.Itoa(len(record))
		panic(err)
	}
	return
}

func (c *Context) parseRecordPointIDA(record string, fInfo *FileInfo) *ParsedRecord {
	fields := c.newParsedRecord()
	checkRecordLengthIDA(record, fInfo, 249, 52)
	fields.FIPS = parseFipsIDA(record[0:5])
	fields.PLANTID = trimString(record[5:20])
	fields.POINTID = trimString(record[20:35])
	fields.STACKID = trimString(record[35:47])
	fields.ORIS_FACILITY_CODE = trimString(record[47:53])
	fields.ORIS_BOILER_ID = trimString(record[53:59])
	fields.SEGMENT = trimString(record[59:61])
	fields.PLANT = trimString(record[61:101])
	fields.SCC = trimString(record[101:111])
	fields.STKHGT = stringToFloat(record[119:123])
	fields.STKDIAM = stringToFloat(record[123:129])
	fields.STKTEMP = stringToFloat(record[129:133])
	fields.STKFLOW = stringToFloat(record[133:143])
	fields.STKVEL = stringToFloat(record[143:152])
	fields.SIC = parseSIC(record[226:230])
	err := fields.parsePointLocHelperIDA(record[239:248], record[230:239], c)
	if err != nil {
		panic(fInfo.fname + "\n" + record + "\n" + err.Error())
	}
	for i, pol := range strings.Split(fInfo.polid, " ") {
		start := 249 + 52*i
		pol = fields.parseEmisHelper(pol, record[start:start+13],
			record[start+13:start+26])
		fields.CEFF[pol] = stringToFloat(record[start+26 : start+33])
		fields.REFF[pol] = stringToFloatDefault100(record[start+33 : start+40])
	}
	return fields
}

func (c *Context) parseRecordAreaIDA(record string, fInfo *FileInfo) *ParsedRecord {
	fields := c.newParsedRecord()
	checkRecordLengthIDA(record, fInfo, 15, 47)
	fields.FIPS = parseFipsIDA(record[0:5])
	fields.SCC = trimString(record[5:15])
	for i, pol := range strings.Split(fInfo.polid, " ") {
		start := 15 + 47*i
		pol = fields.parseEmisHelper(pol, record[start:start+10],
			record[start+10:start+20])
		fields.CEFF[pol] = stringToFloat(record[start+31 : start+38])
		fields.REFF[pol] = stringToFloatDefault100(record[start+38 : start+41])
		fields.RPEN[pol] = stringToFloatDefault100(record[start+41 : start+47])
	}
	return fields
}
func (c *Context) parseRecordMobileIDA(record string,
	fInfo *FileInfo) *ParsedRecord {
	fields := c.newParsedRecord()
	checkRecordLengthIDA(record, fInfo, 25, 20)
	fields.FIPS = parseFipsIDA(record[0:5])
	fields.SCC = trimString(record[15:25])
	for i, pol := range strings.Split(fInfo.polid, " ") {
		start := 25 + 20*i
		fields.parseEmisHelper(pol, record[start:start+10],
			record[start+10:start+20])
	}
	return fields
}

func (config *Context) Inventory(OutputChan chan *ParsedRecord, period string) {
	defer config.ErrorRecover()
	reportMx.Lock()
	if _, ok := Report.SectorResults[config.Sector]; !ok {
		Report.SectorResults[config.Sector] = make(map[string]*Results)
	}
	if _, ok := Report.SectorResults[config.Sector][period]; !ok {
		Report.SectorResults[config.Sector][period] = new(Results)
	}
	reportMx.Unlock()

	// make a list of species that can possibly be double counted.
	doubleCountablePols := make([]string, 0)
	for pol, polInfo := range config.PolsToKeep {
		if polInfo.SpecNames != nil {
			if !IsStringInArray(doubleCountablePols, pol) {
				doubleCountablePols = append(
					doubleCountablePols, pol)
			}
		}
	}
	recordsThatDoubleCount := make(map[string][]string)

	config.Log("Importing inventory for "+period+" "+
		config.Sector+"...", 1)
	reportMx.Lock()
	Report.SectorResults[config.Sector][period].
		InventoryResults = make(map[string]*FileInfo)
	reportMx.Unlock()
	// First, go through files to check for possible double
	// counting in individual records.
	// Records that double count are defined as those that
	// contain a specific pollutant as well as a pollutant
	// group that contains the specific pollutant as one
	// of its component species.
	for _, file := range config.InvFileNames {
		if config.InventoryFreq == "monthly" {
			file = strings.Replace(file, "[month]", period, -1)
		}
		config.Log("Checking file "+file+" for possible double counting", 1)
		fInfo := config.OpenFile(file)
		recordChan := make(chan *ParsedRecord)
		go fInfo.ParseLines(recordChan, config)
		for record := range recordChan {
			for pol, _ := range record.ANN_EMIS {
				if IsStringInArray(doubleCountablePols, pol) {
					key := record.FIPS + record.SCC + record.PLANTID +
						record.POINTID + record.STACKID +
						record.SEGMENT
					if _, ok := recordsThatDoubleCount[key]; !ok {
						recordsThatDoubleCount[key] = make([]string, 0)
					}
					for _, specName := range config.PolsToKeep[cleanPol(pol)].SpecNames {
						if !IsStringInArray(recordsThatDoubleCount[key], specName) {
							recordsThatDoubleCount[key] = append(
								recordsThatDoubleCount[key], specName)
						}
					}
				}
			}
		}
		fInfo.fid.Close()
	}

	// Now, go through the files a second time, marking records
	// that need to be adjusted for double counting and then
	// sending them off for further processing.
	for _, file := range config.InvFileNames {
		if config.InventoryFreq == "monthly" {
			file = strings.Replace(file, "[month]", period, -1)
		}
		config.Log("Processing file "+file, 1)
		fInfo := config.OpenFile(file)
		recordChan := make(chan *ParsedRecord)
		go fInfo.ParseLines(recordChan, config)
		for record := range recordChan {
			key := record.FIPS + record.SCC + record.PLANTID +
				record.POINTID + record.STACKID +
				record.SEGMENT
			if _, ok := recordsThatDoubleCount[key]; ok {
				record.DoubleCountPols =
					recordsThatDoubleCount[key]
			}
			// add emissions to totals for report
			for pol, emis := range record.ANN_EMIS {
				if _, ok := config.PolsToKeep[cleanPol(pol)]; ok {
					fInfo.Totals[pol] += emis.Val
				} else {
					// delete value if we don't want to keep it
					fInfo.DroppedTotals[pol] += emis.Val
					delete(record.ANN_EMIS, pol)
				}
			}
			// send parsed record to the next processing step
			OutputChan <- record
		}
		fInfo.fid.Close()
	reportMx.Lock()
		Report.SectorResults[config.Sector][period].
			InventoryResults[file] = fInfo
	reportMx.Unlock()
	}
	config.msgchan <- "Finished importing inventory for " + period + " " + config.Sector
	// Close output channel to indicate input is finished.
	close(OutputChan)
}

// Also include EVP__xxx etc. pollutants
func cleanPol(pol string) (cleanedPol string) {
	if strings.Index(pol, "__") != -1 {
		cleanedPol = strings.Split(pol, "__")[1]
	} else {
		cleanedPol = pol
	}
	return
}

func (config *Context) OpenFile(file string) (fInfo *FileInfo) {
	var record string
	var err error
	fInfo = newFileInfo()
	fInfo.fname = file
	fInfo.fid, err = os.Open(file)
	fInfo.Units = config.InputUnits
	fInfo.InputConv = config.InputConv
	if err != nil {
		err = fmt.Errorf("While opening file %v\n Error= %v",
			file, err.Error())
		panic(err)
	}
	fInfo.buf = bufio.NewReader(fInfo.fid)
	record, err = fInfo.buf.ReadString('\n')
	if err != nil {
		panic(fInfo.fname + "\n" + record + "\n" + err.Error())
	}
	if strings.Index(record, "ORL") >= 0 {
		fInfo.Format = "ORL"
		if strings.Index(record, "NONROAD") >= 0 {
			fInfo.Format = "ORLNONROAD"
		}
	} else if strings.Index(record, "IDA") >= 0 {
		fInfo.Format = "IDA"
	} else {
		panic("Unknown file type for: " + file)
	}
	for {
		record, err = fInfo.buf.ReadString('\n')
		if err != nil {
			panic(fInfo.fname + "\n" + record + "\n" + err.Error())
		}
		if strings.Index(record, "NONROAD") >= 0 && fInfo.Format == "ORL" {
			fInfo.Format = "ORLNONROAD"
		}
		if len(record) > 5 && record[1:5] == "TYPE" {
			fInfo.Ftype = strings.Replace(strings.Trim(record[5:], "\n "), ",", " ", -1)
		}
		if len(record) > 8 && record[1:8] == "COUNTRY" {
			fInfo.Country = strings.Trim(record[8:], "\n ")
		}
		if len(record) > 5 && record[1:5] == "YEAR" {
			fInfo.Year = strings.Trim(record[5:], "\n ")
		}
		if len(record) > 6 && record[1:6] == "POLID" {
			fInfo.polid = strings.Trim(record[6:], " \n\r")
		}
		if len(record) > 5 && record[1:5] == "DATA" {
			fInfo.polid = strings.Trim(record[5:], " \n\r")
		}
		var nextChar []byte
		nextChar, err = fInfo.buf.Peek(1)
		if err != nil {
			err = fmt.Errorf("While reading file %v\n Error= %v",
				file, err.Error())
			panic(err)
		}
		if string(nextChar) != "#" {
			break
		}
	}

	// Make sure nonroad files are properly assigned
	if fInfo.Format == "ORL" && strings.Index(fInfo.Ftype, "nonroad") >= 0 {
		fInfo.Format = "ORLNONROAD"
	}
	return
}

func (fInfo *FileInfo) ParseLines(recordChan chan *ParsedRecord, config *Context) {
	numProcs := runtime.GOMAXPROCS(-1)
	lineChan := make(chan string)
	var wg sync.WaitGroup
	wg.Add(numProcs)
	for i := 0; i < numProcs; i++ {
		go func() {
			for line := range lineChan {
				record := new(ParsedRecord)
				switch fInfo.Format + config.SectorType {
				case "ORLpoint":
					record = config.parseRecordPointORL(line, fInfo)
				case "ORLarea":
					record = config.parseRecordAreaORL(line, fInfo)
				case "ORLNONROADarea":
					record = config.parseRecordNonroadORL(line, fInfo)
				case "ORLmobile":
					record = config.parseRecordMobileORL(line, fInfo)
				case "IDApoint":
					record = config.parseRecordPointIDA(line, fInfo)
				case "IDAarea":
					record = config.parseRecordAreaIDA(line, fInfo)
				case "IDAmobile":
					record = config.parseRecordMobileIDA(line, fInfo)
				default:
					panic("Unknown format: " + fInfo.Format + " " +
						config.SectorType)
				}
				// Add zeros to 8 digit SCCs so that all SCCs are 10 digits
				// If SCC is less than 8 digits, add 2 zeros to the front and
				// the rest to the end.
				if len(record.SCC) == 8 {
					record.SCC = "00" + record.SCC
				} else if len(record.SCC) == 7 {
					record.SCC = "00" + record.SCC + "0"
				} else if len(record.SCC) == 6 {
					record.SCC = "00" + record.SCC + "00"
				} else if len(record.SCC) == 5 {
					record.SCC = "00" + record.SCC + "000"
				} else if len(record.SCC) == 4 {
					record.SCC = "00" + record.SCC + "0000"
				} else if len(record.SCC) == 3 {
					record.SCC = "00" + record.SCC + "00000"
				} else if len(record.SCC) == 2 {
					record.SCC = "00" + record.SCC + "000000"
				}

				// set which country this record is for
				record.Country = fInfo.Country
				if record.Country == "US" {
					record.Country = "USA"
				} else if record.Country == "CANADA" {
					record.Country = "CA"
				}
				recordChan <- record
			}
			wg.Done()
		}()
	}
	for {
		line, err := fInfo.buf.ReadString('\n')
		if err != nil {
			if err == io.EOF {
				close(lineChan)
				wg.Wait()
				close(recordChan)
				return
			} else {
				panic(fInfo.fname + "\n" + line + "\n" + err.Error())
			}
		}
		lineChan <- line
	}
	return
}
