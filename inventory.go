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
	"encoding/csv"
	"fmt"
	"io"
	"os"
	"reflect"
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
	polid         []string
	fid           *os.File
	buf           *bufio.Reader
}

func newFileInfo() (f *FileInfo) {
	f = new(FileInfo)
	f.Totals = make(map[string]float64)
	f.DroppedTotals = make(map[string]float64)
	return
}

// The ParsedRecord type is a container for all of the needed
// information in the FF10, ORL, and IDA files.
// See the SMOKE manual for file format specification.
// The struct tags indicate the location of the record in each
// input file type.
type ParsedRecord struct {
	// Five digit FIPS code for state and county (required)
	FIPS string `pointorl:"0",areaorl:"0",nonroadorl:"0",mobileorl:"0",pointida:"0:5",areaida:"0:5",mobileida:"0:5",pointff10:"1"`

	// Plant Identification Code (15 characters maximum) (required,
	// this is the same as the State Facility Identifier in the NIF)
	PLANTID string `pointorl:"1",pointida:"5:20",pointff10:"3"`

	// Point Identification Code (15 characters maximum) (required,
	// this is the same as the Emission Unit ID in the NIF)
	POINTID string `pointorl:"2",pointida:"20:35",pointff10:"4"`

	// Stack Identification Code (15 characters maximum) (recommended,
	// this is the same as the Emissions Release Point ID in the NIF)
	STACKID string `pointorl:"3",pointida:"35:47",pointff10:"5"`

	// DOE Plant ID (15 characters maximum) (recommended, this is the
	// same as the Process ID in the NIF)
	SEGMENT string `pointorl:"4",pointida:"59:61",pointff10:"6"`

	// Plant Name (40 characters maximum) (recommended)
	PLANT string `pointorl:"5",pointida:"61:101",pointff10:"15"`

	// Ten character Source Classification Code (required)
	SCC string `pointorl:"6",areaorl:"1",nonroadorl:"1",mobileorl:"1",pointida:"101:111",areaida:"5:15",mobileida:"15:25",pointff10:"11"`

	// Source type (2 characters maximum), used by SMOKE in determining
	// applicable MACT-based controls (required)
	// 	01 = major
	// 	02 = Section 12 area source
	// 	03 = nonroad
	// 	04 = onroad
	SRCTYPE string `pointorl:"8",areaorl:"4",nonroadorl:"8",mobileorl:"5",pointff10:"16"`

	// Stack Height (ft) (required)
	STKHGT float64 `pointorl:"9",pointida:"119:123",pointff10:"17"`

	// Stack Diameter (ft) (required)
	STKDIAM float64 `pointorl:"10",pointida:"123:129",pointff10:"18"`

	// Stack Gas Exit Temperature (°F) (required)
	STKTEMP float64 `pointorl:"11",pointida:"129:133",pointff10:"19"`

	// Stack Gas Flow Rate (ft3/sec) (optional)
	STKFLOW float64 `pointorl:"12",pointida:"133:143",pointff10:"20"`

	// Stack Gas Exit Velocity (ft/sec) (required)
	STKVEL float64 `pointorl:"13",pointida:"143:152",pointff10:"21"`

	// Standard Industrial Classification Code (recommended)
	SIC string `pointorl:"14",areaorl:"2",pointida:"226:230"`

	// Maximum Available Control Technology Code
	// (6 characters maximum) (optional)
	MACT string `pointorl:"15",areaorl:"3"`

	// North American Industrial Classification System Code
	// (6 characters maximum) (optional)
	NAICS string `pointorl:"16",areaorl:"5",pointff10:"22"`

	// Coordinate system type (1 character maximum) (required)
	// U = Universal Transverse Mercator
	// L = Latitude/longitude
	CTYPE string `pointorl:"17",default:"L"`

	// X location (required)
	// If CTYPE = U, Easting value (meters)
	// If CTYPE = L, Longitude (decimal degrees)
	XLOC float64 `pointorl:"18",pointida:"239:248",pointff10:"23"`

	// Y location (required)
	// If CTYPE = U, Northing value (meters)
	// If CTYPE = L, Latitude (decimal degrees)
	YLOC float64 `pointorl:"19",pointida:"230:239",pointff10:"24"`

	//	UTM zone (required if CTYPE = U)
	UTMZ int `pointorl:"20"`

	// Annual Emissions (tons/year) (required)
	// Emissions values must be positive because numbers are used
	// to represent missing data.
	// In the struct tags, there are three numbers. for ORL records,
	// the first number is
	// the pollutant location, the second number is the annual emissions
	// location, and the third number is the average day emissions location.
	// For IDA records, the first number is the start of the first pollutant,
	// and the second two numbers are offsets for the ends of the annual and
	// average day emissions fields.
	// For FF10 record, the first number is the location of the pollutant and
	// the second number (followed by "...") is the location of January emissions.
	ANN_EMIS map[Period]map[string]*SpecValUnits `pointorl:"21,22,23",areaorl:"6,7,8",nonroadorl:"2,3,4",mobileorl:"2,3,4",pointida:"249:13:26",areaida:"15:10:20",mobileida:"25:10:20",pointff10:"12,52..."`

	// Control efficiency percentage (give value of 0-100) (recommended,
	// if left blank, SMOKE default is 0).
	// This can have different values for different pollutants.
	CEFF map[string]float64 `pointorl:"24",areaorl:"9",nonroadorl:"5",pointida:"249;26:33",areaida:"15:31:38",mobileida:"25:20:20"`

	// Rule Effectiveness percentage (give value of 0-100) (recommended,
	// if left blank, SMOKE default is 100)
	// This can have different values for different pollutants.
	REFF map[string]float64 `pointorl:"25",areaorl:"10",nonroadorl:"6",pointida:"249:26:33",areaida:"15:38:41",mobileida:"25:20:20",default:"100"`

	// Rule Penetration percentage (give value of 0-100) (recommended,
	// if left blank, SMOKE default is 100)
	// This can have different values for different pollutants.
	RPEN map[string]float64 `areaorl:"11",nonroadorl:"7",pointida:"249:33:40",areaida:"14:41:47",mobileida:"25:20:20",default:"100"`

	//DOE Plant ID (generally recommended, and required if matching
	// to hour-specific CEM data)
	ORIS_FACILITY_CODE string `pointorl:"29",pointida:"47:53",pointff10:"41"`

	// Boiler Identification Code (recommended)
	ORIS_BOILER_ID string `pointorl:"30",pointida:"53:59",pointff10:"42"`

	PointXcoord float64 // Projected coordinate for point sources
	PointYcoord float64 // Projected coordinate for point sources

	// Pols that should not be included in the speciation of this record
	// to avoid double counting
	DoubleCountPols []string

	// The country that this record applies to.
	Country Country

	// Surrogate to apply emissions to grid cells
	gridSrg []*sparse.SparseArray
}

type SpecValUnits struct {
	Val     float64
	Units   string
	PolType *PolHolder
}

func (c Context) newParsedRecord(p Period) (rec *ParsedRecord) {
	rec = new(ParsedRecord)
	rec.ANN_EMIS = make(map[Period]map[string]*SpecValUnits)
	rec.ANN_EMIS[p] = make(map[string]*SpecValUnits)
	rec.CEFF = make(map[string]float64)
	rec.REFF = make(map[string]float64)
	rec.RPEN = make(map[string]float64)
	return
}

func (r *ParsedRecord) parseEmisHelper(p Period, pol string, ann, avd float64) {
	// if annual emissions not present, fill with average day
	if ann <= 0. {
		if avd >= 0. {
			r.ANN_EMIS[p][pol] = new(SpecValUnits)
			r.ANN_EMIS[p][pol].Val = avd * 365.
		}
	} else if ann != 0. {
		r.ANN_EMIS[p][pol] = new(SpecValUnits)
		r.ANN_EMIS[p][pol].Val = ann
	}
}

func (r *ParsedRecord) setupPointLoc(c *Context) error {
	if r.CTYPE != "L" {
		return fmt.Errorf("ctype needs to equal `L'. It is instead `%v'.",
			r.CTYPE)
	}
	if r.XLOC > 0 && c.ForceWesternHemisphere {
		r.XLOC *= -1
	}
	return nil
}

// Get rid of extra quotation marks, replace spaces with
// zeros.
func (r *ParsedRecord) parseFIPS() {
	r.FIPS = strings.Replace(strings.Trim(r.FIPS, "\""), " ", "0", -1)
}

func stringToFloat(s string) float64 {
	f, err := strconv.ParseFloat(s, 64) // string should already be trimmed
	if err != nil {
		return 0.
	} else {
		return f
	}
}
func stringToInt(s string) int {
	i, err := strconv.ParseInt(s, 0, 32) // string should already be trimmed
	if err != nil {
		return 0.
	} else {
		return int(i)
	}
}

// Get rid of extra quotation marks and copy the string so the
// whole line from the input file isn't held in memory
func trimString(s string) string {
	return string([]byte(strings.Trim(s, "\" ")))
}

// clean up NAICS code so it either has 0 or 6 characters
func (r *ParsedRecord) parseNAICS() {
	r.NAICS = trimString(r.NAICS)
	if r.NAICS == "" || r.NAICS == "-9" {
		r.NAICS = ""
	} else {
		r.NAICS = strings.Replace(fmt.Sprintf("%-6s", r.NAICS), " ", "0", -1)
	}
}

// clean up SIC code so it either has 0 or 4 characters
func (r *ParsedRecord) parseSIC() {
	r.SIC = trimString(r.SIC)
	if r.SIC == "" || r.SIC == "-9" {
		r.SIC = ""
	} else {
		r.SIC = strings.Replace(fmt.Sprintf("%-4s", r.SIC), " ", "0", -1)
	}
}

func (r *ParsedRecord) setup(c *Context) error {
	r.parseSIC()
	r.parseNAICS()
	r.parseFIPS()
	err := r.setupPointLoc(c)
	return err
}

func (c *Context) parseRecord(ftype string, line []string, fInfo *FileInfo,
	p Period) *ParsedRecord {

	r := c.newParsedRecord(p)
	v := reflect.Indirect(reflect.ValueOf(r))
	t := v.Type()
	var pol string
	for i := 0; i < v.NumField(); i++ {
		fieldType := t.Field(i)
		fieldVal := v.Field(i)
		pol = r.setVal(ftype, fInfo, fieldType, fieldVal, line, pol, p)
	}
	err := r.setup(c)
	handleInventoryError(err, fInfo, line)
	return r
}

func handleInventoryError(err error, fInfo *FileInfo, line []string) {
	if err != nil {
		panic(fmt.Sprintf("In file:\n%s\nthere was an error with the "+
			"following record:\n%v\nThe error message was:\n%v",
			fInfo.fname, line, err.Error()))
	}
}

func (r *ParsedRecord) setVal(fileType string, fInfo *FileInfo,
	fieldType reflect.StructField,
	fieldVal reflect.Value, line []string, pol string, p Period) string {

	switch fieldType.Type.Kind() {
	case reflect.Map:
		switch fieldType.Type.Key().Kind() { // what is the type of the map key?
		case reflect.Int: // annual emissions
			pol = r.setEmis(fileType, fInfo, fieldType, line, p)
			return pol
		case reflect.String: // control penetration or efficiency
			setMap(fileType, fInfo, fieldType, fieldVal, line, pol)
			return pol
		default:
			panic("wrong type of map")
		}
	}

	loc := fieldType.Tag.Get(fileType)
	if loc == "" {
		return pol
	}
	defaultVal := fieldType.Tag.Get("default")
	valStr := getValStr(loc, defaultVal, line)
	switch fieldType.Type.Kind() {
	case reflect.Float64:
		fieldVal.SetFloat(stringToFloat(valStr))
	case reflect.Int:
		fieldVal.SetInt(int64(stringToInt(valStr)))
	case reflect.String:
		fieldVal.SetString(valStr)
	default:
		panic("Type can only be string, int, float64")
	}
	return pol
}

func getValStr(loc, defaultVal string, line []string) string {
	var valStr string
	if strings.Contains(loc, ":") {
		// Width delimited file
		endpoints := strings.Split(loc, ":")
		e1 := stringToInt(endpoints[0])
		e2 := stringToInt(endpoints[1])
		valStr = line[0][e1:e2]
	} else {
		valStr = line[stringToInt(loc)]
	}
	valStr = trimString(valStr)
	if valStr == "" {
		// If field is empty, set it to the default if there is one.
		valStr = defaultVal
	}
	return valStr
}

func (r *ParsedRecord) setEmis(fileType string, fInfo *FileInfo,
	fieldType reflect.StructField,
	line []string, p Period) (pol string) {

	loc := fieldType.Tag.Get(fileType)
	if loc == "" {
		panic("loc tag not set for emissions")
	}
	if strings.Contains(loc, ",") &&
		!strings.Contains(loc, "...") { // ORL format
		fields := strings.Split(loc, ",")
		pol = getValStr(fields[0], "", line)
		ann := stringToFloat(getValStr(fields[1], "-9", line))
		avd := stringToFloat(getValStr(fields[2], "-9", line))
		r.parseEmisHelper(p, pol, ann, avd)
		return
	} else if strings.Contains(loc, ":") { // IDA format
		endpoints := strings.Split(loc, ":")
		for i, pol := range fInfo.polid {
			off2 := stringToInt(endpoints[2])
			off1 := stringToInt(endpoints[1])
			start := stringToInt(endpoints[0]) + off2*i
			ann := stringToFloat(line[0][start : start+off1])
			avd := stringToFloat(line[0][start+off1 : start+off2])
			r.parseEmisHelper(p, pol, ann, avd)
		}
		return
	} else if strings.Contains(loc, ",") &&
		strings.Contains(loc, "...") { // FF10 format
		fields := strings.Split(loc, ",")
		pol = getValStr(fields[0], "", line)
		emisStr := trimString(line[stringToInt(fields[1])+int(p)-1])
		ann := stringToFloat(emisStr)
		const avd = -9. // FF10 doesn't have average day emissions
		r.parseEmisHelper(p, pol, ann, avd)
		return
	} else {
		panic("Invalid loc format")
	}
}

func setMap(fileType string, fInfo *FileInfo, fieldType reflect.StructField,
	fieldVal reflect.Value, line []string, pol string) {

	loc := fieldType.Tag.Get(fileType)
	if loc == "" {
		return
	}
	defaultVal := fieldType.Tag.Get("default")
	if strings.Contains(loc, ":") {
		endpoints := strings.Split(loc, ":")
		for i, pol := range fInfo.polid {
			off1 := stringToInt(endpoints[1])
			off2 := stringToInt(endpoints[2])
			start := stringToInt(endpoints[0]) + off2*i
			loc2 := fmt.Sprintf("%d:%d", start+off1,
				start+off2)
			valStr := getValStr(loc2, defaultVal, line)
			fieldVal.SetMapIndex(reflect.ValueOf(pol),
				reflect.ValueOf(stringToFloat(valStr)))
		}
		return
	}
	valStr := getValStr(loc, defaultVal, line)
	// Only set up to work for float values.
	fieldVal.SetMapIndex(reflect.ValueOf(pol),
		reflect.ValueOf(stringToFloat(valStr)))
}

func checkRecordLengthIDA(record string, fInfo *FileInfo, start, length int) {
	pols := len(fInfo.polid)
	if len(record) < start+length*pols {
		err := "In the file:\n" + fInfo.fname + "\n"
		err += "Format type: " + fInfo.Ftype + "\n"
		err += "The following IDA record:\n" + record + "\n"
		err += "is not the right length for the these pollutants:\n"
		err += strings.Join(fInfo.polid, ",") + "\nRequired length = "
		err += strconv.Itoa(start + length*pols)
		err += "\nRecord length = " + strconv.Itoa(len(record))
		panic(err)
	}
	return
}

func (config *Context) Inventory(OutputChan chan *ParsedRecord) {
	defer config.ErrorRecover()

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
	records := make(map[string]*ParsedRecord)

	config.Log("Importing inventory for "+config.Sector+"...", 1)

	for _, p := range config.runPeriods {
		reportMx.Lock()
		if _, ok := Report.SectorResults[config.Sector]; !ok {
			Report.SectorResults[config.Sector] = make(map[string]*Results)
		}
		if _, ok := Report.SectorResults[config.Sector][p.String()]; !ok {
			Report.SectorResults[config.Sector][p.String()] = new(Results)
		}
		Report.SectorResults[config.Sector][p.String()].
			InventoryResults = make(map[string]*FileInfo)
		reportMx.Unlock()
		for _, file := range config.InvFileNames {
			if config.InventoryFreq == "monthly" {
				file = strings.Replace(file, "[month]", p.String(), -1)
			}
			config.Log("Processing file "+file, 1)
			fInfo := config.OpenFile(file)
			recordChan := make(chan *ParsedRecord)
			go fInfo.ParseLines(recordChan, config, p)
			for record := range recordChan {
				// add emissions to totals for report
				for pol, emis := range record.ANN_EMIS[p] {
					if _, ok := config.PolsToKeep[cleanPol(pol)]; ok {
						fInfo.Totals[pol] += emis.Val
					} else {
						// delete value if we don't want to keep it
						fInfo.DroppedTotals[pol] += emis.Val
						delete(record.ANN_EMIS[p], pol)
					}
				}

				key := record.FIPS + record.SCC + record.PLANTID +
					record.POINTID + record.STACKID +
					record.SEGMENT + record.ORIS_FACILITY_CODE +
					record.ORIS_BOILER_ID
				var currentRec *ParsedRecord
				if _, ok := records[key]; !ok {
					// We don't yet have a record for this key
					records[key] = record
					currentRec = record
				} else {
					// There is already a record for this key
					currentRec = records[key]
					for pol, e := range record.ANN_EMIS[p] {
						if _, ok := currentRec.ANN_EMIS[p][pol]; !ok {
							if _, ok := currentRec.ANN_EMIS[p]; !ok {
								currentRec.ANN_EMIS[p] =
									make(map[string]*SpecValUnits)
							}
							// We don't yet have a value for this pol
							currentRec.ANN_EMIS[p][pol] = e
						} else {
							// There is already a value for this pol
							currentRec.ANN_EMIS[p][pol].Val += e.Val
							if currentRec.ANN_EMIS[p][pol].Units != e.Units {
								panic(fmt.Sprintf("Units don't match: %v != %v",
									currentRec.ANN_EMIS[p][pol].Units, e.Units))
							}
						}
					}
				}
				// Check for possible double counting in individual records.
				// Records that double count are defined as those that
				// contain a specific pollutant as well as a pollutant
				// group that contains the specific pollutant as one
				// of its component species.
				for pol, _ := range currentRec.ANN_EMIS[p] {
					if IsStringInArray(doubleCountablePols, pol) {
						if currentRec.DoubleCountPols == nil {
							currentRec.DoubleCountPols = make([]string, 0)
						}
						for _, specName := range config.PolsToKeep[cleanPol(pol)].SpecNames {
							if !IsStringInArray(currentRec.DoubleCountPols, specName) {
								currentRec.DoubleCountPols = append(
									currentRec.DoubleCountPols, specName)
							}
						}
					}
				}
			}
			fInfo.fid.Close()
			reportMx.Lock()
			Report.SectorResults[config.Sector][p.String()].
				InventoryResults[file] = fInfo
			reportMx.Unlock()
		}
	}

	// Now send off the records for further processing
	for key, record := range records {
		OutputChan <- record
		delete(records, key)
	}
	config.msgchan <- "Finished importing inventory for " + config.Sector
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
			fInfo.polid = strings.Split(strings.Trim(record[6:], " \n\r"), " ")
		}
		if len(record) > 5 && record[1:5] == "DATA" {
			fInfo.polid = strings.Split(strings.Trim(record[5:], " \n\r"), "  ")
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
	fInfo.fid.Seek(0, 0)

	// Make sure nonroad files are properly assigned
	if fInfo.Format == "ORL" && strings.Index(fInfo.Ftype, "nonroad") >= 0 {
		fInfo.Format = "ORLNONROAD"
	}
	return
}

func (fInfo *FileInfo) ParseLines(recordChan chan *ParsedRecord,
	config *Context, p Period) {
	numProcs := runtime.GOMAXPROCS(-1)
	lineChan := make(chan []string)
	var wg sync.WaitGroup
	wg.Add(numProcs)
	for i := 0; i < numProcs; i++ {
		go func() {
			var line []string
			for line = range lineChan {
				record := new(ParsedRecord)
				switch fInfo.Format + config.SectorType {
				case "FF10_POINTpoint":
					record = config.parseRecord("pointff10", line, fInfo, p)
				case "ORLpoint":
					record = config.parseRecord("pointorl", line, fInfo, p)
				case "ORLarea":
					record = config.parseRecord("areaorl", line, fInfo, p)
				case "ORLNONROADarea":
					record = config.parseRecord("nonroadorl", line, fInfo, p)
				case "ORLmobile":
					record = config.parseRecord("mobileorl", line, fInfo, p)
				case "IDApoint":
					record = config.parseRecord("pointida", line, fInfo, p)
				case "IDAarea":
					record = config.parseRecord("areaida", line, fInfo, p)
				case "IDAmobile":
					record = config.parseRecord("mobileida", line, fInfo, p)
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
				record.Country = getCountryFromName(fInfo.Country)
				recordChan <- record
			}
			wg.Done()
		}()
	}
	r := csv.NewReader(fInfo.fid)
	r.Comment = '#'
	for {
		line, err := r.Read()
		if err != nil {
			if err == io.EOF {
				close(lineChan)
				wg.Wait()
				close(recordChan)
				return
			} else {
				panic(fInfo.fname + "\n" + strings.Join(line, ",") + "\n" +
					err.Error())
			}
		}
		lineChan <- line
	}
	return
}
