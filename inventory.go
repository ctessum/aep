package main

import (
	"os"
	"strings"
	"fmt"
	"bufio"
	"strconv"
	"path/filepath"
	"sort"
)

// type FileInfo holds information about each inventory file
type FileInfo struct {
	fname   string
	format  string
	ftype   string
	year    string
	country string
	totals  map[string]float64
	polid   string
}

func (c *RunData) inventoryReport(inventoryInfo map[string]FileInfo, period string) {
	var perm os.FileMode
	perm = 0776
	err := os.MkdirAll(c.sectorLogs, perm)
	if err != nil {
		panic(err)
	}
	repStr := ""
	if c.InvRep != nil {
		repStr += "\n\n"
	} else {
		c.InvRep, err = os.Create(filepath.Join(c.sectorLogs, c.sector+"_inventory.csv"))
		if err != nil {
			panic(err)
		}
	}
	var polNamesTemp string
	for _, info := range inventoryInfo {
		for pol, _ := range info.totals {
			if strings.Index(polNamesTemp, pol) == -1 {
				polNamesTemp += " " + pol
			}
		}
	}
	polNames := strings.Split(polNamesTemp, " ")
	sort.Strings(polNames)

	repStr += "Inventory totals by file: "+c.inputUnits+"\n"
	repStr += fmt.Sprintf("Sector: %s\n", c.sector)
	repStr += fmt.Sprintf("Time period: %s\n", period)
	repStr += "file,format,file type,year,country,"
	for _, pol := range polNames {
		repStr += fmt.Sprintf("%s,", pol)
	}
	repStr += "\n"
	for file, info := range inventoryInfo {
		repStr += fmt.Sprintf("%s,%s,%s,%s,%s,", file, info.format, info.ftype, info.year, info.country)
		for _, pol := range polNames {
			repStr += fmt.Sprintf("%e,", info.totals[pol])
		}
		repStr += "\n"
	}
	fmt.Fprint(c.InvRep, repStr)
	return
}

func newFileInfo() (f FileInfo) {
	f.totals = make(map[string]float64)
	return
}

// The ParsedRecord type is a container for all of the needed information in the ORL or IDA files.
// See the SMOKE manual for file format specification.
type ParsedRecord struct {
	FIPS    int    //	Five digit FIPS code for state and county (required)
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
	SIC     int     //		Standard Industrial Classification Code (recommended)
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
	ANN_EMIS           map[string]float64 // Annual Emissions (tons/year) (required)
	AVD_EMIS           map[string]float64 //	Average-day Emissions (tons/average day) (optional)
	CEFF               map[string]float64 //	Control Efficiency percentage (give value of 0-100) (recommended, if left blank, SMOKE default is 0)
	REFF               map[string]float64 //	Rule Effectiveness percentage (give value of 0-100) (recommended, if left blank, SMOKE default is 100)
	RPEN               map[string]float64 //	Rule Penetration percentage (give value of 0-100) (recommended, if left blank, SMOKE default is 100)
	NEI_UNIQUE_ID      string             //Unique ID that ties together HAP and CAP emissions within a common facility ID and also ties together emissions obtained from muptiple data sources which may have different State facility identifiers but really belong to a single facility (optional, not currently used by SMOKE)
	ORIS_FACILITY_CODE string             //DOE Plant ID (generally recommended, and required if matching to hour-specific CEM data)
	ORIS_BOILER_ID     string             //Boiler Identification Code (recommended)
	SpecAnnEmis        map[string]specValUnits
}

func newParsedRecord() (rec ParsedRecord) {
	rec.ANN_EMIS = make(map[string]float64)
	rec.SpecAnnEmis = make(map[string]specValUnits)
	rec.AVD_EMIS = make(map[string]float64)
	rec.CEFF = make(map[string]float64)
	rec.REFF = make(map[string]float64)
	rec.RPEN = make(map[string]float64)
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

func parseRecordPointORL(record string, fInfo FileInfo) ParsedRecord {
	fields := newParsedRecord()
	record = cleanRecordORL(record)
	splitString := strings.Split(record, ",")
	var err error
	fields.FIPS, err = strconv.Atoi(strings.Trim(splitString[0], "\""))
	if err != nil {
		panic(fInfo.fname + "\n" + record + "\n" + err.Error())
	}
	fields.PLANTID = splitString[1]
	fields.POINTID = splitString[2]
	fields.STACKID = splitString[3]
	fields.SEGMENT = splitString[4]
	fields.PLANT = splitString[5]
	fields.SCC = strings.Trim(splitString[6],"\"")
	fields.SRCTYPE = splitString[8]
	fields.STKHGT, err = strconv.ParseFloat(strings.Trim(splitString[9], "\""),64)
	if err != nil {
		fields.STKHGT = 0.
	}
	fields.STKDIAM, err = strconv.ParseFloat(strings.Trim(splitString[10], "\""),64)
	if err != nil {
		fields.STKDIAM = 0.
	}
	fields.STKTEMP, err = strconv.ParseFloat(strings.Trim(splitString[11], "\""),64)
	if err != nil {
		fields.STKTEMP = 0.
	}
	fields.STKFLOW, err = strconv.ParseFloat(strings.Trim(splitString[12], "\""),64)
	if err != nil {
		fields.STKFLOW = 0.
	}
	fields.STKVEL, err = strconv.ParseFloat(strings.Trim(splitString[13], "\""),64)
	if err != nil {
		fields.STKVEL = 0.
	}
	fields.SIC, err = strconv.Atoi(strings.Trim(splitString[14], "\""))
	if err != nil {
		fields.SIC = -9
	}
	fields.MACT = splitString[15]
	fields.NAICS = splitString[16]
	fields.CTYPE = splitString[17]
	fields.XLOC, err = strconv.ParseFloat(strings.Trim(splitString[18], "\""),64)
	if err != nil {
		panic(fInfo.fname + "\n" + record + "\n" + err.Error())
	}
	fields.YLOC, err = strconv.ParseFloat(strings.Trim(splitString[19], "\""),64)
	if err != nil {
		panic(fInfo.fname + "\n" + record + "\n" + err.Error())
	}
	fields.UTMZ, err = strconv.Atoi(strings.Trim(splitString[20], "\""))
	if err != nil {
		fields.UTMZ = 0.
	}
	pol := strings.Trim(splitString[21], "\" ")
	fields.AVD_EMIS[pol], err = strconv.ParseFloat(strings.Trim(splitString[23], "\""),64)
	if err != nil {
		fields.AVD_EMIS[pol] = 0.
	}
	fields.ANN_EMIS[pol], err = strconv.ParseFloat(strings.Trim(splitString[22], "\""),64)
	if err != nil {
		fields.ANN_EMIS[pol] = fields.AVD_EMIS[pol] * 365.
	}
	fields.CEFF[pol], err = strconv.ParseFloat(strings.Trim(splitString[24], "\""),64)
	if err != nil {
		fields.CEFF[pol] = 0.
	}
	fields.REFF[pol], err = strconv.ParseFloat(strings.Trim(splitString[25], "\""),64)
	if err != nil {
		fields.REFF[pol] = 100.
	}
	fields.NEI_UNIQUE_ID = splitString[28]
	fields.ORIS_FACILITY_CODE = splitString[29]
	fields.ORIS_BOILER_ID = splitString[30]
	return fields
}

func parseRecordAreaORL(record string, fInfo FileInfo) ParsedRecord {
	fields := newParsedRecord()
	record = cleanRecordORL(record)
	splitString := strings.Split(record, ",")
	var err error
	fields.FIPS, err = strconv.Atoi(strings.Trim(splitString[0], "\""))
	if err != nil {
		panic(fInfo.fname + "\n" + record + "\n" + err.Error())
	}
	fields.SCC = strings.Trim(splitString[1],"\"")
	fields.SIC, err = strconv.Atoi(strings.Trim(splitString[2], "\""))
	if err != nil {
		fields.SIC = -9
	}
	fields.MACT = splitString[3]
	fields.SRCTYPE = splitString[4]
	fields.NAICS = splitString[5]
	pol := strings.Trim(splitString[6], "\" ")
	fields.AVD_EMIS[pol], err = strconv.ParseFloat(strings.Trim(splitString[8], "\""),64)
	if err != nil {
		fields.AVD_EMIS[pol] = 0.
	}
	fields.ANN_EMIS[pol], err = strconv.ParseFloat(strings.Trim(splitString[7], "\""),64)
	if err != nil {
		fields.ANN_EMIS[pol] = fields.AVD_EMIS[pol] * 365.
	}
	fields.CEFF[pol], err = strconv.ParseFloat(strings.Trim(splitString[9], "\""),64)
	if err != nil {
		fields.CEFF[pol] = 0.
	}
	fields.REFF[pol], err = strconv.ParseFloat(strings.Trim(splitString[10], "\""),64)
	if err != nil {
		fields.REFF[pol] = 100.
	}
	fields.RPEN[pol], err = strconv.ParseFloat(strings.Trim(splitString[11], "\""),64)
	if err != nil {
		fields.RPEN[pol] = 100.
	}
	return fields
}

func parseRecordNonroadORL(record string, fInfo FileInfo) ParsedRecord {
	fields := newParsedRecord()
	record = cleanRecordORL(record)
	splitString := strings.Split(record, ",")
	var err error
	fields.FIPS, err = strconv.Atoi(strings.Trim(splitString[0], "\""))
	if err != nil {
		panic(fInfo.fname + "\n" + record + "\n" + err.Error())
	}
	fields.SCC = strings.Trim(splitString[1],"\"")
	pol := strings.Trim(splitString[2], "\" ")
	fields.AVD_EMIS[pol], err = strconv.ParseFloat(strings.Trim(splitString[4], "\""),64)
	if err != nil {
		fields.AVD_EMIS[pol] = 0.
	}
	fields.ANN_EMIS[pol], err = strconv.ParseFloat(strings.Trim(splitString[3], "\""),64)
	if err != nil {
		fields.ANN_EMIS[pol] = fields.AVD_EMIS[pol] * 365.
	}
	fields.CEFF[pol], err = strconv.ParseFloat(strings.Trim(splitString[5], "\""),64)
	if err != nil {
		fields.CEFF[pol] = 0.
	}
	fields.REFF[pol], err = strconv.ParseFloat(strings.Trim(splitString[6], "\""),64)
	if err != nil {
		fields.REFF[pol] = 100.
	}
	fields.RPEN[pol], err = strconv.ParseFloat(strings.Trim(splitString[7], "\""),64)
	if err != nil {
		fields.RPEN[pol] = 100.
	}
	fields.SRCTYPE = splitString[8]

	return fields
}

func parseRecordMobileORL(record string, fInfo FileInfo) ParsedRecord {
	fields := newParsedRecord()
	record = cleanRecordORL(record)
	splitString := strings.Split(record, ",")
	var err error
	fields.FIPS, err = strconv.Atoi(strings.Trim(splitString[0], "\""))
	if err != nil {
		panic(fInfo.fname + "\n" + record + "\n" + err.Error())
	}
	fields.SCC = strings.Trim(splitString[1],"\"")
	pol := strings.Trim(splitString[2], "\" ")
	fields.AVD_EMIS[pol], err = strconv.ParseFloat(strings.Trim(splitString[4], "\""),64)
	if err != nil {
		fields.AVD_EMIS[pol] = 0.
	}
	fields.ANN_EMIS[pol], err = strconv.ParseFloat(strings.Trim(splitString[3], "\""),64)
	if err != nil {
		fields.ANN_EMIS[pol] = fields.AVD_EMIS[pol] * 365.
	}
	fields.SRCTYPE = splitString[5]

	return fields
}

func checkRecordLengthIDA(record string, fInfo FileInfo, start, length int) {
	pols := len(strings.Split(fInfo.polid, " "))
	if len(record) < start+length*pols {
		err := "In the file:\n" + fInfo.fname + "\n"
		err += "Format type: " + fInfo.ftype + "\n"
		err += "The following IDA record:\n" + record + "\n"
		err += "is not the right length for the these pollutants:\n"
		err += fInfo.polid + "\nRequired length = "
		err += strconv.Itoa(start + length*pols)
		err += "\nRecord length = " + strconv.Itoa(len(record))
		panic(err)
	}
	return
}

func parseRecordPointIDA(record string, fInfo FileInfo) ParsedRecord {
	fields := newParsedRecord()
	checkRecordLengthIDA(record, fInfo, 249, 52)
	var err error
	fields.FIPS, err = strconv.Atoi(strings.Replace(strings.Trim(record[0:5], "\""), " ", "0", -1))
	if err != nil {
		panic(fInfo.fname + "\n" + record + "\n" + err.Error())
	}
	fields.PLANTID = record[5:20]
	fields.POINTID = record[20:35]
	fields.STACKID = record[35:47]
	fields.ORIS_FACILITY_CODE = record[47:53]
	fields.ORIS_BOILER_ID = record[53:59]
	fields.SEGMENT = record[59:61]
	fields.PLANT = record[61:101]
	fields.SCC = strings.Trim(record[101:111],"\"")
	fields.STKHGT, err = strconv.ParseFloat(strings.Trim(record[119:123], " "),64)
	if err != nil {
		fields.STKHGT = 0.
	}
	fields.STKDIAM, err = strconv.ParseFloat(strings.Trim(record[123:129], " "),64)
	if err != nil {
		fields.STKDIAM = 0.
	}
	fields.STKTEMP, err = strconv.ParseFloat(strings.Trim(record[129:133], " "),64)
	if err != nil {
		fields.STKTEMP = 0.
	}
	fields.STKFLOW, err = strconv.ParseFloat(strings.Trim(record[133:143], " "),64)
	if err != nil {
		fields.STKFLOW = 0.
	}
	fields.STKVEL, err = strconv.ParseFloat(strings.Trim(record[143:152], " "),64)
	if err != nil {
		fields.STKVEL = 0.
	}
	fields.SIC, err = strconv.Atoi(strings.Trim(record[226:230], " "))
	if err != nil {
		fields.SIC = -9
	}
	fields.YLOC, err = strconv.ParseFloat(strings.Trim(record[230:239], " "),64)
	if err != nil {
		panic(fInfo.fname + "\n" + record + "\n" + err.Error())
	}
	fields.XLOC, err = strconv.ParseFloat(strings.Trim(record[239:248], " "),64)
	if err != nil {
		panic(fInfo.fname + "\n" + record + "\n" + err.Error())
	}
	for i, pol := range strings.Split(fInfo.polid, " ") {
		start := 249 + 52*i
		fields.AVD_EMIS[strings.Trim(pol, " ")], err = strconv.ParseFloat(strings.Trim(record[start+13:start+13+13], " "),64)
		if err != nil {
			fields.AVD_EMIS[strings.Trim(pol, " ")] = 0.
		}
		fields.ANN_EMIS[strings.Trim(pol, " ")], err = strconv.ParseFloat(strings.Trim(record[start:start+13], " "),64)
		if err != nil {
			fields.ANN_EMIS[strings.Trim(pol, " ")] = fields.AVD_EMIS[strings.Trim(pol, " ")] * 365.
		}
		fields.CEFF[strings.Trim(pol, " ")], err = strconv.ParseFloat(strings.Trim(record[start+13+13:start+13+13+7], " "),64)
		if err != nil {
			fields.CEFF[strings.Trim(pol, " ")] = 0.
		}
		fields.REFF[strings.Trim(pol, " ")], err = strconv.ParseFloat(strings.Trim(record[start+13+13+7:start+13+13+7+7], " "),64)
		if err != nil {
			fields.REFF[strings.Trim(pol, " ")] = 100.
		}
	}
	return fields
}

func parseRecordAreaIDA(record string, fInfo FileInfo) ParsedRecord {
	fields := newParsedRecord()
	checkRecordLengthIDA(record, fInfo, 15, 47)
	var err error
	fields.FIPS, err = strconv.Atoi(strings.Replace(strings.Trim(record[0:5], "\""), " ", "0", -1))
	if err != nil {
		panic(fInfo.fname + "\n" + record + "\n" + err.Error())
	}
	fields.SCC = strings.Trim(record[5:15],"\"")
	for i, pol := range strings.Split(fInfo.polid, " ") {
		start := 15 + 47*i
		fields.AVD_EMIS[strings.Trim(pol, " ")], err = strconv.ParseFloat(strings.Trim(record[start+10:start+10+10], " "),64)
		if err != nil {
			fields.AVD_EMIS[strings.Trim(pol, " ")] = 0.
		}
		fields.ANN_EMIS[strings.Trim(pol, " ")], err = strconv.ParseFloat(strings.Trim(record[start:start+10], " "),64)
		if err != nil {
			fields.ANN_EMIS[strings.Trim(pol, " ")] = fields.AVD_EMIS[strings.Trim(pol, " ")] * 365.
		}
		fields.CEFF[strings.Trim(pol, " ")], err = strconv.ParseFloat(strings.Trim(record[start+10+10+11:start+10+10+11+7], " "),64)
		if err != nil {
			fields.CEFF[strings.Trim(pol, " ")] = 0.
		}
		fields.REFF[strings.Trim(pol, " ")], err = strconv.ParseFloat(strings.Trim(record[start+10+10+11+7:start+10+10+11+7+3], " "),64)
		if err != nil {
			fields.REFF[strings.Trim(pol, " ")] = 100.
		}
		fields.RPEN[strings.Trim(pol, " ")], err = strconv.ParseFloat(strings.Trim(record[start+10+10+11+7+3:start+10+10+11+7+3+6], " "),64)
		if err != nil {
			fields.RPEN[strings.Trim(pol, " ")] = 100.
		}
	}
	return fields
}
func parseRecordMobileIDA(record string, fInfo FileInfo) ParsedRecord {
	fields := newParsedRecord()
	checkRecordLengthIDA(record, fInfo, 25, 20)
	var err error
	fields.FIPS, err = strconv.Atoi(strings.Replace(strings.Trim(record[0:5], "\""), " ", "0", -1))
	if err != nil {
		panic(fInfo.fname + "\n" + record + "\n" + err.Error())
	}
	fields.SCC = strings.Trim(record[15:25],"\"")

	for i, pol := range strings.Split(fInfo.polid, " ") {
		start := 25 + 20*i
		fields.AVD_EMIS[strings.Trim(pol, " ")], err = strconv.ParseFloat(strings.Trim(record[start+10:start+10+10], " "),64)
		if err != nil {
			fields.AVD_EMIS[strings.Trim(pol, " ")] = 0.
		}
		fields.ANN_EMIS[strings.Trim(pol, " ")], err = strconv.ParseFloat(strings.Trim(record[start:start+10], " "),64)
		if err != nil {
			fields.ANN_EMIS[strings.Trim(pol, " ")] = fields.AVD_EMIS[strings.Trim(pol, " ")] * 365.
		}
	}
	return fields
}

func (config *RunData) Inventory(MesgChan chan string, InvSpecChan chan ParsedRecord, period string) {
	defer func() {
		if err := recover(); err != nil {
			// Handle error
			config.ErrorReport(err)
			MesgChan <- config.sector + " failed!"
		}
	}()

	inventoryInfo := make(map[string]FileInfo)
	for _, file := range strings.Fields(config.invFilePaths) {
		if config.inventoryFreq == "monthly" {
			file = strings.Replace(file, "[month]", period, -1)
		}
		var record string
		fInfo := newFileInfo()
		fInfo.fname = file
		fid, err := os.Open(file)
		buf := bufio.NewReader(fid)
		defer fid.Close()
		if err != nil {
			panic(err)
		}
		record, err = buf.ReadString('\n')
		if err != nil {
			panic(fInfo.fname + "\n" + record + "\n" + err.Error())
		}
		if strings.Index(record, "ORL") >= 0 {
			fInfo.format = "ORL"
			if strings.Index(record, "NONROAD") >= 0 {
				fInfo.format = "ORLNONROAD"
			}
		} else if strings.Index(record, "IDA") >= 0 {
			fInfo.format = "IDA"
		} else {
			panic("Unknown file type for: " + file)
		}
		var firstLine bool
		for {
			record, err = buf.ReadString('\n')
			if err != nil {
				panic(fInfo.fname + "\n" + record + "\n" + err.Error())
			}
			if record[0] != '#' {
				firstLine = true
				break
			}
			if len(record) > 5 && record[1:5] == "TYPE" {
				fInfo.ftype = strings.Replace(strings.Trim(record[5:], "\n "), ",", " ", -1)
			}
			if len(record) > 8 && record[1:8] == "COUNTRY" {
				fInfo.country = strings.Trim(record[8:], "\n ")
			}
			if len(record) > 5 && record[1:5] == "YEAR" {
				fInfo.year = strings.Trim(record[5:], "\n ")
			}
			if len(record) > 6 && record[1:6] == "POLID" {
				fInfo.polid = strings.Trim(record[6:], " \n\r")
			}
			if len(record) > 5 && record[1:5] == "DATA" {
				fInfo.polid = strings.Trim(record[5:], " \n\r")
			}
		}

		// Make sure nonroad files are properly assigned
		if fInfo.format == "ORL" && strings.Index(fInfo.ftype, "nonroad") >= 0 {
			fInfo.format = "ORLNONROAD"
		}

		for {
			if firstLine != true {
				record, err = buf.ReadString('\n')
				if err != nil {
					if err.Error() == "EOF" {
						break
					} else {
						panic(fInfo.fname + "\n" + record + "\n" + err.Error())
					}
				}
			}
			firstLine = false
			var fields ParsedRecord
			switch fInfo.format + config.sectorType {
			case "ORLpoint":
				fields = parseRecordPointORL(record, fInfo)
			case "ORLarea":
				fields = parseRecordAreaORL(record, fInfo)
			case "ORLNONROADarea":
				fields = parseRecordNonroadORL(record, fInfo)
			case "ORLmobile":
				fields = parseRecordMobileORL(record, fInfo)
			case "IDApoint":
				fields = parseRecordPointIDA(record, fInfo)
			case "IDAarea":
				fields = parseRecordAreaIDA(record, fInfo)
			case "IDAmobile":
				fields = parseRecordMobileIDA(record, fInfo)
			default:
				panic("Unknown format: " + fInfo.format + " " + config.sectorType)
			}
			// send parsed record to Speciate function for chemical speciation
			InvSpecChan <- fields
			// add emissions to totals for report
			for pol, emis := range fields.ANN_EMIS {
				fInfo.totals[pol] += emis
			}
		}
		inventoryInfo[file] = fInfo
	}
	// Write inventory report 
	config.inventoryReport(inventoryInfo, period)
	MesgChan <- "Finished importing inventory for " + period + " " + config.sector
	// Close channel to Speciate to indicate input is finished.
	close(InvSpecChan)
}
