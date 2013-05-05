package main

import (
	"bufio"
	"errors"
	"fmt"
	"github.com/skelterjohn/go.matrix"
	"bitbucket.org/ctessum/aep/gis"
	"log"
	"os"
	"path/filepath"
	"runtime/debug"
	"sort"
	"strings"
)

// Write a message to standard error.
func (c *RunData) Log(msg interface{}, DebugLevel int) {
	if DebugLevel <= c.DebugLevel {
		log.Print(msg)
	}
	return
}

// Handle an error occuring within a function to allow the function
// to fail without killing the whole program.
func (c *RunData) ErrorRecover(msgchan chan string) {
	if err := recover(); err != nil {
		if c.DebugLevel > 0 {
			panic(err)
		}
		if !c.ErrorFlag {
			c.ErrorReport(err) // Handle error
			c.ErrorFlag = true
		}
		msgchan <- c.Sector + " failed!"
	}
}

// Same as ErrorRecover, but also close a channel
func (c *RunData) ErrorRecoverCloseChan(msgchan chan string,
	recordChan chan *ParsedRecord) {
	if err := recover(); err != nil {
		if c.DebugLevel > 0 {
			panic(err)
		}
		if !c.ErrorFlag {
			c.ErrorReport(err) // Handle error
			c.ErrorFlag = true
		}
		msgchan <- c.Sector + " failed!"
		close(recordChan)
	}
}

// The ErrCat type and methods collect errors while the program is running
// and then print them later so that all errors can be seen and fixed at once,
// instead of just the first one.
type ErrCat struct {
	str string
}

func (e *ErrCat) statOS(path string, varname string) {
	_, err := os.Stat(path)
	if path == "" {
		err = fmt.Errorf("The path to the required file %v is missing"+
			" from the configuration file.", varname)
	}
	e.Add(err)
	return
}

func (e *ErrCat) Add(err error) {
	if err != nil && strings.Index(e.str, err.Error()) == -1 {
		e.str += err.Error() + "\n"
	}
	return
}

func (e *ErrCat) Report() {
	if e.str != "" {
		fmt.Println("The following errors were found:\n" + e.str)
		os.Exit(1)
	}
	return
}

func (c *RunData) ErrorReport(errmesg interface{}) {
	err := "--------------------------\nERROR REPORT\n"
	err += "Sector: " + c.Sector + "\n"
	err += "Error message: "
	err += fmt.Sprintf("%v", errmesg) + "\n"
	err += "Stack trace:\n"
	err += fmt.Sprintf("%s\n", debug.Stack())
	err += "--------------------------\n"
	fmt.Print(err)
	return
}

func (c *RunData) ConfigReport() {
	path := filepath.Join(c.sectorLogs, c.Sector+"_configuration.csv")
	rep, err := os.Create(path)
	defer rep.Close()
	if err != nil {
		panic(err)
	}
	fmt.Fprintf(rep, "sector: %v\n", c.Sector)
	fmt.Fprintf(rep, "sectorType: %v\n", c.SectorType)
	fmt.Fprintf(rep, "sectorLogs: %v\n", c.sectorLogs)
	fmt.Fprintf(rep, "startDate: %v\n", c.startDate)
	fmt.Fprintf(rep, "endDate: %v\n", c.endDate)
	fmt.Fprintf(rep, "tStep: %v\n", c.tStep)
	fmt.Fprintf(rep, "outputType: %v\n", c.OutputType)
	fmt.Fprintf(rep, "specRef: %v\n", c.SpecRefFile)
	fmt.Fprintf(rep, "specRefCombo: %v\n", c.SpecRefComboFile)
	fmt.Fprintf(rep, "specProFile: %v\n", c.SpecProFile)
	fmt.Fprintf(rep, "specType: %v\n", c.SpecType)
	fmt.Fprintf(rep, "SpeciesGroupName: %v\n", c.SpeciesGroupName)
	fmt.Fprintf(rep, "PolsToKeep: %v\n", c.PolsToKeep)
	fmt.Fprintf(rep, "GridRefFile: %v\n", c.GridRefFile)
	fmt.Fprintf(rep, "SrgSpecFile: %v\n", c.SrgSpecFile)
	fmt.Fprintf(rep, "TemporalRefFile: %v\n", c.TemporalRefFile)
	fmt.Fprintf(rep, "CaseName: %v\n", c.CaseName)
	fmt.Fprintf(rep, "inventoryFreq: %v\n", c.InventoryFreq)
	fmt.Fprintf(rep, "inputUnits: %v\n", c.InputUnits)
	fmt.Fprintf(rep, "inputConv: %v\n", c.InputConv)
	fmt.Fprintf(rep, "EarthRadius: %v\n", c.EarthRadius)
	fmt.Fprintf(rep, "WpsNamelist: %v\n", c.WpsNamelist)
	fmt.Fprintf(rep, "SimuationName: %v\n", c.SimulationName)
	fmt.Fprintf(rep, "RegenerateSpatialData: %v\n", c.RegenerateSpatialData)
	_, err = fmt.Fprintf(rep, "invFileNames: %v\n", c.InvFileNames)
	if err != nil {
		panic(err)
	}
	return
}

type ReportHolder struct {
	Config *RunData
	SectorResults map[string]map[string]*Results // map[sector][period]Results
}

type Results struct {
	InventoryResults map[string]*FileInfo
}


func (c *RunData) SpeciationReport(r *reportData, totalDropped map[string]float64, period string) {
	repStr := ""
//	var err error
//	if c.SpecRep != nil {
//		repStr += "\n\n"
//	} else {
//		c.SpecRep, err = os.Create(filepath.Join(c.sectorLogs, c.Sector+"_speciation.csv"))
//		if err != nil {
//			panic(err)
//		}
//	}
	polNames := make([]string, 0)
	for pol, _ := range r.totals {
		if !IsStringInArray(polNames, pol) {
			polNames = append(polNames, pol)
		}
	}
	droppedPolNames := make([]string, 0)
	for pol, _ := range totalDropped {
		if !IsStringInArray(droppedPolNames, pol) {
			droppedPolNames = append(droppedPolNames, pol)
		}
	}
	sort.Strings(polNames)
	sort.Strings(droppedPolNames)

	repStr += "Speciated totals\n"
	repStr += fmt.Sprintf("Sector: %s\n", c.Sector)
	repStr += fmt.Sprintf("Time period: %s\n", period)
	for _, pol := range polNames {
		repStr += fmt.Sprintf("%s|", pol)
	}
	repStr += "\n"
	for _, pol := range polNames {
		repStr += fmt.Sprintf("%s|", r.units[pol])
	}
	repStr += "\n"
	for _, pol := range polNames {
		repStr += fmt.Sprintf("%e|", r.totals[pol])
	}
	repStr += "\n\nTotals of dropped pollutants (g/year)\n"
	for _, pol := range droppedPolNames {
		repStr += fmt.Sprintf("%s|", pol)
	}
	repStr += "\n"
	for _, pol := range droppedPolNames {
		repStr += fmt.Sprintf("%e|", totalDropped[pol])
	}
	repStr += "\n"
//	fmt.Fprint(c.SpecRep, repStr)
	return
}

// Repair a report of the spatialization step, including totals inside and 
// outside of each domain, and maps of emissions for each species and domain.
func (c *RunData) SpatialReport(
	TotalGrid map[*gis.GridDef]map[string]*matrix.SparseMatrix,
	DroppedTotals map[string]map[string]float64, period string, pg *gis.PostGis) {

//	// Create csv report
//	repStr := ""
//	var err error
//	if c.SpatialRep != nil {
//		repStr += "\n\n"
//	} else {
//		c.SpatialRep, err = os.Create(filepath.Join(c.sectorLogs,
//			c.Sector+"_spatial.csv"))
//		if err != nil {
//			panic(err)
//		}
//	}
//	polNames := make([]string, 0)
//	for _, data := range TotalGrid {
//		for pol, _ := range data {
//			if !IsStringInArray(polNames, pol) {
//				polNames = append(polNames, pol)
//			}
//		}
//	}
//	sort.Strings(polNames)
//
//	repStr += fmt.Sprintf("Sector: %s\n", c.Sector)
//	repStr += fmt.Sprintf("Time period: %s\n\n", period)
//	for grid, _ := range TotalGrid {
//		repStr += fmt.Sprintf("Domain %v\n", grid.Name)
//		repStr += ","
//		for _, pol := range polNames {
//			repStr += fmt.Sprintf("%s,", pol)
//		}
//		repStr += "\n"
//		repStr += "Total inside domain,"
//		for _, pol := range polNames {
//			repStr += fmt.Sprintf("%e,", MatrixSum(TotalGrid[grid][pol]))
//		}
//		repStr += "\n"
//		repStr += "Total outside domain,"
//		for _, pol := range polNames {
//			repStr += fmt.Sprintf("%e,", DroppedTotals[grid.Name][pol])
//		}
//		repStr += "\n"
//		repStr += "Percent inside domain,"
//		for _, pol := range polNames {
//			totalInside := MatrixSum(TotalGrid[grid][pol])
//			var val float64
//			if totalInside == 0. && DroppedTotals[grid.Name][pol] == 0. {
//				val = 100.
//			} else if totalInside == 0. {
//				val = 0.
//			} else {
//				val = totalInside /
//					(totalInside + DroppedTotals[grid.Name][pol]) * 100.
//			}
//			repStr += fmt.Sprintf("%.1f%%,", val)
//		}
//		repStr += "\n\n"
//	}
//	fmt.Fprint(c.SpatialRep, repStr)

	for grid, d1 := range TotalGrid {
		tableName := c.Sector+"_"+period+"_"+grid.Name
		pg.MakeRasterTable(c.SimulationName,tableName)
		for pol, data := range d1 {
			pg.AddDataRowToRasterTable(c.SimulationName,tableName,pol,
				grid,data)
			filename := filepath.Join(c.sectorLogs,
				tableName+"_"+pol+".tif")
			pg.WriteOutRaster(c.SimulationName,tableName,pol,filename)
		}
	}
}

type summaryRecord struct {
	NAICS string
	SIC   string
	SCC   string
	emis  map[string]float64
}
type summary struct {
	data       []*summaryRecord
	emisTotals map[string]float64
}

func (s summary) Len() int      { return len(s.data) }
func (s summary) Swap(i, j int) { s.data[i], s.data[j] = s.data[j], s.data[i] }
func (s summary) Less(i, j int) bool {
	isum := 0.
	for pol, val := range s.data[i].emis {
		isum += val / s.emisTotals[pol]
	}
	jsum := 0.
	for pol, val := range s.data[j].emis {
		jsum += val / s.emisTotals[pol]
	}
	return isum > jsum
}

// Summarize emissions by NAICS, SIC, and SCC codes
func (c RunData) TotalInventoryReport(msgChan chan string) {
	// Get descriptions of different codes
	sccDesc, err := c.SCCdesc()
	if err != nil {
		msgChan <- err.Error()
	}
	sicDesc, err := c.SICdesc()
	if err != nil {
		msgChan <- err.Error()
	}
	naicsDesc, err := c.NAICSdesc()
	if err != nil {
		msgChan <- err.Error()
	}

	// First aggregate all of the data.
	records := make(map[string]map[string]map[string]map[string]float64)
	for record := range TotalReportChan {
		if _, ok := records[record.NAICS]; !ok {
			records[record.NAICS] =
				make(map[string]map[string]map[string]float64)
		}
		if _, ok := records[record.NAICS][record.SIC]; !ok {
			records[record.NAICS][record.SIC] =
				make(map[string]map[string]float64)
		}
		if _, ok := records[record.NAICS][record.SIC][record.SCC]; !ok {
			records[record.NAICS][record.SIC][record.SCC] =
				make(map[string]float64)
		}
		for pol, emis := range record.ANN_EMIS {
			if record.InventoryFreq == "monthly" {
				records[record.NAICS][record.SIC][record.SCC][pol] +=
					emis.val / 12.
			} else {
				records[record.NAICS][record.SIC][record.SCC][pol] += emis.val
			}
		}
	}
	// Then, sort data in descending order by normalized emissions
	outData := new(summary)
	outData.data = make([]*summaryRecord, 0)
	outData.emisTotals = make(map[string]float64)
	for NAICS, val := range records {
		for SIC, val2 := range val {
			for SCC, val3 := range val2 {
				x := new(summaryRecord)
				x.emis = make(map[string]float64)
				x.NAICS = NAICS
				x.SIC = SIC
				x.SCC = SCC
				x.emis = val3
				outData.data = append(outData.data, x)
				for pol, emis := range val3 {
					outData.emisTotals[pol] += emis
				}
			}
		}
	}
	sort.Sort(outData)
	// Write to log file
	dir := filepath.Join(c.sectorLogs, "total")
	err = os.MkdirAll(dir, os.ModePerm)
	if err != nil {
		panic(err)
	}
	path := filepath.Join(dir, "inventory.csv")
	rep, err := os.Create(path)
	defer rep.Close()
	if err != nil {
		panic(err)
	}
	fmt.Fprint(rep, "NAICS,NAICS desc.,SIC,SIC desc.,SCC,SCC desc.,")
	polNames := make([]string, 0)
	for pol, _ := range outData.emisTotals {
		polNames = append(polNames, pol)
	}
	sort.Strings(polNames)
	for _, pol := range polNames {
		fmt.Fprintf(rep, "%s,", pol)
	}
	fmt.Fprint(rep, "\n")
	for _, x := range outData.data {
		nd, ok := naicsDesc[x.NAICS]
		if !ok {
			nd = "-"
		}
		sid, ok := sicDesc[x.SIC]
		if !ok {
			sid = "-"
		}
		scd, ok := sccDesc[x.SCC]
		if !ok {
			scd = "-"
		}
		fmt.Fprintf(rep, "%s,%s,%s,%s,%s,%s,", x.NAICS, nd, x.SIC, sid, x.SCC, scd)
		for _, pol := range polNames {
			fmt.Fprintf(rep, "%g,", x.emis[pol])
		}
		fmt.Fprint(rep, "\n")
	}
	msgChan <- "Finished total inventory report."
}

// SCCdesc reads the smoke sccdesc file, which gives descriptions for each SCC code.
func (c *RunData) SCCdesc() (map[string]string, error) {
	sccDesc := make(map[string]string)
	var record string
	fid, err := os.Open(c.SccDesc)
	if err != nil {
		return sccDesc, err
	} else {
		defer fid.Close()
	}
	buf := bufio.NewReader(fid)
	for {
		record, err = buf.ReadString('\n')
		if err != nil {
			if err.Error() == "EOF" {
				err = nil
				break
			} else {
				return sccDesc, errors.New(record + "\n" + err.Error() + "\nFile= " + c.SccDesc + "\nRecord= " + record)
			}
		}
		// Get rid of comments at end of line.
		if i := strings.Index(record, "!"); i != -1 {
			record = record[0:i]
		}
		if record[0] != '#' {
			splitLine := strings.Split(record, ",")
			SCC := strings.Trim(splitLine[0], "\"")
			// Add zeros to 8 digit SCCs so that all SCCs are 10 digits
			// If SCC is less than 8 digits, add 2 zeros to the front and the rest to
			// the end.
			if len(SCC) == 8 {
				SCC = "00" + SCC
			} else if len(SCC) == 7 {
				SCC = "00" + SCC + "0"
			} else if len(SCC) == 6 {
				SCC = "00" + SCC + "00"
			} else if len(SCC) == 5 {
				SCC = "00" + SCC + "000"
			} else if len(SCC) == 4 {
				SCC = "00" + SCC + "0000"
			} else if len(SCC) == 3 {
				SCC = "00" + SCC + "00000"
			} else if len(SCC) == 2 {
				SCC = "00" + SCC + "000000"
			}
			sccDesc[SCC] = cleanDescription(splitLine[1])
		}
	}
	return sccDesc, err
}

// Read SIC description file, which gives descriptions for each SIC code.
func (c *RunData) SICdesc() (map[string]string, error) {
	sicDesc := make(map[string]string)
	var record string
	fid, err := os.Open(c.SicDesc)
	if err != nil {
		return sicDesc, errors.New("SICdesc: " + err.Error() + "\nFile= " + c.SicDesc + "\nRecord= " + record)
	} else {
		defer fid.Close()
	}
	buf := bufio.NewReader(fid)
	for {
		record, err = buf.ReadString('\n')
		if err != nil {
			if err.Error() == "EOF" {
				err = nil
				break
			} else {
				return sicDesc, errors.New(c.SicDesc + "\n" + record + "\n" + err.Error() + "\nFile= " + c.SicDesc + "\nRecord= " + record)
			}
		}
		if record[0] != '#' {
			sicDesc[strings.Trim(record[0:4], " ")] =
				cleanDescription(record[5:])
		}
	}
	return sicDesc, err
}

// Read NAICS description file, which gives descriptions for each NAICS code.
func (c *RunData) NAICSdesc() (map[string]string, error) {
	naicsDesc := make(map[string]string)
	var record string
	fid, err := os.Open(c.NaicsDesc)
	if err != nil {
		return naicsDesc, errors.New("NAICSdesc: " + err.Error() + "\nFile= " + c.NaicsDesc + "\nRecord= " + record)
	} else {
		defer fid.Close()
	}
	buf := bufio.NewReader(fid)
	for {
		record, err = buf.ReadString('\n')
		if err != nil {
			if err.Error() == "EOF" {
				err = nil
				break
			} else {
				return naicsDesc, errors.New(record + "\n" + err.Error() + "\nFile= " + c.NaicsDesc + "\nRecord= " + record)
			}
		}
		if record[0] != '#' {
			splitLine := strings.Split(record, ",")
			naicsDesc[strings.Trim(splitLine[0], "\"")] =
				cleanDescription(splitLine[1])
		}
	}
	return naicsDesc, err
}

func cleanDescription(d string) string {
	return "\"" + strings.Replace(strings.Trim(d, "\n"), "\"", "", -1) + "\""
}
