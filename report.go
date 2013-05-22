package main

import (
	"bitbucket.org/ctessum/aep/gis"
	"bufio"
	"errors"
	"fmt"
	"github.com/skelterjohn/go.matrix"
	"html/template"
	"io"
	"log"
	"net/http"
	_ "net/http/pprof"
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
		Status.Sectors[c.Sector] = "Failed!"
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
		Status.Sectors[c.Sector] = "Failed!"
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
	Status.ErrorMessages += err + "\n\n"
	return
}

type ReportHolder struct {
	Config        *RunData
	SectorResults map[string]map[string]*Results // map[sector][period]Results
	ReportOnly    bool                           // whether main program is running at the same time
	GridNames     []string                       // names of the grids
}

type Results struct {
	InventoryResults  map[string]*FileInfo
	SpeciationResults *SpecTotals
	SpatialResults    *SpatialTotals
}

type StatusHolder struct {
	Sectors       map[string]string
	Surrogates    map[string]string
	ErrorMessages string
}

func NewStatus() *StatusHolder {
	out := new(StatusHolder)
	out.Sectors = make(map[string]string)
	out.Surrogates = make(map[string]string)
	return out
}

func (s *StatusHolder) GetSrgStatus(srg, schema string, pg *gis.PostGis) string {
	if status, ok := s.Surrogates[srg]; ok && status == "Generating" {
		return "Generating"
	} else if status, ok := s.Surrogates[srg]; ok && status == "Ready" {
		return "Ready"
	} else if status, ok := s.Surrogates[srg]; ok && status == "Failed!" {
		err := fmt.Errorf("Surrogate generation has previously failed for %v.\n",
			srg)
		panic(err)
	} else if _, ok := s.Surrogates[srg]; !ok {
		if pg.TableExists(schema, srg) {
			Status.Surrogates[srg] = "Ready"
			return "Ready"
		} else {
			return "Empty"
		}
	} else {
		panic("Unknown status: " + s.Surrogates[srg])
	}
}

// Prepare maps of emissions for each species and domain.
func (c *RunData) ResultMaps(
	TotalGrid map[*gis.GridDef]map[string]*matrix.SparseMatrix,
	period string, pg *gis.PostGis) {

	for grid, d1 := range TotalGrid {
		tableName := c.Sector + "_" + period + "_" + grid.Name
		pg.MakeRasterTable(c.SimulationName, tableName)
		for pol, data := range d1 {
			pg.AddDataRowToRasterTable(c.SimulationName, tableName, pol,
				grid, data)
			dir := filepath.Join(c.sectorLogs, pol+"_maps")
			err := os.MkdirAll(dir, os.ModePerm)
			if err != nil {
				panic(err)
			}
			filename := filepath.Join(dir, tableName+"_"+pol+".tif")
			pg.WriteOutRaster(c.SimulationName, tableName, pol, filename)
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
					emis.Val / 12.
			} else {
				records[record.NAICS][record.SIC][record.SCC][pol] += emis.Val
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

// HTML report server

type htmlData struct {
	PageName      string
	IncludeStatus bool
	IsConfigure   bool
	IsStatus      bool
	IsInv         bool
	IsSpec        bool
	IsSpatial     bool
}

func newHtmlData() *htmlData {
	data := new(htmlData)
	if !Report.ReportOnly {
		data.IncludeStatus = true
	}
	return data
}

func configHandler(w http.ResponseWriter, r *http.Request) {
	pageData := newHtmlData()
	pageData.PageName = "Configuration"
	pageData.IsConfigure = true
	renderHeaderFooter(w, "header", pageData)
	renderHeaderFooter(w, "nav", pageData)
	renderBodyTemplate(w, "config")
	renderHeaderFooter(w, "footer", pageData)
}

func statusHandler(w http.ResponseWriter, r *http.Request) {
	if Report.ReportOnly {
		http.Redirect(w, r, "/config", http.StatusFound)
	}
	pageData := newHtmlData()
	pageData.PageName = "Status"
	pageData.IsStatus = true
	renderHeaderFooter(w, "header", pageData)
	renderHeaderFooter(w, "nav", pageData)
	renderStatusTemplate(w)
	renderHeaderFooter(w, "footer", pageData)
}

func inventoryHandler(w http.ResponseWriter, r *http.Request) {
	pageData := newHtmlData()
	pageData.PageName = "Inventory"
	pageData.IsInv = true
	renderHeaderFooter(w, "header", pageData)
	renderHeaderFooter(w, "nav", pageData)
	const body1 = `
<div class="container">
	<h4>Inventory</h4>
	<p>(See file Report.json in the log directory for more detail)</p>
	<div class="row">
		<div class="span12">
			<h5>Kept pollutants</h5>
			<p>These are the emissions that go on to the next modeling step, as specified in the PolsToKeep setting in the configuration file.</p>`
	fmt.Fprint(w, body1)
	sectors, pols, units, data := Report.PrepDataReport("Inventory", "Totals")
	DrawTable("%.4g", true, sectors, pols, units, data, w)
	const body2 = `
		</div>
	</div>
	<div class="row">
		<div class="span12">
			<h5>Dropped pollutants</h5>
			<p>These are the pollutants that are not specified in the PolsToKeep setting in the configuration file.</p>`
	fmt.Fprint(w, body2)
	sectors, pols, units, data = Report.PrepDataReport("Inventory", "DroppedTotals")
	DrawTable("%.4g", true, sectors, pols, units, data, w)
	const body3 = `
		</div>
	</div>
</div>`
	fmt.Fprint(w, body3)
	renderHeaderFooter(w, "footer", pageData)
}

func speciateHandler(w http.ResponseWriter, r *http.Request) {
	pageData := newHtmlData()
	pageData.PageName = "Speciation"
	pageData.IsSpec = true
	renderHeaderFooter(w, "header", pageData)
	renderHeaderFooter(w, "nav", pageData)
	const body1 = `
<div class="container">
	<h4>Speciation</h4>
	<p>(See file Report.json in the log directory for more detail)</p>
	<div class="row">
		<div class="span12">
			<h5>Kept pollutants</h5>
			<p>These are the emissions that go on to the next modeling step.</p>`
	fmt.Fprint(w, body1)
	sectors, pols, units, data := Report.PrepDataReport("Speciation", "Kept")
	DrawTable("%.4g", true, sectors, pols, units, data, w)
	const body2 = `
		</div>
	</div>
	<div class="row">
		<div class="span12">
			<h5>Pollutants dropped due to double counting</h5>
			<p>When there is a general species group (like VOCs) that is speciated into specific chemicals, but some of the specific chemicals are also explicitely tracked in the inventory, there is a possibility for double counting. So for records that include both the general species group and specific data for some of its components the specific components that are explicitly included in the inventory are dropped from the speciation of the general group.</p>`
	fmt.Fprint(w, body2)
	sectors, pols, units, data = Report.PrepDataReport("Speciation", "DoubleCounted")
	DrawTable("%.4g", true, sectors, pols, units, data, w)
	const body3 = `
		</div>
	</div>
	<div class="row">
		<div class="span12">
			<h5>Pollutants dropped due to lack of a species group</h5>
			<p>The individual chemicals are lumped into groups for representation in air quality model chemical mechanisms. These are the emissions that are speciated from general groups (such as VOCs) but do not fit into any of the specified groups.</p>`
	fmt.Fprint(w, body3)
	sectors, pols, units, data = Report.PrepDataReport("Speciation", "Ungrouped")
	DrawTable("%.4g", true, sectors, pols, units, data, w)
	const body4 = `
		</div>
	</div>
</div>`
	fmt.Fprint(w, body4)
	renderHeaderFooter(w, "footer", pageData)
}

func spatialHandler(w http.ResponseWriter, r *http.Request) {
	pageData := newHtmlData()
	pageData.PageName = "Gridding"
	pageData.IsSpatial = true
	renderHeaderFooter(w, "header", pageData)
	renderHeaderFooter(w, "nav", pageData)
	const body1 = `
<div class="container">
	<h4>Gridding</h4>
	<p>(See file Report.json in the log directory for more detail)</p>`
	fmt.Fprint(w, body1)
	const body2 = `
	<div class="row">
		<div class="span12">`
	const body3 = `
		</div>
	</div>`

	for _, grid := range Report.GridNames {
		fmt.Fprint(w, body2)
		fmt.Fprintf(w, "<h5>Fraction of emissions within domain %v</h5>\n", grid)
		sectors, pols, units, data := Report.PrepDataReport("Spatialization", grid)
		DrawTable("%.3g%%", false, sectors, pols, units, data, w)
		fmt.Fprint(w, body3)
	}
	fmt.Fprint(w, "\n</div")
	renderHeaderFooter(w, "footer", pageData)
}

func rootHandler(w http.ResponseWriter, r *http.Request) {
	if Report.ReportOnly {
		http.Redirect(w, r, "/config", http.StatusFound)
	} else {
		http.Redirect(w, r, "/status", http.StatusFound)
	}
}

func init() {
	template.Must(templates.Funcs(
		template.FuncMap{"class": TableClass}).ParseFiles(
		"reportfiles/config.html", "reportfiles/status.html",
		"reportfiles/header.html", "reportfiles/nav.html",
		"reportfiles/footer.html", "reportfiles/speciate.html"))
}

var templates = template.New("x")

func TableClass(in string) string {
	if strings.Index(in, "Running") >= 0 {
		return "warning"
	} else if in == "Failed!" {
		return "error"
	} else if in == "Finished" {
		return "success"
	} else if in == "Ready" {
		return "success"
	} else if in == "Generating" {
		return "warning"
	} else {
		return "info"
	}
}

func renderBodyTemplate(w http.ResponseWriter, tmpl string) {
	err := templates.ExecuteTemplate(w, tmpl+".html", Report)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}
func renderStatusTemplate(w http.ResponseWriter) {
	err := templates.ExecuteTemplate(w, "status.html", Status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

func renderHeaderFooter(w http.ResponseWriter, tmpl string, data *htmlData) {
	err := templates.ExecuteTemplate(w, tmpl+".html", data)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

// Organize data in report for making a table or plot.
// If procType="Spatializaton", then countType is actually the domain
// (d01, d02 etc).
func (r *ReportHolder) PrepDataReport(procType, countType string) (
	sectors, pols []string, units map[string]string,
	data map[string]map[string]float64) {

	sectors = make([]string, 0)
	pols = make([]string, 0)
	units = make(map[string]string, 0)
	data = make(map[string]map[string]float64)

	for sector, sectorData := range r.SectorResults {
		data[sector] = make(map[string]float64)
		sectors = append(sectors, sector)
		numPeriods := float64(len(sectorData))
		for _, periodData := range sectorData {
			switch procType {
			case "Inventory":
				if periodData.InventoryResults != nil {
					for _, fileData := range periodData.InventoryResults {
						switch countType {
						case "Totals":
							for pol, val := range fileData.Totals {
								if !IsStringInArray(pols, pol) {
									pols = append(pols, pol)
									units[pol] = fileData.Units
								}
								data[sector][pol] += val / numPeriods
							}
						case "DroppedTotals":
							for pol, val := range fileData.DroppedTotals {
								if !IsStringInArray(pols, pol) {
									pols = append(pols, pol)
									units[pol] = fileData.Units
								}
								data[sector][pol] += val / numPeriods
							}
						}
					}
				}
			case "Speciation":
				if periodData.SpeciationResults != nil {
					switch countType {
					case "Kept":
						for pol, poldata := range periodData.SpeciationResults.Kept {
							if !IsStringInArray(pols, pol) {
								pols = append(pols, pol)
								units[pol] = poldata.Units
							}
							data[sector][pol] += poldata.Val / numPeriods
						}
					case "DoubleCounted":
						for pol, poldata := range periodData.SpeciationResults.DoubleCounted {
							if !IsStringInArray(pols, pol) {
								pols = append(pols, pol)
								units[pol] = poldata.Units
							}
							data[sector][pol] += poldata.Val / numPeriods
						}
					case "Ungrouped":
						for pol, poldata := range periodData.SpeciationResults.Ungrouped {
							if !IsStringInArray(pols, pol) {
								pols = append(pols, pol)
								units[pol] = poldata.Units
							}
							data[sector][pol] += poldata.Val / numPeriods
						}
					}
				}
			case "Spatialization":
				if periodData.SpatialResults != nil {
					for pol, inData := range periodData.SpatialResults.InsideDomainTotals[countType] {
						outData := periodData.SpatialResults.
							OutsideDomainTotals[countType][pol]
						if !IsStringInArray(pols, pol) {
							pols = append(pols, pol)
							units[pol] = inData.Units
						}
						// Fraction inside the domain
						data[sector][pol] += inData.Val /
							(inData.Val + outData.Val) * 100. / numPeriods
					}
				}
			}
		}
	}
	sort.Strings(pols)
	sort.Strings(sectors)
	return
}

func DrawTable(format string, includeTotals bool, sectors, pols []string,
	units map[string]string, data map[string]map[string]float64, w io.Writer) {
	fmt.Fprint(w, "<table class=\"table table-striped\">\n<thead>\n<tr><td></td>")
	totals := make(map[string]float64)
	for _, pol := range pols {
		fmt.Fprintf(w, "<td>%v</td>", pol)
	}
	fmt.Fprint(w, "</tr>\n<tr><td></td>")
	for _, pol := range pols {
		fmt.Fprintf(w, "<td>%v</td>", units[pol])
	}
	fmt.Fprint(w, "</tr></thead>\n<tbody>\n")
	for _, sector := range sectors {
		fmt.Fprintf(w, "<tr><td>%v</td>", sector)
		for _, pol := range pols {
			fmt.Fprintf(w, "<td>"+format+"</td>", data[sector][pol])
			totals[pol] += data[sector][pol]
		}
		fmt.Fprint(w, "</tr>\n")
	}
	if includeTotals {
		fmt.Fprint(w, "<tr><td><strong>Totals</strong></td>")
		for _, pol := range pols {
			fmt.Fprintf(w, "<td><strong>"+format+"</strong></td>", totals[pol])
		}
	}
	fmt.Fprint(w, "</tbody></table>\n")
}

func ReportServer() {
	http.Handle("/css/", http.StripPrefix("/css/",
		http.FileServer(http.Dir("reportfiles/css"))))
	http.Handle("/js/", http.StripPrefix("/js/",
		http.FileServer(http.Dir("reportfiles/js"))))
	http.HandleFunc("/inventory", inventoryHandler)
	http.HandleFunc("/speciate", speciateHandler)
	http.HandleFunc("/spatial", spatialHandler)
	http.HandleFunc("/config", configHandler)
	http.HandleFunc("/status", statusHandler)
	http.HandleFunc("/", rootHandler)
	http.ListenAndServe(":6060", nil)
}
