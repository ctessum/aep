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
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"go/build"
	"html/template"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	_ "net/http/pprof"
	"os"
	"path/filepath"
	"runtime/debug"
	"sort"
	"strings"
	"sync"

	"bitbucket.org/ctessum/cdf"
	"bitbucket.org/ctessum/sparse"
)

// Write a message to standard error.
func (c *Context) Log(msg interface{}, DebugLevel int) {
	if DebugLevel <= c.DebugLevel {
		log.Print(msg)
	}
	return
}

// Handle an error occuring within a function to allow the function
// to fail without killing the whole program.
func (c *Context) ErrorRecover() {
	if err := recover(); err != nil {
		if c.DebugLevel > 0 {
			panic(err)
		}
		if !c.ErrorFlag {
			c.ErrorReport(err) // Handle error
			c.ErrorFlag = true
		}
		Status.Sectors[c.Sector] = "Failed!"
		c.msgchan <- c.Sector + " failed!"
	}
}

// Same as ErrorRecover, but also close a channel
func (c *Context) ErrorRecoverCloseChan(recordChan chan *ParsedRecord) {
	if err := recover(); err != nil {
		if c.DebugLevel > 0 {
			panic(err)
		}
		if !c.ErrorFlag {
			c.ErrorReport(err) // Handle error
			c.ErrorFlag = true
		}
		Status.Sectors[c.Sector] = "Failed!"
		c.msgchan <- c.Sector + " failed!"
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

func (c *Context) ErrorReport(errmesg interface{}) {
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

var (
	reportMx sync.Mutex
	Report   = new(ReportHolder)
	Status   *StatusHolder
)

func init() {
	Report.SectorResults = make(map[string]map[string]*Results)
	// track status of all of the running sectors
	Status = NewStatus()
	// Start server for html report (go to localhost:6060 in web browser to view report)
	// Here, we run the report server in the background while the rest of the program is running.
}

type ReportHolder struct {
	Config          *Context
	SectorResults   map[string]map[string]*Results // map[sector][period]Results
	ReportOnly      bool                           // whether main program is running at the same time
	GridNames       []string                       // names of the grids
	TemporalResults *TemporalReport
}

type Results struct {
	InventoryResults  map[string]*FileInfo
	SpeciationResults *SpecTotals
	SpatialResults    *SpatialTotals
}

type StatusHolder struct {
	Sectors           map[string]string
	Surrogates        map[string]string
	ErrorMessages     string
	HTMLerrorMessages template.HTML
	SrgProgress       float64
}

func NewStatus() *StatusHolder {
	out := new(StatusHolder)
	out.Sectors = make(map[string]string)
	out.Surrogates = make(map[string]string)
	return out
}

func (s *StatusHolder) GetSrgStatus(srg, srgfile string) string {
	if status, ok := s.Surrogates[srg]; ok && status == "Generating" {
		return "Generating"
	} else if status, ok := s.Surrogates[srg]; ok &&
		status == "Waiting to generate" {
		return "Waiting to generate"
	} else if status, ok := s.Surrogates[srg]; ok && status == "Ready" {
		return "Ready"
	} else if status, ok := s.Surrogates[srg]; ok && status == "Failed!" {
		err := fmt.Errorf("Surrogate generation has previously failed for %v.\n",
			srg)
		panic(err)
	} else if _, ok := s.Surrogates[srg]; !ok {
		if _, err := os.Stat(srgfile); err == nil {
			Status.Surrogates[srg] = "Ready"
			return "Ready"
		} else {
			return "Empty"
		}
	} else {
		panic("Unknown status: " + s.Surrogates[srg])
	}
}

// Prepare maps of emissions for each species and domain in NetCDF format.
// (http://www.unidata.ucar.edu/software/netcdf/).
func (c *Context) ResultMaps(totals *SpatialTotals,
	TotalGrid map[*GridDef]map[string]*sparse.SparseArray,
	period string) {

	dir := filepath.Join(c.outputDir, "maps")
	err := os.MkdirAll(dir, os.ModePerm)
	if err != nil {
		panic(err)
	}
	for grid, d1 := range TotalGrid {
		filename := filepath.Join(dir, fmt.Sprintf("%v_%v_%v_%v.nc",
			c.SimulationName, c.Sector, period, grid.Name))
		h := cdf.NewHeader([]string{"y", "x"}, []int{grid.Ny, grid.Nx})
		h.AddAttribute("", "TITLE", "Anthropogenic emissions created "+
			"by AEP version "+Version+" ("+Website+")")
		h.AddAttribute("", "CEN_LAT", []float64{c.wrfData.Ref_lat})
		h.AddAttribute("", "CEN_LOC", []float64{c.wrfData.Ref_lon})
		h.AddAttribute("", "TRUELAT1", []float64{c.wrfData.Truelat1})
		h.AddAttribute("", "TRUELAT2", []float64{c.wrfData.Truelat2})
		h.AddAttribute("", "STAND_LON", []float64{c.wrfData.Stand_lon})
		h.AddAttribute("", "MAP_PROJ", c.wrfData.Map_proj)
		h.AddAttribute("", "Northernmost_Northing", []float64{grid.Y0 +
			float64(grid.Ny)*grid.Dy})
		h.AddAttribute("", "Southernmost_Northing", []float64{grid.Y0})
		h.AddAttribute("", "Easternmost_Easting", []float64{grid.X0 +
			float64(grid.Nx)*grid.Dx})
		h.AddAttribute("", "Westernmost_Easting", []float64{grid.X0})
		for pol, _ := range d1 {
			if d, ok := totals.InsideDomainTotals[grid.Name][pol]; ok {
				h.AddVariable(pol, []string{"y", "x"}, []float32{0.})
				h.AddAttribute(pol, "units", d.Units)
			}
		}
		if len(h.Variables()) > 0 {
			h.Define()
			errs := h.Check()
			for _, err := range errs {
				if err != nil {
					panic(err)
				}
			}
			f, err := os.Create(filename)
			if err != nil {
				panic(err)
			}
			ff, err := cdf.Create(f, h)
			if err != nil {
				panic(err)
			}
			for pol, data := range d1 {
				if data.Sum() != 0. {
					r := ff.Writer(pol, []int{0, 0}, []int{grid.Ny, grid.Nx})
					if _, err = r.Write(data.ToDense32()); err != nil {
						panic(err)
					}
				}
			}
			err = cdf.UpdateNumRecs(f)
			if err != nil {
				panic(err)
			}
			f.Close()
		}
	}
}

// SCCdesc reads the smoke sccdesc file, which gives descriptions for each SCC code.
func (c *Context) SCCdesc() (map[string]string, error) {
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
func (c *Context) SICdesc() (map[string]string, error) {
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
func (c *Context) NAICSdesc() (map[string]string, error) {
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
	dr := Report.PrepDataReport("Inventory", "Totals")
	DrawTable("%.4g", true, dr, w)
	const body2 = `
		</div>
	</div>
	<div class="row">
		<div class="span12">
			<h5>Dropped pollutants</h5>
			<p>These are the pollutants that are not specified in the PolsToKeep setting in the configuration file.</p>`
	fmt.Fprint(w, body2)
	dr = Report.PrepDataReport("Inventory", "DroppedTotals")
	DrawTable("%.4g", true, dr, w)
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
	dr := Report.PrepDataReport("Speciation", "Kept")
	DrawTable("%.4g", true, dr, w)
	const body2 = `
		</div>
	</div>
	<div class="row">
		<div class="span12">
			<h5>Pollutants dropped due to double counting</h5>
			<p>When there is a general species group (like VOCs) that is speciated into specific chemicals, but some of the specific chemicals are also explicitely tracked in the inventory, there is a possibility for double counting. So for records that include both the general species group and specific data for some of its components the specific components that are explicitly included in the inventory are dropped from the speciation of the general group.</p>`
	fmt.Fprint(w, body2)
	dr = Report.PrepDataReport("Speciation", "DoubleCounted")
	DrawTable("%.4g", true, dr, w)
	const body3 = `
		</div>
	</div>
	<div class="row">
		<div class="span12">
			<h5>Pollutants dropped due to lack of a species group</h5>
			<p>The individual chemicals are lumped into groups for representation in air quality model chemical mechanisms. These are the emissions that are speciated from general groups (such as VOCs) but do not fit into any of the specified groups.</p>`
	fmt.Fprint(w, body3)
	dr = Report.PrepDataReport("Speciation", "Ungrouped")
	DrawTable("%.4g", true, dr, w)
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
		dr := Report.PrepDataReport("Spatialization", grid)
		DrawTable("%.3g%%", false, dr, w)
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
	pkg, err := build.Import("bitbucket.org/ctessum/aep", "", build.FindOnly)
	if err != nil {
		panic(err)
	}
	reportfiles = filepath.Join(pkg.SrcRoot, pkg.ImportPath, "reportfiles")
	template.Must(templates.Funcs(
		template.FuncMap{"class": TableClass}).ParseFiles(
		filepath.Join(reportfiles, "config.html"),
		filepath.Join(reportfiles, "status.html"),
		filepath.Join(reportfiles, "header.html"),
		filepath.Join(reportfiles, "nav.html"),
		filepath.Join(reportfiles, "footer.html")))
}

var (
	templates   = template.New("x")
	reportfiles = ""
)

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
	} else if in == "Waiting to generate" {
		return "info"
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
	Status.SrgProgress = SrgProgress
	Status.HTMLerrorMessages = template.HTML(strings.Replace(Status.ErrorMessages,
		"\n", "<br>", -1))
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

type dataReport struct {
	sectors, pols []string
	units         map[string]string
	data          map[string]map[string]float64
	polTotals     map[string]float64
}

func (dr *dataReport) Len() int { return len(dr.pols) }
func (dr *dataReport) Swap(i, j int) {
	dr.pols[i], dr.pols[j] = dr.pols[j], dr.pols[i]
}
func (dr *dataReport) Less(i, j int) bool {
	// descending order
	return dr.polTotals[dr.pols[i]] > dr.polTotals[dr.pols[j]]
}

func (dr *dataReport) sortByTotals() {
	dr.polTotals = make(map[string]float64)
	for _, d1 := range dr.data {
		for pol, d2 := range d1 {
			dr.polTotals[pol] += d2
		}
	}
	sort.Sort(dr)
}

// Organize data in report for making a table or plot.
// If procType="Spatializaton", then countType is actually the domain
// (d01, d02 etc).
func (r *ReportHolder) PrepDataReport(procType, countType string) *dataReport {

	dr := new(dataReport)
	dr.sectors = make([]string, 0)
	dr.pols = make([]string, 0)
	dr.units = make(map[string]string, 0)
	dr.data = make(map[string]map[string]float64)

	for sector, sectorData := range r.SectorResults {
		dr.data[sector] = make(map[string]float64)
		dr.sectors = append(dr.sectors, sector)
		numPeriods := float64(len(sectorData))
		for _, periodData := range sectorData {
			switch procType {
			case "Inventory":
				if periodData.InventoryResults != nil {
					for _, fileData := range periodData.InventoryResults {
						switch countType {
						case "Totals":
							for pol, val := range fileData.Totals {
								if !IsStringInArray(dr.pols, pol) {
									dr.pols = append(dr.pols, pol)
									dr.units[pol] = fileData.Units
								}
								dr.data[sector][pol] += val / numPeriods
							}
						case "DroppedTotals":
							for pol, val := range fileData.DroppedTotals {
								if !IsStringInArray(dr.pols, pol) {
									dr.pols = append(dr.pols, pol)
									dr.units[pol] = fileData.Units
								}
								dr.data[sector][pol] += val / numPeriods
							}
						}
					}
				}
			case "Speciation":
				if periodData.SpeciationResults != nil {
					switch countType {
					case "Kept":
						for pol, poldata := range periodData.
							SpeciationResults.Kept {
							if !IsStringInArray(dr.pols, pol) {
								dr.pols = append(dr.pols, pol)
								dr.units[pol] = poldata.Units
							}
							dr.data[sector][pol] += poldata.Val / numPeriods
						}
					case "DoubleCounted":
						for pol, poldata := range periodData.SpeciationResults.
							DoubleCounted {
							if !IsStringInArray(dr.pols, pol) {
								dr.pols = append(dr.pols, pol)
								dr.units[pol] = poldata.Units
							}
							dr.data[sector][pol] += poldata.Val / numPeriods
						}
					case "Ungrouped":
						for pol, poldata := range periodData.SpeciationResults.
							Ungrouped {
							if !IsStringInArray(dr.pols, pol) {
								dr.pols = append(dr.pols, pol)
								dr.units[pol] = poldata.Units
							}
							dr.data[sector][pol] += poldata.Val / numPeriods
						}
					}
				}
			case "Spatialization":
				if periodData.SpatialResults != nil {
					for pol, inData := range periodData.SpatialResults.
						InsideDomainTotals[countType] {
						outData := periodData.SpatialResults.
							OutsideDomainTotals[countType][pol]
						if !IsStringInArray(dr.pols, pol) {
							dr.pols = append(dr.pols, pol)
							dr.units[pol] = inData.Units
						}
						// Fraction inside the domain
						dr.data[sector][pol] += inData.Val /
							(inData.Val + outData.Val) * 100. / numPeriods
					}
				}
			}
		}
	}
	sort.Strings(dr.pols)
	sort.Strings(dr.sectors)
	return dr
}

func DrawTable(format string, includeTotals bool, dr *dataReport,
	w io.Writer) {
	if includeTotals {
		dr.sortByTotals()
	}
	fmt.Fprint(w, "<table class=\"table table-striped\">\n<thead>\n<tr><td></td>")
	for _, pol := range dr.pols {
		fmt.Fprintf(w, "<td>%v</td>", pol)
	}
	fmt.Fprint(w, "</tr>\n<tr><td></td>")
	for _, pol := range dr.pols {
		fmt.Fprintf(w, "<td>%v</td>", dr.units[pol])
	}
	fmt.Fprint(w, "</tr></thead>\n<tbody>\n")
	for _, sector := range dr.sectors {
		fmt.Fprintf(w, "<tr><td>%v</td>", sector)
		for _, pol := range dr.pols {
			fmt.Fprintf(w, "<td>"+format+"</td>", dr.data[sector][pol])
		}
		fmt.Fprint(w, "</tr>\n")
	}
	if includeTotals {
		fmt.Fprint(w, "<tr><td><strong>Totals</strong></td>")
		for _, pol := range dr.pols {
			fmt.Fprintf(w, "<td><strong>"+format+"</strong></td>",
				dr.polTotals[pol])
		}
	}
	fmt.Fprint(w, "</tbody></table>\n")
}

func (c *Context) ReportServer(reportOnly bool) {
	// read in the report
	if reportOnly {
		file := filepath.Join(c.outputDir, "Report.json")
		f, err := os.Open(file)
		if err != nil {
			panic(err)
		}
		reader := bufio.NewReader(f)
		bytes, err := ioutil.ReadAll(reader)
		if err != nil {
			panic(err)
		}
		err = json.Unmarshal(bytes, Report)
		if err != nil {
			panic(err)
		}
		Report.ReportOnly = reportOnly

	}
	Report.ReportOnly = reportOnly
	Report.Config = c
	http.Handle("/css/", http.FileServer(http.Dir(reportfiles)))
	http.Handle("/js/", http.FileServer(http.Dir(reportfiles)))
	http.HandleFunc("/inventory", inventoryHandler)
	http.HandleFunc("/speciate", speciateHandler)
	http.HandleFunc("/spatial", spatialHandler)
	http.HandleFunc("/config", configHandler)
	http.HandleFunc("/status", statusHandler)
	http.HandleFunc("/", rootHandler)
	http.ListenAndServe(":6060", nil)

}

// Write out the report
func (c *Context) WriteReport() {
	b, err := json.MarshalIndent(Report, "", "  ")
	if err != nil {
		panic(err)
	}
	f, err := os.Create(filepath.Join(
		c.outputDir, "Report.json"))
	if err != nil {
		panic(err)
	}
	_, err = f.Write(b)
	if err != nil {
		panic(err)
	}
	f.Close()
}
