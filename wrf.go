/*
Copyright (C) 2012 the AEP authors.
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
	"errors"
	"fmt"
	"io"
	"log"
	"math"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/ctessum/geom/proj"
	"github.com/ctessum/unit"

	"bitbucket.org/ctessum/cdf"
	"bitbucket.org/ctessum/sparse"
)

const (
	feetToMeters = 0.3048
	g            = 9.80665 // m/s2
	po           = 101300. // Pa, reference pressure
	kappa        = 0.2854  // related to von karman's constant
)

// WRFOutputter outputs emissions in WRF-Chem format.
type WRFOutputter struct {
	filebase      string
	dateFormat    string
	files         *wrfFiles
	met           *MetData
	tstepsPerFile int
	config        *WRFconfigData
	oldWRFOut     string
}

// Layers returns the number of vertical emission layers being used.
func (w *WRFOutputter) Layers() int {
	return w.files.config.Kemit
}

// NewOutputter creates a new WRF-formatted file outputter.
func (d *WRFconfigData) NewOutputter(outputDir, oldWRFOut string) *WRFOutputter {
	w := new(WRFOutputter)
	w.config = d
	w.oldWRFOut = oldWRFOut
	w.tstepsPerFile = d.FramesPerAuxInput5[0] // TODO: Allow different nest to have different numbers of records.
	w.filebase = filepath.Join(outputDir, "wrfchemi_[DOMAIN]_[DATE]")
	if d.Nocolons == true {
		w.dateFormat = "2006-01-02_15_04_05"
	} else {
		w.dateFormat = "2006-01-02_15:04:05"
	}
	return w
}

// Output outputs the emissions in recs for every timeStep between
// startTime and endTime.
func (w *WRFOutputter) Output(recs []Record, sp *SpatialProcessor, tp *TemporalProcessor, partialMatch bool, startTime, endTime time.Time, timeStep time.Duration) error {
	tstepsInFile := 0
	totals := EmissionsTotal(recs)
	w.newFiles(totals, sp.Grids, startTime)
	if w.files.config.Kemit > 1 {
		w.met = w.files.NewMetData(startTime, tstepsInFile)
	}
	ot := newOutputTimer(startTime, endTime, timeStep)
	for {
		ts, err := EmissionsGriddedAtTime(recs, ot.currentTime, w, sp, tp, partialMatch)
		if err != nil {
			return err
		}
		log.Printf("Writing WRF output for %v...", ot.currentTime)
		if tstepsInFile == w.tstepsPerFile {
			// open new set of files
			w.closeFiles()
			w.newFiles(totals, sp.Grids, ot.currentTime)
			tstepsInFile = 0
		}
		w.files.writeTimestep(ts, ot.currentTime, tstepsInFile)
		if w.files.config.Kemit > 1 {
			w.met.Close()
			w.met = w.files.NewMetData(ot.currentTime, tstepsInFile)
		}
		// either advance to next date or end loop
		keepGoing := ot.NextTime()
		if !keepGoing {
			break
		}
		tstepsInFile++
	}
	w.closeFiles()
	log.Printf("Finished writing WRF output.")
	return nil
}

type wrfFiles struct {
	fids         []*cdf.File
	fidsToClose  []*os.File
	config       *WRFconfigData
	polsAndUnits map[Pollutant]*unit.Unit
	oldWRFout    string
	grids        []*GridDef
}

func (w *WRFOutputter) newFiles(totals map[Pollutant]*unit.Unit, grids []*GridDef, date time.Time) {
	var err error
	w.files = new(wrfFiles)
	w.files.grids = grids
	w.files.fids = make([]*cdf.File, w.config.MaxDom)
	w.files.fidsToClose = make([]*os.File, w.config.MaxDom)
	w.files.config = w.config
	w.files.polsAndUnits = totals
	w.files.oldWRFout = w.oldWRFOut
	filename := strings.Replace(w.filebase, "[DATE]", date.Format(w.dateFormat), -1)
	for i, domain := range w.config.DomainNames {
		outfile := strings.Replace(filename, "[DOMAIN]", domain, -1)
		wrfoutH := cdf.NewHeader([]string{"Time", "DateStrLen", "west_east",
			"south_north", "emissions_zdim"},
			[]int{0, 19, w.config.Nx[i], w.config.Ny[i], w.config.Kemit})

		wrfoutH.AddAttribute("", "TITLE", "Anthropogenic emissions created "+
			"by AEP version "+Version+" ("+Website+")")
		wrfoutH.AddAttribute("", "CEN_LAT", []float64{w.config.RefLat})
		wrfoutH.AddAttribute("", "CEN_LOC", []float64{w.config.RefLon})
		wrfoutH.AddAttribute("", "TRUELAT1", []float64{w.config.TrueLat1})
		wrfoutH.AddAttribute("", "TRUELAT2", []float64{w.config.TrueLat2})
		wrfoutH.AddAttribute("", "STAND_LON", []float64{w.config.StandLon})
		wrfoutH.AddAttribute("", "MAP_PROJ", w.config.MapProj)
		wrfoutH.AddAttribute("", "REF_X", []float64{w.config.RefX})
		wrfoutH.AddAttribute("", "REF_Y", []float64{w.config.RefY})

		wrfoutH.AddVariable("Times", []string{"Time", "DateStrLen"}, "")
		// Create variables
		for pol, units := range w.files.polsAndUnits {
			createWRFvar(wrfoutH, "E_"+pol.Name, units.Dimensions().String())
		}

		wrfoutH.Define()
		errs := wrfoutH.Check()
		for _, err := range errs {
			if err != nil {
				panic(err)
			}
		}
		w.files.fidsToClose[i], err = os.Create(outfile)
		if err != nil {
			panic(err)
		}
		w.files.fids[i], err = cdf.Create(w.files.fidsToClose[i], wrfoutH)
		if err != nil {
			panic(err)
		}
	}
}

func (w *WRFOutputter) closeFiles() {
	for _, f := range w.files.fidsToClose {
		err := cdf.UpdateNumRecs(f)
		if err != nil {
			panic(err)
		}
		f.Close()
	}
}

func createWRFvar(h *cdf.Header, name, unitsIn string) {
	dims := []string{"Time", "emissions_zdim", "south_north", "west_east"}
	h.AddVariable(name, dims, []float32{0.})
	var units string
	switch unitsIn {
	case "mol/hour":
		units = "mol km^-2 hr^-1"
	case "g/hour", "gram/hour":
		units = "ug/m3 m/s"
	default:
		panic(fmt.Errorf("Unknown units: %v", unitsIn))
	}
	h.AddAttribute(name, "FieldType", []int32{104})
	h.AddAttribute(name, "MemoryOrder", "XYZ")
	h.AddAttribute(name, "description", "EMISSIONS")
	h.AddAttribute(name, "units", units)
	h.AddAttribute(name, "stagger", "")
	h.AddAttribute(name, "coordinates", "XLONG XLAT")
}

func (w *wrfFiles) writeTimestep(ts map[Pollutant][]*sparse.SparseArray, timestep time.Time, ihr int) {
	var err error
	// Write out time
	for _, f := range w.fids {
		start := []int{ihr, 0}
		end := []int{ihr + 1, 0}
		r := f.Writer("Times", start, end)
		_, err = r.Write(timestep.Format("2006-01-02_15:04:05"))
		if err != nil {
			panic(err)
		}
	}
	for pol := range ts {
		if _, ok := w.polsAndUnits[pol]; !ok {
			panic(fmt.Sprintf("Pollutant %v not in the output file.", pol))
		}
	}
	for pol, units := range w.polsAndUnits {
		if _, ok := ts[pol]; !ok {
			continue
		}
		for i, f := range w.fids {
			var outData *sparse.SparseArray
			// convert units
			switch units.Dimensions().String() {
			case "mol/hour":
				// gas conversion mole/hr --> mole/km(2)/hr
				gasconv := float64(1. / (1.e-3 * w.config.Dx[i] *
					1.e-3 * w.config.Dy[i]))
				outData = ts[pol][i].ScaleCopy(gasconv)
			case "g/hour", "gram/hour":
				// aerosol conversion g/hr --> microgram/m(2)/sec
				partconv := float64(1.e6 / w.config.Dx[i] /
					w.config.Dy[i] / 3600.)
				outData = ts[pol][i].ScaleCopy(partconv)
			default:
				panic(fmt.Errorf("Can't handle units `%v'.", units))
			}

			start := []int{ihr, 0, 0, 0}
			end := []int{ihr + 1, 0, 0, 0}
			r := f.Writer("E_"+pol.Name, start, end)
			if _, err = r.Write(outData.ToDense32()); err != nil {
				panic(err)
			}
		}
	}
}

// WRFconfigData hold information about a WRF simulation configuration.
type WRFconfigData struct {
	MaxDom             int
	ParentID           []int
	ParentGridRatio    []float64
	IParentStart       []int
	JParentStart       []int
	EWE                []int
	ESN                []int
	Dx0                float64
	Dy0                float64
	MapProj            string
	RefLat             float64
	RefLon             float64
	TrueLat1           float64
	TrueLat2           float64
	StandLon           float64
	RefX               float64
	RefY               float64
	S                  []float64
	W                  []float64
	Dx                 []float64
	Dy                 []float64
	Nx                 []int
	Ny                 []int
	DomainNames        []string
	FramesPerAuxInput5 []int
	Kemit              int
	Nocolons           bool
	sr                 *proj.SR
}

// ParseWRFConfig extracts configuration information from a set of WRF namelists.
func ParseWRFConfig(wpsnamelist, wrfnamelist string) (d *WRFconfigData, err error) {
	e := new(wrfErrCat)
	d = new(WRFconfigData)
	d.parseWPSnamelist(wpsnamelist, e)
	d.parseWRFnamelist(wrfnamelist, e)
	d.projection(e)
	err = e.convertToError()
	return
}

// projection calculates the spatial projection of a WRF configuration.
func (d *WRFconfigData) projection(e *wrfErrCat) {
	const EarthRadius = 6370997.

	var mapProj string
	switch d.MapProj {
	case "lambert":
		mapProj = "lcc"
	case "lat-lon":
		mapProj = "longlat"
	case "merc":
		mapProj = "merc"
	default:
		e.Add(fmt.Errorf("ERROR: `lambert', `lat-lon', and `merc' "+
			"are the only map projections"+
			" that are currently supported (your projection is `%v').",
			d.MapProj))
	}
	d.sr = proj.NewSR()
	d.sr.Name = mapProj
	d.sr.Lat1 = d.TrueLat1
	d.sr.Lat2 = d.TrueLat2
	d.sr.Lat0 = d.RefLat
	d.sr.Long0 = d.RefLon
	d.sr.A = EarthRadius
	d.sr.B = EarthRadius
	d.sr.ToMeter = 1.
	d.sr.DeriveConstants()
}

// Grids creates grid definitions for the grids in WRF configuration d,
// where tzFile is a shapefile containing timezone information, and tzColumn
// is the data attribute column within that shapefile that contains the
// timezone offsets in hours.
func (d *WRFconfigData) Grids() []*GridDef {
	grids := make([]*GridDef, d.MaxDom)
	for i := 0; i < d.MaxDom; i++ {
		grids[i] = NewGridRegular(d.DomainNames[i], d.Nx[i], d.Ny[i],
			d.Dx[i], d.Dy[i], d.W[i], d.S[i], d.sr)
	}
	return grids
}

// Parse a WPS namelist
func (d *WRFconfigData) parseWPSnamelist(filename string, e *wrfErrCat) {
	file, err := os.Open(filename)
	if err != nil {
		e.Add(err)
		return
	}
	includesRefx := false
	includesRefy := false
	f := bufio.NewReader(file)
	for {
		line, err := f.ReadString('\n')
		if err != nil {
			if err != io.EOF {
				e.Add(err)
				break
			} else {
				break
			}
		}
		i := strings.Index(line, "=")
		if i != -1 {
			name := strings.Trim(line[:i], " ,")
			val := strings.Trim(line[i+1:], " ,\n")
			switch name {
			case "max_dom":
				d.MaxDom = namelistInt(val)
			case "map_proj":
				d.MapProj = strings.Trim(val, " '")
			case "ref_lat":
				d.RefLat = namelistFloat(val)
			case "ref_lon":
				d.RefLon = namelistFloat(val)
			case "truelat1":
				d.TrueLat1 = namelistFloat(val)
			case "truelat2":
				d.TrueLat2 = namelistFloat(val)
			case "stand_lon":
				d.StandLon = namelistFloat(val)
			case "ref_x":
				d.RefX = namelistFloat(val)
				includesRefx = true
			case "ref_y":
				d.RefY = namelistFloat(val)
				includesRefy = true
			case "parent_id":
				d.ParentID = namelistIntList(val)
			case "parent_grid_ratio":
				d.ParentGridRatio = namelistFloatList(val)
			case "i_parent_start":
				d.IParentStart = namelistIntList(val)
			case "j_parent_start":
				d.JParentStart = namelistIntList(val)
			case "e_we":
				d.EWE = namelistIntList(val)
			case "e_sn":
				d.ESN = namelistIntList(val)
			case "dx":
				d.Dx0 = namelistFloat(val)
			case "dy":
				d.Dy0 = namelistFloat(val)
			}
		}
	}
	if !includesRefx {
		d.RefX = float64(d.EWE[0]) / 2.
	}
	if !includesRefy {
		d.RefY = float64(d.ESN[0]) / 2.
	}
	d.S = make([]float64, d.MaxDom)
	d.W = make([]float64, d.MaxDom)
	switch d.MapProj {
	case "lat-lon":
		d.S[0] = d.RefLat - (d.RefY-0.5)*d.Dy0
		d.W[0] = d.RefLon - (d.RefX-0.5)*d.Dx0
	default:
		d.S[0] = 0 - (d.RefY-0.5)*d.Dy0
		d.W[0] = 0 - (d.RefX-0.5)*d.Dx0
	}
	d.Dx = make([]float64, d.MaxDom)
	d.Dy = make([]float64, d.MaxDom)
	d.Dx[0] = d.Dx0
	d.Dy[0] = d.Dy0
	d.Nx = make([]int, d.MaxDom)
	d.Ny = make([]int, d.MaxDom)
	d.Nx[0] = d.EWE[0] - 1
	d.Ny[0] = d.ESN[0] - 1
	d.DomainNames = make([]string, d.MaxDom)
	for i := 0; i < d.MaxDom; i++ {
		parentID := d.ParentID[i] - 1
		d.DomainNames[i] = fmt.Sprintf("d%02v", i+1)
		d.S[i] = d.S[parentID] +
			float64(d.JParentStart[i]-1)*d.Dy[parentID]
		d.W[i] = d.W[parentID] +
			float64(d.IParentStart[i]-1)*d.Dx[parentID]
		d.Dx[i] = d.Dx[parentID] /
			d.ParentGridRatio[i]
		d.Dy[i] = d.Dy[parentID] /
			d.ParentGridRatio[i]
		d.Nx[i] = d.EWE[i] - 1
		d.Ny[i] = d.ESN[i] - 1
	}
}

// Parse a WRF namelist
func (d *WRFconfigData) parseWRFnamelist(filename string, e *wrfErrCat) {
	file, err := os.Open(filename)
	if err != nil {
		return
	}
	f := bufio.NewReader(file)
	for {
		line, err := f.ReadString('\n')
		if err != nil {
			if err != io.EOF {
				panic(err)
			} else {
				break
			}
		}
		i := strings.Index(line, "=")
		if i != -1 {
			name := strings.Trim(line[:i], " ,")
			val := strings.Trim(line[i+1:], " ,\n")
			switch name {
			case "max_dom":
				e.compare(d.MaxDom, namelistInt(val), name)
			case "parent_id":
				e.compare(d.ParentID, namelistIntList(val), name)
			case "parent_grid_ratio":
				e.compare(d.ParentGridRatio, namelistFloatList(val), name)
			case "i_parent_start":
				e.compare(d.IParentStart, namelistIntList(val), name)
			case "j_parent_start":
				e.compare(d.JParentStart, namelistIntList(val), name)
			case "e_we":
				e.compare(d.EWE, namelistIntList(val), name)
			case "e_sn":
				e.compare(d.ESN, namelistIntList(val), name)
			case "dx":
				e.compare(d.Dx0, namelistFloatList(val)[0], name)
			case "dy":
				e.compare(d.Dy0, namelistFloatList(val)[0], name)
			case "frames_per_auxinput5":
				// Interval will be 60 minutes regardless of input file
				// All domains will have the same number of frames per file
				d.FramesPerAuxInput5 = namelistIntList(val)
			case "kemit":
				d.Kemit = namelistInt(val)
			case "nocolons":
				d.Nocolons = namelistBool(val)
			}
		}
	}
}

func namelistInt(str string) (out int) {
	out, err := strconv.Atoi(strings.Trim(str, " "))
	if err != nil {
		panic(err)
	}
	return
}
func namelistIntList(str string) (out []int) {
	out = make([]int, 0)
	for _, ival := range strings.Split(str, ",") {
		xval, err := strconv.Atoi(strings.Trim(ival, " "))
		if err != nil {
			panic(err)
		}
		out = append(out, xval)
	}
	return
}
func namelistFloat(str string) (out float64) {
	out, err := strconv.ParseFloat(strings.Trim(str, " "), 64)
	if err != nil {
		panic(err)
	}
	return
}
func namelistFloatList(str string) (out []float64) {
	out = make([]float64, 0)
	for _, ival := range strings.Split(str, ",") {
		xval, err := strconv.ParseFloat(strings.Trim(ival, " "), 64)
		if err != nil {
			panic(err)
		}
		out = append(out, xval)
	}
	return
}
func namelistBool(str string) (out bool) {
	out, err := strconv.ParseBool(strings.Trim(str, " ."))
	if err != nil {
		panic(err)
	}
	return
}

// The ErrCat type and methods collect errors while the program is running
// and then print them later so that all errors can be seen and fixed at once,
// instead of just the first one.
type wrfErrCat struct {
	str string
}

func (e *wrfErrCat) Add(err error) {
	if err != nil && strings.Index(e.str, err.Error()) == -1 {
		e.str += err.Error() + "\n"
	}
	return
}
func (e *wrfErrCat) convertToError() error {
	if e.str != "" {
		return errors.New(e.str)
	}
	return nil
}

func (e *wrfErrCat) compare(val1, val2 interface{}, name string) {
	errFlag := false
	switch val1.(type) {
	case int:
		if val1.(int) != val2.(int) {
			errFlag = true
		}
	case float64:
		errFlag = floatcompare(val1.(float64), val2.(float64))
	case []int:
		for i := 0; i < min(len(val1.([]int)), len(val2.([]int))); i++ {
			if val1.([]int)[i] != val2.([]int)[i] {
				errFlag = true
				break
			}
		}
	case []float64:
		for i := 0; i < min(len(val1.([]float64)), len(val2.([]float64))); i++ {
			if floatcompare(val1.([]float64)[i], val2.([]float64)[i]) {
				errFlag = true
				break
			}
		}
	case string:
		if val1.(string) != val2.(string) {
			errFlag = true
		}
	default:
		panic("Unknown type")
	}
	if errFlag {
		e.Add(fmt.Errorf("WRF variable mismatch for %v, WPS namelist=%v; "+
			"WRF namelist=%v.", name, val1, val2))
	}
}

func floatcompare(val1, val2 float64) bool {
	return math.Abs((val1-val2)/val2) > 1.e-8
}

func min(val1, val2 int) int {
	if val1 > val2 {
		return val2
	}
	return val1
}

// MetData is a WRF meteorology data holder.
type MetData struct {
	wrfout       []*cdf.File
	fid          []*os.File
	LayerHeights [][][][]float32 // [grid][i][j][k]
	Uspd         [][][][]float32 // [grid][i][j][k]
	Temp         [][][][]float32 // temperature
	S1           [][][][]float32 // stability parameter
	Sclass       [][][][]string  // Stability class
	h            int             // file record index
	Kemit        int             // number of levels in emissions file
	grids        []*GridDef
}

// This assumes that the wrfout and wrfchemi files each have 24 frames in
// one hour increments
func (w *wrfFiles) NewMetData(date time.Time, timeIndex int) *MetData {
	var err error
	m := new(MetData)
	m.grids = w.grids
	m.h = timeIndex
	m.wrfout = make([]*cdf.File, len(m.grids))
	m.fid = make([]*os.File, len(m.grids))
	m.Kemit = w.config.Kemit
	// Open old wrfout files
	var WRFdateFormat string
	if w.config.Nocolons == true {
		WRFdateFormat = "2006-01-02_00_00_00"
	} else {
		WRFdateFormat = "2006-01-02_00:00:00"
	}
	filename := strings.Replace(w.oldWRFout, "[DATE]",
		date.Format(WRFdateFormat), -1)
	for i, grid := range m.grids {
		file2 := strings.Replace(filename, "[DOMAIN]", grid.Name, -1)
		m.fid[i], err = os.Open(file2)
		if err != nil {
			panic(err)
		}
		m.wrfout[i], err = cdf.Open(m.fid[i])
		if err != nil {
			panic(err)
		}
	}
	// Elevation at grid cell top (m)
	m.layerHeights()
	// horizontal wind speeds
	m.windSpeed()
	// Calculate temperature and stability parameters
	m.temp()
	return m
}

// Close Closes the files associated with the receiver.
func (m *MetData) Close() {
	for _, f := range m.fid {
		f.Close()
	}
}

// PlumeRise calculation, ASME (1973), as described in Sienfeld and Pandis,
// ``Atmospheric Chemistry and Physics - From Air Pollution to Climate Change
// Uses meteorology from WRF output from a previous run.
// It returns the layer index of the plume.
func (w *WRFOutputter) PlumeRise(point PointSource, sp *SpatialProcessor, gi int) (int, error) {
	if w.files.config.Kemit == 1 {
		return 0, nil
	}
	srg, _, inGrid, err := point.Spatialize(sp, gi)
	if err != nil {
		return -1, err
	}
	if !inGrid {
		return 0, nil
	}

	index := srg.IndexNd(srg.Nonzero()[0])
	j, i := index[0], index[1]

	// deal with points that are inside one grid but not inside the others
	if j >= w.files.grids[gi].Nx || i >= w.files.grids[gi].Ny || j < 0 || i < 0 {
		return 0, nil
	}
	pointData := point.PointData()
	stackHeight := math.Max(0, pointData.StackHeight.Value()) // m
	// Find K level of stack
	var stackLayer int
	for w.met.LayerHeights[gi][j][i][stackLayer+1] < float32(stackHeight) {
		if stackLayer > w.met.Kemit {
			msg := "stack height > top of emissions file"
			panic(msg)
		}
		stackLayer++
	}

	// Make sure all parameters are reasonable values
	airTemp := float64(w.met.Temp[gi][j][i][stackLayer])                     // K
	windSpd := math.Max(float64(w.met.Uspd[gi][j][i][stackLayer]), 1.)       // m/s, small numbers cause problems
	stackVel := math.Max(0., math.Min(pointData.StackVelocity.Value(), 40.)) // m/s
	stackDiam := math.Max(0, pointData.StackDiameter.Value())                // m
	stackTemp := math.Max(pointData.StackTemp.Value(), airTemp+10.)          // K

	////////////////////////////////////////////////////////////////////////////
	// Plume rise calculation, ASME (1973), as described in Sienfeld and Pandis,
	// ``Atmospheric Chemistry and Physics - From Air Pollution to Climate Change

	var deltaH float64 // Plume rise, (m).
	var calcType string
	if (stackTemp-airTemp) < 50. &&
		stackVel > windSpd && stackVel > 10. {
		// Plume is dominated by momentum forces
		calcType = "Momentum"

		deltaH = stackDiam * math.Pow(stackVel, 1.4) / math.Pow(windSpd, 1.4)

	} else { // Plume is dominated by buoyancy forces

		// Bouyancy flux, m4/s3
		F := g * (stackTemp - airTemp) / stackTemp * stackVel *
			math.Pow(stackDiam/2, 2)

		if w.met.Sclass[gi][j][i][stackLayer] == "S" { // stable conditions
			calcType = "Stable"

			deltaH = 29. * math.Pow(
				F/float64(w.met.S1[gi][j][i][stackLayer]), 0.333333333) /
				math.Pow(windSpd, 0.333333333)

		} else { // unstable conditions
			calcType = "Unstable"

			deltaH = 7.4 * math.Pow(F*math.Pow(stackHeight, 2.),
				0.333333333) / windSpd

		}
	}
	if math.IsNaN(deltaH) {
		msg := "plume height == NaN\n" +
			fmt.Sprintf("calcType: %v, deltaH: %v, stackDiam: %v,\n",
				calcType, deltaH, stackDiam) +
			fmt.Sprintf("stackVel: %v, windSpd: %v, stackTemp: %v,\n",
				stackVel, windSpd, stackTemp) +
			fmt.Sprintf("airTemp: %v, stackHeight: %v\n", airTemp, stackHeight)
		panic(msg)
	}

	/////////////////////////////////////////////////////////////////////////////

	plumeHeight := stackHeight + deltaH

	var layer int
	// Find K level of plume
	for layer = 0; w.met.LayerHeights[gi][j][i][layer+1] < float32(plumeHeight); layer++ {
		if layer >= w.met.Kemit-1 {
			layer = w.met.Kemit - 2
			break
		}
	}
	return layer, err
}

// Layer heights above ground level. For more information, refer to
// http://www.openwfm.org/wiki/How_to_interpret_WRF_variables
func (m *MetData) layerHeights() {

	m.LayerHeights = make([][][][]float32, len(m.wrfout))
	for fi, f := range m.wrfout {
		nx := int(f.Header.GetAttribute("",
			"WEST-EAST_PATCH_END_UNSTAG").([]int32)[0])
		ny := int(f.Header.GetAttribute("",
			"SOUTH-NORTH_PATCH_END_UNSTAG").([]int32)[0])
		nlay := int(f.Header.GetAttribute("",
			"BOTTOM-TOP_PATCH_END_STAG").([]int32)[0]) // number of WRF layers

		// get the necessary data for calculating layer heights
		dims := []int{24, nx, ny, nlay}
		layerStart := []int{m.h, 0, 0, 0}
		layerEnd := []int{m.h + 1, 0, 0, 0}
		PHB := getVarFloat32(f, "PHB", dims, layerStart, layerEnd)
		PH := getVarFloat32(f, "PH", dims, layerStart, layerEnd)

		m.LayerHeights[fi] = make([][][]float32, ny)
		for j := 0; j < ny; j++ {
			m.LayerHeights[fi][j] = make([][]float32, nx)
			for i := 0; i < nx; i++ {
				m.LayerHeights[fi][j][i] = make([]float32, nlay)
				for k := 0; k < nlay; k++ {
					index := indexTo1d([]int{k, j, i}, []int{nlay, ny, nx})
					zeroIndex := indexTo1d([]int{0, j, i}, []int{nlay, ny, nx})
					m.LayerHeights[fi][j][i][k] = (PH[index] + PHB[index] -
						PH[zeroIndex] - PHB[zeroIndex]) / g // m
				}
			}
		}
	}
	return
}

func (m *MetData) windSpeed() {
	m.Uspd = make([][][][]float32, len(m.wrfout))
	for fi, f := range m.wrfout {
		nxv := int(f.Header.GetAttribute("",
			"WEST-EAST_PATCH_END_UNSTAG").([]int32)[0])
		nxu := int(f.Header.GetAttribute("",
			"WEST-EAST_PATCH_END_STAG").([]int32)[0])
		nyu := int(f.Header.GetAttribute("",
			"SOUTH-NORTH_PATCH_END_UNSTAG").([]int32)[0])
		nyv := int(f.Header.GetAttribute("",
			"SOUTH-NORTH_PATCH_END_STAG").([]int32)[0])
		nlay := int(f.Header.GetAttribute("",
			"BOTTOM-TOP_PATCH_END_UNSTAG").([]int32)[0]) // number of WRF layers

		dimsU := []int{24, nxu, nyu, nlay}
		layerStart := []int{m.h, 0, 0, 0}
		layerEnd := []int{m.h + 1, 0, 0, 0}
		U := getVarFloat32(f, "U", dimsU, layerStart, layerEnd) // m2/s2
		dimsV := []int{24, nxv, nyv, nlay}
		layerEnd = []int{m.h + 1, 0, 0, 0}
		V := getVarFloat32(f, "V", dimsV, layerStart, layerEnd) // m2/s2

		m.Uspd[fi] = make([][][]float32, nyu)
		for j := 1; j < nyv; j++ {
			m.Uspd[fi][j-1] = make([][]float32, nxv)
			for i := 1; i < nxu; i++ {
				m.Uspd[fi][j-1][i-1] = make([]float32, nlay)
				for k := 0; k < nlay; k++ {
					rightIndex := indexTo1d([]int{k, j - 1, i},
						[]int{nlay, nyu, nxu})
					leftIndex := indexTo1d([]int{k, j - 1, i - 1},
						[]int{nlay, nyu, nxu})
					topIndex := indexTo1d([]int{k, j, i - 1},
						[]int{nlay, nyv, nxv})
					downIndex := indexTo1d([]int{k, j - 1, i - 1},
						[]int{nlay, nyv, nxv})

					ucenter := float64(U[rightIndex]+U[leftIndex]) / 2.
					vcenter := float64(V[topIndex]+V[downIndex]) / 2.
					m.Uspd[fi][j-1][i-1][k] = float32(math.Pow(math.Pow(ucenter, 2.)+
						math.Pow(vcenter, 2.), 0.5))
				}
			}
		}
	}
	return
}

func (m *MetData) temp() {
	m.Temp = make([][][][]float32, len(m.wrfout))
	m.S1 = make([][][][]float32, len(m.wrfout))
	m.Sclass = make([][][][]string, len(m.wrfout))
	for fi, f := range m.wrfout {
		nx := int(f.Header.GetAttribute("",
			"WEST-EAST_PATCH_END_UNSTAG").([]int32)[0])
		ny := int(f.Header.GetAttribute("",
			"SOUTH-NORTH_PATCH_END_UNSTAG").([]int32)[0])
		nlay := int(f.Header.GetAttribute("",
			"BOTTOM-TOP_PATCH_END_UNSTAG").([]int32)[0]) // number of WRF layers

		dims := []int{24, nx, ny, nlay}
		Start := []int{m.h, 0, 0, 0}
		End := []int{m.h + 1, 0, 0, 0}
		T := getVarFloat32(f, "T", dims, Start, End)   // K
		PB := getVarFloat32(f, "PB", dims, Start, End) // Pa
		P := getVarFloat32(f, "P", dims, Start, End)   // Pa

		m.Temp[fi] = make([][][]float32, ny)
		m.S1[fi] = make([][][]float32, ny)
		m.Sclass[fi] = make([][][]string, ny)
		for j := 0; j < ny; j++ {
			m.Temp[fi][j] = make([][]float32, nx)
			m.S1[fi][j] = make([][]float32, nx)
			m.Sclass[fi][j] = make([][]string, nx)
			for i := 0; i < nx; i++ {
				m.Temp[fi][j][i] = make([]float32, nlay)
				m.S1[fi][j][i] = make([]float32, nlay)
				m.Sclass[fi][j][i] = make([]string, nlay)
				for k := 0; k < nlay; k++ {
					index := indexTo1d([]int{k, j, i}, []int{nlay, ny, nx})

					// potential temperature gradient
					dThetaDz := float32(0.)
					if k > 0 {
						indexbelow := indexTo1d([]int{k - 1, j, i},
							[]int{nlay, ny, nx})
						dThetaDz = (T[index] - T[indexbelow]) /
							(m.LayerHeights[fi][j][i][k] -
								m.LayerHeights[fi][j][i][k-1]) // K/m
					}

					pressureCorrection := float32(math.Pow(
						float64(P[index]+PB[index])/
							po, kappa))

					// Ambient temperature
					m.Temp[fi][j][i][k] = (T[index] + 300.) *
						pressureCorrection // K

					// Stability parameter
					m.S1[fi][j][i][k] = dThetaDz / m.Temp[fi][j][i][k] *
						pressureCorrection

					// Stability class
					if dThetaDz < 0.005 {
						m.Sclass[fi][j][i][k] = "U"
					} else {
						m.Sclass[fi][j][i][k] = "S"
					}
				}
			}
		}
	}
	return
}

func getVarFloat32(f *cdf.File, v string, dims, begin, end []int) []float32 {
	if !IsStringInArray(f.Header.Variables(), v) {
		panic(fmt.Errorf("Variable %v is not in OldWRFout file", v))
	}
	nRead := indexTo1d(end, dims) - indexTo1d(begin, dims)
	r := f.Reader(v, begin, end)
	buf := make([]float32, nRead)
	_, err := r.Read(buf)
	if err != nil {
		panic(err)
	}
	return buf
}

// Function indexTo1d takes an array of indecies for a
// multi-dimensional array and the dimensions of that array,
// calculates the 1D-array index.
func indexTo1d(index []int, dims []int) (index1d int) {
	for i := 0; i < len(index); i++ {
		mul := 1
		for j := i + 1; j < len(index); j++ {
			mul = mul * dims[j]
		}
		index1d = index1d + index[i]*mul
	}
	return
}
