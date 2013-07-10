package aep

import (
	"bitbucket.org/ctessum/aep/sparse"
	"bufio"
	"code.google.com/p/lvd.go/cdf"
	"errors"
	"fmt"
	"io"
	"math"
	"os"
	"strconv"
	"strings"
	"time"
)

const (
	feetToMeters = 0.3048
	g            = 9.80665 // m/s2
	po           = 101300. // Pa, reference pressure
	kappa        = 0.2854  // related to von karman's constant
)

type WrfFiles struct {
	fids         []*cdf.File
	fidsToClose  []*os.File
	config       *WRFconfigData
	polsAndUnits map[string]string
	oldWRFout    string
}

func (c *RunData) NewWRFfiles(polsAndUnits map[string]string) *WrfFiles {
	o := new(WrfFiles)
	o.fids = make([]*cdf.File, c.wrfData.Max_dom)
	o.fidsToClose = make([]*os.File, c.wrfData.Max_dom)
	o.config = c.wrfData
	o.polsAndUnits = polsAndUnits
	o.oldWRFout = c.OldWRFout
	return o
}

// Create new output WRF files
func (c *RunData) NewWRFoutput(filebase string, polsAndUnits map[string]string,
	date time.Time) *WrfFiles {
	var err error
	out := c.NewWRFfiles(polsAndUnits)
	var WRFdateFormat string
	if c.wrfData.Nocolons == true {
		WRFdateFormat = "2006-01-02_15_04_05"
	} else {
		WRFdateFormat = "2006-01-02_15:04:05"
	}
	filename := strings.Replace(filebase, "[DATE]", date.Format(WRFdateFormat), -1)
	for i, domain := range c.wrfData.DomainNames {
		outfile := strings.Replace(filename, "[DOMAIN]", domain, -1)
		wrfoutH := cdf.NewHeader([]string{"Time", "DateStrLen", "west_east",
			"south_north", "emissions_zdim"},
			[]int{0, 19, c.wrfData.Nx[i], c.wrfData.Ny[i], c.wrfData.Kemit})

		wrfoutH.AddVariable("Times", []string{"Time", "DateStrLen"}, "")
		// Create variables
		for pol, units := range polsAndUnits {
			createVar(wrfoutH, pol, units)
		}

		wrfoutH.Define()
		errs := wrfoutH.Check()
		for _, err := range errs {
			if err != nil {
				panic(err)
			}
		}
		out.fidsToClose[i], err = os.Create(outfile)
		if err != nil {
			panic(err)
		}
		out.fids[i], err = cdf.Create(out.fidsToClose[i], wrfoutH)
		if err != nil {
			panic(err)
		}
	}
	return out
}

func createVar(h *cdf.Header, name, unitsIn string) {
	dims := []string{"Time", "emissions_zdim", "south_north", "west_east"}
	h.AddVariable(name, dims, []float32{0.})
	var units string
	if unitsIn == "mol/year" {
		units = "mol km^-2 hr^-1"
	} else if unitsIn == "g/year" {
		units = "ug/m3 m/s"
	} else {
		panic(fmt.Errorf("Unknown units: %v", unitsIn))
	}
	h.AddAttribute(name, "FieldType", []int32{104})
	h.AddAttribute(name, "MemoryOrder", "XYZ")
	h.AddAttribute(name, "description", "EMISSIONS")
	h.AddAttribute(name, "units", units)
	h.AddAttribute(name, "stagger", "")
	h.AddAttribute(name, "coordinates", "XLONG XLAT")
}

func (w *WrfFiles) WriteTimesteps(tstepchan chan *TimeStep) {
	var err error
	var met *MetData
	ihr := 0
	for tstep := range tstepchan {
		// Write out time
		for _, f := range w.fids {
			start := []int{ihr, 0}
			end := []int{ihr + 1, 19}
			r := f.Writer("Times", start, end)
			_, err = r.Write(tstep.Time.Format("2006-01-02_15:04:05"))
			if err != nil {
				panic(err)
			}
		}
		// prepare plume rise calculator
		if w.config.Kemit > 1 {
			met = w.NewMetData(tstep.Time, ihr)
		}

		// Write out data.
		for pol, units := range w.polsAndUnits {
			for i, f := range w.fids {
				outData := sparse.ZerosSparse(w.config.Nx[i],
					w.config.Ny[i], w.config.Kemit)
				// add area data
				if areaDat, ok := tstep.area[pol]; ok {
					dat := areaDat[i]
					for _, ix := range dat.Nonzero() {
						val := dat.Get1d(ix)
						index := dat.IndexNd(ix)
						outData.AddVal(val, index[1], index[0], 0) // transpose
					}
				}
				// add point data
				for _, point := range tstep.point {
					k := 0
					if w.config.Kemit > 1 {
						k = met.PlumeRise(i, point)
					}
					if vals, ok := point.ANN_EMIS[pol]; ok {
						dat := vals.gridded[i]
						for _, ix := range dat.Nonzero() {
							val := dat.Get1d(ix)
							index := dat.IndexNd(ix)
							outData.AddVal(val, index[1], index[0], k) // transpose
						}
					}
				}

				// convert units
				switch units {
				case "mol/year":
					// gas conversion mole/hr --> mole/km(2)/hr
					gasconv := float64(1. / (1.e-3 * w.config.Dx[i] *
						1.e-3 * w.config.Dy[i]))
					outData.Scale(gasconv)
				case "g/year":
					// aerosol conversion g/hr --> microgram/m(2)/sec
					partconv := float64(1.e6 / w.config.Dx[i] /
						w.config.Dy[i] / 3600.)
					outData.Scale(partconv)
				default:
					panic(fmt.Errorf("Can't handle units `%v'.", units))
				}

				start := []int{ihr, 0, 0, 0}
				end := []int{ihr + 1, 0, 0, 0}
				r := f.Writer(pol, start, end)
				if _, err = r.Write(outData.ToDense32()); err != nil {
					panic(err)
				}
			}
		}
		met.Close()
		ihr++
	}
	for i, _ := range w.fids {
		err = cdf.UpdateNumRecs(w.fidsToClose[i])
		if err != nil {
			panic(err)
		}
		w.fidsToClose[i].Close()
	}
}

type WRFconfigData struct {
	Max_dom              int
	Parent_id            []int
	Parent_grid_ratio    []float64
	I_parent_start       []int
	J_parent_start       []int
	E_we                 []int
	E_sn                 []int
	Dx0                  float64
	Dy0                  float64
	Map_proj             string
	Ref_lat              float64
	Ref_lon              float64
	Truelat1             float64
	Truelat2             float64
	Stand_lon            float64
	Ref_x                float64
	Ref_y                float64
	S                    []float64
	W                    []float64
	Dx                   []float64
	Dy                   []float64
	Nx                   []int
	Ny                   []int
	DomainNames          []string
	Frames_per_auxinput5 []int
	Kemit                int
	Nocolons             bool
}

func ParseWRFConfig(wpsnamelist, wrfnamelist string) (d *WRFconfigData, err error) {
	e := new(errCat)
	d = new(WRFconfigData)
	parseWPSnamelist(wpsnamelist, d, e)
	parseWRFnamelist(wrfnamelist, d, e)
	err = e.convertToError()
	return
}

// Parse a WPS namelist
func parseWPSnamelist(filename string, d *WRFconfigData, e *errCat) {
	file, err := os.Open(filename)
	if err != nil {
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
				d.Max_dom = namelistInt(val)
			case "map_proj":
				d.Map_proj = strings.Trim(val, " '")
			case "ref_lat":
				d.Ref_lat = namelistFloat(val)
			case "ref_lon":
				d.Ref_lon = namelistFloat(val)
			case "truelat1":
				d.Truelat1 = namelistFloat(val)
			case "truelat2":
				d.Truelat2 = namelistFloat(val)
			case "stand_lon":
				d.Stand_lon = namelistFloat(val)
			case "ref_x":
				d.Ref_x = namelistFloat(val)
				includesRefx = true
			case "ref_y":
				d.Ref_y = namelistFloat(val)
				includesRefy = true
			case "parent_id":
				d.Parent_id = namelistIntList(val)
			case "parent_grid_ratio":
				d.Parent_grid_ratio = namelistFloatList(val)
			case "i_parent_start":
				d.I_parent_start = namelistIntList(val)
			case "j_parent_start":
				d.J_parent_start = namelistIntList(val)
			case "e_we":
				d.E_we = namelistIntList(val)
			case "e_sn":
				d.E_sn = namelistIntList(val)
			case "dx":
				d.Dx0 = namelistFloat(val)
			case "dy":
				d.Dy0 = namelistFloat(val)
			}
		}
	}
	if !includesRefx {
		d.Ref_x = float64(d.E_we[0]) / 2.
	}
	if !includesRefy {
		d.Ref_y = float64(d.E_sn[0]) / 2.
	}
	d.S = make([]float64, d.Max_dom)
	d.W = make([]float64, d.Max_dom)
	d.S[0] = 0 - (d.Ref_y-0.5)*d.Dy0
	d.W[0] = 0 - (d.Ref_x-0.5)*d.Dx0
	d.Dx = make([]float64, d.Max_dom)
	d.Dy = make([]float64, d.Max_dom)
	d.Dx[0] = d.Dx0
	d.Dy[0] = d.Dy0
	d.Nx = make([]int, d.Max_dom)
	d.Ny = make([]int, d.Max_dom)
	d.Nx[0] = d.E_we[0] - 1
	d.Ny[0] = d.E_sn[0] - 1
	d.DomainNames = make([]string, d.Max_dom)
	for i := 0; i < d.Max_dom; i++ {
		parentID := d.Parent_id[i] - 1
		d.DomainNames[i] = fmt.Sprintf("d%02v", i+1)
		d.S[i] = d.S[parentID] +
			float64(d.J_parent_start[i]-1)*d.Dy[parentID]
		d.W[i] = d.W[parentID] +
			float64(d.I_parent_start[i]-1)*d.Dx[parentID]
		d.Dx[i] = d.Dx[parentID] /
			d.Parent_grid_ratio[i]
		d.Dy[i] = d.Dy[parentID] /
			d.Parent_grid_ratio[i]
		d.Nx[i] = d.E_we[i] - 1
		d.Ny[i] = d.E_sn[i] - 1
	}
}

// Parse a WRF namelist
func parseWRFnamelist(filename string, d *WRFconfigData, e *errCat) {
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
				e.compare(d.Max_dom, namelistInt(val), name)
			case "parent_id":
				e.compare(d.Parent_id, namelistIntList(val), name)
			case "parent_grid_ratio":
				e.compare(d.Parent_grid_ratio, namelistFloatList(val), name)
			case "i_parent_start":
				e.compare(d.I_parent_start, namelistIntList(val), name)
			case "j_parent_start":
				e.compare(d.J_parent_start, namelistIntList(val), name)
			case "e_we":
				e.compare(d.E_we, namelistIntList(val), name)
			case "e_sn":
				e.compare(d.E_we, namelistIntList(val), name)
			case "dx":
				e.compare(d.Dx0, namelistFloatList(val)[0], name)
			case "dy":
				e.compare(d.Dy0, namelistFloatList(val)[0], name)
			case "frames_per_auxinput5":
				// Interval will be 60 minutes regardless of input file
				// All domains will have the same number of frames per file
				d.Frames_per_auxinput5 = namelistIntList(val)
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
type errCat struct {
	str string
}

func (e *errCat) Add(err error) {
	if err != nil && strings.Index(e.str, err.Error()) == -1 {
		e.str += err.Error() + "\n"
	}
	return
}
func (e *errCat) convertToError() error {
	return errors.New(e.str)
}

func (e *errCat) compare(val1, val2 interface{}, name string) {
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
		e.Add(fmt.Errorf("WRF variable mismatch for %v, WRF namelist=%v; "+
			"WPS namelist=%v.", name, val1, val2))
	}
}

func floatcompare(val1, val2 float64) bool {
	return math.Abs((val1-val2)/val2) > 1.e-8
}

func min(val1, val2 int) int {
	if val1 > val2 {
		return val2
	} else {
		return val1
	}
}

// WRF meteorology data holder
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
}

// This assumes that the wrfout files each have 24 frames in
// one hour increments
func (w *WrfFiles) NewMetData(date time.Time, timeIndex int) *MetData {
	var err error
	m := new(MetData)
	m.h = timeIndex
	m.wrfout = make([]*cdf.File, len(grids))
	m.fid = make([]*os.File, len(grids))
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
	for i, grid := range grids {
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

func (m *MetData) Close() {
	for _, f := range m.fid {
		f.Close()
	}
}

// Plume rise calculation, ASME (1973), as described in Sienfeld and Pandis,
// ``Atmospheric Chemistry and Physics - From Air Pollution to Climate Change
// Uses meteorology from WRF output from a previous run.
func (m *MetData) PlumeRise(gridIndex int, point *PointRecord) (kPlume int) {
	gi := gridIndex
	i := point.Col[gi]
	j := point.Row[gi]

	// deal with points that are inside one grid but not inside the others
	if j >= grids[gi].Nx || i >= grids[gi].Ny || j < 0 || i < 0 {
		return
	}
	stackHeight := point.STKHGT * feetToMeters // m
	// Find K level of stack
	kStak := 0
	for m.LayerHeights[gi][j][i][kStak+1] < float32(stackHeight) {
		if kStak > m.Kemit {
			msg := "stack height > top of emissions file"
			panic(msg)
		}
		kStak++
	}

	// Make sure all parameters are reasonable values
	airTemp := float64(m.Temp[gi][j][i][kStak])                       // K
	windSpd := math.Max(float64(m.Uspd[gi][j][i][kStak]), 1.)         // m/s, small numbers cause problems
	stackVel := math.Min(point.STKVEL*feetToMeters, 40.)              // m/s
	stackDiam := point.STKDIAM * feetToMeters                         // m
	stackTemp := math.Max((point.STKTEMP-32)/1.8+273.15, airTemp+10.) // K

	////////////////////////////////////////////////////////////////////////////
	// Plume rise calculation, ASME (1973), as described in Sienfeld and Pandis,
	// ``Atmospheric Chemistry and Physics - From Air Pollution to Climate Change

	deltaH := 0. // Plume rise, (m).
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

		if m.Sclass[gi][j][i][kStak] == "S" { // stable conditions
			calcType = "Stable"

			deltaH = 29. * math.Pow(
				F/float64(m.S1[gi][j][i][kStak]), 0.333333333) /
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

	// Find K level of plume
	for kPlume := 0; m.LayerHeights[gi][j][i][kPlume+1] < float32(plumeHeight); kPlume++ {
		if kPlume > m.Kemit {
			break
		}
	}
	return
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
					kIndex := indexTo1d([]int{k, j, i}, []int{nlay, ny, nx})
					zeroIndex := indexTo1d([]int{0, j, i}, []int{nlay, ny, nx})
					m.LayerHeights[fi][j][i][k] = (PH[kIndex] + PHB[kIndex] -
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
					dtheta_dz := float32(0.)
					if k > 0 {
						indexbelow := indexTo1d([]int{k - 1, j, i},
							[]int{nlay, ny, nx})
						dtheta_dz = (T[index] - T[indexbelow]) /
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
					m.S1[fi][j][i][k] = dtheta_dz / m.Temp[fi][j][i][k] *
						pressureCorrection

					// Stability class
					if dtheta_dz < 0.005 {
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
