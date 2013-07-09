package aep

import (
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

type WrfFiles struct {
	fids         []*cdf.File
	fidsToClose  []*os.File
	config       *WRFconfigData
	polsAndUnits map[string]string
}

func (c *WRFconfigData) NewWRFfiles(polsAndUnits map[string]string) *WrfFiles {
	o := new(WrfFiles)
	o.fids = make([]*cdf.File, c.Max_dom)
	o.fidsToClose = make([]*os.File, c.Max_dom)
	o.config = c
	o.polsAndUnits = polsAndUnits
	return o
}

// Create new output WRF files
func (c *WRFconfigData) NewWRFoutput(filebase string, polsAndUnits map[string]string,
	date time.Time) *WrfFiles {
	var err error
	out := c.NewWRFfiles(polsAndUnits)
	var WRFdateFormat string
	if c.Nocolons == true {
		WRFdateFormat = "2006-01-02_15_04_05"
	} else {
		WRFdateFormat = "2006-01-02_15:04:05"
	}
	filename := strings.Replace(filebase, "[DATE]", date.Format(WRFdateFormat), -1)
	for i, domain := range c.DomainNames {
		outfile := strings.Replace(filename, "[DOMAIN]", domain, -1)
		wrfoutH := cdf.NewHeader([]string{"Time", "DateStrLen", "west_east",
			"south_north", "emissions_zdim"},
			[]int{0, 19, c.Nx[i], c.Ny[i], c.Kemit})

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
		// Write out data.
		for pol, units := range w.polsAndUnits {
			for i, f := range w.fids {
				outData := tstep.area[pol][i].Copy()
				for _, point := range tstep.point {
					if vals, ok := point.ANN_EMIS[pol]; ok {
						outData.AddSparse(vals.gridded[i])
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
				end := []int{ihr + 1, w.config.Kemit, w.config.Ny[i],
					w.config.Nx[i]}
				r := f.Writer(pol, start, end)
				if _, err = r.Write(outData.ToDense32()); err != nil {
					panic(err)
				}
			}
		}
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
				e.compare(d.Dx0, namelistFloat(val), name)
			case "dy":
				e.compare(d.Dy0, namelistFloat(val), name)
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
