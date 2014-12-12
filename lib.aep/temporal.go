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
	"encoding/csv"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"

	"bitbucket.org/ctessum/sparse"
)

type TemporalProcessor struct {
	c              *Context
	sectors        map[string]map[string]*temporalSector // map[sector][period]data
	temporalReport *TemporalReport
	Units          map[string]string // map[pol]units
	mu             sync.RWMutex
}

// Read in data and start up subroutines for temporal processing.
func (c *Context) NewTemporalProcessor() *TemporalProcessor {
	tp := new(TemporalProcessor)
	tp.c = c
	tp.sectors = make(map[string]map[string]*temporalSector)
	tp.Units = make(map[string]string)
	tp.temporalReport = newTemporalReport(c, tp.Units)
	reportMx.Lock()
	Report.TemporalResults = tp.temporalReport
	reportMx.Unlock()
	return tp
}

type temporalSector struct {
	c             *Context
	tp            *TemporalProcessor
	monthlyTpro   map[string][]float64 // map[code]vals
	weeklyTpro    map[string][]float64 // map[code]vals
	weekdayTpro   map[string][]float64 // map[code]vals
	weekendTpro   map[string][]float64 // map[code]vals
	temporalRef   map[string]map[string]interface{}
	holidays      map[string]string
	cemArray      map[[2]string]map[string]*cemData // map[id,boiler][time]data
	cemSum        map[[2]string]*cemSum             // map[id,boiler]data
	mu            sync.RWMutex
	InputChan     chan *ParsedRecord
	OutputChan    chan *ParsedRecord
	period        string
	PointData     map[[3]string][]*PointRecord
	AreaData      map[[3]string]map[string][]*sparse.SparseArray
	aggregate     func(*temporalSector, *ParsedRecord)
	addEmisAtTime func(*temporalSector, time.Time,
		map[string][]*sparse.SparseArray, []*PointRecord) (
		map[string][]*sparse.SparseArray, []*PointRecord)
}

func (tp *TemporalProcessor) NewSector(c *Context,
	InputChan, OutputChan chan *ParsedRecord, period string) {
	t := new(temporalSector)
	t.mu.Lock()
	tp.mu.Lock()
	if _, ok := tp.sectors[c.Sector]; !ok {
		tp.sectors[c.Sector] = make(map[string]*temporalSector)
	}
	tp.sectors[c.Sector][period] = t
	tp.mu.Unlock()
	t.tp = tp
	t.InputChan = InputChan
	t.OutputChan = OutputChan
	t.period = period
	t.c = c
	t.PointData = make(map[[3]string][]*PointRecord)
	t.AreaData = make(map[[3]string]map[string][]*sparse.SparseArray)
	// Choose which temporal aggregation function to use.
	switch t.c.SectorType {
	case "point":
		t.aggregate = aggregatePoint
		if t.c.InventoryFreq == "cem" {
			t.addEmisAtTime = addEmisAtTimeCEM
		} else {
			t.addEmisAtTime = addEmisAtTimeTproPoint
		}
	case "area", "mobile":
		t.aggregate = aggregateArea
		t.addEmisAtTime = addEmisAtTimeTproArea
	default:
		err := fmt.Errorf("Unknown sectorType %v", c.SectorType)
		panic(err)
	}
	//e.Add(t.getHolidays())
	//e.Add(t.getTemporalRef())
	//e.Add(t.getTemporalPro())
	err := t.getHolidays()
	if err != nil {
		panic(err)
	}
	err = t.getTemporalRef()
	if err != nil {
		panic(err)
	}
	err = t.getTemporalPro()
	if err != nil {
		panic(err)
	}

	go func() {
		defer t.c.ErrorRecoverCloseChan(t.InputChan)
		if t.c.InventoryFreq == "cem" {
			t.getCEMdata()
		}
		t.c.Log("Aggregating by temporal profile "+t.period+" "+t.c.Sector+"...", 1)
		for record := range InputChan {
			t.aggregate(t, record)
			t.OutputChan <- record
		}
		close(t.OutputChan)
		t.c.msgchan <- "Finished temporalizing " + t.period + " " + t.c.Sector
		t.mu.Unlock()
	}()
}

// temporalRef reads the SMOKE tref file, which maps FIPS and SCC
// codes to grid surrogates. Although the tref file allows the
// specification of code by pollutant name, that functionality is
// not included here.
func (t *temporalSector) getTemporalRef() (err error) {
	t.temporalRef = make(map[string]map[string]interface{})
	var record string
	fid, err := os.Open(t.c.TemporalRefFile)
	if err != nil {
		err = fmt.Errorf("termporalRef: %v \nFile= %v\nRecord= ",
			err.Error(), t.c.TemporalRefFile, record)
		return
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
				err = fmt.Errorf("TemporalRef: %v \nFile= %v\nRecord= ",
					err.Error(), t.c.TemporalRefFile, record)
				return
			}
		}
		// Get rid of comments at end of line.
		if i := strings.Index(record, "!"); i != -1 {
			record = record[0:i]
		}

		if record[0] != '#' && record[0] != '\n' && record[0] != '/' {
			splitLine := strings.Split(record, ";")
			SCC := splitLine[0]
			if len(SCC) == 0 {
				SCC = "0000000000"
			} else if len(SCC) == 8 {
				SCC = "00" + SCC
			}
			monthCode := splitLine[1]
			weekCode := splitLine[2]
			diurnalCode := splitLine[3]
			FIPS := splitLine[5]
			if len(FIPS) == 0 {
				FIPS = "00000"
			} else if len(FIPS) == 6 {
				FIPS = FIPS[1:]
			} else if len(FIPS) != 5 {
				return fmt.Errorf("in TemporalRef, record %v FIPS %v has "+
					"wrong number of digits", record, FIPS)
			}

			if _, ok := t.temporalRef[SCC]; !ok {
				t.temporalRef[SCC] = make(map[string]interface{})
			}
			t.temporalRef[SCC][FIPS] = [3]string{
				monthCode, weekCode, diurnalCode}
		}
	}
	return
}

// get decimal number of weeks in the current month.
func weeksInMonth(t time.Time) float64 {
	t2 := time.Date(t.Year(), t.Month(), 32, 0, 0, 0, 0, time.UTC)
	return (32. - float64(t2.Day())) / 7.
}

const holidayFormat = "20060102"

func (t *temporalSector) getHolidays() error {
	t.holidays = make(map[string]string)
	fid, err := os.Open(t.c.HolidayFile)
	if err != nil {
		return fmt.Errorf("Holidays: %v \nFile= %v",
			err.Error(), t.c.HolidayFile)
	} else {
		defer fid.Close()
	}
	scanner := bufio.NewScanner(fid)
	for scanner.Scan() {
		line := scanner.Text()
		if len(line) < 19 || line[0] == '#' {
			continue
		}
		holiday, err := time.Parse("01 02 2006", line[8:18])
		if err != nil {
			return fmt.Errorf("Holidays: %v \nFile= %v",
				err.Error(), t.c.HolidayFile)
		}
		if holiday.After(t.c.startDate) && holiday.Before(t.c.endDate) {
			t.holidays[holiday.Format(holidayFormat)] = ""
		}
	}
	if err = scanner.Err(); err != nil {
		return fmt.Errorf("Holidays: %v \nFile= %v",
			err.Error(), t.c.HolidayFile)
	}
	return nil
}

func (t *temporalSector) getTemporalPro() error {
	t.monthlyTpro = make(map[string][]float64) // map[code]vals
	t.weeklyTpro = make(map[string][]float64)  // map[code]vals
	t.weekdayTpro = make(map[string][]float64) // map[code]vals
	t.weekendTpro = make(map[string][]float64) // map[code]vals
	fid, err := os.Open(t.c.TemporalProFile)
	if err != nil {
		return fmt.Errorf("temporalPro: %v \nFile= %v",
			err.Error(), t.c.TemporalRefFile)
	} else {
		defer fid.Close()
	}
	scanner := bufio.NewScanner(fid)
	tType := ""
	// read in Tpro file
	for scanner.Scan() {
		line := scanner.Text()
		if line[0] == '#' {
			continue
		}
		if line[0] == '/' {
			tType = strings.ToLower(strings.Trim(line, "/ "))
			continue
		}
		switch tType {
		case "monthly":
			code, pro, err := parseTproLine(line, 12)
			if err != nil {
				return err
			}
			t.monthlyTpro[code] = pro
		case "weekly":
			code, pro, err := parseTproLine(line, 7)
			if err != nil {
				return err
			}
			t.weeklyTpro[code] = pro
		case "diurnal weekday":
			code, pro, err := parseTproLine(line, 24)
			if err != nil {
				return err
			}
			t.weekdayTpro[code] = pro
		case "diurnal weekend":
			code, pro, err := parseTproLine(line, 24)
			if err != nil {
				return err
			}
			t.weekendTpro[code] = pro
		default:
			err = fmt.Errorf("In %v: Unknown temporal type %v.",
				t.c.TemporalProFile, tType)
			return err
		}
	}
	if err = scanner.Err(); err != nil {
		return fmt.Errorf("TemporalPro: %v \nFile= %v",
			err.Error(), t.c.TemporalRefFile)
	}
	return nil
}

func parseTproLine(line string, n int) (code string, pro []float64, err error) {
	code = strings.TrimSpace(line[0:5])
	pro = make([]float64, n)
	j := 0
	total := 0.
	for i := 5; i < 4*n+5; i += 4 {
		pro[j], err = strconv.ParseFloat(strings.TrimSpace(line[i:i+4]), 64)
		if err != nil {
			return
		}
		total += pro[j]
		j++
	}
	for i := 0; i < n; i++ {
		pro[i] /= total
	}
	return
}

type cemData struct {
	// ORISID string // DOE Plant ID (required) (should match the same field in
	// the PTINV file in IDA format)
	// BLRID string // Boiler Identification Code (required) (should match the
	// same field in the PTINV file in IDA format)
	// YYMMDD  int     // Date of data in YYMMDD format (required): NO DAYLIGHT SAVINGS
	// HOUR    int     // Hour value from 0 to 23
	NOXMASS float32 // Nitrogen oxide emissions (lb/hr) (required)
	SO2MASS float32 // Sulfur dioxide emissions (lb/hr) (required)
	// NOXRATE float64 // Nitrogen oxide emissions rate (lb/MMBtu) (not used by SMOKE)
	// OPTIME         float64 // Fraction of hour unit was operating (optional)
	GLOAD   float32 // Gross load (MW) (optional)
	SLOAD   float32 // Steam load (1000 lbs/hr) (optional)
	HTINPUT float32 // Heat input (mmBtu) (required)
	// HTINPUTMEASURE string  // Code number indicating measured or substituted, not used by SMOKE.
	// SO2MEASURE     string  // Code number indicating measured or substituted, not used by SMOKE.
	// NOXMMEASURE    string  // Code number indicating measured or substituted, not used
	// by SMOKE.
	// NOXRMEASURE string // Code number indicating measured or substituted, not used
	// by SMOKE.
	// UNITFLOW float64 //  Flow rate (ft3/sec) for the Boiler Unit (optional; must be
	// present for all records or not any records; not yet used by SMOKE)
}
type cemSum struct {
	NOXMASS float64 // Nitrogen oxide emissions (lb/hr) (required)
	SO2MASS float64 // Sulfur dioxide emissions (lb/hr) (required)
	GLOAD   float64 // Gross load (MW) (optional)
	SLOAD   float64 // Steam load (1000 lbs/hr) (optional)
	HTINPUT float64 // Heat input (mmBtu) (required)
}

func (t *temporalSector) getCEMdata() {
	t.c.Log("Getting CEM data...", 1)
	t.cemArray = make(map[[2]string]map[string]*cemData) // map[id,boiler][time]data
	t.cemSum = make(map[[2]string]*cemSum)               // map[id,boiler]data

	for _, fname := range t.c.CEMFileNames {
		f, err := os.Open(fname)
		if err != nil {
			panic(err)
		}
		r := csv.NewReader(f)
		for {
			rec, err := r.Read()
			if err != nil {
				if err == io.EOF {
					break
				}
				panic(err)
			}
			orisID := trimString(rec[0])
			if orisID == "" {
				continue
			}
			blrID := trimString(rec[1])
			yymmdd := rec[2]
			hour := rec[3]
			noxmass := float32(stringToFloat(rec[4]))
			if noxmass < 0. {
				noxmass = 0
			}
			so2mass := float32(stringToFloat(rec[5]))
			if so2mass < 0. {
				so2mass = 0
			}
			gload := float32(stringToFloat(rec[6]))
			if gload < 0. {
				gload = 0
			}
			sload := float32(stringToFloat(rec[7]))
			if sload < 0. {
				sload = 0
			}
			htinput := float32(stringToFloat(rec[8]))
			if htinput < 0. {
				htinput = 0
			}
			id := [2]string{orisID, blrID}
			if _, ok := t.cemArray[id]; !ok {
				t.cemArray[id] = make(map[string]*cemData)
				t.cemSum[id] = new(cemSum)
			}
			// cem time format = "060102 15"
			t.cemArray[id][yymmdd+" "+hour] = &cemData{NOXMASS: noxmass,
				SO2MASS: so2mass, GLOAD: gload, SLOAD: sload, HTINPUT: htinput}
		}
		f.Close()
	}
	// Calculate annual totals
	for id, vals := range t.cemArray {
		for _, val := range vals {
			cs := t.cemSum[id]
			cs.NOXMASS += float64(val.NOXMASS)
			cs.SO2MASS += float64(val.SO2MASS)
			cs.GLOAD += float64(val.GLOAD)
			cs.SLOAD += float64(val.SLOAD)
			cs.HTINPUT += float64(val.HTINPUT)
		}
	}
	// delete records where HTINPUT, GLOAD, and SLOAD are all not > 0.
	for id, cemsum := range t.cemSum {
		if cemsum.GLOAD <= 0. && cemsum.SLOAD <= 0. && cemsum.HTINPUT <= 0. {
			delete(t.cemSum, id)
			delete(t.cemArray, id)
		}
	}
	t.c.Log("Finished getting CEM data...", 1)
}

func (t *temporalSector) getTemporalCodes(SCC, FIPS string) [3]string {
	var codes interface{}
	var err error
	if !t.c.MatchFullSCC {
		_, _, codes, err =
			MatchCodeDouble(SCC, FIPS, t.temporalRef)
	} else {
		_, codes, err = MatchCode(FIPS, t.temporalRef[SCC])
	}
	if err != nil {
		err = fmt.Errorf("In temporal reference file: %v. (SCC=%v, FIPS=%v).",
			err.Error(), SCC, FIPS)
		panic(err)
	}
	return codes.([3]string)
}

var aggregateArea = func(t *temporalSector, record *ParsedRecord) {
	temporalCodes := t.getTemporalCodes(record.SCC, record.FIPS)
	// Create matrices if they don't exist
	if _, ok := t.AreaData[temporalCodes]; !ok {
		t.AreaData[temporalCodes] = make(map[string][]*sparse.SparseArray)
	}
	// Add data from record into matricies.
	for pol, vals := range record.ANN_EMIS {
		if _, ok := t.AreaData[temporalCodes][pol]; !ok {
			t.AreaData[temporalCodes][pol] =
				make([]*sparse.SparseArray, len(Grids))
			for i, grid := range Grids {
				t.AreaData[temporalCodes][pol][i] =
					sparse.ZerosSparse(grid.Ny, grid.Nx)
			}
		}
		// change units from emissions per year to emissions per hour
		units := strings.Replace(vals.Units, "/year", "/hour", -1)
		t.addUnits(pol, units)
		for i, _ := range Grids {
			if vals.Gridded[i] != nil {
				t.AreaData[temporalCodes][pol][i].AddSparse(vals.Gridded[i])
			}
		}
	}
}

// add units into an existing units list.
func (ts *temporalSector) addUnits(pol string, newUnits string) {
	if unitsCheck, ok := ts.tp.Units[pol]; ok {
		if unitsCheck != newUnits {
			panic(fmt.Sprintf("Units don't match: %v != %v",
				ts.tp.Units[pol], newUnits))
		}
	} else {
		ts.tp.Units[pol] = newUnits
	}
}

type PointRecord struct {
	STKHGT             float64                  //	Stack Height (ft) (required)
	STKDIAM            float64                  //	Stack Diameter (ft) (required)
	STKTEMP            float64                  //	Stack Gas Exit Temperature (°F) (required)
	STKFLOW            float64                  //	Stack Gas Flow Rate (ft3/sec) (optional, automatically calculated by Smkinven from velocity and diameter if not given in file)
	STKVEL             float64                  //	Stack Gas Exit Velocity (ft/sec) (required)
	ANN_EMIS           map[string]*SpecValUnits // Annual Emissions (tons/year) (required)
	ORIS_FACILITY_CODE string
	ORIS_BOILER_ID     string
}

func newPointRecord(r *ParsedRecord) *PointRecord {
	out := new(PointRecord)
	out.STKHGT = r.STKHGT
	out.STKDIAM = r.STKDIAM
	out.STKTEMP = r.STKTEMP
	out.STKFLOW = r.STKFLOW
	out.STKVEL = r.STKVEL
	out.ANN_EMIS = r.ANN_EMIS
	out.ORIS_FACILITY_CODE = r.ORIS_FACILITY_CODE
	out.ORIS_BOILER_ID = r.ORIS_BOILER_ID
	return out
}

var aggregatePoint = func(t *temporalSector, record *ParsedRecord) {
	temporalCodes := t.getTemporalCodes(record.SCC, record.FIPS)
	// Create point arrays if they don't exist
	if _, ok := t.PointData[temporalCodes]; !ok {
		t.PointData[temporalCodes] = make([]*PointRecord, 0)
	}
	if len(record.ANN_EMIS) != 0 {
		t.PointData[temporalCodes] = append(t.PointData[temporalCodes],
			newPointRecord(record))
	}
	// change units from emissions per year to emissions per hour
	for pol, rec := range record.ANN_EMIS {
		units := strings.Replace(rec.Units, "/year", "/hour", -1)
		t.addUnits(pol, units)
	}
}

var addEmisAtTimeTproArea = func(t *temporalSector, time time.Time,
	areaEmis map[string][]*sparse.SparseArray,
	pointEmis []*PointRecord) (map[string][]*sparse.SparseArray,
	[]*PointRecord) {
	t.mu.RLock()
	// add area data.
	for temporalCodes, data := range t.AreaData {
		tFactors := t.griddedTemporalFactors(temporalCodes, time)
		for pol, gridData := range data {
			if _, ok := areaEmis[pol]; !ok { // initialize array
				areaEmis[pol] = make([]*sparse.SparseArray, len(Grids))
				for i, grid := range Grids {
					areaEmis[pol][i] = sparse.ZerosSparse(grid.Ny, grid.Nx)
				}
			}
			for i, g := range gridData {
				// multiply by temporal factor to get time step
				for _, ix := range g.Nonzero() {
					val := g.Get1d(ix)
					tFactor := tFactors[i].Get1d(ix)
					index := g.IndexNd(ix)
					areaEmis[pol][i].
						AddVal(val*tFactor, index[0], index[1])
				}
			}
		}
	}
	t.mu.RUnlock()
	return areaEmis, pointEmis
}

var addEmisAtTimeTproPoint = func(t *temporalSector, time time.Time,
	areaEmis map[string][]*sparse.SparseArray,
	pointEmis []*PointRecord) (map[string][]*sparse.SparseArray,
	[]*PointRecord) {
	t.mu.RLock()
	// add point data
	for temporalCodes, data := range t.PointData {
		tFactors := t.griddedTemporalFactors(temporalCodes, time)
		for _, record := range data {
			out := emisAtTimeTproPoint(tFactors, record)
			pointEmis = append(pointEmis, out)
		}
	}
	t.mu.RUnlock()
	return areaEmis, pointEmis
}

func emisAtTimeTproPoint(tFactors []*sparse.SparseArray,
	record *PointRecord) *PointRecord {
	out := new(PointRecord)
	out.STKHGT = record.STKHGT
	out.STKDIAM = record.STKDIAM
	out.STKTEMP = record.STKTEMP
	out.STKFLOW = record.STKFLOW
	out.STKVEL = record.STKVEL
	out.ANN_EMIS = make(map[string]*SpecValUnits)
	for pol, emis := range record.ANN_EMIS {
		out.ANN_EMIS[pol] = new(SpecValUnits)
		out.ANN_EMIS[pol].Units = emis.Units
		out.ANN_EMIS[pol].Gridded = make([]*sparse.SparseArray,
			len(emis.Gridded))
		for i, gridVal := range emis.Gridded {
			out.ANN_EMIS[pol].Gridded[i] = sparse.ZerosSparse(gridVal.Shape...)
			// multiply by temporal factor to get time step
			for _, ix := range gridVal.Nonzero() {
				val := gridVal.Get1d(ix)
				tFactor := tFactors[i].Get1d(ix)
				index := gridVal.IndexNd(ix)
				out.ANN_EMIS[pol].Gridded[i].
					AddVal(val*tFactor, index[0], index[1])
			}
		}
	}
	return out
}

// First, try to match the record ORIS ID and Boiler ID to the cem database
// and use CEM temporalization.
// If there is no match, use the normal TPRO temporalization.
var addEmisAtTimeCEM = func(t *temporalSector, time time.Time,
	areaEmis map[string][]*sparse.SparseArray,
	pointEmis []*PointRecord) (map[string][]*sparse.SparseArray,
	[]*PointRecord) {
	t.mu.RLock()
	cemTimes := griddedTimeNoDST(time)
	// add point data
	for temporalCodes, data := range t.PointData {
		tproTFactors := t.griddedTemporalFactors(temporalCodes, time)
		for _, record := range data {
			var out *PointRecord
			id := [2]string{record.ORIS_FACILITY_CODE, record.ORIS_BOILER_ID}
			if cemsum, ok := t.cemSum[id]; ok {
				out = new(PointRecord)
				out.STKHGT = record.STKHGT
				out.STKDIAM = record.STKDIAM
				out.STKTEMP = record.STKTEMP
				out.STKFLOW = record.STKFLOW
				out.STKVEL = record.STKVEL
				out.ANN_EMIS = make(map[string]*SpecValUnits)
				for pol, emis := range record.ANN_EMIS {
					out.ANN_EMIS[pol] = new(SpecValUnits)
					out.ANN_EMIS[pol].Units = emis.Units
					out.ANN_EMIS[pol].Gridded = make([]*sparse.SparseArray,
						len(emis.Gridded))
					for i, gridVal := range emis.Gridded {
						out.ANN_EMIS[pol].Gridded[i] =
							sparse.ZerosSparse(Grids[i].Ny, Grids[i].Nx)
						// multiply by temporal factor to get time step
						for _, ix := range gridVal.Nonzero() {
							tFactor := getCEMtFactor(emis.PolType,
								cemsum, t.cemArray[id][cemTimes[i][ix]])
							val := gridVal.Get1d(ix)
							index := gridVal.IndexNd(ix)
							out.ANN_EMIS[pol].Gridded[i].
								AddVal(val*tFactor, index[0], index[1])
						}
					}
				}
			} else {
				out = emisAtTimeTproPoint(tproTFactors, record)
			}
			pointEmis = append(pointEmis, out)
		}
	}
	t.mu.RUnlock()
	return areaEmis, pointEmis
}

// If the pollutant is NOx or SOx and the total annual NOx or SOx emissions are
// greater than zero, the temporal factor is the NOx or SOx emissions at
// the given time divided by the total annual NOx emissions.
// If the pollutant isn't NOx or SOx or the total annual NOx or SOx emissions
// are zero, the temporal factor is the hourly heat input divided by the
// total annual heat input. If total annual heat input is zero,
// we use steam load, and if total annual steam load is zero, we
// use gross load.
// In all cases, if the hourly value is zero or missing but the annual
// total value is greater than zero, the temporal factor is zero.
func getCEMtFactor(polType *PolHolder,
	cemsum *cemSum, cemtime *cemData) float64 {
	if cemtime == nil {
		return 0.
	}
	var heatcalc = func() float64 {
		if cemsum.HTINPUT > 0. {
			return float64(cemtime.HTINPUT) / cemsum.HTINPUT
		} else if cemsum.SLOAD > 0. {
			return float64(cemtime.SLOAD) / cemsum.SLOAD
		} else if cemsum.GLOAD > 0. {
			return float64(cemtime.GLOAD) / cemsum.GLOAD
		} else {
			panic("HTINPUT, SLOAD, and GLOAD are all not > 0. " +
				"This shouldn't happen.")
		}
	}
	// Figure out what type of pollutant it is.
	var isNOx, isSOx bool
	if polType.SpecType == "NOx" ||
		IsStringInArray(polType.SpecNames, "Nitrogen Dioxide") {
		isNOx = true
	} else if polType.SpecType == "SOx" ||
		IsStringInArray(polType.SpecNames, "Sulfur dioxide") {
		isSOx = true
	}
	if isNOx {
		if cemsum.NOXMASS <= 0. {
			return heatcalc()
		}
		return float64(cemtime.NOXMASS) / cemsum.NOXMASS
	} else if isSOx {
		if cemsum.SO2MASS <= 0. {
			return heatcalc()
		}
		return float64(cemtime.SO2MASS) / cemsum.SO2MASS
	} else {
		return heatcalc()
	}
}

func (t *temporalSector) getTemporalFactor(monthlyCode, weeklyCode,
	diurnalCode string, localTime time.Time) float64 {
	weeksinmonth := weeksInMonth(localTime)
	month := localTime.Month() - 1
	mFac := t.monthlyTpro[monthlyCode][month]
	var weekday int
	if _, ok := t.holidays[localTime.Format(holidayFormat)]; ok {
		// if it's a holiday, use Sunday temporal profiles.
		// Note: this can cause the checksums to not quite add up.
		weekday = 6
	} else {
		// switch from sunday to monday for first weekday
		weekday = (int(localTime.Weekday()) + 6) % 7
	}
	wFac := t.weeklyTpro[weeklyCode][weekday]
	hour := localTime.Hour()
	var dFac float64
	if weekday < 5 {
		dFac = t.weekdayTpro[diurnalCode][hour]
	} else {
		dFac = t.weekendTpro[diurnalCode][hour]
	}
	return 1. * mFac / weeksinmonth * wFac * dFac
}

// get temporal fractors for a given time. This should properly account for
// daylight savings time.
func (t *temporalSector) griddedTemporalFactors(codes [3]string,
	outputTime time.Time) (out []*sparse.SparseArray) {
	out = make([]*sparse.SparseArray, len(Grids))
	for i, grid := range Grids {
		out[i] = sparse.ZerosSparse(grid.Ny, grid.Nx)
		for tz, cells := range grid.TimeZones {
			location, err := time.LoadLocation(
				strings.Replace(tz, " ", "_", -1))
			if err != nil {
				panic(fmt.Errorf("Unknown timezone %v.", tz))
			}
			localTime := outputTime.In(location)
			fac := t.getTemporalFactor(codes[0], codes[1], codes[2], localTime)
			out[i].AddSparse(cells.ScaleCopy(fac))
		}
	}
	return
}

// get times in grid cells with no daylight savings (needed for CEM data)
func griddedTimeNoDST(outputTime time.Time) []map[int]string {
	const format = "060102 15"
	out := make([]map[int]string, len(Grids))
	for i, grid := range Grids {
		out[i] = make(map[int]string)
		for tz, cells := range grid.TimeZones {
			location, err := time.LoadLocation(tz)
			if err != nil {
				panic(fmt.Errorf("Unknown timezone %v.", tz))
			}
			localTimeNoDST := timeNoDST(outputTime, location)
			timeString := localTimeNoDST.Format(format)
			for index, _ := range cells.Elements {
				out[i][index] = timeString
			}
		}
	}
	return out
}

// get time with no daylight savings (needed for CEM data)
func timeNoDST(output time.Time, loc *time.Location) time.Time {
	localTime := output.In(loc)
	_, winterOffset := time.Date(localTime.Year(), 1, 1, 0, 0, 0, 0, loc).Zone()
	_, summerOffset := time.Date(localTime.Year(), 7, 1, 0, 0, 0, 0, loc).Zone()

	if winterOffset > summerOffset {
		winterOffset, summerOffset = summerOffset, winterOffset
	}
	locNoDST := time.FixedZone(loc.String()+" No DST", winterOffset)
	return localTime.In(locNoDST)
}

type TimeStepData struct {
	Time      time.Time
	AreaData  map[string][]*sparse.SparseArray // map[pol][grid]array
	PointData []*PointRecord
}

func (c *Context) CurrentMonth() string {
	return strings.ToLower(c.currentTime.Format("Jan"))
}

// Function EmisAtTime returns emissions for the
// given time (in the output timezone).
func (tp *TemporalProcessor) EmisAtTime(time time.Time) *TimeStepData {
	ts := new(TimeStepData)
	ts.Time = time
	ts.AreaData = make(map[string][]*sparse.SparseArray) // map[pol][grid]array
	ts.PointData = make([]*PointRecord, 0)
	month := strings.ToLower(time.Format("Jan"))
	for _, sectorData := range tp.sectors {
		var periodData *temporalSector
		var ok bool
		// Get data for the correct period
		if periodData, ok = sectorData["annual"]; ok {
		} else if periodData, ok = sectorData["cem"]; ok {
		} else if periodData, ok = sectorData[month]; ok {
		} else {
			panic(fmt.Errorf("Sector %v does not have data for period %v",
				periodData.c.Sector, periodData.period))
		}
		ts.AreaData, ts.PointData = periodData.addEmisAtTime(
			periodData, time, ts.AreaData, ts.PointData)
	}
	tp.temporalReport.addTstep(ts)
	return ts
}

type TemporalReport struct {
	mu        sync.RWMutex
	Time      []time.Time
	Data      map[string][][]float64 // map[pol][time][grids]emissions
	numTsteps int
	Units     map[string]string // map[pol]units
}

func newTemporalReport(c *Context, units map[string]string) *TemporalReport {
	tr := new(TemporalReport)
	tr.Data = make(map[string][][]float64)
	tr.numTsteps = int(c.endDate.Sub(c.startDate).Hours()/c.tStep.Hours() + 0.5)
	tr.Time = make([]time.Time, 0, tr.numTsteps)
	tr.Units = units
	return tr
}

func (tr *TemporalReport) addTstep(ts *TimeStepData) {
	tr.mu.Lock()
	tr.Time = append(tr.Time, ts.Time)
	vals := make(map[string][]float64)
	for pol, data := range ts.AreaData {
		vals[pol] = make([]float64, len(data))
		for i, gridData := range data {
			vals[pol][i] += gridData.Sum()
		}
	}
	for _, point := range ts.PointData {
		for pol, data := range point.ANN_EMIS {
			if _, ok := vals[pol]; !ok {
				vals[pol] = make([]float64, len(data.Gridded))
			}
			for i, gridData := range data.Gridded {
				vals[pol][i] += gridData.Sum()
			}
		}
	}
	for pol, data := range vals {
		if _, ok := tr.Data[pol]; !ok {
			tr.Data[pol] = make([][]float64, 0, tr.numTsteps)
		}
		tr.Data[pol] = append(tr.Data[pol], data)
	}
	tr.mu.Unlock()
}
