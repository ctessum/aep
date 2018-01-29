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
	"encoding/csv"
	"fmt"
	"io"
	"strconv"
	"strings"
	"time"

	"github.com/ctessum/unit"
)

// EmisAtTime returns the emissions from the given Record occurring
// during the hour after t, after being adjusted with the temporal profiles
// in tp. If partialMatch is true, temporal profiles will be
// used for SCC codes partially matching the SCC code of the given record
// if no exact match is found.
func EmisAtTime(r Record, t time.Time, tp *TemporalProcessor, partialMatch bool) (map[Pollutant]*unit.Unit, error) {
	if pointData := r.PointData(); pointData != nil {
		// Check if we should be using CEM allocation.
		id := [2]string{pointData.ORISFacilityCode, pointData.ORISBoilerID}
		_, ok := tp.cemSum[id]
		if tp.useCEM && ok {
			return emisAtTimeCEM(r, t, tp, partialMatch)
		}
	}

	location, ok := tp.TimeZones[r.GetFIPS()]
	if !ok {
		return nil, fmt.Errorf("aep: can't find timezone for FIPS %s", r.GetFIPS())
	}
	tLocal := t.In(location)
	emis := r.PeriodTotals(tLocal, tLocal.Add(time.Hour))
	codes, err := tp.getTemporalCodes(r.GetSCC(), r.GetFIPS(), partialMatch)
	if err != nil {
		return nil, err
	}

	// Get the fraction of annual emissions occuring at during the hour
	// including our midpoint.
	factor, err := tp.temporalFactor(codes[0], codes[1], codes[2], tLocal)
	if err != nil {
		return nil, err
	}

	// Adjust the factor to account for that fact that we have already
	// done linear scaling of one hour of emissions, so we just need
	// to adjust that value to account for the temporal profiles.
	yearBegin := time.Date(t.Year(), time.January, 1, 0, 0, 0, 0, time.UTC)
	yearEnd := time.Date(t.Year()+1, time.January, 1, 0, 0, 0, 0, time.UTC)
	hoursInYear := yearEnd.Sub(yearBegin).Hours()
	adjFactor := factor * hoursInYear
	for _, v := range emis {
		v.Mul(unit.New(adjFactor, unit.Dimless))
	}
	return emis, nil
}

// emisAtTimeCEM returns the emissions from the given Record occurring
// during the hour after t, after being adjusted to match records
// from continuous emissions monitoring data.
// If no matching CEM data is found, results will be returned using
// EmisAtTime instead. In that case, if partialMatch is true,
// temporal profiles will be used for SCC codes partially matching
// the SCC code of the given record if no exact match is found.
func emisAtTimeCEM(r Record, t time.Time, tp *TemporalProcessor, partialMatch bool) (map[Pollutant]*unit.Unit, error) {
	pointData := r.PointData()
	id := [2]string{pointData.ORISFacilityCode, pointData.ORISBoilerID}
	cemsum := tp.cemSum[id]
	location, ok := tp.TimeZones[r.GetFIPS()]
	if !ok {
		return nil, fmt.Errorf("aep: can't find timezone for FIPS %s", r.GetFIPS())
	}
	tLocal := t.In(location)
	timeNoDS := timeNoDST(tLocal) // Time with no daylight savings.

	// Get the CEM data. If the result is nil, it means that emissions for
	// the hour of interest are zero.
	const cemFormat = "060102 15"
	cemTime := tp.cemArray[id][timeNoDS.Format(cemFormat)]
	emis := r.Totals()
	for pol, v := range emis {
		tFactor := getCEMtFactor(pol, cemsum, cemTime)
		v.Mul(unit.New(tFactor, unit.Dimless))
	}
	return emis, nil
}

// timeNoDST returns time with no daylight savings (needed for CEM data)
func timeNoDST(localTime time.Time) time.Time {
	_, winterOffset := time.Date(localTime.Year(), 1, 1, 0, 0, 0, 0, localTime.Location()).Zone()
	_, summerOffset := time.Date(localTime.Year(), 7, 1, 0, 0, 0, 0, localTime.Location()).Zone()

	if winterOffset > summerOffset {
		return localTime.In(time.FixedZone(localTime.Location().String()+" No DST", summerOffset))
	}
	return localTime.In(time.FixedZone(localTime.Location().String()+" No DST", winterOffset))
}

// TemporalProcessor calculates emissions at specific times.
type TemporalProcessor struct {
	monthlyTpro map[string][]float64 // map[code]vals
	weeklyTpro  map[string][]float64 // map[code]vals
	weekdayTpro map[string][]float64 // map[code]vals
	weekendTpro map[string][]float64 // map[code]vals
	temporalRef map[string]map[string]interface{}
	holidays    map[string]string

	// useCEM specifies whether continuous emissions monitoring data
	// should be used for temporal allocation.
	useCEM   bool
	cemArray map[[2]string]map[string]*cemData // map[id,boiler][time]data
	cemSum   map[[2]string]*cemSum             // map[id,boiler]data

	// TimeZones hold the time zone code that each FIPS code
	// belongs to.
	TimeZones map[string]*time.Location
}

// NewTemporalProcessor initializes a new TemporalProcessor.
func (c *Context) NewTemporalProcessor(holidays, tref, tpro io.Reader, cem []io.Reader, useCEM bool) (*TemporalProcessor, error) {
	tp := new(TemporalProcessor)

	if err := tp.getHolidays(holidays); err != nil {
		return nil, err
	}
	if err := tp.getTemporalRef(tref); err != nil {
		return nil, err
	}
	if err := tp.getTemporalPro(tpro); err != nil {
		return nil, err
	}
	tp.useCEM = useCEM
	if useCEM {
		if err := tp.getCEMdata(cem); err != nil {
			return nil, err
		}
	}
	return tp, nil
}

// temporalRef reads the SMOKE tref file, which maps FIPS and SCC
// codes to grid surrogates. Although the tref file allows the
// specification of code by pollutant name, that functionality is
// not included here.
func (t *TemporalProcessor) getTemporalRef(fid io.Reader) error {
	t.temporalRef = make(map[string]map[string]interface{})
	var record string
	buf := bufio.NewReader(fid)
	var err error
	for {
		record, err = buf.ReadString('\n')
		if err != nil {
			if err == io.EOF {
				break
			} else {
				return fmt.Errorf("aep: reading temporal reference file: %v", err)
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
				return fmt.Errorf("aep: in TemporalRef, record %v FIPS %v has wrong number of digits", record, FIPS)
			}

			if _, ok := t.temporalRef[SCC]; !ok {
				t.temporalRef[SCC] = make(map[string]interface{})
			}
			t.temporalRef[SCC][FIPS] = [3]string{
				monthCode, weekCode, diurnalCode}
		}
	}
	return nil
}

// get decimal number of weeks in the current month.
func weeksInMonth(t time.Time) float64 {
	t2 := time.Date(t.Year(), t.Month(), 32, 0, 0, 0, 0, time.UTC)
	return (32. - float64(t2.Day())) / 7.
}

const holidayFormat = "20060102"

func (t *TemporalProcessor) getHolidays(fid io.Reader) error {
	t.holidays = make(map[string]string)
	scanner := bufio.NewScanner(fid)
	var err error
	for scanner.Scan() {
		line := scanner.Text()
		if len(line) < 19 || line[0] == '#' {
			continue
		}
		var holiday time.Time
		holiday, err = time.Parse("01 02 2006", line[8:18])
		if err != nil {
			return fmt.Errorf("aep: reading holiday file: %v", err)
		}
		t.holidays[holiday.Format(holidayFormat)] = ""
	}
	if err = scanner.Err(); err != nil {
		return fmt.Errorf("aep: reading holiday file: %v", err)
	}
	return nil
}

func (t *TemporalProcessor) getTemporalPro(fid io.Reader) error {
	t.monthlyTpro = make(map[string][]float64) // map[code]vals
	t.weeklyTpro = make(map[string][]float64)  // map[code]vals
	t.weekdayTpro = make(map[string][]float64) // map[code]vals
	t.weekendTpro = make(map[string][]float64) // map[code]vals
	scanner := bufio.NewScanner(fid)
	var tType string
	var err error
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
			var code string
			var pro []float64
			code, pro, err = parseTproLine(line, 12)
			if err != nil {
				return err
			}
			t.monthlyTpro[code] = pro
		case "weekly":
			var code string
			var pro []float64
			code, pro, err = parseTproLine(line, 7)
			if err != nil {
				return fmt.Errorf("aep: processing temporal profile line %v: %v", line, err)
			}
			t.weeklyTpro[code] = pro
		case "diurnal weekday":
			var code string
			var pro []float64
			code, pro, err = parseTproLine(line, 24)
			if err != nil {
				return fmt.Errorf("aep: processing temporal profile line %v: %v", line, err)
			}
			t.weekdayTpro[code] = pro
		case "diurnal weekend":
			var code string
			var pro []float64
			code, pro, err = parseTproLine(line, 24)
			if err != nil {
				return fmt.Errorf("aep: processing temporal profile line %v: %v", line, err)
			}
			t.weekendTpro[code] = pro
		default:
			return fmt.Errorf("aep: processing temporal profiles: unknown temporal type %v", tType)
		}
	}
	if err = scanner.Err(); err != nil {
		return fmt.Errorf("aep: reading temporal profile file: %v", err)
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

func (t *TemporalProcessor) getCEMdata(files []io.Reader) error {
	fmt.Println("Getting CEM data...")
	t.cemArray = make(map[[2]string]map[string]*cemData) // map[id,boiler][time]data
	t.cemSum = make(map[[2]string]*cemSum)               // map[id,boiler]data

	for _, f := range files {
		r := csv.NewReader(f)
		for {
			rec, err := r.Read()
			if err != nil {
				if err == io.EOF {
					break
				}
				return fmt.Errorf("aep: reading CEM data: %v", err)
			}
			orisID := trimString(rec[0])
			if orisID == "" {
				continue
			}
			blrID := trimString(rec[1])
			yymmdd := rec[2]
			hour := rec[3]
			noxmass, err := stringToFloat(rec[4])
			if err != nil {
				return fmt.Errorf("aep: reading CEM data: %v", err)
			}
			if noxmass < 0. {
				noxmass = 0
			}
			so2mass, err := stringToFloat(rec[5])
			if err != nil {
				return fmt.Errorf("aep: reading CEM data: %v", err)
			}
			if so2mass < 0. {
				so2mass = 0
			}
			gload, err := stringToFloat(rec[6])
			if err != nil {
				return fmt.Errorf("aep: reading CEM data: %v", err)
			}
			if gload < 0. {
				gload = 0
			}
			sload, err := stringToFloat(rec[7])
			if err != nil {
				return fmt.Errorf("aep: reading CEM data: %v", err)
			}
			if sload < 0. {
				sload = 0
			}
			htinput, err := stringToFloat(rec[8])
			if err != nil {
				return fmt.Errorf("aep: reading CEM data: %v", err)
			}
			if htinput < 0. {
				htinput = 0
			}
			id := [2]string{orisID, blrID}
			if _, ok := t.cemArray[id]; !ok {
				t.cemArray[id] = make(map[string]*cemData)
				t.cemSum[id] = new(cemSum)
			}
			// cem time format = "060102 15"
			t.cemArray[id][yymmdd+" "+hour] = &cemData{NOXMASS: float32(noxmass),
				SO2MASS: float32(so2mass), GLOAD: float32(gload), SLOAD: float32(sload), HTINPUT: float32(htinput)}
		}
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
	fmt.Println("Finished getting CEM data...")
	return nil
}

func (t *TemporalProcessor) getTemporalCodes(SCC, FIPS string, partialMatch bool) ([3]string, error) {
	var codes interface{}
	var err error
	if partialMatch {
		_, _, codes, err = MatchCodeDouble(SCC, FIPS, t.temporalRef)
	} else {
		_, codes, err = MatchCode(FIPS, t.temporalRef[SCC])
	}
	if err != nil {
		return [3]string{}, fmt.Errorf("aep: getting temporal code for SCC=%v, FIPS=%v: %v", SCC, FIPS, err)
	}
	return codes.([3]string), nil
}

// getCEMtFactor returns the fraction of annual total emissions occurring
// during the hour of interest.
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
func getCEMtFactor(pol Pollutant, cemsum *cemSum, cemtime *cemData) float64 {
	if cemtime == nil {
		return 0 // We don't have any emissions at this time.
	}
	var heatcalc = func() float64 {
		if cemsum.HTINPUT > 0. {
			return float64(cemtime.HTINPUT) / cemsum.HTINPUT
		} else if cemsum.SLOAD > 0. {
			return float64(cemtime.SLOAD) / cemsum.SLOAD
		} else if cemsum.GLOAD > 0. {
			return float64(cemtime.GLOAD) / cemsum.GLOAD
		}
		panic("HTINPUT, SLOAD, and GLOAD are all not > 0. This shouldn't happen.")
	}
	// Figure out what type of pollutant it is.
	var isNOx, isSOx bool
	if IsStringInArray([]string{"nox", "no2", "no"}, strings.ToLower(pol.Name)) {
		isNOx = true
	} else if IsStringInArray([]string{"so2", "sox"}, strings.ToLower(pol.Name)) {
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

// temporalFactor returns the fraction of annual total emissions occurring during
// the hour starting at localTime for the given temporal profile codes.
func (t *TemporalProcessor) temporalFactor(monthlyCode, weeklyCode, diurnalCode string, localTime time.Time) (float64, error) {
	weeksinmonth := weeksInMonth(localTime)
	month := localTime.Month() - 1
	if _, ok := t.monthlyTpro[monthlyCode]; !ok {
		return -1, fmt.Errorf("aep: can't find temporal factor for month code %s; month %v", monthlyCode, month)
	}
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
	if _, ok := t.weeklyTpro[weeklyCode]; !ok {
		return -1, fmt.Errorf("aep: can't find temporal factor for week code %s; weekday %v", weeklyCode, weekday)
	}
	wFac := t.weeklyTpro[weeklyCode][weekday]
	hour := localTime.Hour()
	var dFac float64
	if weekday < 5 {
		if _, ok := t.weekdayTpro[diurnalCode]; !ok {
			return -1, fmt.Errorf("aep: can't find weekday temporal factor for diurnal code %s; hour %v", diurnalCode, hour)
		}
		dFac = t.weekdayTpro[diurnalCode][hour]
	} else {
		if _, ok := t.weekendTpro[diurnalCode]; !ok {
			return -1, fmt.Errorf("aep: can't find weekend temporal factor for diurnal code %s; hour %v", diurnalCode, hour)
		}
		dFac = t.weekendTpro[diurnalCode][hour]
	}
	return 1. * mFac / weeksinmonth * wFac * dFac, nil
}
