package main

import (
	"bitbucket.org/ctessum/aep/sparse"
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"
)

var (
	temporalRef         = make(map[string]map[string]interface{})
	MonthlyTemporalChan = make(chan *temporalAggregator)
	AnnualTemporalChan  = make(chan *temporalAggregator)
	monthlyTpro         = make(map[string][]float64) // map[code]vals
	weeklyTpro          = make(map[string][]float64) // map[code]vals
	weekdayTpro         = make(map[string][]float64) // map[code]vals
	weekendTpro         = make(map[string][]float64) // map[code]vals
	holidays            = make(map[string]string)
)

const holidayFormat = "20060102"

// Read in data and start up subroutines for temporal processing.
func (c *RunData) TemporalSetup(e *ErrCat) {
	e.Add(c.holidays())
	e.Add(c.temporalRef())
	e.Add(c.temporalPro())
}

// temporalRef reads the SMOKE tref file, which maps FIPS and SCC
// codes to grid surrogates. Although the tref file allows the
// specification of code by pollutant name, that functionality is
// not included here.
func (c *RunData) temporalRef() (err error) {
	var record string
	fid, err := os.Open(c.TemporalRefFile)
	if err != nil {
		err = fmt.Errorf("termporalRef: %v \nFile= %v\nRecord= ",
			err.Error(), c.TemporalRefFile, record)
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
					err.Error(), c.TemporalRefFile, record)
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

			if _, ok := temporalRef[SCC]; !ok {
				temporalRef[SCC] = make(map[string]interface{})
			}
			temporalRef[SCC][FIPS] = [3]string{
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

func (c *RunData) holidays() error {
	fid, err := os.Open(c.HolidayFile)
	if err != nil {
		return fmt.Errorf("Holidays: %v \nFile= %v",
			err.Error(), c.HolidayFile)
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
				err.Error(), c.HolidayFile)
		}
		if holiday.After(c.startDate) && holiday.Before(c.endDate) {
			holidays[holiday.Format(holidayFormat)] = ""
		}
	}
	if err = scanner.Err(); err != nil {
		return fmt.Errorf("Holidays: %v \nFile= %v",
			err.Error(), c.HolidayFile)
	}
	return nil
}

func (c *RunData) temporalPro() error {
	fid, err := os.Open(c.TemporalProFile)
	if err != nil {
		return fmt.Errorf("temporalPro: %v \nFile= %v",
			err.Error(), c.TemporalRefFile)
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
			monthlyTpro[code] = pro
		case "weekly":
			code, pro, err := parseTproLine(line, 7)
			if err != nil {
				return err
			}
			weeklyTpro[code] = pro
		case "diurnal weekday":
			code, pro, err := parseTproLine(line, 24)
			if err != nil {
				return err
			}
			weekdayTpro[code] = pro
		case "diurnal weekend":
			code, pro, err := parseTproLine(line, 24)
			if err != nil {
				return err
			}
			weekendTpro[code] = pro
		default:
			err = fmt.Errorf("In %v: Unknown temporal type %v.",
				c.TemporalProFile, tType)
			return err
		}
	}
	if err = scanner.Err(); err != nil {
		return fmt.Errorf("TemporalPro: %v \nFile= %v",
			err.Error(), c.TemporalRefFile)
	}
	return nil
}

func parseTproLine(line string, n int) (code string, pro []float64, err error) {
	code = strings.TrimSpace(line[0:6])
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

func (c *RunData) GetTemporalCodes(SCC, FIPS string) [3]string {
	var codes interface{}
	var err error
	if !c.MatchFullSCC {
		_, _, codes, err =
			MatchCodeDouble(SCC, FIPS, temporalRef)
	} else {
		_, codes, err = MatchCode(FIPS, temporalRef[SCC])
	}
	if err != nil {
		err = fmt.Errorf("In temperal reference file: %v. (SCC=%v, FIPS=%v).",
			err.Error(), SCC, FIPS)
		panic(err)
	}
	return codes.([3]string)
}

// combine together multiple temporal aggregators
func (c *RunData) temporalCombine(a ...*temporalAggregator) *temporalAggregator {
	out := c.newTemporalAggregator(0)
	for _, t := range a {
		for temporalCodes, data := range t.AreaData {
			if _, ok := out.AreaData[temporalCodes]; !ok {
				out.AreaData[temporalCodes] =
					make(map[string][]*sparse.SparseArray)
			}
			for pol, gridData := range data {
				if _, ok := out.AreaData[temporalCodes][pol]; !ok {
					out.AreaData[temporalCodes][pol] =
						make([]*sparse.SparseArray, len(grids))
					for i, grid := range grids {
						out.AreaData[temporalCodes][pol][i] =
							sparse.ZerosSparse(grid.Ny, grid.Nx)
					}
				}
				for i, g := range gridData {
					out.AreaData[temporalCodes][pol][i].AddSparse(g)
				}
			}
		}
		for temporalCodes, data := range t.PointData {
			if _, ok := out.PointData[temporalCodes]; !ok {
				out.PointData[temporalCodes] =
					make([]*PointRecord, 0, 10000)
			}
			for _, val := range data {
				out.PointData[temporalCodes] = append(
					out.PointData[temporalCodes], val)
			}
		}
	}
	return out
}

type temporalAggregator struct {
	PointData map[[3]string][]*PointRecord
	PointLock sync.Mutex
	AreaData  map[[3]string]map[string][]*sparse.SparseArray
	AreaLock  sync.Mutex
	config    *RunData
	WaitGroup sync.WaitGroup
}

func (c *RunData) newTemporalAggregator(numSectors int) *temporalAggregator {
	t := new(temporalAggregator)
	t.config = c
	t.PointData = make(map[[3]string][]*PointRecord)
	t.AreaData = make(map[[3]string]map[string][]*sparse.SparseArray)
	t.WaitGroup.Add(numSectors)
	return t
}

func (t *temporalAggregator) AggregateArea(record *ParsedRecord) {

	temporalCodes := t.config.GetTemporalCodes(record.SCC, record.FIPS)

	// Create matricies if they don't exist
	if _, ok := t.AreaData[temporalCodes]; !ok {
		t.AreaData[temporalCodes] = make(map[string][]*sparse.SparseArray)
	}
	// Add data from record into matricies.
	t.AreaLock.Lock()
	defer t.AreaLock.Unlock()
	for pol, vals := range record.ANN_EMIS {
		if _, ok := t.AreaData[temporalCodes][pol]; !ok {
			t.AreaData[temporalCodes][pol] =
				make([]*sparse.SparseArray, len(grids))
			for i, grid := range grids {
				t.AreaData[temporalCodes][pol][i] =
					sparse.ZerosSparse(grid.Ny, grid.Nx)
			}
		}
		for i, _ := range grids {
			t.AreaData[temporalCodes][pol][i].AddSparse(vals.gridded[i])
		}
	}
}

type PointRecord struct {
	STKHGT   float64                  //	Stack Height (ft) (required)
	STKDIAM  float64                  //	Stack Diameter (ft) (required)
	STKTEMP  float64                  //	Stack Gas Exit Temperature (°F) (required)
	STKFLOW  float64                  //	Stack Gas Flow Rate (ft3/sec) (optional, automatically calculated by Smkinven from velocity and diameter if not given in file)
	STKVEL   float64                  //	Stack Gas Exit Velocity (ft/sec) (required)
	ANN_EMIS map[string]*specValUnits // Annual Emissions (tons/year) (required)
}

func newPointRecord(r *ParsedRecord) *PointRecord {
	out := new(PointRecord)
	out.STKHGT = r.STKHGT
	out.STKDIAM = r.STKDIAM
	out.STKTEMP = r.STKTEMP
	out.STKFLOW = r.STKFLOW
	out.STKVEL = r.STKVEL
	out.ANN_EMIS = r.ANN_EMIS
	return out
}

func (t *temporalAggregator) AggregatePoint(record *ParsedRecord) {

	temporalCodes := t.config.GetTemporalCodes(record.SCC, record.FIPS)

	// Create point arrays if they don't exist
	t.PointLock.Lock()
	defer t.PointLock.Unlock()
	if _, ok := t.PointData[temporalCodes]; !ok {
		t.PointData[temporalCodes] = make([]*PointRecord, 0, 10000)
	}
	t.PointData[temporalCodes] = append(t.PointData[temporalCodes],
		newPointRecord(record))
}

func getTemporalFactor(monthlyCode, weeklyCode, diurnalCode string,
	localTime time.Time) float64 {
	weeksinmonth := weeksInMonth(localTime)
	month := localTime.Month() - 1
	mFac := monthlyTpro[monthlyCode][month]
	var weekday int
	if _, ok := holidays[localTime.Format(holidayFormat)]; ok {
		// if it's a holiday, use Sunday temporal profiles.
		// Note: this can cause the checksums to not quite add up.
		fmt.Println("It's a Holiday BIOTCH!")
		weekday = 6
	} else {
		// switch from sunday to monday for first weekday
		weekday = (int(localTime.Weekday()) + 6) % 7
	}
	wFac := weeklyTpro[weeklyCode][weekday]
	hour := localTime.Hour()
	var dFac float64
	if weekday < 5 {
		dFac = weekdayTpro[diurnalCode][hour]
	} else {
		dFac = weekendTpro[diurnalCode][hour]
	}
	return 1. * mFac / weeksinmonth * wFac * dFac
}

func griddedTemporalFactors(codes [3]string, outputTime time.Time) (
	out []*sparse.SparseArray) {
	out = make([]*sparse.SparseArray, len(grids))
	for i, grid := range grids {
		out[i] = sparse.ZerosSparse(grid.Ny, grid.Nx)
		for tz, cells := range grid.TimeZones {
			location := time.FixedZone("tz", tz)
			localTime := outputTime.In(location)
			fac := getTemporalFactor(codes[0], codes[1], codes[2], localTime)
			out[i].AddSparse(cells.ScaleCopy(fac))
		}
	}
	return
}

type timeStep struct {
	area  map[string][]*sparse.SparseArray
	point []*PointRecord
}

func newTimeStep() *timeStep {
	t := new(timeStep)
	t.area = make(map[string][]*sparse.SparseArray)
	t.point = make([]*PointRecord, 0, 350000)
	return t
}

func (t *timeStep) Sum() (area, point map[string][]float64) {
	area = make(map[string][]float64)
	point = make(map[string][]float64)
	for pol, vals := range t.area {
		if _, ok := area[pol]; !ok {
			area[pol] = make([]float64, len(vals))
		}
		for i, val := range vals {
			area[pol][i] += val.Sum()
		}
	}
	for _, record := range t.point {
		for pol, vals := range record.ANN_EMIS {
			if _, ok := point[pol]; !ok {
				point[pol] = make([]float64, len(vals.gridded))
			}
			for i, val := range vals.gridded {
				point[pol][i] += val.Sum()
			}
		}
	}
	return
}

func (ta *temporalAggregator) calcTimestep(t time.Time) *timeStep {
	Tstep := newTimeStep()
	for temporalCodes, data := range ta.AreaData {
		tFactors := griddedTemporalFactors(temporalCodes, t)
		for pol, gridData := range data {
			if _, ok := Tstep.area[pol]; !ok {
				Tstep.area[pol] = make([]*sparse.SparseArray, len(grids))
				for i, grid := range grids {
					Tstep.area[pol][i] = sparse.ZerosSparse(grid.Ny, grid.Nx)
				}
			}
			for i, g := range gridData {
				Tstep.area[pol][i].AddSparse(sparse.ArrayMultiply(
					tFactors[i], g))
			}
		}
	}
	for temporalCodes, data := range ta.PointData {
		tFactors := griddedTemporalFactors(temporalCodes, t)
		for _, record := range data {
			point := new(PointRecord)
			point.STKHGT = record.STKHGT
			point.STKDIAM = record.STKDIAM
			point.STKTEMP = record.STKTEMP
			point.STKFLOW = record.STKFLOW
			point.STKVEL = record.STKVEL
			point.ANN_EMIS = make(map[string]*specValUnits)
			for pol, emis := range record.ANN_EMIS {
				point.ANN_EMIS[pol] = new(specValUnits)
				point.ANN_EMIS[pol].gridded =
					make([]*sparse.SparseArray, len(grids))
				for i, val := range emis.gridded {
					point.ANN_EMIS[pol].gridded[i] = sparse.ArrayMultiply(
						tFactors[i], val)
				}
			}
			Tstep.point = append(Tstep.point, point)
		}
	}
	return Tstep
}

func (c *RunData) SectorTemporal(InputChan chan *ParsedRecord,
	OutputChan chan *ParsedRecord, period string) {
	defer c.ErrorRecoverCloseChan(InputChan)
	c.Log("Aggregating by temporal profile "+period+" "+c.Sector+"...", 0)
	var temporalAgg *temporalAggregator
	switch c.InventoryFreq {
	case "annual":
		temporalAgg = <-AnnualTemporalChan
		fmt.Println("qqqqqqqqqqqqqqqqqqqqqqqqqq Got annual Aggregateor", c.Sector)
	case "monthly":
		temporalAgg = <-MonthlyTemporalChan
		fmt.Println("qqqqqqqqqqqqqqqqqqqqqqqqqq Got monthly Aggregateor", c.Sector)
	}
	defer temporalAgg.WaitGroup.Done()
	switch c.SectorType {
	case "point":
		for record := range InputChan {
			temporalAgg.AggregatePoint(record)
			OutputChan <- record
		}
	case "area", "mobile":
		for record := range InputChan {
			temporalAgg.AggregateArea(record)
			OutputChan <- record
		}
	default:
		err := fmt.Errorf("Unknown sectorType %v", c.SectorType)
		panic(err)
	}
	if OutputChan != TotalReportChan {
		close(OutputChan)
	}
	c.msgchan <- "Finished temporalizing " + period + " " + c.Sector
	return
}

func (c *RunData) Temporal(numAnnualSectors,
	numMonthlySectors int, msgChan chan string) {
	tReport := newTemporalReport(c.startDate, c.endDate)
	temporalMonth := c.CurrentMonth()
	annualData := c.newTemporalAggregator(numAnnualSectors)
	monthData := c.newTemporalAggregator(numMonthlySectors)
	for i := 0; i < numAnnualSectors; i++ {
		AnnualTemporalChan <- annualData
	}
	for i := 0; i < numMonthlySectors; i++ {
		MonthlyTemporalChan <- monthData
	}
	annualData.WaitGroup.Wait() // wait for sectors to finish
	fmt.Println("Annual Finished")
	monthData.WaitGroup.Wait() // wait for sectors to finish
	fmt.Println("Monthly Finished")
	Tdata := c.temporalCombine(annualData, monthData)
	fmt.Println("temporal combined")
	fmt.Println(c.currentTime, c.endDate)
	for c.currentTime.Before(c.endDate) {
		month := c.CurrentMonth()
		if month != temporalMonth {
			fmt.Println("New month calc")
			temporalMonth = month
			monthData = c.newTemporalAggregator(numMonthlySectors)
			for i := 0; i < numMonthlySectors; i++ {
				MonthlyTemporalChan <- monthData
			}
			fmt.Println("finished sending chans")
			monthData.WaitGroup.Wait() // wait for sectors to finish
			fmt.Println("Monthly Finished")
			Tdata = c.temporalCombine(annualData, monthData)
		}
		fmt.Println("xxxxxxxxxxxxxxxx", c.currentTime)
		Tstep := Tdata.calcTimestep(c.currentTime)
		tReport.addTstep(Tstep)
		c.nextTime()
	}
	Report.TemporalResults = tReport
	msgChan <- "Temporal allocation complete"
}

type TemporalReport struct {
	Area      map[string][]float64
	Point     map[string][]float64
	NumTsteps int
	i         int
}

func newTemporalReport(begin, end time.Time) *TemporalReport {
	t := new(TemporalReport)
	t.Area = make(map[string][]float64)
	t.Point = make(map[string][]float64)
	t.NumTsteps = int(end.Sub(begin).Hours() + 0.5)
	return t
}

func (t *TemporalReport) addTstep(Tstep *timeStep) {
	area, point := Tstep.Sum()
	for pol, vals := range area {
		if _, ok := t.Area[pol]; !ok {
			t.Area[pol] = make([]float64, len(vals))
		}
		for i, val := range vals {
			t.Area[pol][i] += val
		}
	}
	for pol, vals := range point {
		if _, ok := t.Point[pol]; !ok {
			t.Point[pol] = make([]float64, len(vals))
		}
		for i, val := range vals {
			t.Point[pol][i] += val
		}
	}
	t.i++
}
