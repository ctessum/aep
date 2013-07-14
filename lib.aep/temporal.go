package aep

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

func (c *RunData) GetTemporalCodes(SCC, FIPS string) [3]string {
	var codes interface{}
	var err error
	if !c.MatchFullSCC {
		_, _, codes, err =
			matchCodeDouble(SCC, FIPS, temporalRef)
	} else {
		_, codes, err = matchCode(FIPS, temporalRef[SCC])
	}
	if err != nil {
		err = fmt.Errorf("In temporal reference file: %v. (SCC=%v, FIPS=%v).",
			err.Error(), SCC, FIPS)
		panic(err)
	}
	return codes.([3]string)
}

// combine together multiple temporal aggregators
func (ta *temporalAggregator) Combine(a ...*temporalAggregator) {
	// zero data
	ta.overallLock.Lock()
	defer ta.overallLock.Unlock()
	ta.PointData = make(map[[3]string][]*PointRecord)
	ta.AreaData = make(map[[3]string]map[string][]*sparse.SparseArray)
	for _, t := range a {
		for temporalCodes, data := range t.AreaData {
			if _, ok := ta.AreaData[temporalCodes]; !ok {
				ta.AreaData[temporalCodes] =
					make(map[string][]*sparse.SparseArray)
			}
			for pol, gridData := range data {
				if _, ok := ta.AreaData[temporalCodes][pol]; !ok {
					ta.AreaData[temporalCodes][pol] =
						make([]*sparse.SparseArray, len(grids))
					for i, grid := range grids {
						ta.AreaData[temporalCodes][pol][i] =
							sparse.ZerosSparse(grid.Ny, grid.Nx)
					}
				}
				for i, g := range gridData {
					ta.AreaData[temporalCodes][pol][i].AddSparse(g)
				}
			}
		}
		for temporalCodes, data := range t.PointData {
			if _, ok := ta.PointData[temporalCodes]; !ok {
				ta.PointData[temporalCodes] =
					make([]*PointRecord, 0, 10000)
			}
			for _, val := range data {
				ta.PointData[temporalCodes] = append(
					ta.PointData[temporalCodes], val)
			}
		}
	}
}

type temporalAggregator struct {
	PointData map[[3]string][]*PointRecord
	PointLock sync.Mutex
	AreaData  map[[3]string]map[string][]*sparse.SparseArray
	AreaLock  sync.Mutex
	config    *RunData
	WaitGroup sync.WaitGroup
	overallLock sync.RWMutex
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
	Row      []int                    // grid row number
	Col      []int                    // grid column number
	ANN_EMIS map[string]*specValUnits // Annual Emissions (tons/year) (required)
}

func newPointRecord(r *ParsedRecord) *PointRecord {
	out := new(PointRecord)
	out.STKHGT = r.STKHGT
	out.STKDIAM = r.STKDIAM
	out.STKTEMP = r.STKTEMP
	out.STKFLOW = r.STKFLOW
	out.STKVEL = r.STKVEL
	out.Row = make([]int, len(grids))
	out.Col = make([]int, len(grids))
	for i, grid := range grids {
		out.Row[i] = int((r.PointYcoord - grid.Y0) /
			grid.Dy)
		out.Col[i] = int((r.PointXcoord - grid.X0) /
			grid.Dx)
	}
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
	if len(record.ANN_EMIS) != 0 {
		t.PointData[temporalCodes] = append(t.PointData[temporalCodes],
			newPointRecord(record))
	}
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

func (c *RunData) SectorTemporal(InputChan chan *ParsedRecord,
	OutputChan chan *ParsedRecord, period string) {
	defer c.ErrorRecoverCloseChan(InputChan)
	c.Log("Aggregating by temporal profile "+period+" "+c.Sector+"...", 1)
	var temporalAgg *temporalAggregator
	switch c.InventoryFreq {
	case "annual":
		temporalAgg = <-AnnualTemporalChan
		c.Log(c.Sector+" got annual aggregator", 1)
	case "monthly":
		temporalAgg = <-MonthlyTemporalChan
		c.Log(c.Sector+" got monthly aggregator", 1)
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
	close(OutputChan)
	c.msgchan <- "Finished temporalizing " + period + " " + c.Sector
	return
}

func (c *RunData) NextTime() (keepGoing bool) {
	c.currentTime = c.currentTime.Add(c.tStep)
	keepGoing = c.currentTime.Before(c.endDate)
	return
}

func (c *RunData) CurrentTime() time.Time {
	return c.currentTime
}

func (c *RunData) CurrentMonth() (month string) {
	month = strings.ToLower(c.currentTime.Format("Jan"))
	return
}

type timeStep struct {
	ta   *temporalAggregator
	Time time.Time
}

func (c *RunData) Temporal(numAnnualSectors,
	numMonthlySectors int, outchan chan *OutputDataChan, msgChan chan string) {
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
	c.Log("Annual temporal aggregation finished", 1)
	monthData.WaitGroup.Wait() // wait for sectors to finish
	c.Log("Monthly temporal aggregation finished", 1)
	Tdata := c.newTemporalAggregator(0)
	Tdata.Combine(annualData, monthData)
	// get all of the pollutants we will be outputting, and their units
	dr := Report.PrepDataReport("Spatialization", "d01")
	polsAndUnits := dr.units
	tstepsInFile := 0
	tstepChan := make(chan timeStep)
	// iterate through time steps
	for c.currentTime.Before(c.endDate) {
		if tstepsInFile == 0 {
			close(tstepChan)
			tstepChan = make(chan timeStep)
			outchan <- &OutputDataChan{tstepChan, c.currentTime, polsAndUnits}
		}
		month := c.CurrentMonth()
		if month != temporalMonth {
			temporalMonth = month
			monthData = c.newTemporalAggregator(numMonthlySectors)
			for i := 0; i < numMonthlySectors; i++ {
				MonthlyTemporalChan <- monthData
			}
			monthData.WaitGroup.Wait() // wait for sectors to finish
			c.Log("Monthly temporal aggregation finished", 1)
			Tdata.Combine(annualData, monthData)
			// get all of the pollutants we will be outputting, and their units
			dr := Report.PrepDataReport("Spatialization", "d01")
			polsAndUnits = dr.units
		}
		c.Log(c.currentTime, 0)
		tstepChan <- timeStep{Tdata, c.currentTime}
		tstepsInFile++
		if tstepsInFile == 24 {
			tstepsInFile = 0
		}
		// either advance to next date or end loop
		keepGoing := c.NextTime()
		if !keepGoing {
			break
		}
	}
	close(tstepChan)
	close(outchan)
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
