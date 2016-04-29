package aep

import (
	"reflect"
	"testing"
	"time"

	"github.com/ctessum/unit"
	"github.com/ctessum/unit/badunit"
)

func TestEmissions(t *testing.T) {
	e := new(Emissions)

	begin, _ := time.Parse("Jan 2006", "Jan 2005")
	end, _ := time.Parse("Jan 2006", "Jan 2006")
	rate := unit.New(1, map[unit.Dimension]int{unit.MassDim: 1, unit.TimeDim: -1})
	e.Add(begin, end, "testpol", "", rate)

	begin2, _ := time.Parse("Jan 2006", "Jun 2005")
	end2, _ := time.Parse("Jan 2006", "Jun 2006")
	rate2 := unit.New(1, map[unit.Dimension]int{unit.MassDim: 1, unit.TimeDim: -1})
	e.Add(begin2, end2, "testpol", "", rate2)
	e.Add(begin2, end2, "testpol2", "", rate2)
	e.Add(begin2, end2, "testpol3", "", rate2)

	wantTotals := map[Pollutant]*unit.Unit{
		Pollutant{Name: "testpol"}:  unit.New(6.3072e+07, unit.Kilogram),
		Pollutant{Name: "testpol2"}: unit.New(3.1536e+07, unit.Kilogram),
		Pollutant{Name: "testpol3"}: unit.New(3.1536e+07, unit.Kilogram),
	}
	haveTotals := e.Totals()
	if !reflect.DeepEqual(wantTotals, haveTotals) {
		t.Errorf("totals: want %v but have %v", wantTotals, haveTotals)
	}

	begin3, _ := time.Parse("Jan 2006", "Jun 2004")
	havePeriod1 := e.PeriodTotals(begin3, end)
	wantPeriod1 := map[Pollutant]*unit.Unit{
		Pollutant{Name: "testpol"}:  unit.New(5.00256e+07, unit.Kilogram),
		Pollutant{Name: "testpol2"}: unit.New(1.84896e+07, unit.Kilogram),
		Pollutant{Name: "testpol3"}: unit.New(1.84896e+07, unit.Kilogram),
	}
	if !reflect.DeepEqual(wantPeriod1, havePeriod1) {
		t.Errorf("period1: want %v but have %v", wantPeriod1, havePeriod1)
	}

	begin4, _ := time.Parse("Jan 2006", "Jun 2004")
	end3, _ := time.Parse("Jan 2006", "Jun 2007")
	havePeriod2 := e.PeriodTotals(begin4, end3) // should be the same as above
	if !reflect.DeepEqual(wantTotals, havePeriod2) {
		t.Errorf("period2: want %v but have %v", wantPeriod1, havePeriod2)
	}

	begin5, _ := time.Parse("Jan 2006", "Jun 2004")
	end5, _ := time.Parse("Jan 2006", "Jan 2005")
	havePeriod3 := e.PeriodTotals(begin5, end5)
	if len(havePeriod3) != 0 {
		t.Errorf("period3: want empty map but have %v", havePeriod3)
	}

	begin6, _ := time.Parse("Jan 2006", "Jun 2007")
	end6, _ := time.Parse("Jan 2006", "Jan 2008")
	havePeriod4 := e.PeriodTotals(begin6, end6)
	if len(havePeriod4) != 0 {
		t.Errorf("period4: want empty map but have %v", havePeriod4)
	}

	droppedTotals := e.DropPols(map[string]*PolHolder{"testpol": &PolHolder{}})
	newTotals := e.Totals()
	wantDroppedTotals := map[Pollutant]*unit.Unit{
		Pollutant{Name: "testpol2"}: unit.New(3.1536e+07, unit.Kilogram),
		Pollutant{Name: "testpol3"}: unit.New(3.1536e+07, unit.Kilogram),
	}
	wantNewTotals := map[Pollutant]*unit.Unit{
		Pollutant{Name: "testpol"}: unit.New(6.3072e+07, unit.Kilogram),
	}
	if !reflect.DeepEqual(wantDroppedTotals, droppedTotals) {
		t.Errorf("dropped totals: want %v but have %v", wantDroppedTotals, droppedTotals)
	}
	if !reflect.DeepEqual(wantNewTotals, newTotals) {
		t.Errorf("new totals: want %v but have %v", wantNewTotals, newTotals)
	}

	v, err := parseEmisRateAnnual(nullVal, "1", badunit.Ton) // 1 ton/day
	if err != nil {
		t.Error(err)
	}
	want := unit.New(0.010499826388888888, unit.Dimensions{unit.MassDim: 1, unit.TimeDim: -1})
	if !reflect.DeepEqual(v, want) {
		t.Errorf("parseEmisRate: want %v but have %v", want, v)
	}

	v, err = parseEmisRateAnnual("1", "1", badunit.Ton) // 1 ton/year
	if err != nil {
		t.Error(err)
	}
	want = unit.New(2.8766647640791475e-05, unit.Dimensions{unit.MassDim: 1, unit.TimeDim: -1})
	if !reflect.DeepEqual(v, want) {
		t.Errorf("parseEmisRate: want %v but have %v", want, v)
	}

}
