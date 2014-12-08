package aep

import (
	"fmt"
	"io"
	"math"
	"os"
	"path/filepath"
	"runtime"
	"testing"

	"github.com/ctessum/shapefile"
)

const (
	Tolerance = 1.e-5
	shpDir    = "/media/chris/data1/2005_nei_data/shapefiles"
)

var (
	c          *Context
	configFile = "../scripts/config2005nei.json"
	//configFile = "../scripts/config2005nei_na12.json"
)

func init() {
	e := new(ErrCat) // error report
	testMode := false
	c = ReadConfigFile(&configFile, &testMode, nil, e).DefaultSettings
	runtime.GOMAXPROCS(c.Ncpus)
	c.SpatialSetupRegularGrid(e)
	e.Report() // Print errors, if any
}

func TestSrg100(t *testing.T) {
	//aep_test.go:142: d01___USA100.dbf, 27123, surrogate total=0.927: FAIL
	//aep_test.go:142: d01___USA596.dbf, 27123, surrogate total=0.743: FAIL
	const srgNum = "USA100"
	inputMap := SrgSpec[srgNum].DATASHAPEFILE
	inputColumn := SrgSpec[srgNum].DATAATTRIBUTE
	surrogateMap := SrgSpec[srgNum].WEIGHTSHAPEFILE
	WeightColumns := SrgSpec[srgNum].WeightColumns
	FilterFunction := SrgSpec[srgNum].FilterFunction
	inputFilePath := filepath.Join(shpDir, inputMap+".shp")
	surrogateFilePath := filepath.Join(shpDir, surrogateMap+".shp")
	err := CreateGriddingSurrogate(srgNum, inputFilePath,
		inputColumn, surrogateFilePath, WeightColumns, FilterFunction,
		Grids[0], ".", nil)
	if err != nil {
		panic(err)
	}
	srgDB := "d01___" + srgNum + ".dbf"
	f, err := os.Open(srgDB)
	if err != nil {
		panic(err)
	}
	defer f.Close()
	data, err := shapefile.OpenDBFFile(f)
	if err != nil {
		panic(err)
	}
	columnIDs := make([]int, 4)
	for i, column := range []string{"inputID",
		"shapeFrac", "allCovered"} {
		var ok bool
		columnIDs[i], ok = data.FieldIndicies[column]
		if !ok {
			err = fmt.Errorf("Column %v not found in %v. "+
				"Column options are %v",
				column, srgDB, data.FieldIndicies)
			panic(err)
		}
	}

	srgSums := make(map[string]float64)
	for {
		tempVals, err := data.NextRecord()
		if err != nil {
			if err == io.EOF {
				err = nil
				break
			}
			panic(err)
		}
		inputID := tempVals[columnIDs[0]].(string)
		shapeFraction := tempVals[columnIDs[1]].(float64)
		allCovered := tempVals[columnIDs[2]].(int)
		if allCovered == 1 {
			srgSums[inputID] += shapeFraction
		}
	}
	for inputID, sum := range srgSums {
		var passfail string
		difference := diff(sum, 1.0)
		if difference > Tolerance || math.IsNaN(difference) {
			t.Fail()
			passfail = "FAIL"
		} else {
			passfail = "PASS"
		}
		t.Logf("%v, %v, surrogate total=%.3f: %v\n",
			srgDB, inputID, sum, passfail)
	}
}

// match determines fractional difference between 2 numbers.
func diff(val1, val2 float64) float64 {
	if val1 == 0. && val2 == 0. {
		return 0.
	} else {
		return math.Abs((val1 - val2) / (val1 + val2) * 2)
	}
}
