package main

import (
	"bitbucket.org/ctessum/aep/gis"
	"github.com/skelterjohn/go.matrix"
	"math"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

const (
	Tolerance = 1.e-5
)

var (
	testFIPS = []string{"27123"}
)

func TestModelRun(t *testing.T) {
	gopath := os.Getenv("GOPATH")
	config := filepath.Join(gopath, "src", "bitbucket.org", "ctessum", "aep", "test", "Minneapolis2005.json")
	os.Args = append(os.Args, "-config="+config, "-testmode=true") //, "-sectors=ptipm")
	main()
}

// Check whether the total mass of all pollutants in inventory matches
// total mass of all pollutants after speciation.
func TestSpeciation(t *testing.T) {
	for sector, data1 := range Report.SectorResults {
		for period, results := range data1 {
			var inventoryTotal, speciationTotal float64
			for _, vals := range results.SpeciationResults.Kept {
				speciationTotal += vals.Val
			}
			for _, vals := range results.SpeciationResults.DoubleCounted {
				speciationTotal += vals.Val
			}
			for _, vals := range results.SpeciationResults.Ungrouped {
				speciationTotal += vals.Val
			}
			var InputConv float64
			for _, data := range results.InventoryResults {
				InputConv = data.InputConv // This should be the same for every file
				for _, val := range data.Totals {
					inventoryTotal += val
				}
			}
			speciationTotal /= InputConv
			difference := diff(inventoryTotal, speciationTotal)
			t.Logf("%v, %v, inventory=%.5e, speciation=%.5e, "+
				"fractional difference=%.1e\n", sector, period, inventoryTotal,
				speciationTotal, difference)
			if difference > Tolerance || math.IsNaN(difference) {
				t.Fail()
			}
		}
	}
}

// Check whether the sums of gridding surrogates for counties known to fall
// completely within all of the gridding domains are equal to one.
// This test might fail if there are surrogates for more than one country
func TestGriddingSurrogates(t *testing.T) {
	pg, err := gis.Connect(Report.Config.PostGISuser,
		Report.Config.PostGISdatabase, Report.Config.PostGISpassword)
	defer pg.Disconnect()
	if err != nil {
		panic(err)
	}
	var passfail string
	for _, FIPS := range testFIPS {
		for tableName, status := range Status.Surrogates {
			if status == "Ready" {
				splitSrg := strings.Split(tableName, "_")
				var srg *matrix.SparseMatrix
				for _, grid := range grids {
					if grid.Name == splitSrg[0] {
						srg = Report.Config.retrieveSurrogate(splitSrg[1],
							FIPS, grid, pg, make([]string, 0))
					}
				}
				sum := MatrixSum(srg)
				difference := diff(sum, 1.0)
				if difference > Tolerance || math.IsNaN(difference) {
					t.Fail()
					passfail = "FAIL"
				} else {
					passfail = "PASS"
				}
				t.Logf("%v, %v, surrogate error=%.1e: %v\n",
					tableName, FIPS, difference, passfail)
			} else {
				t.Logf("%v, %v, %v, Surrogate did not generate "+
					"correctly. Status=%v\n", tableName, FIPS, status)
			}
		}
	}
}

// Check whether the total mass of each pollutant after speciation matches
// the mass of the same pollutant after gridding.
func TestSpatialization(t *testing.T) {
	for sector, data1 := range Report.SectorResults {
		for period, results := range data1 {
			for pol, specVal := range results.SpeciationResults.Kept {
				for grid, _ := range results.SpatialResults.InsideDomainTotals {
					in := results.SpatialResults.InsideDomainTotals[grid][pol].Val
					out := results.SpatialResults.OutsideDomainTotals[grid][pol].Val
					spatialTotal := in + out
					difference := diff(specVal.Val, spatialTotal)
					t.Logf("%v, %v, %v, %v:\nspeciation=%.5e, "+
						"spatial total=%.5e, fractional difference=%.1e\n",
						sector, period, pol, grid, specVal.Val, spatialTotal,
						difference)
					if difference > Tolerance || math.IsNaN(difference) {
						t.Fail()
					}
				}
			}
		}
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
