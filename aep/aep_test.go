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

package main

import (
	"fmt"
	"io"
	"math"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"bitbucket.org/ctessum/aep/lib.aep"

	"github.com/ctessum/shapefile"
	"github.com/gonum/floats"
)

const (
	Tolerance = 1.e-5
)

var gopath string

func init() {
	gopath = os.Getenv("GOPATH")
}

func TestModelRun(t *testing.T) {
	config := filepath.Join(gopath, "src", "github.com", "ctessum", "aep", "aep",
		"scripts", "config2005nei.json")
	os.Args = append(os.Args, "-config="+config, "-testmode=true", "-seq") //, "-sectors=othon")
	main()
}

// Check whether the total mass of all pollutants in inventory matches
// total mass of all pollutants after speciation.
func TestSpeciation(t *testing.T) {
	for sector, data1 := range aep.Report.SectorResults {
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

// Check whether the sums of gridding surrogates for counties that fall
// completely within the gridding domains are equal to one.
func TestGriddingSurrogates(t *testing.T) {
	srgDir := filepath.Join(gopath, "src", "bitbucket.org", "ctessum", "aep",
		"test", "Output", "srgs")
	err := filepath.Walk(srgDir,
		func(path string, info os.FileInfo, err error) error {
			fname := info.Name()
			if strings.HasSuffix(path, ".dbf") && strings.Contains(fname, "_") {
				f, err := os.Open(path)
				defer f.Close()
				if err != nil {
					return err
				}
				data, err := shapefile.OpenDBFFile(f)
				if err != nil {
					return err
				}
				columnIDs := make([]int, 4)
				for i, column := range []string{"inputID",
					"shapeFrac", "allCovered"} {
					var ok bool
					columnIDs[i], ok = data.FieldIndicies[column]
					if !ok {
						err = fmt.Errorf("Column %v not found in %v. "+
							"Column options are %v",
							column, path, data.FieldIndicies)
						return err
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
						return err
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
						fname, inputID, sum, passfail)
				}
				return err
			}
			return err
		})
	if err != nil {
		panic(err)
	}
}

// Check whether the total mass of each pollutant after speciation matches
// the mass of the same pollutant after gridding.
func TestSpatialization(t *testing.T) {
	for sector, data1 := range aep.Report.SectorResults {
		for period, results := range data1 {
			for pol, specVal := range results.SpeciationResults.Kept {
				for grid, _ := range results.SpatialResults.InsideDomainTotals {
					var in, out float64
					if x, ok := results.SpatialResults.
						InsideDomainTotals[grid][pol]; ok {
						in = x.Val
					}
					if x, ok := results.SpatialResults.
						OutsideDomainTotals[grid][pol]; ok {
						out = x.Val
					}
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

// Check whether the total mass of each pollutant after temporalization matches
// the mass of the same pollutant after gridding.
func TestTemporalization(t *testing.T) {
	// Calculate total spatialized emissions
	spatialData := make(map[string]map[string]float64)
	for _, grid := range aep.Grids {
		spatialData[grid.Name] = make(map[string]float64)
		for _, sectorDataPeriod := range aep.Report.SectorResults {
			numPeriods := float64(len(sectorDataPeriod))
			for _, sectorData := range sectorDataPeriod {
				for pol, inData := range sectorData.SpatialResults.
					InsideDomainTotals[grid.Name] {
					// Total emissions inside the domain
					spatialData[grid.Name][pol] += inData.Val / numPeriods
				}
			}
		}
	}
	for _, grid := range aep.Grids {
		for pol, d := range aep.Report.TemporalResults.Data {
			tData := d[grid.Name]
			tSum := floats.Sum(tData)
			difference := diff(tSum, spatialData[grid.Name][pol])
			t.Logf("%v, %v:\nspatial total=%.5e, "+
				"temporal total=%.5e, fractional difference=%.1e\n",
				pol, grid.Name, spatialData[grid.Name][pol], tSum, difference)
			if difference > Tolerance || math.IsNaN(difference) {
				t.Fail()
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
