/*
Copyright © 2017 the AEP authors.
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

package aeputil

import (
	"os"
	"testing"

	"github.com/BurntSushi/toml"
	"github.com/ctessum/aep"
	"github.com/ctessum/geom"
	"github.com/ctessum/geom/proj"
	"github.com/ctessum/unit"
)

func TestSpatial(t *testing.T) {
	type config struct {
		Inventory InventoryConfig
		Spatial   SpatialConfig
	}
	r, err := os.Open("testdata/example_config.toml")
	if err != nil {
		t.Fatal(err)
	}

	c := new(config)

	// Read the configuration file into the configuration variable.
	if _, err = toml.DecodeReader(r, c); err != nil {
		t.Fatal(err)
	}

	sr, err := proj.Parse(c.Spatial.OutputSR)
	if err != nil {
		t.Fatal(err)
	}
	grid := aep.NewGridRegular("test", 111, 84, 48000, 48000, -2736000.0, -2088000.0, sr)
	g := make([]geom.Polygonal, len(grid.Cells))
	for i, c := range grid.Cells {
		g[i] = c.Polygonal
	}
	c.Spatial.GridCells = g

	records, _, err := c.Inventory.ReadEmissions()
	if err != nil {
		t.Fatal(err)
	}

	wantEmis := map[string]map[aep.Pollutant]float64{
		"2280003010": map[aep.Pollutant]float64{
			aep.Pollutant{Name: "NOX"}:   1.9694509976996027e+07,
			aep.Pollutant{Name: "VOC"}:   650406.6575234848,
			aep.Pollutant{Name: "PM2_5"}: 1.3251549508572659e+06,
			aep.Pollutant{Name: "SO2"}:   1.5804381260919824e+07,
		},
		"2280003030": map[aep.Pollutant]float64{
			aep.Pollutant{Name: "VOC"}:   20.29296822897,
			aep.Pollutant{Name: "PM2_5"}: 186.401532717915,
			aep.Pollutant{Name: "NH3"}:   34.056105917699995,
			aep.Pollutant{Name: "SO2"}:   1939.6783010388299,
			aep.Pollutant{Name: "NOX"}:   3329.29929452133,
		},
	}

	wantUnits := map[string]map[aep.Pollutant]unit.Dimensions{
		"2280003010": map[aep.Pollutant]unit.Dimensions{
			aep.Pollutant{Name: "VOC"}:   unit.Dimensions{4: 1},
			aep.Pollutant{Name: "PM2_5"}: unit.Dimensions{4: 1},
			aep.Pollutant{Name: "SO2"}:   unit.Dimensions{4: 1},
			aep.Pollutant{Name: "NOX"}:   unit.Dimensions{4: 1},
		},
		"2280003030": map[aep.Pollutant]unit.Dimensions{
			aep.Pollutant{Name: "PM2_5"}: unit.Dimensions{4: 1},
			aep.Pollutant{Name: "NH3"}:   unit.Dimensions{4: 1},
			aep.Pollutant{Name: "SO2"}:   unit.Dimensions{4: 1},
			aep.Pollutant{Name: "NOX"}:   unit.Dimensions{4: 1},
			aep.Pollutant{Name: "VOC"}:   unit.Dimensions{4: 1},
		},
	}
	for scc, recs := range records {
		emis, units, err := c.Spatial.SpatializeTotal(recs...)
		if err != nil {
			t.Fatal(err)
		}
		for pol, grid := range emis {
			if grid[0].Sum() != wantEmis[scc][pol] {
				t.Errorf("emissions for %v %v, have %g but want %g", scc, pol, grid[0].Sum(), wantEmis[scc][pol])
			}
		}
		for pol, u := range units {
			if !u.Matches(wantUnits[scc][pol]) {
				t.Errorf("units for %v %v: have %v but want %v", scc, pol, wantUnits[scc][pol], units)
			}
		}
	}
}
