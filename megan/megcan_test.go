/*
Copyright (C) 2017 the AEP authors.
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

package megan
 
import (
	"testing"
	"os"
)

func GetWithinCanopyMeteorologyTestData() WithinCanopyMeteorology {
	SunleafTK := []float64{295.6182, 295.8877, 295.1347, 295.1322, 295.3771}
	ShadeleafTK := []float64{295.1602, 295.4964, 292.3763, 291.6741, 291.9568}
	SunPPFD := []float64{617.0796, 581.216, 534.6226, 496.9291, 476.1383}
	ShadePPFD := []float64{235.1983, 199.3347, 152.7413, 115.0479, 94.25706}
	SunFrac := []float64{0.9111186, 0.6343481, 0.3760942, 0.22486, 0.1589793}
	
	return WithinCanopyMeteorology{SunleafTK, ShadeleafTK, SunPPFD, ShadePPFD, SunFrac}
}

func TestMegcan(t *testing.T) {
	go_output, err := run_go_megcan()
	if err != nil {
		t.Errorf("ERROR:", err)
	}
	
	expected_output := GetWithinCanopyMeteorologyTestData()

	if !are_megcan_outputs_equal(go_output, expected_output) {
		t.Errorf("Go and standalone versions produce different results (epsilon=%v)\n", EPSILON)
	}
}

func TestMegcanAgainstStandalone(t *testing.T) {
	if os.Getenv("MEGAN_STANDALONE_TEST") == "" {
		t.Skip("skipping test; $MEGAN_STANDALONE_TEST not set")
	}

	go_output, err := run_go_megcan()
	if err != nil {
		t.Errorf("ERROR:", err)
	}
	
	standalone_output := run_standalone_megcan()
	
	if !are_megcan_outputs_equal(go_output, standalone_output) {
		t.Errorf("Go and standalone versions produce different results (epsilon=%v)\n", EPSILON)
	}
}

func run_go_megcan() (output WithinCanopyMeteorology, err error) {
	date := 2013145
	time := 0
	latitude := 24.9699
	longitude := -106.4971
	leaf_area_index := 1.5165
	temperature := 297.082306
	incoming_photosynthetic_active_radiation := 170.3816
	wind_speed := 5.094378
	pressure := 82208.09
	water_vapor_mixing_ratio := 0.007762248
	canopy_type_factor := []float64{0, 21.6363, 30.5448, 34.4223, 33.8522, 36.0447}
	
	return ConvertAboveCanopyMeteorologyToWithinCanopyMeteorology(date, time, latitude, longitude, leaf_area_index, temperature, incoming_photosynthetic_active_radiation, wind_speed, pressure, water_vapor_mixing_ratio, canopy_type_factor)
}

func run_standalone_megcan() WithinCanopyMeteorology {
	// Run standalone MEGCAN script
	run_command("cd ./MEGAN3/work/; ./run.megcan.v3.single.csh")
	
	// Extract outputs from NETCDF files
	output_file := "./MEGAN3/Output/INT/CANMET.tceq_12km.2013145.single.nc"
	SunleafTK := parse_netcdf_file("SunleafTK", output_file)
	ShadeleafTK := parse_netcdf_file("ShadeleafTK", output_file)
	SunPPFD := parse_netcdf_file("SunPPFD", output_file)
	ShadePPFD := parse_netcdf_file("ShadePPFD", output_file)
	SunFrac := parse_netcdf_file("SunFrac", output_file)
	
	return WithinCanopyMeteorology{SunleafTK, ShadeleafTK, SunPPFD, ShadePPFD, SunFrac}
}

func are_megcan_outputs_equal(output1 WithinCanopyMeteorology, output2 WithinCanopyMeteorology) bool {
	SunleafTK_equal := arrays_approximately_equal(output1.SunleafTK, output2.SunleafTK, EPSILON, "SunleafTK")
	ShadeleafTK_equal := arrays_approximately_equal(output1.ShadeleafTK, output2.ShadeleafTK, EPSILON, "ShadeleafTK")
	SunPPFD_equal := arrays_approximately_equal(output1.SunPPFD, output2.SunPPFD, EPSILON, "SunPPFD")
	ShadePPFD_equal := arrays_approximately_equal(output1.ShadePPFD, output2.ShadePPFD, EPSILON, "ShadePPFD")
	SunFrac_equal := arrays_approximately_equal(output1.SunFrac, output2.SunFrac, EPSILON, "SunFrac")
	
	return SunleafTK_equal && ShadeleafTK_equal && SunPPFD_equal && ShadePPFD_equal && SunFrac_equal
}