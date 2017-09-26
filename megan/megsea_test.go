package megan

import (
	"testing"
	"os"
)

func GetSoilMoistureAndNOEmissionActivityTestData() SoilMoistureAndNOEmissionActivity {
	NOEmissionActivity := []float64{0.6901199}
	SoilMoistureActivity := []float64{0.5020496}
	return SoilMoistureAndNOEmissionActivity{NOEmissionActivity, SoilMoistureActivity}
}

func TestMegsea(t *testing.T) {
	go_output, err := run_go_megsea()
	if err != nil {
		t.Errorf("ERROR:", err)
	}
	
	expected_output := GetSoilMoistureAndNOEmissionActivityTestData()

	if !are_megsea_outputs_equal(go_output, expected_output) {
		t.Errorf("Go and standalone versions produce different results (epsilon=%v)\n", EPSILON)
	}
}

func TestMegseaAgainstStandalone(t *testing.T) {
	if os.Getenv("MEGAN_STANDALONE_TEST") == "" {
		t.Skip("skipping test; $MEGAN_STANDALONE_TEST not set")
	}

	go_output, err := run_go_megsea()
	if err != nil {
		t.Errorf("ERROR:", err)
	}
	
	standalone_output := run_standalone_megsea()
	
	if !are_megsea_outputs_equal(go_output, standalone_output) {
		t.Errorf("Go and standalone versions produce different results (epsilon=%v)\n", EPSILON)
	}
}
 
func run_go_megsea() (output SoilMoistureAndNOEmissionActivity, err error) {
	start_date := 2013145
	start_time := 0
	time_increment := 10000
	use_PX_version_of_MCIP := true
	temperature := []float64{297.0823}	
	soil_moisture := []float64{0.195082}	
	soil_temperature := []float64{303.537}			
	precip_adjustment := []float64{2}		
	lai := []float64{1.5165}	
	lattitude := []float64{24.9699}	
	soil_type := []float64{6}
	canopy_type_factor := []float64{0, 21.6363, 30.5448, 34.4223, 33.8522, 36.0447}
	
	return SoilMoistureAndNOEmissionActivityFactors(start_date, start_time, time_increment, use_PX_version_of_MCIP, temperature, soil_moisture, soil_temperature, precip_adjustment, lai, lattitude, soil_type, canopy_type_factor)
}

func run_standalone_megsea() SoilMoistureAndNOEmissionActivity {
	// Run standalone MEGSEA script
	run_command("cd ./MEGAN3/work/; ./run.megsea.v3.single.csh")
	
	// Extract outputs from NETCDF files
	output_file := "./MEGAN3/Output/INT/MGNSEA.tceq_12km.2013145.single.nc"
	gamno := parse_netcdf_file("GAMNO", output_file)
	gamsm := parse_netcdf_file("GAMSM", output_file)
	
	return SoilMoistureAndNOEmissionActivity{gamno, gamsm}
}

func are_megsea_outputs_equal(output1 SoilMoistureAndNOEmissionActivity, output2 SoilMoistureAndNOEmissionActivity) bool {
	GAMNO_equal := arrays_approximately_equal(output1.NOEmissionActivity, output2.NOEmissionActivity, EPSILON, "GAMNO")
	GAMSM_equal := arrays_approximately_equal(output1.SoilMoistureActivity, output2.SoilMoistureActivity, EPSILON, "GAMSM")

	return GAMNO_equal && GAMSM_equal
}