package megan

import (
	"testing"
	"os"
)

func GetSoilMoistureAndNOEmissionActivityTestData() SoilMoistureAndNOEmissionActivity {
	NOEmissionActivity := []float64{0.6491075, 0.6640053, 0.6189721, 0.5696779, 0.5248248, 0.5029572, 0.523435, 0.546968, 0.6322287, 0.7603613, 0.7641032, 0.699164, 0.6803498, 0.6906353, 0.6858764, 0.6965074, 0.7716522, 0.8314256, 0.8096505, 0.8013604, 0.7773128, 0.7740801, 0.8019726, 0.8279191, 0.8342465}
	SoilMoistureActivity := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.427885, 0.4877728, 0.05184002, 0, 0, 0, 0.4081927, 0.8413952, 1, 1, 0.820465, 1}
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
	temperature := []float64{297.0823, 297.7277, 296.1826, 294.3032, 292.3727, 291.442, 292.1451, 293.1522, 296.5544, 300.8124, 300.7801, 298.4268, 297.1981, 297.1527, 296.9184, 297.636, 301.0106, 303.6024, 301.8526, 301.2423, 300.1718, 299.744, 300.7241, 301.8956, 301.7262}	
	soil_moisture := []float64{0.09508198, 0.09217762, 0.09338263, 0.1172753, 0.1491531, 0.1688348, 0.1636439, 0.1466109, 0.1253012, 0.122324, 0.1337502, 0.1340503, 0.164309, 0.1921154, 0.1945109, 0.1770736, 0.1356585, 0.1313382, 0.1664971, 0.1913277, 0.2086558, 0.2300829, 0.2300689, 0.2078186, 0.2377395}	
	soil_temperature := []float64{303.537, 303.9827, 302.1665, 300.2706, 298.6183, 297.6171, 298.619, 299.5035, 301.3833, 307.6646, 309.4813, 305.3076, 305.4087, 305.0209, 304.9799, 307.18, 310.0703, 312.6512, 310.1442, 309.9953, 308.3934, 309.0668, 309.3318, 311.3359, 308.7086}			
	precip_adjustment := []float64{2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}		
	lai := []float64{1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165}	
	lattitude := []float64{24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699}	
	soil_type := []float64{6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}
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