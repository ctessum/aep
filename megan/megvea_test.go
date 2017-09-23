package megan

import (
	"testing"
	"os"
)

func TestMegvea(t *testing.T) {
	go_output := run_go_megvea()
	
	ISOP_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06770491, 0.1506119, 0.01119377, 0, 0, 0, 0.196123, 0.1344049, 0.1061472, 0.1205298, 0.1168384, 0.2244172}
	MBO_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06770491, 0.1506119, 0.01119377, 0, 0, 0, 0.196123, 0.1344049, 0.1061472, 0.1205298, 0.1168384, 0.2244172}
	MT_PINE_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06933573, 0.1202633, 0.01123249, 0, 0, 0, 0.134856, 0.1982512, 0.2188023, 0.2465584, 0.2356006, 0.3204671}
	MT_ACYC_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.08496049, 0.1642323, 0.01366621, 0, 0, 0, 0.1953614, 0.2000265, 0.1960313, 0.219812, 0.2091313, 0.3202239}
	MT_CAMP_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.05284291, 0.08925098, 0.008574537, 0, 0, 0, 0.09848323, 0.1572113, 0.1770349, 0.1996476, 0.1909046, 0.2545713}
	MT_SABI_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.05447638, 0.09219893, 0.008838497, 0, 0, 0, 0.101865, 0.1615902, 0.1816995, 0.2048965, 0.1959141, 0.2616284}
	MT_AROM_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.08056435, 0.1449134, 0.01302163, 0, 0, 0, 0.1659329, 0.217198, 0.2321275, 0.2612407, 0.2493507, 0.3501392}
	MT_OXY_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.03801988, 0.06323272, 0.006174963, 0, 0, 0, 0.06910365, 0.1156103, 0.1315724, 0.1484378, 0.1419873, 0.1873812}
	SQT_HR_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.03663687, 0.08102604, 0.007240801, 0, 0, 0, 0.1088041, 0.1389914, 0.1483657, 0.1817109, 0.1929337, 0.2755488}
	SQT_LR_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.03296247, 0.07232749, 0.006560703, 0, 0, 0, 0.0965313, 0.1286724, 0.1389317, 0.170283, 0.1809583, 0.2555993}
	MEOH_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.09560993, 0.1912228, 0.01457586, 0, 0, 0, 0.2235541, 0.1625942, 0.1300601, 0.1399062, 0.1269294, 0.2372466}
	ACTO_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.03786996, 0.06298337, 0.006150613, 0, 0, 0, 0.06883116, 0.1151544, 0.1310535, 0.1478524, 0.1414274, 0.1866423}
	ETOH_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.05224184, 0.1083723, 0.008910099, 0, 0, 0, 0.136238, 0.135417, 0.1317498, 0.1524265, 0.1508592, 0.2336758}
	ACID_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06889792, 0.1429243, 0.01175087, 0, 0, 0, 0.1796743, 0.1785914, 0.1737551, 0.2010241, 0.198957, 0.3081778}
	LVOC_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.03786996, 0.06298337, 0.006150613, 0, 0, 0, 0.06883116, 0.1151544, 0.1310535, 0.1478524, 0.1414274, 0.1866423}
	OXPROD_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.03786996, 0.06298337, 0.006150613, 0, 0, 0, 0.06883116, 0.1151544, 0.1310535, 0.1478524, 0.1414274, 0.1866423}
	STRESS_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0847534, 0.1627171, 0.01363934, 0, 0, 0, 0.1928948, 0.2023743, 0.2003172, 0.2247161, 0.21388, 0.3242324}
	OTHER_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.03786996, 0.06298337, 0.006150613, 0, 0, 0, 0.06883116, 0.1151544, 0.1310535, 0.1478524, 0.1414274, 0.1866423}
	CO_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.09411947, 0.1882419, 0.01434864, 0, 0, 0, 0.2200691, 0.1600596, 0.1280326, 0.1377252, 0.1249507, 0.2335482}
	NO_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

	expected_output := Megvea_output{ISOP_expected, MBO_expected, MT_PINE_expected, MT_ACYC_expected, MT_CAMP_expected, MT_SABI_expected, MT_AROM_expected, MT_OXY_expected, SQT_HR_expected, SQT_LR_expected, MEOH_expected, ACTO_expected, ETOH_expected, ACID_expected, LVOC_expected, OXPROD_expected, STRESS_expected, OTHER_expected, CO_expected, NO_expected}
	
	if !are_megvea_outputs_equal(go_output, expected_output) {
		t.Errorf("Go and standalone versions produce different results (epsilon=%v)\n", EPSILON)
	}
}

func TestMegveaAgainstStandalone(t *testing.T) {
	if os.Getenv("MEGAN_STANDALONE_TEST") == "" {
		t.Skip("skipping test; $MEGAN_STANDALONE_TEST not set")
	}

	go_output := run_go_megvea()
	standalone_output := run_standalone_megvea()
	
	if !are_megvea_outputs_equal(go_output, standalone_output) {
		t.Errorf("Go and standalone versions produce different results (epsilon=%v)\n", EPSILON)
	}
}

func run_go_megvea() Megvea_output {
	return RunMegvea()
}

func run_standalone_megvea() Megvea_output {
	// Run standalone script
	run_command("cd ./MEGAN3/work/; ./run.megvea.v3.single.csh")
	
	// Extract outputs from NETCDF files
	output_file := "./MEGAN3/Output/INT/MGNERS.tceq_12km.J4.2013145.single.nc"
	ISOP := parse_netcdf_file("ISOP", output_file)
	MBO := parse_netcdf_file("MBO", output_file)    
	MT_PINE := parse_netcdf_file("MT_PINE", output_file)
	MT_ACYC := parse_netcdf_file("MT_ACYC", output_file)
	MT_CAMP := parse_netcdf_file("MT_CAMP", output_file)
	MT_SABI := parse_netcdf_file("MT_SABI", output_file)
	MT_AROM := parse_netcdf_file("MT_AROM", output_file)
	MT_OXY := parse_netcdf_file("MT_OXY", output_file) 
	SQT_HR := parse_netcdf_file("SQT_HR", output_file) 
	SQT_LR := parse_netcdf_file("SQT_LR", output_file) 
	MEOH := parse_netcdf_file("MEOH", output_file)   
	ACTO := parse_netcdf_file("ACTO", output_file)   
	ETOH := parse_netcdf_file("ETOH", output_file)   
	ACID := parse_netcdf_file("ACID", output_file)   
	LVOC := parse_netcdf_file("LVOC", output_file)   
	OXPROD := parse_netcdf_file("OXPROD", output_file) 
	STRESS := parse_netcdf_file("STRESS", output_file) 
	OTHER := parse_netcdf_file("OTHER", output_file)  
	CO := parse_netcdf_file("CO", output_file)     
	NO := parse_netcdf_file("NO", output_file)     
	
	return Megvea_output{ISOP, MBO, MT_PINE, MT_ACYC, MT_CAMP, MT_SABI, MT_AROM, MT_OXY, SQT_HR, SQT_LR, MEOH, ACTO, ETOH, ACID, LVOC, OXPROD, STRESS, OTHER, CO, NO}
}

func are_megvea_outputs_equal(output1 Megvea_output, output2 Megvea_output) bool {
	ISOP_equal := arrays_approximately_equal(output1.ISOP, output2.ISOP, EPSILON, "ISOP")
	MBO_equal := arrays_approximately_equal(output1.MBO, output2.MBO, EPSILON, "MBO") 
	MT_PINE_equal := arrays_approximately_equal(output1.MT_PINE, output2.MT_PINE, EPSILON, "MT_PINE")
	MT_ACYC_equal := arrays_approximately_equal(output1.MT_ACYC, output2.MT_ACYC, EPSILON, "MT_ACYC")
	MT_CAMP_equal := arrays_approximately_equal(output1.MT_CAMP, output2.MT_CAMP, EPSILON, "MT_CAMP")
	MT_SABI_equal := arrays_approximately_equal(output1.MT_SABI, output2.MT_SABI, EPSILON, "MT_SABI")
	MT_AROM_equal := arrays_approximately_equal(output1.MT_AROM, output2.MT_AROM, EPSILON, "MT_AROM")
	MT_OXY_equal := arrays_approximately_equal(output1.MT_OXY, output2.MT_OXY, EPSILON, "MT_OXY") 
	SQT_HR_equal := arrays_approximately_equal(output1.SQT_HR, output2.SQT_HR, EPSILON, "SQT_HR") 
	SQT_LR_equal := arrays_approximately_equal(output1.SQT_LR, output2.SQT_LR, EPSILON, "SQT_LR") 
	MEOH_equal := arrays_approximately_equal(output1.MEOH, output2.MEOH, EPSILON, "MEOH")
	ACTO_equal := arrays_approximately_equal(output1.ACTO, output2.ACTO, EPSILON, "ACTO")
	ETOH_equal := arrays_approximately_equal(output1.ETOH, output2.ETOH, EPSILON, "ETOH")
	ACID_equal := arrays_approximately_equal(output1.ACID, output2.ACID, EPSILON, "ACID")
	LVOC_equal := arrays_approximately_equal(output1.LVOC, output2.LVOC, EPSILON, "LVOC")
	OXPROD_equal := arrays_approximately_equal(output1.OXPROD, output2.OXPROD, EPSILON, "OXPROD") 
	STRESS_equal := arrays_approximately_equal(output1.STRESS, output2.STRESS, EPSILON, "STRESS") 
	OTHER_equal := arrays_approximately_equal(output1.OTHER, output2.OTHER, EPSILON, "OTHER")  
	CO_equal := arrays_approximately_equal(output1.CO, output2.CO, EPSILON, "CO")  
	NO_equal := arrays_approximately_equal(output1.NO, output2.NO, EPSILON, "NO")  

	return ISOP_equal && MBO_equal && MT_PINE_equal && MT_ACYC_equal && MT_CAMP_equal && MT_SABI_equal && MT_AROM_equal && MT_OXY_equal && SQT_HR_equal && SQT_LR_equal && MEOH_equal && ACTO_equal && ETOH_equal && ACID_equal && LVOC_equal && OXPROD_equal && STRESS_equal && OTHER_equal && CO_equal && NO_equal
}