package megan

import (
	"testing"
	"os"
)

func GetEmissionActivityPerEmissionTypeTestData() EmissionActivityPerEmissionType {
	ISOP := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1129666, 0.1572217, 0.01281096, 0, 0, 0, 0.2273491, 0.1709133, 0.1355664, 0.1515557, 0.143397, 0.2591042}
	MBO := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1129666, 0.1572217, 0.01281096, 0, 0, 0, 0.2273491, 0.1709133, 0.1355664, 0.1515557, 0.143397, 0.2591042}
	MT_PINE := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.08573905, 0.1223627, 0.01201405, 0, 0, 0, 0.14768, 0.2217502, 0.2431421, 0.2717206, 0.2561145, 0.3374725}
	MT_ACYC := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.120845, 0.1701025, 0.01509229, 0, 0, 0, 0.2193201, 0.233728, 0.2265996, 0.2515241, 0.2355007, 0.3493145}
	MT_CAMP := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06309339, 0.09038071, 0.009103394, 0, 0, 0, 0.107081, 0.1744208, 0.1954789, 0.2186989, 0.2063631, 0.2663547}
	MT_SABI := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06522062, 0.09340055, 0.009388953, 0, 0, 0, 0.110821, 0.1793867, 0.200722, 0.2245467, 0.2118643, 0.2738686}
	MT_AROM := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1044658, 0.1483643, 0.01407344, 0, 0, 0, 0.1833626, 0.2460077, 0.2606371, 0.2907475, 0.2735648, 0.3724301}
	MT_OXY := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04447569, 0.06385349, 0.00652819, 0, 0, 0, 0.07480949, 0.1277065, 0.1447994, 0.1620938, 0.1530378, 0.1953786}
	SQT_HR := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.05222754, 0.08130884, 0.007873109, 0, 0, 0, 0.1242197, 0.1655256, 0.1752274, 0.2111252, 0.2183125, 0.294441}
	SQT_LR := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04605928, 0.07245424, 0.007106985, 0, 0, 0, 0.1097725, 0.1524601, 0.1634149, 0.1971025, 0.2040717, 0.2721384}
	MEOH := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1451326, 0.2020758, 0.01643923, 0, 0, 0, 0.2499566, 0.1959032, 0.1575004, 0.1679996, 0.1504079, 0.2704136 }
	ACTO := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04430031, 0.0636017, 0.006502447, 0, 0, 0, 0.07451449, 0.127203, 0.1442284, 0.1614546, 0.1524343, 0.1946082}
	ETOH := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.07787183, 0.1116421, 0.009901169, 0, 0, 0, 0.1552547, 0.1619912, 0.1559296, 0.177973, 0.172432, 0.2560415}
	ACID := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1026994, 0.1472365, 0.01305792, 0, 0, 0, 0.2047539, 0.2136383, 0.2056441, 0.2347155, 0.2274078, 0.3376742}
	LVOC := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04430031, 0.0636017, 0.006502447, 0, 0, 0, 0.07451449, 0.127203, 0.1442284, 0.1614546, 0.1524343, 0.1946082}
	OXPROD := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04430031, 0.0636017, 0.006502447, 0, 0, 0, 0.07451449, 0.127203, 0.1442284, 0.1614546, 0.1524343, 0.1946082}
	STRESS := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1195072, 0.1683552, 0.01503099, 0, 0, 0, 0.2162502, 0.2356694, 0.2307619, 0.2562928, 0.2401036, 0.3527067 }
	OTHER := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04430031, 0.0636017, 0.006502447, 0, 0, 0, 0.07451449, 0.127203, 0.1442284, 0.1614546, 0.1524343, 0.1946082}
	CO := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1428702, 0.1989256, 0.01618296, 0, 0, 0, 0.24606, 0.1928492, 0.1550451, 0.1653806, 0.1480632, 0.2661982}
	NO := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

	return EmissionActivityPerEmissionType{ISOP, MBO, MT_PINE, MT_ACYC, MT_CAMP, MT_SABI, MT_AROM, MT_OXY, SQT_HR, SQT_LR, MEOH, ACTO, ETOH, ACID, LVOC, OXPROD, STRESS, OTHER, CO, NO}
}

func TestMegvea(t *testing.T) {
	go_output, err := run_go_megvea()
	if err != nil {
		t.Errorf("ERROR:", err)
	}

	expected_output := GetEmissionActivityPerEmissionTypeTestData() 
	
	if !are_megvea_outputs_equal(go_output, expected_output) {
		t.Errorf("Go and standalone versions produce different results (epsilon=%v)\n", EPSILON)
	}
}

func TestMegveaAgainstStandalone(t *testing.T) {
	if os.Getenv("MEGAN_STANDALONE_TEST") == "" {
		t.Skip("skipping test; $MEGAN_STANDALONE_TEST not set")
	}

	go_output, err := run_go_megvea()
	if err != nil {
		t.Errorf("ERROR:", err)
	}
	standalone_output := run_standalone_megvea()
	
	if !are_megvea_outputs_equal(go_output, standalone_output) {
		t.Errorf("Go and standalone versions produce different results (epsilon=%v)\n", EPSILON)
	}
}

func run_go_megvea() (output EmissionActivityPerEmissionType, err error) {
	start_date := 2013145
	start_time := 0
	time_increment := 10000
	
	use_EA_for_bidirectional_exchange_LAI_response := true
	use_EA_for_response_to_air_pollution := true
	use_EA_for_CO2_response := true
	use_EA_for_response_to_high_wind_storms := true 
	use_EA_for_resposne_to_high_temperature := true
	use_EA_for_response_to_low_temperature := true 
	use_EA_for_response_to_soil_moisture := true
	
	soil_moisture_activity := []float64{0,0,0,0,0,0,0,0,0,0,0,0,0,0.427884996,0.487772763,0.0518400222,0,0,0,0.408192724,0.841395199,1,1,0.820465028,1}
	air_quality_index := []float64{11.621}
	light_dependent_fraction_map := []float64{1,1,0.4445464,0.8168195,0.3,0.312323,0.6,0.2,0.4630309,0.4,1,0.2,0.8,0.8,0.2,0.2,0.8,0.2,1,0}
	previous_time_step_LAI := []float64{1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469}
	current_time_step_LAI := []float64{1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165}
	
	withinCanopyMeteorologyTestData := GetWithinCanopyMeteorologyTestData()
	
	max_temperature := []float64{299.7025}
	max_wind_speed := []float64{6.027919}
	min_temperature := []float64{287.4598}
	daily_average_temperature := []float64{293.5855}
	daily_average_PPFD := []float64{739.8692}
	
	return CalculateVariousEmissionActivityFactors(start_date, start_time, time_increment, use_EA_for_bidirectional_exchange_LAI_response, use_EA_for_response_to_air_pollution, use_EA_for_CO2_response, use_EA_for_response_to_high_wind_storms, use_EA_for_resposne_to_high_temperature, use_EA_for_response_to_low_temperature, use_EA_for_response_to_soil_moisture, soil_moisture_activity, air_quality_index, light_dependent_fraction_map, previous_time_step_LAI, current_time_step_LAI, 
	withinCanopyMeteorologyTestData.SunleafTK, 
	withinCanopyMeteorologyTestData.ShadeleafTK, 
	withinCanopyMeteorologyTestData.SunPPFD, 
	withinCanopyMeteorologyTestData.ShadePPFD, 
	withinCanopyMeteorologyTestData.SunFrac, 
	max_temperature, max_wind_speed, min_temperature, daily_average_temperature, daily_average_PPFD)
}

func run_standalone_megvea() EmissionActivityPerEmissionType {
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
	
	return EmissionActivityPerEmissionType{ISOP, MBO, MT_PINE, MT_ACYC, MT_CAMP, MT_SABI, MT_AROM, MT_OXY, SQT_HR, SQT_LR, MEOH, ACTO, ETOH, ACID, LVOC, OXPROD, STRESS, OTHER, CO, NO}
}

func are_megvea_outputs_equal(output1 EmissionActivityPerEmissionType, output2 EmissionActivityPerEmissionType) bool {
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