package megan

import (
	"testing"
	"os"
)

func GetEmissionActivityPerEmissionTypeTestData() EmissionActivityPerEmissionType {
	ISOP :=  	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1126758, 0.1120101, 0.01001419, 0, 0, 0, 0.1942912, 0.1508418, 0.1193487, 0.1317834, 0.1217297, 0.2097917}
	MBO := 		[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1126758, 0.1120101, 0.01001419, 0, 0, 0, 0.1942912, 0.1508418, 0.1193487, 0.1317834, 0.1217297, 0.2097917}
	MT_PINE := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.08565098, 0.08774662, 0.009268718, 0, 0, 0, 0.1250906, 0.1909419, 0.2080862, 0.2286444, 0.2086941, 0.2630515}
	MT_ACYC := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.120648, 0.1252148, 0.01196196, 0, 0, 0, 0.1895526, 0.2050563, 0.1972126, 0.2156608, 0.1962656, 0.280285}
	MT_CAMP := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06303897, 0.06435107, 0.006977767, 0, 0, 0, 0.09016265, 0.1496461, 0.1668268, 0.1834564, 0.1675314, 0.2064757}
	MT_SABI := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06516352, 0.06653857, 0.007200237, 0, 0, 0, 0.09335626, 0.1539478, 0.1713362, 0.1884044, 0.1720437, 0.212387}
	MT_AROM := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1043362, 0.1073843, 0.01095518, 0, 0, 0, 0.1564747, 0.2129951, 0.2240661, 0.2458854, 0.2242535, 0.2927549}
	MT_OXY := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04444172, 0.04526935, 0.004985217, 0, 0, 0, 0.0627585, 0.1093526, 0.1233942, 0.1357518, 0.1239998, 0.1510056}
	SQT_HR := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.05210111, 0.04704434, 0.00511502, 0, 0, 0, 0.09382819, 0.12902, 0.135213, 0.1585944, 0.1559099, 0.1964421}
	SQT_LR := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04595375, 0.04159787, 0.004584486, 0, 0, 0, 0.08238269, 0.1183442, 0.1256814, 0.1475238, 0.145121, 0.1804582}
	MEOH := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.144919, 0.1628459, 0.0140604, 0, 0, 0, 0.2264456, 0.1807782, 0.1450561, 0.1534334, 0.1351359, 0.2355322}
	ACTO := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04426647, 0.04509084, 0.004965558, 0, 0, 0, 0.06251102, 0.1089214, 0.1229077, 0.1352165, 0.1235108, 0.1504101}
	ETOH := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.07770792, 0.07679414, 0.007433405, 0, 0, 0, 0.1296403, 0.1375358, 0.1309355, 0.1466685, 0.1371551, 0.1948155}
	ACID := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1024832, 0.1012781, 0.009803369, 0, 0, 0, 0.170973, 0.1813858, 0.1726812, 0.1934302, 0.1808838, 0.2569278}
	LVOC := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04426647, 0.04509084, 0.004965558, 0, 0, 0, 0.06251102, 0.1089214, 0.1229077, 0.1352165, 0.1235108, 0.1504101}
	OXPROD := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04426647, 0.04509084, 0.004965558, 0, 0, 0, 0.06251102, 0.1089214, 0.1229077, 0.1352165, 0.1235108, 0.1504101}
	STRESS := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1193166, 0.1237401, 0.01189285, 0, 0, 0, 0.1866933, 0.2064675, 0.2005497, 0.2193986, 0.1997169, 0.2823807}
	OTHER := 	[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04426647, 0.04509084, 0.004965558, 0, 0, 0, 0.06251102, 0.1089214, 0.1229077, 0.1352165, 0.1235108, 0.1504101}
	CO := 		[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1426599, 0.1603074, 0.01384122, 0, 0, 0, 0.2229156, 0.1779601, 0.1427949, 0.1510416, 0.1330292, 0.2318605}
	NO := 		[]float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

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