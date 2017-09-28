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

func GetEmissionActivityPerEmissionTypeTestData() EmissionActivityPerEmissionType {
	ISOP :=  	0.1956952
	MBO := 		0.1956952
	MT_PINE := 	0.1198538
	MT_ACYC := 	0.1942892
	MT_CAMP := 	0.084581
	MT_SABI := 	0.08772708
	MT_AROM := 	0.153811
	MT_OXY := 	0.05809301
	SQT_HR := 	0.07359937
	SQT_LR := 	0.06386646
	MEOH := 	0.2623324
	ACTO := 	0.05786393
	ETOH := 	0.1241714
	ACID := 	0.1637605
	LVOC := 	0.05786393
	OXPROD := 	0.05786393
	STRESS := 	0.1906811
	OTHER := 	0.05786393
	CO := 		0.2582429
	NO := 		0.

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
	date := 2013145
	time := 0
	
	use_EA_for_bidirectional_exchange_LAI_response := true
	use_EA_for_response_to_air_pollution := true
	use_EA_for_CO2_response := true
	use_EA_for_response_to_high_wind_storms := true 
	use_EA_for_resposne_to_high_temperature := true
	use_EA_for_response_to_low_temperature := true 
	use_EA_for_response_to_soil_moisture := true
	
	soil_moisture_activity := 0.502049565
	air_quality_index := 11.621
	light_dependent_fraction_map := []float64{1,1,0.4445464,0.8168195,0.3,0.312323,0.6,0.2,0.4630309,0.4,1,0.2,0.8,0.8,0.2,0.2,0.8,0.2,1,0}
	previous_time_step_LAI := 1.6469
	current_time_step_LAI := 1.5165
	
	withinCanopyMeteorologyTestData := GetWithinCanopyMeteorologyTestData()
	
	max_temperature := []float64{299.7025}
	max_wind_speed := []float64{6.027919}
	min_temperature := []float64{287.4598}
	daily_average_temperature := 293.5855
	daily_average_PPFD := 739.8692
	
	return CalculateVariousEmissionActivityFactors(date, time, use_EA_for_bidirectional_exchange_LAI_response, use_EA_for_response_to_air_pollution, use_EA_for_CO2_response, use_EA_for_response_to_high_wind_storms, use_EA_for_resposne_to_high_temperature, use_EA_for_response_to_low_temperature, use_EA_for_response_to_soil_moisture, soil_moisture_activity, air_quality_index, light_dependent_fraction_map, previous_time_step_LAI, current_time_step_LAI, 
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
	ISOP := parse_netcdf_file("ISOP", output_file)[0]
	MBO := parse_netcdf_file("MBO", output_file)[0]    
	MT_PINE := parse_netcdf_file("MT_PINE", output_file)[0]
	MT_ACYC := parse_netcdf_file("MT_ACYC", output_file)[0]
	MT_CAMP := parse_netcdf_file("MT_CAMP", output_file)[0]
	MT_SABI := parse_netcdf_file("MT_SABI", output_file)[0]
	MT_AROM := parse_netcdf_file("MT_AROM", output_file)[0]
	MT_OXY := parse_netcdf_file("MT_OXY", output_file)[0] 
	SQT_HR := parse_netcdf_file("SQT_HR", output_file)[0] 
	SQT_LR := parse_netcdf_file("SQT_LR", output_file)[0] 
	MEOH := parse_netcdf_file("MEOH", output_file)[0]   
	ACTO := parse_netcdf_file("ACTO", output_file)[0]   
	ETOH := parse_netcdf_file("ETOH", output_file)[0]   
	ACID := parse_netcdf_file("ACID", output_file)[0]   
	LVOC := parse_netcdf_file("LVOC", output_file)[0]   
	OXPROD := parse_netcdf_file("OXPROD", output_file)[0] 
	STRESS := parse_netcdf_file("STRESS", output_file)[0] 
	OTHER := parse_netcdf_file("OTHER", output_file)[0]  
	CO := parse_netcdf_file("CO", output_file)[0]     
	NO := parse_netcdf_file("NO", output_file)[0]     
	
	return EmissionActivityPerEmissionType{ISOP, MBO, MT_PINE, MT_ACYC, MT_CAMP, MT_SABI, MT_AROM, MT_OXY, SQT_HR, SQT_LR, MEOH, ACTO, ETOH, ACID, LVOC, OXPROD, STRESS, OTHER, CO, NO}
}

func are_megvea_outputs_equal(output1 EmissionActivityPerEmissionType, output2 EmissionActivityPerEmissionType) bool {
	ISOP_equal := approximately_equal(output1.ISOP, output2.ISOP, EPSILON, "ISOP")
	MBO_equal := approximately_equal(output1.MBO, output2.MBO, EPSILON, "MBO") 
	MT_PINE_equal := approximately_equal(output1.MT_PINE, output2.MT_PINE, EPSILON, "MT_PINE")
	MT_ACYC_equal := approximately_equal(output1.MT_ACYC, output2.MT_ACYC, EPSILON, "MT_ACYC")
	MT_CAMP_equal := approximately_equal(output1.MT_CAMP, output2.MT_CAMP, EPSILON, "MT_CAMP")
	MT_SABI_equal := approximately_equal(output1.MT_SABI, output2.MT_SABI, EPSILON, "MT_SABI")
	MT_AROM_equal := approximately_equal(output1.MT_AROM, output2.MT_AROM, EPSILON, "MT_AROM")
	MT_OXY_equal := approximately_equal(output1.MT_OXY, output2.MT_OXY, EPSILON, "MT_OXY") 
	SQT_HR_equal := approximately_equal(output1.SQT_HR, output2.SQT_HR, EPSILON, "SQT_HR") 
	SQT_LR_equal := approximately_equal(output1.SQT_LR, output2.SQT_LR, EPSILON, "SQT_LR") 
	MEOH_equal := approximately_equal(output1.MEOH, output2.MEOH, EPSILON, "MEOH")
	ACTO_equal := approximately_equal(output1.ACTO, output2.ACTO, EPSILON, "ACTO")
	ETOH_equal := approximately_equal(output1.ETOH, output2.ETOH, EPSILON, "ETOH")
	ACID_equal := approximately_equal(output1.ACID, output2.ACID, EPSILON, "ACID")
	LVOC_equal := approximately_equal(output1.LVOC, output2.LVOC, EPSILON, "LVOC")
	OXPROD_equal := approximately_equal(output1.OXPROD, output2.OXPROD, EPSILON, "OXPROD") 
	STRESS_equal := approximately_equal(output1.STRESS, output2.STRESS, EPSILON, "STRESS") 
	OTHER_equal := approximately_equal(output1.OTHER, output2.OTHER, EPSILON, "OTHER")  
	CO_equal := approximately_equal(output1.CO, output2.CO, EPSILON, "CO")  
	NO_equal := approximately_equal(output1.NO, output2.NO, EPSILON, "NO")  

	return ISOP_equal && MBO_equal && MT_PINE_equal && MT_ACYC_equal && MT_CAMP_equal && MT_SABI_equal && MT_AROM_equal && MT_OXY_equal && SQT_HR_equal && SQT_LR_equal && MEOH_equal && ACTO_equal && ETOH_equal && ACID_equal && LVOC_equal && OXPROD_equal && STRESS_equal && OTHER_equal && CO_equal && NO_equal
}