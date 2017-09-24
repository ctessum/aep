package megan

import (
	"testing"
	"os"
)

func TestMegvea(t *testing.T) {
	go_output, err := run_go_megvea()
	if err != nil {
		t.Errorf("ERROR:", err)
	}
	
	ISOP_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1129666, 0.1572217, 0.01281096, 0, 0, 0, 0.2273491, 0.1709133, 0.1355664, 0.1515557, 0.143397, 0.2591042}
	MBO_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1129666, 0.1572217, 0.01281096, 0, 0, 0, 0.2273491, 0.1709133, 0.1355664, 0.1515557, 0.143397, 0.2591042}
	MT_PINE_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.08573905, 0.1223627, 0.01201405, 0, 0, 0, 0.14768, 0.2217502, 0.2431421, 0.2717206, 0.2561145, 0.3374725}
	MT_ACYC_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.120845, 0.1701025, 0.01509229, 0, 0, 0, 0.2193201, 0.233728, 0.2265996, 0.2515241, 0.2355007, 0.3493145}
	MT_CAMP_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06309339, 0.09038071, 0.009103394, 0, 0, 0, 0.107081, 0.1744208, 0.1954789, 0.2186989, 0.2063631, 0.2663547}
	MT_SABI_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06522062, 0.09340055, 0.009388953, 0, 0, 0, 0.110821, 0.1793867, 0.200722, 0.2245467, 0.2118643, 0.2738686}
	MT_AROM_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1044658, 0.1483643, 0.01407344, 0, 0, 0, 0.1833626, 0.2460077, 0.2606371, 0.2907475, 0.2735648, 0.3724301}
	MT_OXY_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04447569, 0.06385349, 0.00652819, 0, 0, 0, 0.07480949, 0.1277065, 0.1447994, 0.1620938, 0.1530378, 0.1953786}
	SQT_HR_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.05222754, 0.08130884, 0.007873109, 0, 0, 0, 0.1242197, 0.1655256, 0.1752274, 0.2111252, 0.2183125, 0.294441}
	SQT_LR_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04605928, 0.07245424, 0.007106985, 0, 0, 0, 0.1097725, 0.1524601, 0.1634149, 0.1971025, 0.2040717, 0.2721384}
	MEOH_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1451326, 0.2020758, 0.01643923, 0, 0, 0, 0.2499566, 0.1959032, 0.1575004, 0.1679996, 0.1504079, 0.2704136 }
	ACTO_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04430031, 0.0636017, 0.006502447, 0, 0, 0, 0.07451449, 0.127203, 0.1442284, 0.1614546, 0.1524343, 0.1946082}
	ETOH_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.07787183, 0.1116421, 0.009901169, 0, 0, 0, 0.1552547, 0.1619912, 0.1559296, 0.177973, 0.172432, 0.2560415}
	ACID_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1026994, 0.1472365, 0.01305792, 0, 0, 0, 0.2047539, 0.2136383, 0.2056441, 0.2347155, 0.2274078, 0.3376742}
	LVOC_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04430031, 0.0636017, 0.006502447, 0, 0, 0, 0.07451449, 0.127203, 0.1442284, 0.1614546, 0.1524343, 0.1946082}
	OXPROD_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04430031, 0.0636017, 0.006502447, 0, 0, 0, 0.07451449, 0.127203, 0.1442284, 0.1614546, 0.1524343, 0.1946082}
	STRESS_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1195072, 0.1683552, 0.01503099, 0, 0, 0, 0.2162502, 0.2356694, 0.2307619, 0.2562928, 0.2401036, 0.3527067 }
	OTHER_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04430031, 0.0636017, 0.006502447, 0, 0, 0, 0.07451449, 0.127203, 0.1442284, 0.1614546, 0.1524343, 0.1946082}
	CO_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1428702, 0.1989256, 0.01618296, 0, 0, 0, 0.24606, 0.1928492, 0.1550451, 0.1653806, 0.1480632, 0.2661982}
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

	go_output, err := run_go_megvea()
	if err != nil {
		t.Errorf("ERROR:", err)
	}
	standalone_output := run_standalone_megvea()
	
	if !are_megvea_outputs_equal(go_output, standalone_output) {
		t.Errorf("Go and standalone versions produce different results (epsilon=%v)\n", EPSILON)
	}
}

func run_go_megvea() (output Megvea_output, err error) {
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
	
	/*sunleafTK := []float64{295.134735,298.878723,290.011017,288.210510,286.313507,285.310883,285.782288,286.480377,289.430908,293.125854,292.991791,291.126129,290.263153,298.708862,290.946136,291.447235,294.649658,297.164398,295.892944,295.891693,294.841980,294.545990,295.512268,296.566101,296.393219}
	shadeleafTK := []float64{292.377167,293.018463,296.240540,294.385162,292.481384,291.564545,292.256256,293.248566,296.616180,300.831940,300.689026,298.355408,297.210754,291.657776,297.478180,297.859436,301.461639,304.401489,302.153503,301.902435,300.167267,299.641510,300.614044,301.777802,301.712982}
	sunPPFD := []float64{534.6226,1169.987,0,0,0,0,0,0,0,0,0,0,0,1360.212,144.8788,99.57848,132.5539,178.8786,101.0769,164.225,54.78764,37.30027,36.69876,36.47827,54.99779}
	shadePPFD := []float64{152.7413,100.3765,0,0,0,0,0,0,0,0,0,0,0,83.73295,144.8788,99.57848,132.5539,178.8786,101.0769,164.225,54.78764,37.30027,36.69876,36.47827,54.99779}
	sunFrac := []float64{0.3760942,0.1114906,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.05476007,0.3350512,0.5015498,0.5927957,0.644033,0.6709146,0.6802174,0.6740549,0.65104,0.6055322,0.5242341,0.3770557}*/
	
	sunleafTK := [][]float64{
		{295.6182, 295.8868, 295.9344, 296.7096, 297.2047}, 
		{297.5717, 297.8929, 298.8764, 299.3094, 299.4378},  
		{294.3002, 294.5813, 290.01, 289.1228, 289.5467},  
		{292.5432, 292.8118, 288.2076, 287.3328, 287.7618}, 
		{290.731, 290.9875, 286.3112, 285.4392, 285.8764}, 
		{289.8369, 290.0908, 285.3087, 284.4257, 284.8632}, 
		{290.4691, 290.7321, 285.7799, 284.8607, 285.3189}, 
		{291.3685, 291.6464, 286.4776, 285.5127, 285.9867}, 
		{294.6352, 294.9277, 289.4296, 288.3796, 288.8675}, 
		{298.6524, 298.972, 293.1242, 291.9768, 292.4762}, 
		{298.1715, 298.5288, 292.9952, 291.8577, 292.3363}, 
		{295.7728, 296.1438, 291.1315, 290.1029, 290.5687}, 
		{295.0275, 295.3451, 290.2635, 289.2626, 289.7245}, 
		{297.2255, 297.5618, 298.7084, 299.2278, 299.3832}, 
		{294.641, 294.9754, 290.9479, 290.0226, 290.3605}, 
		{295.333, 295.6651, 291.4496, 290.5166, 290.868}, 
		{298.3278, 298.7189, 294.6498, 293.7299, 294.0988}, 
		{300.7791, 301.1797, 297.1625, 296.2168, 296.5847}, 
		{299.8176, 300.1142, 295.8929, 294.9984, 295.3488}, 
		{299.3605, 299.6327, 295.8914, 295.0311, 295.3308}, 
		{298.7494, 298.9302, 294.8402, 293.9897, 294.2693}, 
		{298.438, 298.5879, 294.544, 293.6347, 293.8754}, 
		{299.3921, 299.5466, 295.5102, 294.5973, 294.8353}, 
		{300.4998, 300.6654, 296.5637, 295.632, 295.8712}, 
		{300.1864, 300.3878, 296.3911, 295.5673, 295.851}}
	shadeleafTK := [][]float64{
		{295.1596, 295.4961, 292.3825, 291.6773, 291.9568}, 
		{295.8603, 296.2286, 293.0186, 292.0315, 292.1794}, 
		{294.2982, 294.5833, 296.2413, 296.5161, 296.5336}, 
		{292.5418, 292.8131, 294.3852, 294.6535, 294.6705}, 
		{290.7301, 290.9876, 292.4813, 292.7442, 292.7606}, 
		{289.8362, 290.0909, 291.5644, 291.8278, 291.8408}, 
		{290.4683, 290.7323, 292.256, 292.5234, 292.5387}, 
		{291.3675, 291.6465, 293.2484, 293.5233, 293.5382}, 
		{294.634, 294.9278, 296.6163, 296.8936, 296.909}, 
		{298.6506, 298.9738, 300.8321, 301.1232, 301.1385}, 
		{298.1722, 298.5282, 300.6912, 300.9904, 300.9817}, 
		{295.774, 296.1423, 298.3589, 298.6775, 298.6761}, 
		{295.0266, 295.3449, 297.212, 297.5029, 297.5128}, 
		{295.1815, 295.5639, 291.6578, 290.43, 290.6259}, 
		{294.6262, 294.9632, 297.4828, 297.6406, 297.4586}, 
		{295.3326, 295.6651, 297.8638, 297.9648, 297.8167}, 
		{298.3281, 298.7188, 301.4618, 301.5846, 301.3908}, 
		{300.7792, 301.1796, 304.3954, 304.571, 304.3264}, 
		{299.817, 300.1148, 302.1549, 302.1889, 302.0243}, 
		{299.3603, 299.6332, 301.903, 301.9866, 301.7834}, 
		{298.7492, 298.9305, 300.167, 300.1884, 300.0975}, 
		{298.4378, 298.5882, 299.6407, 299.7058, 299.6474}, 
		{299.3918, 299.5469, 300.6133, 300.6803, 300.6218}, 
		{300.4995, 300.6657, 301.7771, 301.8479, 301.7896}, 
		{300.1861, 300.3882, 301.7128, 301.7371, 301.6449}}
	sunPPFD := [][]float64{
		{617.0796, 581.216, 534.6226, 496.9291, 476.1383}, 
		{1229, 1218.555, 1169.987, 1133.71, 1117.465}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{1432.69, 1418.28, 1360.212, 1327.029, 1314.218}, 
		{241.908, 196.2697, 144.8788, 107.2672, 87.51214}, 
		{166.2688, 134.9006, 99.57848, 73.72714, 60.14907}, 
		{221.3287, 179.5729, 132.5539, 98.14188, 80.06743}, 
		{298.6783, 242.3297, 178.8786, 132.4403, 108.0493}, 
		{168.7707, 136.9305, 101.0769, 74.83653, 61.05415}, 
		{274.2107, 222.4782, 164.225, 121.5909, 99.19792}, 
		{91.48035, 74.22169, 54.78764, 40.56435, 33.09375}, 
		{62.28123, 50.53127, 37.30027, 27.61683, 22.53074}, 
		{61.27688, 49.7164, 36.69876, 27.17148, 22.1674}, 
		{60.90872, 49.41769, 36.47827, 27.00823, 22.03422}, 
		{91.83125, 74.50639, 54.99779, 40.71994, 33.22069}}
	shadePPFD := [][]float64{
		{235.1983, 199.3347, 152.7413, 115.0479, 94.25706}, 
		{159.3897, 148.9443, 100.3765, 64.09938, 47.8544}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 0}, 
		{156.2114, 141.8008, 83.73295, 50.55008, 37.73889}, 
		{241.908, 196.2697, 144.8788, 107.2672, 87.51214}, 
		{166.2688, 134.9006, 99.57848, 73.72714, 60.14907}, 
		{221.3287, 179.5729, 132.5539, 98.14188, 80.06743}, 
		{298.6783, 242.3297, 178.8786, 132.4403, 108.0493}, 
		{168.7707, 136.9305, 101.0769, 74.83653, 61.05415}, 
		{274.2107, 222.4782, 164.225, 121.5909, 99.19792}, 
		{91.48035, 74.22169, 54.78764, 40.56435, 33.09375}, 
		{62.28123, 50.53127, 37.30027, 27.61683, 22.53074}, 
		{61.27688, 49.7164, 36.69876, 27.17148, 22.1674}, 
		{60.90872, 49.41769, 36.47827, 27.00823, 22.03422}, 
		{91.83125, 74.50639, 54.99779, 40.71994, 33.22069}}
	sunFrac := [][]float64{
		{0.9111186, 0.6343481, 0.3760942, 0.22486, 0.1589793}, 
		{0.8086445, 0.3566621, 0.1114906, 0.03613561, 0.01703344}, 
		{0.2, 0.2, 0.2, 0.2, 0.2}, 
		{0.2, 0.2, 0.2, 0.2, 0.2}, 
		{0.2, 0.2, 0.2, 0.2, 0.2}, 
		{0.2, 0.2, 0.2, 0.2, 0.2}, 
		{0.2, 0.2, 0.2, 0.2, 0.2}, 
		{0.2, 0.2, 0.2, 0.2, 0.2}, 
		{0.2, 0.2, 0.2, 0.2, 0.2}, 
		{0.2, 0.2, 0.2, 0.2, 0.2}, 
		{0.2, 0.2, 0.2, 0.2, 0.2}, 
		{0.2, 0.2, 0.2, 0.2, 0.2}, 
		{0.2, 0.2, 0.2, 0.2, 0.2}, 
		{0.7528764, 0.2536033, 0.05476007, 0.01251686, 0.004684461}, 
		{0.9009956, 0.6008364, 0.3350512, 0.1887849, 0.1283043}, 
		{0.9366858, 0.7258638, 0.5015498, 0.3480518, 0.2718508}, 
		{0.95176, 0.7847132, 0.5927957, 0.4489425, 0.3718549}, 
		{0.9592984, 0.8155808, 0.644033, 0.5094811, 0.434578}, 
		{0.9630322, 0.8312343, 0.6709146, 0.5423182, 0.4693694}, 
		{0.9642918, 0.8365706, 0.6802174, 0.5538514, 0.4817112}, 
		{0.9634591, 0.8330401, 0.6740549, 0.5462016, 0.4735181}, 
		{0.9602855, 0.8196954, 0.65104, 0.51797, 0.443522}, 
		{0.9536892, 0.7925196, 0.6055322, 0.4637364, 0.3870063}, 
		{0.9406589, 0.7410105, 0.5242341, 0.372282, 0.2953098}, 
		{0.9113433, 0.6351085, 0.3770557, 0.2257312, 0.1597351}}
	
	max_temperature := []float64{299.7025}
	max_wind_speed := []float64{6.027919}
	min_temperature := []float64{287.4598}
	daily_average_temperature := []float64{293.5855}
	daily_average_PPFD := []float64{739.8692}
	
	return CalculateVariousEmissionActivityFactors(start_date, start_time, time_increment, use_EA_for_bidirectional_exchange_LAI_response, use_EA_for_response_to_air_pollution, use_EA_for_CO2_response, use_EA_for_response_to_high_wind_storms, use_EA_for_resposne_to_high_temperature, use_EA_for_response_to_low_temperature, use_EA_for_response_to_soil_moisture, soil_moisture_activity, air_quality_index, light_dependent_fraction_map, previous_time_step_LAI, current_time_step_LAI, sunleafTK, shadeleafTK, sunPPFD, shadePPFD, sunFrac, max_temperature, max_wind_speed, min_temperature, daily_average_temperature, daily_average_PPFD)
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