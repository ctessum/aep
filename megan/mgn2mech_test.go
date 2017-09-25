package megan

import (
	"testing"
	"os"
)

func TestMgn2mech(t *testing.T) {
	go_output, err := run_go_mgn2mech()
	if err != nil {
		t.Errorf("ERROR:", err)
	}
	
	ISOP_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1426557, 0.1985416, 0.01617785, 0, 0, 0, 0.2870994, 0.2158315, 0.171195, 0.1913865, 0.1810836, 0.3272001}
	TERP_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.004674373, 0.006666114, 0.0006511473, 0, 0, 0, 0.008074618, 0.01192501, 0.01301347, 0.01453947, 0.01370112, 0.01813486}
	PAR_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.001149452, 0.001634236, 0.0001565454, 0, 0, 0, 0.002053949, 0.002805091, 0.002992571, 0.003349341, 0.003163213, 0.004283844}
	XYL_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	OLE_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0006949258, 0.000997701, 0.000102002, 0, 0, 0, 0.001168887, 0.001995395, 0.002262468, 0.002532691, 0.002391191, 0.003052761}
	NR_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	MEOH_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.02162095, 0.03010399, 0.002449014, 0, 0, 0, 0.03723697, 0.02918443, 0.02346343, 0.02502753, 0.02240683, 0.04028454}
	CH4_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.709384e-06, 6.761235e-06, 6.912485e-07, 0, 0, 0, 7.921331e-06, 1.352243e-05, 1.533233e-05, 1.716358e-05, 1.620466e-05, 2.068799e-05}
	NH3_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	NO_expected := []float64{9.940225e-07, 1.016836e-06, 9.478741e-07, 8.723866e-07, 8.036999e-07, 7.702124e-07, 8.015715e-07, 8.376093e-07, 9.681748e-07, 1.164393e-06, 1.170123e-06, 1.070677e-06, 1.041866e-06, 1.057617e-06, 1.050329e-06, 1.066609e-06, 1.181683e-06, 1.273218e-06, 1.239873e-06, 1.227178e-06, 1.190352e-06, 1.185402e-06, 1.228115e-06, 1.267849e-06, 1.277538e-06}
	ALD2_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.001160937, 0.001664394, 0.0001476096, 0, 0, 0, 0.002314583, 0.002415014, 0.002324646, 0.002653275, 0.002570668, 0.003817143}
	ETOH_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.001096378, 0.001571838, 0.0001394011, 0, 0, 0, 0.00218587, 0.002280716, 0.002195373, 0.002505728, 0.002427714, 0.003604874}
	FORM_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0001510419, 0.00021685, 2.21701e-05, 0, 0, 0, 0.0002540572, 0.0004336986, 0.0004917467, 0.0005504795, 0.0005197248, 0.0006635166}
	ALDX_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	TOL_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	IOLE_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0004417441, 0.0006181842, 5.26673e-05, 0, 0, 0, 0.0008635624, 0.000772994, 0.0006884884, 0.000770066, 0.0007280394, 0.001174933}
	CO_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01637223, 0.02279592, 0.00185449, 0, 0, 0, 0.02819729, 0.02209959, 0.01776743, 0.01895183, 0.01696732, 0.03050503}
	ETHA_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0004113323, 0.0005905473, 6.03758e-05, 0, 0, 0, 0.0006918737, 0.001181091, 0.001339173, 0.00149912, 0.001415366, 0.001806954}
	ETH_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0007721889, 0.001108627, 0.0001133427, 0, 0, 0, 0.001298846, 0.002217247, 0.002514014, 0.00281428, 0.002657049, 0.003392172}
	ETHY_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	PRPA_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0002574701, 0.0003696483, 3.779174e-05, 0, 0, 0, 0.0004330727, 0.0007392943, 0.0008382447, 0.0009383622, 0.0008859368, 0.001131048}
	BENZ_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	ACET_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0009137884, 0.001311921, 0.0001341269, 0, 0, 0, 0.00153702, 0.002623833, 0.002975018, 0.003330346, 0.003144283, 0.004014209}
	KET_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	AACD_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0001483901, 0.0002127416, 1.886735e-05, 0, 0, 0, 0.0002958483, 0.0003086853, 0.0002971345, 0.0003391396, 0.0003285808, 0.0004879044}
	FACD_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0007321584, 0.00104967, 9.309175e-05, 0, 0, 0, 0.001459719, 0.001523057, 0.001466066, 0.001673319, 0.001621222, 0.002407327}
	HCN_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.612568e-05, 7.906677e-05, 7.059197e-06, 0, 0, 0, 0.0001015603, 0.0001106804, 0.0001083756, 0.000120366, 0.0001127629, 0.0001656461}
	ISPD_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0005472004, 0.0007711386, 6.903671e-05, 0, 0, 0, 0.0009888771, 0.00108819, 0.001070027, 0.001188702, 0.001113885, 0.001629572}
	N2O_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	SESQ_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0001679682, 0.0002631191, 2.567569e-05, 0, 0, 0, 0.0003999859, 0.0005464077, 0.0005828123, 0.0007026649, 0.0007271476, 0.000973999}
	TRS_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7.775959e-06, 1.11639e-05, 1.141364e-06, 0, 0, 0, 1.307941e-05, 2.232773e-05, 2.531617e-05, 2.833986e-05, 2.675654e-05, 3.415925e-05}
	CH3BR_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.698785e-07, 6.746018e-07, 6.896927e-08, 0, 0, 0, 7.903503e-07, 1.349199e-06, 1.529782e-06, 1.712495e-06, 1.616819e-06, 2.064143e-06}
	CH3CL_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3.769627e-06, 5.412032e-06, 5.5331e-07, 0, 0, 0, 6.34063e-06, 1.082403e-05, 1.227276e-05, 1.373859e-05, 1.297103e-05, 1.655971e-05}
	CH3I_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.201193e-05, 1.724546e-05, 1.763124e-06, 0, 0, 0, 2.020444e-05, 3.44908e-05, 3.910721e-05, 4.377806e-05, 4.133223e-05, 5.276758e-05}
	ISP_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1426557, 0.1985416, 0.01617785, 0, 0, 0, 0.2870994, 0.2158315, 0.171195, 0.1913865, 0.1810836, 0.3272001}
	TRP_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.004674373, 0.006666114, 0.0006511473, 0, 0, 0, 0.008074618, 0.01192501, 0.01301347, 0.01453947, 0.01370112, 0.01813486}
	XYLA_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	SQT_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0001679682, 0.0002631191, 2.567569e-05, 0, 0, 0, 0.0003999859, 0.0005464077, 0.0005828123, 0.0007026649, 0.0007271476, 0.000973999}
	TOLA_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

		
	expected_output := Species_CB6X{ISOP_expected, TERP_expected, PAR_expected, XYL_expected, OLE_expected, NR_expected, MEOH_expected, CH4_expected, NH3_expected, NO_expected, ALD2_expected, ETOH_expected, FORM_expected, ALDX_expected, TOL_expected, IOLE_expected, CO_expected, ETHA_expected, ETH_expected, ETHY_expected, PRPA_expected, BENZ_expected, ACET_expected, KET_expected, AACD_expected, FACD_expected, HCN_expected, ISPD_expected, N2O_expected, SESQ_expected, TRS_expected, CH3BR_expected, CH3CL_expected, CH3I_expected, ISP_expected, TRP_expected, XYLA_expected, SQT_expected, TOLA_expected}

	if !are_mgn2mech_outputs_equal(go_output, expected_output) {
		t.Errorf("Go and standalone versions produce different results (epsilon=%v)\n", EPSILON)
	}
}

func TestMgn2mechAgainstStandalone(t *testing.T) {
	if os.Getenv("MEGAN_STANDALONE_TEST") == "" {
		t.Skip("skipping test; $MEGAN_STANDALONE_TEST not set")
	}

	go_output, err := run_go_mgn2mech()
	if err != nil {
		t.Errorf("ERROR:", err)
	}
	
	standalone_output := run_standalone_mgn2mech()
	
	if !are_mgn2mech_outputs_equal(go_output, standalone_output) {
		t.Errorf("Go and standalone versions produce different results (epsilon=%v)\n", EPSILON)
	}
}
 
func run_go_mgn2mech() (output interface{}, err error) {
	start_date := 2013145
	start_time := 0
	time_increment := 10000
	use_mechanism_conversion := true
	output_in_ton_per_hr := false
	area_per_grid_cell := 144000000.0 
	mechanism := CB6X		
	inputEmissionActivity := GetEmissionActivityPerEmissionTypeTestData()
	emission_factor := []float64{8.769534, 0.0224195, 0.3402439, 0.01646902, 0.007909732, 0.008048705, 0.00181239, 0.0019196, 0.00901152, 0.0150626, 1.03454, 0.143244, 0.2013025, 0.060192, 0.3470387, 0.02525556, 0.03697741, 0.00553815, 0.7958, 1.06345e-05}
	inputSoilMoistureAndNOEmissionActivity := GetSoilMoistureAndNOEmissionActivityTestData()
	return ChemicalSpeciationAndMechanismConversion(start_date, start_time, time_increment, use_mechanism_conversion, output_in_ton_per_hr, area_per_grid_cell, mechanism, inputEmissionActivity, emission_factor, inputSoilMoistureAndNOEmissionActivity)
}

func run_standalone_mgn2mech() Species_CB6X {
	// Run standalone script
	run_command("cd ./MEGAN3/work/; ./run.mgn2mech.v3.single.csh")
	
	// Extract outputs from NETCDF files
	output_file := "./MEGAN3/Output/MEGANv3.tceq_12km.J4.CB6X.2013145.single.baseline.nc"
	ISOP  := parse_netcdf_file("ISOP", output_file)
	TERP  := parse_netcdf_file("TERP", output_file)
	PAR   := parse_netcdf_file("PAR", output_file)
	XYL   := parse_netcdf_file("XYL", output_file)
	OLE   := parse_netcdf_file("OLE", output_file)
	NR    := parse_netcdf_file("NR", output_file)
	MEOH  := parse_netcdf_file("MEOH", output_file)
	CH4   := parse_netcdf_file("CH4", output_file)
	NH3   := parse_netcdf_file("NH3", output_file)
	NO    := parse_netcdf_file("NO", output_file)
	ALD2  := parse_netcdf_file("ALD2", output_file)
	ETOH  := parse_netcdf_file("ETOH", output_file)
	FORM  := parse_netcdf_file("FORM", output_file)
	ALDX  := parse_netcdf_file("ALDX", output_file)
	TOL   := parse_netcdf_file("TOL", output_file)
	IOLE  := parse_netcdf_file("IOLE", output_file)
	CO    := parse_netcdf_file("CO", output_file)
	ETHA  := parse_netcdf_file("ETHA", output_file)
	ETH   := parse_netcdf_file("ETH", output_file)
	ETHY  := parse_netcdf_file("ETHY", output_file)
	PRPA  := parse_netcdf_file("PRPA", output_file)
	BENZ  := parse_netcdf_file("BENZ", output_file)
	ACET  := parse_netcdf_file("ACET", output_file)
	KET   := parse_netcdf_file("KET", output_file)
	AACD  := parse_netcdf_file("AACD", output_file)
	FACD  := parse_netcdf_file("FACD", output_file)
	HCN   := parse_netcdf_file("HCN", output_file)
	ISPD  := parse_netcdf_file("ISPD", output_file)
	N2O   := parse_netcdf_file("N2O", output_file)
	SESQ  := parse_netcdf_file("SESQ", output_file)
	TRS   := parse_netcdf_file("TRS", output_file)
	CH3BR := parse_netcdf_file("CH3BR", output_file)
	CH3CL := parse_netcdf_file("CH3CL", output_file)
	CH3I  := parse_netcdf_file("CH3I", output_file)
	ISP   := parse_netcdf_file("ISP", output_file)
	TRP   := parse_netcdf_file("TRP", output_file)
	XYLA  := parse_netcdf_file("XYLA", output_file)
	SQT   := parse_netcdf_file("SQT", output_file)
	TOLA  := parse_netcdf_file("TOLA", output_file)
	
	return Species_CB6X{ISOP, TERP, PAR, XYL, OLE, NR, MEOH, CH4, NH3, NO, ALD2, ETOH, FORM, ALDX, TOL, IOLE, CO, ETHA, ETH, ETHY, PRPA, BENZ, ACET, KET, AACD, FACD, HCN, ISPD, N2O, SESQ, TRS, CH3BR, CH3CL, CH3I, ISP, TRP, XYLA, SQT, TOLA}
}

func are_mgn2mech_outputs_equal(output1 interface{}, output2 interface{}) bool {

	if output1, ok := output1.(Species_CB6X); ok {
		if output2, ok := output2.(Species_CB6X); ok {
			ISOP_equal  := arrays_approximately_equal(output1.ISOP, output2.ISOP, EPSILON, "ISOP")
			TERP_equal  := arrays_approximately_equal(output1.TERP, output2.TERP, EPSILON, "TERP")
			PAR_equal   := arrays_approximately_equal(output1.PAR, output2.PAR, EPSILON, "PAR")
			XYL_equal   := arrays_approximately_equal(output1.XYL, output2.XYL, EPSILON, "XYL")
			OLE_equal   := arrays_approximately_equal(output1.OLE, output2.OLE, EPSILON, "OLE")
			NR_equal    := arrays_approximately_equal(output1.NR, output2.NR, EPSILON, "NR")
			MEOH_equal  := arrays_approximately_equal(output1.MEOH, output2.MEOH, EPSILON, "MEOH")
			CH4_equal   := arrays_approximately_equal(output1.CH4, output2.CH4, EPSILON, "CH4")
			NH3_equal   := arrays_approximately_equal(output1.NH3, output2.NH3, EPSILON, "NH3")
			NO_equal    := arrays_approximately_equal(output1.NO, output2.NO, EPSILON, "NO")
			ALD2_equal  := arrays_approximately_equal(output1.ALD2, output2.ALD2, EPSILON, "ALD2")
			ETOH_equal  := arrays_approximately_equal(output1.ETOH, output2.ETOH, EPSILON, "ETOH")
			FORM_equal  := arrays_approximately_equal(output1.FORM, output2.FORM, EPSILON, "FORM")
			ALDX_equal  := arrays_approximately_equal(output1.ALDX, output2.ALDX, EPSILON, "ALDX")
			TOL_equal   := arrays_approximately_equal(output1.TOL, output2.TOL, EPSILON, "TOL")
			IOLE_equal  := arrays_approximately_equal(output1.IOLE, output2.IOLE, EPSILON, "IOLE")
			CO_equal    := arrays_approximately_equal(output1.CO, output2.CO, EPSILON, "CO")
			ETHA_equal  := arrays_approximately_equal(output1.ETHA, output2.ETHA, EPSILON, "ETHA")
			ETH_equal   := arrays_approximately_equal(output1.ETH, output2.ETH, EPSILON, "ETH")
			ETHY_equal  := arrays_approximately_equal(output1.ETHY, output2.ETHY, EPSILON, "ETHY")
			PRPA_equal  := arrays_approximately_equal(output1.PRPA, output2.PRPA, EPSILON, "PRPA")
			BENZ_equal  := arrays_approximately_equal(output1.BENZ, output2.BENZ, EPSILON, "BENZ")
			ACET_equal  := arrays_approximately_equal(output1.ACET, output2.ACET, EPSILON, "ACET")
			KET_equal   := arrays_approximately_equal(output1.KET, output2.KET, EPSILON, "KET")
			AACD_equal  := arrays_approximately_equal(output1.AACD, output2.AACD, EPSILON, "AACD")
			FACD_equal  := arrays_approximately_equal(output1.FACD, output2.FACD, EPSILON, "FACD")
			HCN_equal   := arrays_approximately_equal(output1.HCN, output2.HCN, EPSILON, "HCN")
			ISPD_equal  := arrays_approximately_equal(output1.ISPD, output2.ISPD, EPSILON, "ISPD")
			N2O_equal   := arrays_approximately_equal(output1.N2O, output2.N2O, EPSILON, "N2O")
			SESQ_equal  := arrays_approximately_equal(output1.SESQ, output2.SESQ, EPSILON, "SESQ")
			TRS_equal   := arrays_approximately_equal(output1.TRS, output2.TRS, EPSILON, "TRS")
			CH3BR_equal := arrays_approximately_equal(output1.CH3BR, output2.CH3BR, EPSILON, "CH3BR")
			CH3CL_equal := arrays_approximately_equal(output1.CH3CL, output2.CH3CL, EPSILON, "CH3CL")
			CH3I_equal  := arrays_approximately_equal(output1.CH3I, output2.CH3I, EPSILON, "CH3I")
			ISP_equal   := arrays_approximately_equal(output1.ISP, output2.ISP, EPSILON, "ISP")
			TRP_equal   := arrays_approximately_equal(output1.TRP, output2.TRP, EPSILON, "TRP")
			XYLA_equal  := arrays_approximately_equal(output1.XYLA, output2.XYLA, EPSILON, "XYLA")
			SQT_equal   := arrays_approximately_equal(output1.SQT, output2.SQT, EPSILON, "SQT")
			TOLA_equal  := arrays_approximately_equal(output1.TOLA, output2.TOLA, EPSILON, "TOLA")

			return ISOP_equal && TERP_equal && PAR_equal && XYL_equal && OLE_equal && NR_equal && MEOH_equal && CH4_equal && NH3_equal && NO_equal && ALD2_equal && ETOH_equal && FORM_equal && ALDX_equal && TOL_equal && IOLE_equal && CO_equal && ETHA_equal && ETH_equal && ETHY_equal && PRPA_equal && BENZ_equal && ACET_equal && KET_equal && AACD_equal && FACD_equal && HCN_equal && ISPD_equal && N2O_equal && SESQ_equal && TRS_equal && CH3BR_equal && CH3CL_equal && CH3I_equal && ISP_equal && TRP_equal && XYLA_equal && SQT_equal && TOLA_equal
		}
	}
	return false
}