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
	
	ISOP_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1422884, 0.1414478, 0.01264605, 0, 0, 0, 0.2453534, 0.190485, 0.1507151, 0.1664178, 0.1537218, 0.2649276}
	TERP_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.004669419, 0.004787037, 0.0005030177, 0, 0, 0, 0.006847403, 0.01027617, 0.01114405, 0.0122429, 0.01117343, 0.01415238}
	PAR_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.001147914, 0.001160368, 0.0001202914, 0, 0, 0, 0.001734591, 0.002416447, 0.002562532, 0.002820718, 0.002580902, 0.003341706}
	XYL_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	OLE_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.000694395, 0.0007073266, 7.789323e-05, 0, 0, 0, 0.0009805918, 0.001708618, 0.001928016, 0.002121101, 0.001937477, 0.002359439}
	NR_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	MEOH_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.02158913, 0.02425977, 0.002094631, 0, 0, 0, 0.03373446, 0.02693121, 0.02160956, 0.02285756, 0.0201317, 0.03508812}
	CH4_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.705787e-06, 4.793422e-06, 5.278682e-07, 0, 0, 0, 6.64529e-06, 1.157899e-05, 1.306581e-05, 1.437431e-05, 1.312992e-05, 1.598948e-05}
	NH3_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	NO_expected 	:= []float64{9.940225e-07, 1.016836e-06, 9.478741e-07, 8.723866e-07, 8.036999e-07, 7.702124e-07, 8.015715e-07, 8.376093e-07, 9.681748e-07, 1.164393e-06, 1.170123e-06, 1.070677e-06, 1.041866e-06, 1.057617e-06, 1.050329e-06, 1.066609e-06, 1.181683e-06, 1.273218e-06, 1.239873e-06, 1.227178e-06, 1.190352e-06, 1.185402e-06, 1.228115e-06, 1.267849e-06, 1.277538e-06}
	ALD2_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.001158493, 0.00114487, 0.0001108194, 0, 0, 0, 0.001932717, 0.002050425, 0.001952026, 0.002186578, 0.00204475, 0.002904368}
	ETOH_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.00109407, 0.001081205, 0.0001046568, 0, 0, 0, 0.001825239, 0.001936402, 0.001843475, 0.002064983, 0.001931042, 0.002742857}
	FORM_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0001509266, 0.0001537372, 1.693007e-05, 0, 0, 0, 0.0002131314, 0.0003713676, 0.0004190537, 0.0004610206, 0.0004211101, 0.0005128235}
	ALDX_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	TOL_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	IOLE_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0004407463, 0.0004400308, 4.09654e-05, 0, 0, 0, 0.0007359629, 0.0006764019, 0.0005990526, 0.0006606028, 0.0006077883, 0.0009388684}
	CO_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01634813, 0.01837045, 0.001586137, 0, 0, 0, 0.02554506, 0.02039337, 0.01636361, 0.01730864, 0.01524451, 0.0265701}
	ETHA_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0004110182, 0.0004186725, 4.610566e-05, 0, 0, 0, 0.0005804204, 0.001011345, 0.001141209, 0.001255497, 0.001146809, 0.001396571}
	ETH_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0007715993, 0.0007859686, 8.655356e-05, 0, 0, 0, 0.001089616, 0.001898585, 0.002142376, 0.002356929, 0.002152889, 0.002621766}
	ETHY_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	PRPA_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0002572735, 0.0002620647, 2.885946e-05, 0, 0, 0, 0.0003633094, 0.0006330432, 0.0007143301, 0.0007858681, 0.0007178356, 0.0008741727}
	BENZ_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	ACET_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0009130905, 0.0009300949, 0.0001024252, 0, 0, 0, 0.001289423, 0.002246737, 0.002535233, 0.002789128, 0.002547673, 0.00310253}
	KET_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	AACD_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0001480777, 0.0001463364, 1.416486e-05, 0, 0, 0, 0.0002470384, 0.0002620838, 0.0002495066, 0.0002794867, 0.0002613585, 0.0003712342}
	FACD_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0007306174, 0.0007220259, 6.988958e-05, 0, 0, 0, 0.001218891, 0.001293125, 0.001231068, 0.001378991, 0.001289546, 0.001831674}
	HCN_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.603617e-05, 5.811363e-05, 5.585389e-06, 0, 0, 0, 8.767913e-05, 9.696594e-05, 9.418671e-05, 0.000103039, 9.379558e-05, 0.000132618}
	ISPD_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0005463361, 0.0005664041, 5.458235e-05, 0, 0, 0, 0.0008533022, 0.0009527762, 0.0009293779, 0.0010169, 0.000925773, 0.001303419}
	N2O_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	SESQ_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0001675745, 0.0001515364, 1.660991e-05, 0, 0, 0, 0.0003009698, 0.0004248342, 0.0004488195, 0.0005266677, 0.0005179571, 0.0006474273}
	TRS_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7.770021e-06, 7.914719e-06, 8.715962e-07, 0, 0, 0, 1.097246e-05, 1.911879e-05, 2.157377e-05, 2.373432e-05, 2.167964e-05, 2.640125e-05}
	CH3BR_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.695196e-07, 4.782634e-07, 5.266801e-08, 0, 0, 0, 6.630334e-07, 1.155293e-06, 1.30364e-06, 1.434196e-06, 1.310037e-06, 1.59535e-06}
	CH3CL_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3.766748e-06, 3.836895e-06, 4.225321e-07, 0, 0, 0, 5.319223e-06, 9.268402e-06, 1.045853e-05, 1.150591e-05, 1.050985e-05, 1.279878e-05}
	CH3I_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.200276e-05, 1.222628e-05, 1.3464e-06, 0, 0, 0, 1.694973e-05, 2.95338e-05, 3.332614e-05, 3.666365e-05, 3.348968e-05, 4.078338e-05}
	ISP_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1422884, 0.1414478, 0.01264605, 0, 0, 0, 0.2453534, 0.190485, 0.1507151, 0.1664178, 0.1537218, 0.2649276}
	TRP_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.004669419, 0.004787037, 0.0005030177, 0, 0, 0, 0.006847403, 0.01027617, 0.01114405, 0.0122429, 0.01117343, 0.01415238}
	XYLA_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	SQT_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0001675745, 0.0001515364, 1.660991e-05, 0, 0, 0, 0.0003009698, 0.0004248342, 0.0004488195, 0.0005266677, 0.0005179571, 0.0006474273}
	TOLA_expected 	:= []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

		
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