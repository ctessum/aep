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
	
	ISOP_expected 	:= 0.2471264
	TERP_expected 	:= 0.006587192
	PAR_expected 	:= 0.001656804
	XYL_expected 	:= 0.
	OLE_expected 	:= 0.0009076943
	NR_expected 	:= 0.
	MEOH_expected 	:= 0.03908064
	CH4_expected 	:= 6.151277e-06
	NH3_expected 	:= 0.
	NO_expected 	:= 1.056828e-06
	ALD2_expected 	:= 0.001851185
	ETOH_expected 	:= 0.001748241
	FORM_expected 	:= 0.0001972871
	ALDX_expected 	:= 0.
	TOL_expected 	:= 0.
	IOLE_expected 	:= 0.0007324139
	CO_expected 	:= 0.02959339
	ETHA_expected 	:= 0.0005372717
	ETH_expected 	:= 0.001008613
	ETHY_expected 	:= 0.
	PRPA_expected 	:= 0.0003363009
	BENZ_expected 	:= 0.
	ACET_expected 	:= 0.001193567
	KET_expected 	:= 0.
	AACD_expected 	:= 0.0002366171
	FACD_expected 	:= 0.001167472
	HCN_expected 	:= 8.955201e-05
	ISPD_expected 	:= 0.0008701621
	N2O_expected 	:= 0.
	SESQ_expected 	:= 0.0002344448
	TRS_expected 	:= 1.015676e-05
	CH3BR_expected 	:= 6.137433e-07
	CH3CL_expected 	:= 4.923791e-06
	CH3I_expected 	:= 1.568968e-05
	ISP_expected 	:= 0.2471264
	TRP_expected 	:= 0.006587192
	XYLA_expected 	:= 0.
	SQT_expected 	:= 0.0002344448
	TOLA_expected 	:= 0.
		
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
	date := 2013145
	time := 0
	use_mechanism_conversion := true
	output_in_ton_per_hr := false
	area_per_grid_cell := 144000000.0 
	mechanism := CB6X		
	inputEmissionActivity := GetEmissionActivityPerEmissionTypeTestData()
	emission_factor := []float64{8.769534, 0.0224195, 0.3402439, 0.01646902, 0.007909732, 0.008048705, 0.00181239, 0.0019196, 0.00901152, 0.0150626, 1.03454, 0.143244, 0.2013025, 0.060192, 0.3470387, 0.02525556, 0.03697741, 0.00553815, 0.7958, 1.06345e-05}
	inputSoilMoistureAndNOEmissionActivity := GetSoilMoistureAndNOEmissionActivityTestData()
	return ChemicalSpeciationAndMechanismConversion(date, time, use_mechanism_conversion, output_in_ton_per_hr, area_per_grid_cell, mechanism, inputEmissionActivity, emission_factor, inputSoilMoistureAndNOEmissionActivity)
}

func run_standalone_mgn2mech() Species_CB6X {
	// Run standalone script
	run_command("cd ./MEGAN3/work/; ./run.mgn2mech.v3.single.csh")
	
	// Extract outputs from NETCDF files
	output_file := "./MEGAN3/Output/MEGANv3.tceq_12km.J4.CB6X.2013145.single.baseline.nc"
	ISOP  := parse_netcdf_file("ISOP", output_file)[0]
	TERP  := parse_netcdf_file("TERP", output_file)[0]
	PAR   := parse_netcdf_file("PAR", output_file)[0]
	XYL   := parse_netcdf_file("XYL", output_file)[0]
	OLE   := parse_netcdf_file("OLE", output_file)[0]
	NR    := parse_netcdf_file("NR", output_file)[0]
	MEOH  := parse_netcdf_file("MEOH", output_file)[0]
	CH4   := parse_netcdf_file("CH4", output_file)[0]
	NH3   := parse_netcdf_file("NH3", output_file)[0]
	NO    := parse_netcdf_file("NO", output_file)[0]
	ALD2  := parse_netcdf_file("ALD2", output_file)[0]
	ETOH  := parse_netcdf_file("ETOH", output_file)[0]
	FORM  := parse_netcdf_file("FORM", output_file)[0]
	ALDX  := parse_netcdf_file("ALDX", output_file)[0]
	TOL   := parse_netcdf_file("TOL", output_file)[0]
	IOLE  := parse_netcdf_file("IOLE", output_file)[0]
	CO    := parse_netcdf_file("CO", output_file)[0]
	ETHA  := parse_netcdf_file("ETHA", output_file)[0]
	ETH   := parse_netcdf_file("ETH", output_file)[0]
	ETHY  := parse_netcdf_file("ETHY", output_file)[0]
	PRPA  := parse_netcdf_file("PRPA", output_file)[0]
	BENZ  := parse_netcdf_file("BENZ", output_file)[0]
	ACET  := parse_netcdf_file("ACET", output_file)[0]
	KET   := parse_netcdf_file("KET", output_file)[0]
	AACD  := parse_netcdf_file("AACD", output_file)[0]
	FACD  := parse_netcdf_file("FACD", output_file)[0]
	HCN   := parse_netcdf_file("HCN", output_file)[0]
	ISPD  := parse_netcdf_file("ISPD", output_file)[0]
	N2O   := parse_netcdf_file("N2O", output_file)[0]
	SESQ  := parse_netcdf_file("SESQ", output_file)[0]
	TRS   := parse_netcdf_file("TRS", output_file)[0]
	CH3BR := parse_netcdf_file("CH3BR", output_file)[0]
	CH3CL := parse_netcdf_file("CH3CL", output_file)[0]
	CH3I  := parse_netcdf_file("CH3I", output_file)[0]
	ISP   := parse_netcdf_file("ISP", output_file)[0]
	TRP   := parse_netcdf_file("TRP", output_file)[0]
	XYLA  := parse_netcdf_file("XYLA", output_file)[0]
	SQT   := parse_netcdf_file("SQT", output_file)[0]
	TOLA  := parse_netcdf_file("TOLA", output_file)[0]
	
	return Species_CB6X{ISOP, TERP, PAR, XYL, OLE, NR, MEOH, CH4, NH3, NO, ALD2, ETOH, FORM, ALDX, TOL, IOLE, CO, ETHA, ETH, ETHY, PRPA, BENZ, ACET, KET, AACD, FACD, HCN, ISPD, N2O, SESQ, TRS, CH3BR, CH3CL, CH3I, ISP, TRP, XYLA, SQT, TOLA}
}

func are_mgn2mech_outputs_equal(output1 interface{}, output2 interface{}) bool {

	if output1, ok := output1.(Species_CB6X); ok {
		if output2, ok := output2.(Species_CB6X); ok {
			ISOP_equal  := approximately_equal(output1.ISOP, output2.ISOP, EPSILON, "ISOP")
			TERP_equal  := approximately_equal(output1.TERP, output2.TERP, EPSILON, "TERP")
			PAR_equal   := approximately_equal(output1.PAR, output2.PAR, EPSILON, "PAR")
			XYL_equal   := approximately_equal(output1.XYL, output2.XYL, EPSILON, "XYL")
			OLE_equal   := approximately_equal(output1.OLE, output2.OLE, EPSILON, "OLE")
			NR_equal    := approximately_equal(output1.NR, output2.NR, EPSILON, "NR")
			MEOH_equal  := approximately_equal(output1.MEOH, output2.MEOH, EPSILON, "MEOH")
			CH4_equal   := approximately_equal(output1.CH4, output2.CH4, EPSILON, "CH4")
			NH3_equal   := approximately_equal(output1.NH3, output2.NH3, EPSILON, "NH3")
			NO_equal    := approximately_equal(output1.NO, output2.NO, EPSILON, "NO")
			ALD2_equal  := approximately_equal(output1.ALD2, output2.ALD2, EPSILON, "ALD2")
			ETOH_equal  := approximately_equal(output1.ETOH, output2.ETOH, EPSILON, "ETOH")
			FORM_equal  := approximately_equal(output1.FORM, output2.FORM, EPSILON, "FORM")
			ALDX_equal  := approximately_equal(output1.ALDX, output2.ALDX, EPSILON, "ALDX")
			TOL_equal   := approximately_equal(output1.TOL, output2.TOL, EPSILON, "TOL")
			IOLE_equal  := approximately_equal(output1.IOLE, output2.IOLE, EPSILON, "IOLE")
			CO_equal    := approximately_equal(output1.CO, output2.CO, EPSILON, "CO")
			ETHA_equal  := approximately_equal(output1.ETHA, output2.ETHA, EPSILON, "ETHA")
			ETH_equal   := approximately_equal(output1.ETH, output2.ETH, EPSILON, "ETH")
			ETHY_equal  := approximately_equal(output1.ETHY, output2.ETHY, EPSILON, "ETHY")
			PRPA_equal  := approximately_equal(output1.PRPA, output2.PRPA, EPSILON, "PRPA")
			BENZ_equal  := approximately_equal(output1.BENZ, output2.BENZ, EPSILON, "BENZ")
			ACET_equal  := approximately_equal(output1.ACET, output2.ACET, EPSILON, "ACET")
			KET_equal   := approximately_equal(output1.KET, output2.KET, EPSILON, "KET")
			AACD_equal  := approximately_equal(output1.AACD, output2.AACD, EPSILON, "AACD")
			FACD_equal  := approximately_equal(output1.FACD, output2.FACD, EPSILON, "FACD")
			HCN_equal   := approximately_equal(output1.HCN, output2.HCN, EPSILON, "HCN")
			ISPD_equal  := approximately_equal(output1.ISPD, output2.ISPD, EPSILON, "ISPD")
			N2O_equal   := approximately_equal(output1.N2O, output2.N2O, EPSILON, "N2O")
			SESQ_equal  := approximately_equal(output1.SESQ, output2.SESQ, EPSILON, "SESQ")
			TRS_equal   := approximately_equal(output1.TRS, output2.TRS, EPSILON, "TRS")
			CH3BR_equal := approximately_equal(output1.CH3BR, output2.CH3BR, EPSILON, "CH3BR")
			CH3CL_equal := approximately_equal(output1.CH3CL, output2.CH3CL, EPSILON, "CH3CL")
			CH3I_equal  := approximately_equal(output1.CH3I, output2.CH3I, EPSILON, "CH3I")
			ISP_equal   := approximately_equal(output1.ISP, output2.ISP, EPSILON, "ISP")
			TRP_equal   := approximately_equal(output1.TRP, output2.TRP, EPSILON, "TRP")
			XYLA_equal  := approximately_equal(output1.XYLA, output2.XYLA, EPSILON, "XYLA")
			SQT_equal   := approximately_equal(output1.SQT, output2.SQT, EPSILON, "SQT")
			TOLA_equal  := approximately_equal(output1.TOLA, output2.TOLA, EPSILON, "TOLA")

			return ISOP_equal && TERP_equal && PAR_equal && XYL_equal && OLE_equal && NR_equal && MEOH_equal && CH4_equal && NH3_equal && NO_equal && ALD2_equal && ETOH_equal && FORM_equal && ALDX_equal && TOL_equal && IOLE_equal && CO_equal && ETHA_equal && ETH_equal && ETHY_equal && PRPA_equal && BENZ_equal && ACET_equal && KET_equal && AACD_equal && FACD_equal && HCN_equal && ISPD_equal && N2O_equal && SESQ_equal && TRS_equal && CH3BR_equal && CH3CL_equal && CH3I_equal && ISP_equal && TRP_equal && XYLA_equal && SQT_equal && TOLA_equal
		}
	}
	return false
}