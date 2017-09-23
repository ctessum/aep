package megan

import (
	"testing"
	"os"
)

func TestMgn2mech(t *testing.T) {
	go_output := run_go_mgn2mech()
	
	ISOP_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.08549863, 0.1901947, 0.01413564, 0, 0, 0, 0.2476667, 0.1697283, 0.134044, 0.1522065, 0.147545, 0.2833969}
	TERP_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.003753447, 0.006545584, 0.0006078602, 0, 0, 0, 0.007363195, 0.01064267, 0.01169429, 0.0131755, 0.01258804, 0.01719705}
	PAR_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0008894194, 0.001601944, 0.000145092, 0, 0, 0, 0.001852462, 0.002473795, 0.00266421, 0.003008224, 0.002882492, 0.004030752}
	XYL_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	OLE_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0005940548, 0.0009880014, 9.648284e-05, 0, 0, 0, 0.001079734, 0.001806393, 0.002055798, 0.002319317, 0.00221853, 0.002927803}
	NR_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	MEOH_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01424337, 0.02848718, 0.002171421, 0, 0, 0, 0.03330369, 0.02422228, 0.01937554, 0.02084235, 0.01890915, 0.03534352}
	CH4_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.0258e-06, 6.695504e-06, 6.538464e-07, 0, 0, 0, 7.317158e-06, 1.224159e-05, 1.393176e-05, 1.571759e-05, 1.503457e-05, 1.984118e-05}
	NH3_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	NO_expected := []float64{9.940225e-07, 1.016836e-06, 9.478741e-07, 8.723866e-07, 8.036999e-07, 7.702124e-07, 8.015715e-07, 8.376093e-07, 9.681748e-07, 1.164393e-06, 1.170123e-06, 1.070677e-06, 1.041866e-06, 1.057617e-06, 1.050329e-06, 1.066609e-06, 1.181683e-06, 1.273218e-06, 1.239873e-06, 1.227178e-06, 1.190352e-06, 1.185402e-06, 1.228115e-06, 1.267849e-06, 1.277538e-06}
	ALD2_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0007788371, 0.001615647, 0.0001328344, 0, 0, 0, 0.002031078, 0.002018837, 0.001964166, 0.00227242, 0.002249054, 0.00348371}
	ETOH_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0007355264, 0.001525802, 0.0001254476, 0, 0, 0, 0.001918131, 0.00190657, 0.00185494, 0.002146052, 0.002123985, 0.003289982}
	FORM_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0001291176, 0.0002147418, 2.097052e-05, 0, 0, 0, 0.0002346799, 0.0003926189, 0.0004468271, 0.0005041029, 0.0004821969, 0.0006363572}
	ALDX_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	TOL_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	IOLE_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0002844379, 0.0005957698, 4.683446e-05, 0, 0, 0, 0.000752868, 0.0006341767, 0.0005705989, 0.0006462465, 0.0006231555, 0.001049096}
	CO_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01078564, 0.02157161, 0.001644285, 0, 0, 0, 0.02521886, 0.01834206, 0.01467192, 0.01578264, 0.01431875, 0.0267635}
	ETHA_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0003516259, 0.0005848063, 5.710899e-05, 0, 0, 0, 0.0006391035, 0.001069219, 0.001216844, 0.001372823, 0.001313166, 0.001732991}
	ETH_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.000660103, 0.001097849, 0.00010721, 0, 0, 0, 0.001199781, 0.002007231, 0.002284365, 0.002577183, 0.00246519, 0.003253322}
	ETHY_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	PRPA_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0002200974, 0.0003660547, 3.57469e-05, 0, 0, 0, 0.0004000416, 0.0006692688, 0.0007616734, 0.0008593073, 0.0008219657, 0.001084751}
	BENZ_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	ACET_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0007811487, 0.001299167, 0.0001268695, 0, 0, 0, 0.00141979, 0.002375305, 0.002703259, 0.003049772, 0.002917242, 0.003849897}
	KET_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	AACD_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9.955039e-05, 0.0002065109, 1.69788e-05, 0, 0, 0, 0.0002596109, 0.0002580462, 0.0002510583, 0.0002904591, 0.0002874724, 0.0004452852}
	FACD_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.000491183, 0.001018927, 8.377361e-05, 0, 0, 0, 0.001280923, 0.001273203, 0.001238725, 0.001433129, 0.001418393, 0.002197043}
	HCN_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3.980381e-05, 7.641892e-05, 6.405617e-06, 0, 0, 0, 9.059165e-05, 9.504362e-05, 9.407754e-05, 0.0001055363, 0.0001004472, 0.0001522734}
	ISPD_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0003895442, 0.0007456595, 6.270209e-05, 0, 0, 0, 0.0008826172, 0.0009358057, 0.0009301949, 0.001043685, 0.0009935179, 0.001499785}
	N2O_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	SESQ_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0001192427, 0.0002624757, 2.366675e-05, 0, 0, 0, 0.0003511755, 0.0004602313, 0.0004947009, 0.0006061591, 0.0006439399, 0.0009135043}
	TRS_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6.64725e-06, 1.105537e-05, 1.079607e-06, 0, 0, 0, 1.208182e-05, 2.021286e-05, 2.300361e-05, 2.595229e-05, 2.482452e-05, 3.276102e-05}
	CH3BR_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.016739e-07, 6.680434e-07, 6.523749e-08, 0, 0, 0, 7.30069e-07, 1.221404e-06, 1.390041e-06, 1.568221e-06, 1.500073e-06, 1.979653e-06}
	CH3CL_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3.222452e-06, 5.359417e-06, 5.233715e-07, 0, 0, 0, 5.857021e-06, 9.798783e-06, 1.115168e-05, 1.258114e-05, 1.203442e-05, 1.588188e-05}
	CH3I_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.026836e-05, 1.70778e-05, 1.667725e-06, 0, 0, 0, 1.866342e-05, 3.122386e-05, 3.553488e-05, 4.008986e-05, 3.834773e-05, 5.060766e-05}
	ISP_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.08549863, 0.1901947, 0.01413564, 0, 0, 0, 0.2476667, 0.1697283, 0.134044, 0.1522065, 0.147545, 0.2833969}
	TRP_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.003753447, 0.006545584, 0.0006078602, 0, 0, 0, 0.007363195, 0.01064267, 0.01169429, 0.0131755, 0.01258804, 0.01719705}
	XYLA_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	SQT_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0001192427, 0.0002624757, 2.366675e-05, 0, 0, 0, 0.0003511755, 0.0004602313, 0.0004947009, 0.0006061591, 0.0006439399, 0.0009135043}
	TOLA_expected := []float64{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
		
	expected_output := Mgn2mech_output{ISOP_expected, TERP_expected, PAR_expected, XYL_expected, OLE_expected, NR_expected, MEOH_expected, CH4_expected, NH3_expected, NO_expected, ALD2_expected, ETOH_expected, FORM_expected, ALDX_expected, TOL_expected, IOLE_expected, CO_expected, ETHA_expected, ETH_expected, ETHY_expected, PRPA_expected, BENZ_expected, ACET_expected, KET_expected, AACD_expected, FACD_expected, HCN_expected, ISPD_expected, N2O_expected, SESQ_expected, TRS_expected, CH3BR_expected, CH3CL_expected, CH3I_expected, ISP_expected, TRP_expected, XYLA_expected, SQT_expected, TOLA_expected}

	if !are_mgn2mech_outputs_equal(go_output, expected_output) {
		t.Errorf("Go and standalone versions produce different results (epsilon=%v)\n", EPSILON)
	}
}

func TestMgn2mechAgainstStandalone(t *testing.T) {
	if os.Getenv("MEGAN_STANDALONE_TEST") == "" {
		t.Skip("skipping test; $MEGAN_STANDALONE_TEST not set")
	}

	go_output := run_go_mgn2mech()
	standalone_output := run_standalone_mgn2mech()
	
	if !are_mgn2mech_outputs_equal(go_output, standalone_output) {
		t.Errorf("Go and standalone versions produce different results (epsilon=%v)\n", EPSILON)
	}
}
 
func run_go_mgn2mech() Mgn2mech_output {
	return RunMgn2mech()
}

func run_standalone_mgn2mech() Mgn2mech_output {
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
	
	return Mgn2mech_output{ISOP, TERP, PAR, XYL, OLE, NR, MEOH, CH4, NH3, NO, ALD2, ETOH, FORM, ALDX, TOL, IOLE, CO, ETHA, ETH, ETHY, PRPA, BENZ, ACET, KET, AACD, FACD, HCN, ISPD, N2O, SESQ, TRS, CH3BR, CH3CL, CH3I, ISP, TRP, XYLA, SQT, TOLA}
}

func are_mgn2mech_outputs_equal(output1 Mgn2mech_output, output2 Mgn2mech_output) bool {
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