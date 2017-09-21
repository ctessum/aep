package main
 
/* 
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h> 

#cgo LDFLAGS: -L ./MEGAN -lmegan -lm -lgfortran

void run_mgn2mech_c(int* SDATE, int* STIME, int* MXREC, int* NCOLS, int* NROWS, int* TSTEP, int* NVAR, _Bool* CONVERSION, _Bool* TONPHR, char* MECHANISM, int* MECHANISM_len, float* garea, float* inper, float* EF, float* GAMNO, float* ISOP, float* TERP, float* PAR, float* XYL, float* OLE, float* NR, float* MEOH, float* CH4, float* NH3, float* NO, float* ALD2, float* ETOH, float* FORM, float* ALDX, float* TOL, float* IOLE, float* CO, float* ETHA, float* ETH, float* ETHY, float* PRPA, float* BENZ, float* ACET, float* KET, float* AACD, float* FACD, float* HCN, float* ISPD, float* N2O, float* SESQ, float* TRS, float* CH3BR, float* CH3CL, float* CH3I, float* ISP, float* TRP, float* XYLA, float* SQT, float* TOLA );
*/
import "C"
import "unsafe"

type mgn2mech_output struct {
	ISOP  []C.float
	TERP  []C.float
	PAR   []C.float
	XYL   []C.float
	OLE   []C.float
	NR    []C.float
	MEOH  []C.float
	CH4   []C.float
	NH3   []C.float
	NO    []C.float
	ALD2  []C.float
	ETOH  []C.float
	FORM  []C.float
	ALDX  []C.float
	TOL   []C.float
	IOLE  []C.float
	CO    []C.float
	ETHA  []C.float
	ETH   []C.float
	ETHY  []C.float
	PRPA  []C.float
	BENZ  []C.float
	ACET  []C.float
	KET   []C.float
	AACD  []C.float
	FACD  []C.float
	HCN   []C.float
	ISPD  []C.float
	N2O   []C.float
	SESQ  []C.float
	TRS   []C.float
	CH3BR []C.float
	CH3CL []C.float
	CH3I  []C.float
	ISP   []C.float
	TRP   []C.float
	XYLA  []C.float
	SQT   []C.float
	TOLA  []C.float
}
 
func run_go_mgn2mech() mgn2mech_output {
	var (
		SDATE C.int = 2013145
		STIME C.int = 0
		MXREC C.int = 25
		NCOLS C.int = 1
		NROWS C.int = 1
		TSTEP C.int = 10000
		NVAR C.int = 39
		CONVERSION C.bool = true
		TONPHR C.bool = false
		garea C.float = 144000000.0
		SIZE int = int(MXREC * NROWS * NCOLS)
	)
	
	MECHANISM := C.CString("CB6X")
	defer C.free(unsafe.Pointer(MECHANISM))
	var MECHANISM_len C.int = 4
	
	inper := []C.float{0,0,0,0,0,0,0,0,0,0,0,0,0,0.06770491,0.1506119,0.01119377,0,0,0,0.196123,0.1344049,0.1061472,0.1205298,0.1168384,0.2244172,0,0,0,0,0,0,0,0,0,0,0,0,0,0.06770491,0.1506119,0.01119377,0,0,0,0.196123,0.1344049,0.1061472,0.1205298,0.1168384,0.2244172,0,0,0,0,0,0,0,0,0,0,0,0,0,0.06933573,0.1202633,0.01123249,0,0,0,0.134856,0.1982512,0.2188023,0.2465584,0.2356006,0.3204671,0,0,0,0,0,0,0,0,0,0,0,0,0,0.08496049,0.1642323,0.01366621,0,0,0,0.1953614,0.2000265,0.1960313,0.219812,0.2091313,0.3202239,0,0,0,0,0,0,0,0,0,0,0,0,0,0.05284291,0.08925098,0.008574537,0,0,0,0.09848323,0.1572113,0.1770349,0.1996476,0.1909046,0.2545713,0,0,0,0,0,0,0,0,0,0,0,0,0,0.05447638,0.09219893,0.008838497,0,0,0,0.101865,0.1615902,0.1816995,0.2048965,0.1959141,0.2616284,0,0,0,0,0,0,0,0,0,0,0,0,0,0.08056435,0.1449134,0.01302163,0,0,0,0.1659329,0.217198,0.2321275,0.2612407,0.2493507,0.3501392,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03801988,0.06323272,0.006174963,0,0,0,0.06910365,0.1156103,0.1315724,0.1484378,0.1419873,0.1873812,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03663687,0.08102604,0.007240801,0,0,0,0.1088041,0.1389914,0.1483657,0.1817109,0.1929337,0.2755488,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03296247,0.07232749,0.006560703,0,0,0,0.0965313,0.1286724,0.1389317,0.170283,0.1809583,0.2555993,0,0,0,0,0,0,0,0,0,0,0,0,0,0.09560993,0.1912228,0.01457586,0,0,0,0.2235541,0.1625942,0.1300601,0.1399062,0.1269294,0.2372466,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03786996,0.06298337,0.006150613,0,0,0,0.06883116,0.1151544,0.1310535,0.1478524,0.1414274,0.1866423,0,0,0,0,0,0,0,0,0,0,0,0,0,0.05224184,0.1083723,0.008910099,0,0,0,0.136238,0.135417,0.1317498,0.1524265,0.1508592,0.2336758,0,0,0,0,0,0,0,0,0,0,0,0,0,0.06889792,0.1429243,0.01175087,0,0,0,0.1796743,0.1785914,0.1737551,0.2010241,0.198957,0.3081778,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03786996,0.06298337,0.006150613,0,0,0,0.06883116,0.1151544,0.1310535,0.1478524,0.1414274,0.1866423,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03786996,0.06298337,0.006150613,0,0,0,0.06883116,0.1151544,0.1310535,0.1478524,0.1414274,0.1866423,0,0,0,0,0,0,0,0,0,0,0,0,0,0.0847534,0.1627171,0.01363934,0,0,0,0.1928948,0.2023743,0.2003172,0.2247161,0.21388,0.3242324,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03786996,0.06298337,0.006150613,0,0,0,0.06883116,0.1151544,0.1310535,0.1478524,0.1414274,0.1866423,0,0,0,0,0,0,0,0,0,0,0,0,0,0.09411947,0.1882419,0.01434864,0,0,0,0.2200691,0.1600596,0.1280326,0.1377252,0.1249507,0.2335482,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
	EF := []C.float{8.769534, 0.0224195, 0.3402439, 0.01646902, 0.007909732, 0.008048705, 0.00181239, 0.0019196, 0.00901152, 0.0150626, 1.03454, 0.143244, 0.2013025, 0.060192, 0.3470387, 0.02525556, 0.03697741, 0.00553815, 0.7958, 1.06345e-05}
	GAMNO := []C.float{0.6491075, 0.6640053, 0.6189721, 0.5696779, 0.5248248, 0.5029572, 0.523435, 0.546968, 0.6322287, 0.7603613, 0.7641032, 0.699164, 0.6803498, 0.6906353, 0.6858764, 0.6965074, 0.7716522, 0.8314256, 0.8096505, 0.8013604, 0.7773128, 0.7740801, 0.8019726, 0.8279191, 0.8342465}

	
	var ISOP, TERP, PAR, XYL, OLE, NR, MEOH, CH4, NH3, NO, ALD2, ETOH, FORM, ALDX, TOL, IOLE, CO, ETHA, ETH, ETHY, PRPA, BENZ, ACET, KET, AACD, FACD, HCN, ISPD, N2O, SESQ, TRS, CH3BR, CH3CL, CH3I, ISP, TRP, XYLA, SQT, TOLA []C.float
	ISOP  = make([]C.float, SIZE)
	TERP  = make([]C.float, SIZE)
	PAR   = make([]C.float, SIZE)
	XYL   = make([]C.float, SIZE)
	OLE   = make([]C.float, SIZE)
	NR    = make([]C.float, SIZE)
	MEOH  = make([]C.float, SIZE)
	CH4   = make([]C.float, SIZE)
	NH3   = make([]C.float, SIZE)
	NO    = make([]C.float, SIZE)
	ALD2  = make([]C.float, SIZE)
	ETOH  = make([]C.float, SIZE)
	FORM  = make([]C.float, SIZE)
	ALDX  = make([]C.float, SIZE)
	TOL   = make([]C.float, SIZE)
	IOLE  = make([]C.float, SIZE)
	CO    = make([]C.float, SIZE)
	ETHA  = make([]C.float, SIZE)
	ETH   = make([]C.float, SIZE)
	ETHY  = make([]C.float, SIZE)
	PRPA  = make([]C.float, SIZE)
	BENZ  = make([]C.float, SIZE)
	ACET  = make([]C.float, SIZE)
	KET   = make([]C.float, SIZE)
	AACD  = make([]C.float, SIZE)
	FACD  = make([]C.float, SIZE)
	HCN   = make([]C.float, SIZE)
	ISPD  = make([]C.float, SIZE)
	N2O   = make([]C.float, SIZE)
	SESQ  = make([]C.float, SIZE)
	TRS   = make([]C.float, SIZE)
	CH3BR = make([]C.float, SIZE)
	CH3CL = make([]C.float, SIZE)
	CH3I  = make([]C.float, SIZE)
	ISP   = make([]C.float, SIZE)
	TRP   = make([]C.float, SIZE)
	XYLA  = make([]C.float, SIZE)
	SQT   = make([]C.float, SIZE)
	TOLA  = make([]C.float, SIZE)
	
	// Call FORTRAN
    C.run_mgn2mech_c(&SDATE, &STIME, &MXREC, &NCOLS, &NROWS, &TSTEP, &NVAR, &CONVERSION, &TONPHR, MECHANISM, &MECHANISM_len, &garea, &inper[0], &EF[0], &GAMNO[0], &ISOP[0], &TERP[0], &PAR[0], &XYL[0], &OLE[0], &NR[0], &MEOH[0], &CH4[0], &NH3[0], &NO[0], &ALD2[0], &ETOH[0], &FORM[0], &ALDX[0], &TOL[0], &IOLE[0], &CO[0], &ETHA[0], &ETH[0], &ETHY[0], &PRPA[0], &BENZ[0], &ACET[0], &KET[0], &AACD[0], &FACD[0], &HCN[0], &ISPD[0], &N2O[0], &SESQ[0], &TRS[0], &CH3BR[0], &CH3CL[0], &CH3I[0], &ISP[0], &TRP[0], &XYLA[0], &SQT[0], &TOLA[0])
	
	//fmt.Printf("outer: %v\n", outer)
	
	return mgn2mech_output{ISOP, TERP, PAR, XYL, OLE, NR, MEOH, CH4, NH3, NO, ALD2, ETOH, FORM, ALDX, TOL, IOLE, CO, ETHA, ETH, ETHY, PRPA, BENZ, ACET, KET, AACD, FACD, HCN, ISPD, N2O, SESQ, TRS, CH3BR, CH3CL, CH3I, ISP, TRP, XYLA, SQT, TOLA}
}

func run_standalone_mgn2mech() mgn2mech_output {
	// Run standalone script
	run_command("cd ../../work/; ./run.mgn2mech.v3.single.csh")
	
	// Extract outputs from NETCDF files
	output_file := "../../Output/MEGANv3.tceq_12km.J4.CB6X.2013145.single.nc"
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
	
	return mgn2mech_output{ISOP, TERP, PAR, XYL, OLE, NR, MEOH, CH4, NH3, NO, ALD2, ETOH, FORM, ALDX, TOL, IOLE, CO, ETHA, ETH, ETHY, PRPA, BENZ, ACET, KET, AACD, FACD, HCN, ISPD, N2O, SESQ, TRS, CH3BR, CH3CL, CH3I, ISP, TRP, XYLA, SQT, TOLA}
}

func test_mgn2mech(EPSILON float64) bool {	
	go_output := run_go_mgn2mech()
	standalone_output := run_standalone_mgn2mech()

	ISOP_equal  := arrays_approximately_equal(go_output.ISOP, standalone_output.ISOP, EPSILON, "ISOP")
	TERP_equal  := arrays_approximately_equal(go_output.TERP, standalone_output.TERP, EPSILON, "TERP")
	PAR_equal   := arrays_approximately_equal(go_output.PAR, standalone_output.PAR, EPSILON, "PAR")
	XYL_equal   := arrays_approximately_equal(go_output.XYL, standalone_output.XYL, EPSILON, "XYL")
	OLE_equal   := arrays_approximately_equal(go_output.OLE, standalone_output.OLE, EPSILON, "OLE")
	NR_equal    := arrays_approximately_equal(go_output.NR, standalone_output.NR, EPSILON, "NR")
	MEOH_equal  := arrays_approximately_equal(go_output.MEOH, standalone_output.MEOH, EPSILON, "MEOH")
	CH4_equal   := arrays_approximately_equal(go_output.CH4, standalone_output.CH4, EPSILON, "CH4")
	NH3_equal   := arrays_approximately_equal(go_output.NH3, standalone_output.NH3, EPSILON, "NH3")
	NO_equal    := arrays_approximately_equal(go_output.NO, standalone_output.NO, EPSILON, "NO")
	ALD2_equal  := arrays_approximately_equal(go_output.ALD2, standalone_output.ALD2, EPSILON, "ALD2")
	ETOH_equal  := arrays_approximately_equal(go_output.ETOH, standalone_output.ETOH, EPSILON, "ETOH")
	FORM_equal  := arrays_approximately_equal(go_output.FORM, standalone_output.FORM, EPSILON, "FORM")
	ALDX_equal  := arrays_approximately_equal(go_output.ALDX, standalone_output.ALDX, EPSILON, "ALDX")
	TOL_equal   := arrays_approximately_equal(go_output.TOL, standalone_output.TOL, EPSILON, "TOL")
	IOLE_equal  := arrays_approximately_equal(go_output.IOLE, standalone_output.IOLE, EPSILON, "IOLE")
	CO_equal    := arrays_approximately_equal(go_output.CO, standalone_output.CO, EPSILON, "CO")
	ETHA_equal  := arrays_approximately_equal(go_output.ETHA, standalone_output.ETHA, EPSILON, "ETHA")
	ETH_equal   := arrays_approximately_equal(go_output.ETH, standalone_output.ETH, EPSILON, "ETH")
	ETHY_equal  := arrays_approximately_equal(go_output.ETHY, standalone_output.ETHY, EPSILON, "ETHY")
	PRPA_equal  := arrays_approximately_equal(go_output.PRPA, standalone_output.PRPA, EPSILON, "PRPA")
	BENZ_equal  := arrays_approximately_equal(go_output.BENZ, standalone_output.BENZ, EPSILON, "BENZ")
	ACET_equal  := arrays_approximately_equal(go_output.ACET, standalone_output.ACET, EPSILON, "ACET")
	KET_equal   := arrays_approximately_equal(go_output.KET, standalone_output.KET, EPSILON, "KET")
	AACD_equal  := arrays_approximately_equal(go_output.AACD, standalone_output.AACD, EPSILON, "AACD")
	FACD_equal  := arrays_approximately_equal(go_output.FACD, standalone_output.FACD, EPSILON, "FACD")
	HCN_equal   := arrays_approximately_equal(go_output.HCN, standalone_output.HCN, EPSILON, "HCN")
	ISPD_equal  := arrays_approximately_equal(go_output.ISPD, standalone_output.ISPD, EPSILON, "ISPD")
	N2O_equal   := arrays_approximately_equal(go_output.N2O, standalone_output.N2O, EPSILON, "N2O")
	SESQ_equal  := arrays_approximately_equal(go_output.SESQ, standalone_output.SESQ, EPSILON, "SESQ")
	TRS_equal   := arrays_approximately_equal(go_output.TRS, standalone_output.TRS, EPSILON, "TRS")
	CH3BR_equal := arrays_approximately_equal(go_output.CH3BR, standalone_output.CH3BR, EPSILON, "CH3BR")
	CH3CL_equal := arrays_approximately_equal(go_output.CH3CL, standalone_output.CH3CL, EPSILON, "CH3CL")
	CH3I_equal  := arrays_approximately_equal(go_output.CH3I, standalone_output.CH3I, EPSILON, "CH3I")
	ISP_equal   := arrays_approximately_equal(go_output.ISP, standalone_output.ISP, EPSILON, "ISP")
	TRP_equal   := arrays_approximately_equal(go_output.TRP, standalone_output.TRP, EPSILON, "TRP")
	XYLA_equal  := arrays_approximately_equal(go_output.XYLA, standalone_output.XYLA, EPSILON, "XYLA")
	SQT_equal   := arrays_approximately_equal(go_output.SQT, standalone_output.SQT, EPSILON, "SQT")
	TOLA_equal  := arrays_approximately_equal(go_output.TOLA, standalone_output.TOLA, EPSILON, "TOLA")

	return ISOP_equal && TERP_equal && PAR_equal && XYL_equal && OLE_equal && NR_equal && MEOH_equal && CH4_equal && NH3_equal && NO_equal && ALD2_equal && ETOH_equal && FORM_equal && ALDX_equal && TOL_equal && IOLE_equal && CO_equal && ETHA_equal && ETH_equal && ETHY_equal && PRPA_equal && BENZ_equal && ACET_equal && KET_equal && AACD_equal && FACD_equal && HCN_equal && ISPD_equal && N2O_equal && SESQ_equal && TRS_equal && CH3BR_equal && CH3CL_equal && CH3I_equal && ISP_equal && TRP_equal && XYLA_equal && SQT_equal && TOLA_equal
}