package main
 
/* 
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h> 

#cgo LDFLAGS: -L ./MEGAN -lmegan -lm -lgfortran

void run_megvea_c(int* SDATE, int* STIME, int* MXREC, int* NCOLS, int* NROWS, int* TSTEP, int* NEMIS, int* Layers, int* N_MaxT, int* N_MinT, int* N_MaxWS, _Bool* GAMBD_YN, _Bool* GAMAQ_YN, _Bool* GAMCO2_YN, _Bool* GAMHW_YN, _Bool* GAMHT_YN, _Bool* GAMLT_YN, _Bool* GAMSM_YN, float* GAMSM, float* AQI, float* LDFMAP, float* LAIp, float* LAIc, float* SunT, float* ShaT, float* SunP, float* ShaP, float* SunF, float* Max_temp, float* Max_wind, float* Min_temp, float* D_TEMP, float* D_PPFD, float* ISOP, float* MBO, float* MT_PINE, float* MT_ACYC, float* MT_CAMP, float* MT_SABI, float* MT_AROM, float* MT_OXY, float* SQT_HR, float* SQT_LR, float* MEOH, float* ACTO, float* ETOH, float* ACID, float* LVOC, float* OXPROD, float* STRESS, float* OTHER, float* CO, float* NO);

*/
import "C"
//import "fmt"

type megvea_output struct {
	ISOP []C.float   
	MBO []C.float    
	MT_PINE []C.float
	MT_ACYC []C.float
	MT_CAMP []C.float
	MT_SABI []C.float
	MT_AROM []C.float
	MT_OXY []C.float 
	SQT_HR []C.float 
	SQT_LR []C.float 
	MEOH []C.float   
	ACTO []C.float   
	ETOH []C.float   
	ACID []C.float   
	LVOC []C.float   
	OXPROD []C.float 
	STRESS []C.float 
	OTHER []C.float  
	CO []C.float     
	NO []C.float     	
}
 
func run_go_megvea() megvea_output {
	var (
		SDATE C.int = 2013145
		STIME C.int = 0
		MXREC C.int = 25
		NCOLS C.int = 1
		NROWS C.int = 1
		TSTEP C.int = 10000
		NEMIS C.int = 20
		Layers C.int = 1
		N_MaxT C.int = 1
		N_MinT C.int = 1
		N_MaxWS C.int = 1
		GAMBD_YN C.bool = true
		GAMAQ_YN C.bool = true
		GAMCO2_YN C.bool = true
		GAMHW_YN C.bool = true
		GAMHT_YN C.bool = true
		GAMLT_YN C.bool = true
		GAMSM_YN C.bool = true
		SIZE int = int(MXREC * NCOLS * NROWS)
	)
	
	GAMSM := []C.float{0,0,0,0,0,0,0,0,0,0,0,0,0,0.427884996,0.487772763,0.0518400222,0,0,0,0.408192724,0.841395199,1,1,0.820465028,1}
	AQI := []C.float{11.621}
	LDFMAP := []C.float{1,1,0.4445464,0.8168195,0.3,0.312323,0.6,0.2,0.4630309,0.4,1,0.2,0.8,0.8,0.2,0.2,0.8,0.2,1,0}
	LAIp := []C.float{1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469,1.6469}
	LAIc := []C.float{1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165,1.5165}
	SunT := []C.float{295.134735,298.878723,290.011017,288.210510,286.313507,285.310883,285.782288,286.480377,289.430908,293.125854,292.991791,291.126129,290.263153,298.708862,290.946136,291.447235,294.649658,297.164398,295.892944,295.891693,294.841980,294.545990,295.512268,296.566101,296.393219}
	ShaT := []C.float{292.377167,293.018463,296.240540,294.385162,292.481384,291.564545,292.256256,293.248566,296.616180,300.831940,300.689026,298.355408,297.210754,291.657776,297.478180,297.859436,301.461639,304.401489,302.153503,301.902435,300.167267,299.641510,300.614044,301.777802,301.712982}
	
	
	
	SunP := []C.float{534.6226,1169.987,0,0,0,0,0,0,0,0,0,0,0,1360.212,144.8788,99.57848,132.5539,178.8786,101.0769,164.225,54.78764,37.30027,36.69876,36.47827,54.99779}
	ShaP := []C.float{152.7413,100.3765,0,0,0,0,0,0,0,0,0,0,0,83.73295,144.8788,99.57848,132.5539,178.8786,101.0769,164.225,54.78764,37.30027,36.69876,36.47827,54.99779}
	SunF := []C.float{0.3760942,0.1114906,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.05476007,0.3350512,0.5015498,0.5927957,0.644033,0.6709146,0.6802174,0.6740549,0.65104,0.6055322,0.5242341,0.3770557}
	Max_temp := []C.float{299.7025}
	Max_wind := []C.float{6.027919}
	Min_temp := []C.float{287.4598}
	D_TEMP := []C.float{293.5855}
	D_PPFD := []C.float{739.8692}
	
	var ISOP, MBO, MT_PINE, MT_ACYC, MT_CAMP, MT_SABI, MT_AROM, MT_OXY, SQT_HR, SQT_LR, MEOH, ACTO, ETOH, ACID, LVOC, OXPROD, STRESS, OTHER, CO, NO []C.float
	ISOP = make([]C.float, SIZE)   
	MBO = make([]C.float, SIZE)    
	MT_PINE = make([]C.float, SIZE)
	MT_ACYC = make([]C.float, SIZE)
	MT_CAMP = make([]C.float, SIZE)
	MT_SABI = make([]C.float, SIZE)
	MT_AROM = make([]C.float, SIZE)
	MT_OXY = make([]C.float, SIZE) 
	SQT_HR = make([]C.float, SIZE) 
	SQT_LR = make([]C.float, SIZE) 
	MEOH = make([]C.float, SIZE)   
	ACTO = make([]C.float, SIZE)   
	ETOH = make([]C.float, SIZE)   
	ACID = make([]C.float, SIZE)   
	LVOC = make([]C.float, SIZE)   
	OXPROD = make([]C.float, SIZE) 
	STRESS = make([]C.float, SIZE) 
	OTHER = make([]C.float, SIZE)  
	CO = make([]C.float, SIZE)     
	NO = make([]C.float, SIZE)     
	
	// Call FORTRAN
    C.run_megvea_c(&SDATE, &STIME, &MXREC, &NCOLS, &NROWS, &TSTEP, &NEMIS, &Layers, &N_MaxT, &N_MinT, &N_MaxWS, &GAMBD_YN, &GAMAQ_YN, &GAMCO2_YN, &GAMHW_YN, &GAMHT_YN, &GAMLT_YN, &GAMSM_YN, &GAMSM[0], &AQI[0], &LDFMAP[0], &LAIp[0], &LAIc[0], &SunT[0], &ShaT[0], &SunP[0], &ShaP[0], &SunF[0], &Max_temp[0], &Max_wind[0], &Min_temp[0], &D_TEMP[0], &D_PPFD[0], &ISOP[0], &MBO[0], &MT_PINE[0], &MT_ACYC[0], &MT_CAMP[0], &MT_SABI[0], &MT_AROM[0], &MT_OXY[0], &SQT_HR[0], &SQT_LR[0], &MEOH[0], &ACTO[0], &ETOH[0], &ACID[0], &LVOC[0], &OXPROD[0], &STRESS[0], &OTHER[0], &CO[0], &NO[0])
	
	//fmt.Printf("NON_DIMGARMA: %v\n", NON_DIMGARMA)
	
	return megvea_output{ISOP, MBO, MT_PINE, MT_ACYC, MT_CAMP, MT_SABI, MT_AROM, MT_OXY, SQT_HR, SQT_LR, MEOH, ACTO, ETOH, ACID, LVOC, OXPROD, STRESS, OTHER, CO, NO}
}

func run_standalone_megvea() megvea_output {
	// Run standalone script
	run_command("cd ../../work/; ./run.megvea.v3.single.csh")
	
	// Extract outputs from NETCDF files
	output_file := "../../Output/INT/MGNERS.tceq_12km.J4.2013145.single.nc"
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
	
	return megvea_output{ISOP, MBO, MT_PINE, MT_ACYC, MT_CAMP, MT_SABI, MT_AROM, MT_OXY, SQT_HR, SQT_LR, MEOH, ACTO, ETOH, ACID, LVOC, OXPROD, STRESS, OTHER, CO, NO}
}

func test_megvea(EPSILON float64) bool {	
	go_output := run_go_megvea()
	standalone_output := run_standalone_megvea()

	ISOP_equal := arrays_approximately_equal(go_output.ISOP, standalone_output.ISOP, EPSILON, "ISOP")
	MBO_equal := arrays_approximately_equal(go_output.MBO, standalone_output.MBO, EPSILON, "MBO") 
	MT_PINE_equal := arrays_approximately_equal(go_output.MT_PINE, standalone_output.MT_PINE, EPSILON, "MT_PINE")
	MT_ACYC_equal := arrays_approximately_equal(go_output.MT_ACYC, standalone_output.MT_ACYC, EPSILON, "MT_ACYC")
	MT_CAMP_equal := arrays_approximately_equal(go_output.MT_CAMP, standalone_output.MT_CAMP, EPSILON, "MT_CAMP")
	MT_SABI_equal := arrays_approximately_equal(go_output.MT_SABI, standalone_output.MT_SABI, EPSILON, "MT_SABI")
	MT_AROM_equal := arrays_approximately_equal(go_output.MT_AROM, standalone_output.MT_AROM, EPSILON, "MT_AROM")
	MT_OXY_equal := arrays_approximately_equal(go_output.MT_OXY, standalone_output.MT_OXY, EPSILON, "MT_OXY") 
	SQT_HR_equal := arrays_approximately_equal(go_output.SQT_HR, standalone_output.SQT_HR, EPSILON, "SQT_HR") 
	SQT_LR_equal := arrays_approximately_equal(go_output.SQT_LR, standalone_output.SQT_LR, EPSILON, "SQT_LR") 
	MEOH_equal := arrays_approximately_equal(go_output.MEOH, standalone_output.MEOH, EPSILON, "MEOH")
	ACTO_equal := arrays_approximately_equal(go_output.ACTO, standalone_output.ACTO, EPSILON, "ACTO")
	ETOH_equal := arrays_approximately_equal(go_output.ETOH, standalone_output.ETOH, EPSILON, "ETOH")
	ACID_equal := arrays_approximately_equal(go_output.ACID, standalone_output.ACID, EPSILON, "ACID")
	LVOC_equal := arrays_approximately_equal(go_output.LVOC, standalone_output.LVOC, EPSILON, "LVOC")
	OXPROD_equal := arrays_approximately_equal(go_output.OXPROD, standalone_output.OXPROD, EPSILON, "OXPROD") 
	STRESS_equal := arrays_approximately_equal(go_output.STRESS, standalone_output.STRESS, EPSILON, "STRESS") 
	OTHER_equal := arrays_approximately_equal(go_output.OTHER, standalone_output.OTHER, EPSILON, "OTHER")  
	CO_equal := arrays_approximately_equal(go_output.CO, standalone_output.CO, EPSILON, "CO")  
	NO_equal := arrays_approximately_equal(go_output.NO, standalone_output.NO, EPSILON, "NO")  

	return ISOP_equal && MBO_equal && MT_PINE_equal && MT_ACYC_equal && MT_CAMP_equal && MT_SABI_equal && MT_AROM_equal && MT_OXY_equal && SQT_HR_equal && SQT_LR_equal && MEOH_equal && ACTO_equal && ETOH_equal && ACID_equal && LVOC_equal && OXPROD_equal && STRESS_equal && OTHER_equal && CO_equal && NO_equal
}