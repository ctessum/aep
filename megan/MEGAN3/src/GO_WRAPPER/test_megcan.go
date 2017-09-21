package main
 
/* 
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h> 

#cgo LDFLAGS: -L ./MEGAN -lmegan -lm -lgfortran

void run_megcan_c(int* SDATE, int* STIME, int* MXREC, int* NCOLS, int* NROWS, int* TSTEP, int* NRTYP, int* Layers, float* LAT, float* LONG, float* LAIc, float* TEMP, float* PPFD, float* WIND, float* PRES, float* QV, float* CTF, float* SunleafTK, float* ShadeleafTK, float* SunPPFD, float* ShadePPFD, float* SunFrac);
*/
import "C"

type megcan_output struct {
	SunleafTK []C.float
	ShadeleafTK []C.float
	SunPPFD []C.float
	ShadePPFD []C.float
	SunFrac []C.float
}
 
func run_go_megcan() megcan_output {
	var (
		SDATE C.int = 2013145
		STIME C.int = 0
		MXREC C.int = 25
		NCOLS C.int = 1
		NROWS C.int = 1
		TSTEP C.int = 10000
		NRTYP C.int = 6
		Layers C.int = 1
		SIZE int = int(MXREC * NROWS * NCOLS * Layers)
	)
	
	LAT := []C.float{24.9699}
	LONG := []C.float{-106.4971}
	LAIc := []C.float{1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165}
	TEMP := []C.float{297.082306, 297.727692, 296.182587, 294.303192, 292.372711, 291.441986, 292.145111, 293.152191, 296.554413, 300.812408, 300.780090, 298.426788, 297.198090, 297.152710, 296.918396, 297.635986, 301.010590, 303.602386, 301.852600, 301.242310, 300.171814, 299.743988, 300.724091, 301.895599, 301.726196}	
	PPFD := []C.float{170.3816, 169.4689, 169.6857, 170.1833, 170.7162, 171.3177, 169.3093, 168.2702, 165.8572, 164.0267, 117.4694, 131.7137, 154.3903, 151.1533, 79.20123, 51.97546, 69.20062, 93.79958, 52.06078, 85.48535, 27.98305, 19.00437, 18.71786, 18.64834, 28.50706}	
	WIND := []C.float{5.094378, 4.802262, 4.926229, 5.486905, 6.240457, 6.69226, 6.650759, 6.507776, 6.637711, 6.326972, 4.112444, 3.278793, 4.672177, 5.296508, 5.2028, 4.718732, 4.439407, 5.261704, 7.043137, 8.519012, 9.130258, 9.083385, 8.90703, 8.649946, 7.982096}
	PRES := []C.float{82208.09, 82074.07, 79915.18, 77888.27, 75807.18, 74659.62, 74743.4, 74867.3, 77366.51, 80634.96, 80539.65, 78878.96, 77915.15, 77785.88, 77854.4, 79161.82, 82624.84, 84891.8, 84355.8, 84116.77, 84441.45, 84297.27, 85458.9, 86283.28, 85663.48}
	QV := []C.float{0.007762248, 0.007521978, 0.007424137, 0.007346272, 0.007168575, 0.00690266, 0.006551034, 0.006116394, 0.005739891, 0.00535665, 0.005357311, 0.005891337, 0.006285786, 0.006444469, 0.006613181, 0.006978375, 0.007246723, 0.00755674, 0.008359249, 0.008913932, 0.009405961, 0.009717779, 0.009829475, 0.009771289, 0.009766233}
	CTF := []C.float{0, 21.6363, 30.5448, 34.4223, 33.8522, 36.0447}
	
	for i, _ := range PPFD {
        PPFD[i] *= C.float(4.5)
    }
	
	var SunleafTK, ShadeleafTK, SunPPFD, ShadePPFD, SunFrac []C.float
	SunleafTK = make([]C.float, SIZE)
	ShadeleafTK = make([]C.float, SIZE)
	SunPPFD = make([]C.float, SIZE)
	ShadePPFD = make([]C.float, SIZE)
	SunFrac = make([]C.float, SIZE)
	
	// Call FORTRAN
    C.run_megcan_c(&SDATE, &STIME, &MXREC, &NCOLS, &NROWS, &TSTEP, &NRTYP, &Layers, &LAT[0], &LONG[0], &LAIc[0], &TEMP[0], &PPFD[0], &WIND[0], &PRES[0], &QV[0], &CTF[0], &SunleafTK[0], &ShadeleafTK[0], &SunPPFD[0], &ShadePPFD[0], &SunFrac[0])
	
	//fmt.Printf("SunleafTK: %v\n", SunleafTK)
	//fmt.Printf("ShadeleafTK: %v\n", ShadeleafTK)
	//fmt.Printf("SunPPFD: %v\n", SunPPFD)
	//fmt.Printf("ShadePPFD: %v\n", ShadePPFD)
	//fmt.Printf("SunFrac: %v\n", SunFrac)	
	
	return megcan_output{SunleafTK, ShadeleafTK, SunPPFD, ShadePPFD, SunFrac}
}

func run_standalone_megcan() megcan_output {
	// Run standalone MEGCAN script
	run_command("cd ../../work/; ./run.megcan.v3.single.csh")
	run_command("sleep 2")
	
	// Extract outputs from NETCDF files
	output_file := "../../Output/INT/CANMET.tceq_12km.2013145.single.nc"
	SunleafTK := parse_netcdf_file("SunleafTK", output_file)
	ShadeleafTK := parse_netcdf_file("ShadeleafTK", output_file)
	SunPPFD := parse_netcdf_file("SunPPFD", output_file)
	ShadePPFD := parse_netcdf_file("ShadePPFD", output_file)
	SunFrac := parse_netcdf_file("SunFrac", output_file)
	
	return megcan_output{SunleafTK, ShadeleafTK, SunPPFD, ShadePPFD, SunFrac}
}

func test_megcan(EPSILON float64) bool {	
	go_output := run_go_megcan()
	standalone_output := run_standalone_megcan()

	SunleafTK_equal := arrays_approximately_equal(go_output.SunleafTK, standalone_output.SunleafTK, EPSILON, "SunleafTK")
	ShadeleafTK_equal := arrays_approximately_equal(go_output.ShadeleafTK, standalone_output.ShadeleafTK, EPSILON, "ShadeleafTK")
	SunPPFD_equal := arrays_approximately_equal(go_output.SunPPFD, standalone_output.SunPPFD, EPSILON, "SunPPFD")
	ShadePPFD_equal := arrays_approximately_equal(go_output.ShadePPFD, standalone_output.ShadePPFD, EPSILON, "ShadePPFD")
	SunFrac_equal := arrays_approximately_equal(go_output.SunFrac, standalone_output.SunFrac, EPSILON, "SunFrac")
	
	return SunleafTK_equal && ShadeleafTK_equal && SunPPFD_equal && ShadePPFD_equal && SunFrac_equal
}