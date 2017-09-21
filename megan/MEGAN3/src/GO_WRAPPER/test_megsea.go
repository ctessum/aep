package main
 
/* 
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h> 

#cgo LDFLAGS: -L ./MEGAN -lmegan -lm -lgfortran

void run_megsea_c(int* SDATE, int* STIME, int* MXREC, int* NCOLS, int* NROWS, int* TSTEP, int* NRTYP, float* TEMP, float* PRECADJ, float* CTF, float* LAIc, float* LAT, float* SOILM, float* SOILT, float* RSTYP, _Bool* LSOIL, float* GAMNO, float* GAMSM);
*/
import "C"

type megsea_output struct {
	GAMNO []C.float
	GAMSM []C.float
}

func run_go_megsea() megsea_output {
	var (
		SDATE C.int = 2013145
		STIME C.int = 0
		MXREC C.int = 25
		NCOLS C.int = 1
		NROWS C.int = 1
		TSTEP C.int = 10000
		NRTYP C.int = 6
		LSOIL C.bool = true
		SIZE int = int(NROWS * NCOLS * MXREC)
	)
	
	TEMP := []C.float{297.0823, 297.7277, 296.1826, 294.3032, 292.3727, 291.442, 292.1451, 293.1522, 296.5544, 300.8124, 300.7801, 298.4268, 297.1981, 297.1527, 296.9184, 297.636, 301.0106, 303.6024, 301.8526, 301.2423, 300.1718, 299.744, 300.7241, 301.8956, 301.7262}
	PRECADJ := []C.float{2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}
	CTF := []C.float{0, 21.6363, 30.5448, 34.4223, 33.8522, 36.0447}
	LAIc := []C.float{1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165}
	LAT := []C.float{24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699}
	SOILM := []C.float{0.09508198, 0.09217762, 0.09338263, 0.1172753, 0.1491531, 0.1688348, 0.1636439, 0.1466109, 0.1253012, 0.122324, 0.1337502, 0.1340503, 0.164309, 0.1921154, 0.1945109, 0.1770736, 0.1356585, 0.1313382, 0.1664971, 0.1913277, 0.2086558, 0.2300829, 0.2300689, 0.2078186, 0.2377395}
	SOILT := []C.float{303.537, 303.9827, 302.1665, 300.2706, 298.6183, 297.6171, 298.619, 299.5035, 301.3833, 307.6646, 309.4813, 305.3076, 305.4087, 305.0209, 304.9799, 307.18, 310.0703, 312.6512, 310.1442, 309.9953, 308.3934, 309.0668, 309.3318, 311.3359, 308.7086}
	RSTYP := []C.float{6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}
	
	var GAMNO, GAMSM []C.float
	GAMNO = make([]C.float, SIZE)
	GAMSM = make([]C.float, SIZE)

	// Call FORTRAN
    C.run_megsea_c(&SDATE, &STIME, &MXREC, &NCOLS, &NROWS, &TSTEP, &NRTYP, &TEMP[0], &PRECADJ[0], &CTF[0], &LAIc[0], &LAT[0], &SOILM[0], &SOILT[0], &RSTYP[0], &LSOIL, &GAMNO[0], &GAMSM[0])
	
	//fmt.Println("GAMNO: %v", GAMNO)
	//fmt.Println("GAMSM: %v", GAMSM)
	
	return megsea_output{GAMNO, GAMSM}
}

func run_standalone_megsea() megsea_output {
	// Run standalone MEGSEA script
	run_command("cd ../../work/; ./run.megsea.v3.single.csh")
	
	// Extract outputs from NETCDF files
	output_file := "../../Output/INT/MGNSEA.tceq_12km.2013145.single.nc"
	GAMNO := parse_netcdf_file("GAMNO", output_file)
	GAMSM := parse_netcdf_file("GAMSM", output_file)
	
	return megsea_output{GAMNO, GAMSM}
}

func test_megsea(EPSILON float64) bool {	
	go_output := run_go_megsea()
	standalone_output := run_standalone_megsea()

	GAMNO_equal := arrays_approximately_equal(go_output.GAMNO, standalone_output.GAMNO, EPSILON, "GAMNO")
	GAMSM_equal := arrays_approximately_equal(go_output.GAMSM, standalone_output.GAMSM, EPSILON, "GAMSM")

	return GAMNO_equal && GAMSM_equal
}