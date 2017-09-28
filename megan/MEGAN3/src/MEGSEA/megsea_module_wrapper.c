#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h> 

void run_megsea_c(int* SDATE, int* STIME, int* MXREC, int* NCOLS, int* NROWS, int* TSTEP, int* NRTYP, float* TEMP, float* PRECADJ, float* CTF, float* LAIc, float* LAT, float* SOILM, float* SOILT, float* RSTYP, _Bool* LSOIL, float* GAMNO, float* GAMSM);

main()
{
    printf("main\n");
	
	int SDATE, STIME, MXREC, NCOLS, NROWS, TSTEP, NRTYP;
	_Bool LSOIL;
	float *TEMP, *PRECADJ, *CTF, *LAIc, *LAT, *SOILM, *SOILT, *RSTYP, *GAMNO, *GAMSM;
	
	SDATE = 2013145;
	STIME = 0;
	MXREC = 25;
	NCOLS = 1;
	NROWS = 1;
	TSTEP = 10000;
	NRTYP = 6;
	LSOIL = 1;
	
	// Allocate memory
	int SIZE = NROWS * NCOLS * MXREC;
	TEMP = malloc(sizeof(float) * SIZE);
	PRECADJ = malloc(sizeof(float) * SIZE);
	CTF = malloc(sizeof(float) * NROWS * NCOLS * NRTYP);
	LAIc = malloc(sizeof(float) * SIZE);
	LAT = malloc(sizeof(float) * SIZE);
	SOILM = malloc(sizeof(float) * SIZE);
	SOILT = malloc(sizeof(float) * SIZE);
	RSTYP = malloc(sizeof(float) * SIZE);
	GAMNO = malloc(sizeof(float) * SIZE);
	GAMSM = malloc(sizeof(float) * SIZE);
	
	// Set array values
	memcpy(TEMP, (float []){ 297.0823, 297.7277, 296.1826, 294.3032, 292.3727, 291.442, 292.1451, 293.1522, 296.5544, 300.8124, 300.7801, 298.4268, 297.1981, 297.1527, 296.9184, 297.636, 301.0106, 303.6024, 301.8526, 301.2423, 300.1718, 299.744, 300.7241, 301.8956, 301.7262 }, sizeof(float) * SIZE);
	memcpy(PRECADJ, (float []){ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 }, sizeof(float) * SIZE);
	memcpy(CTF, (float []){ 0, 21.6363, 30.5448, 34.4223, 33.8522, 36.0447 }, sizeof(float) * NROWS * NCOLS * NRTYP);
	memcpy(LAIc, (float []){ 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165, 1.5165}, sizeof(float) * SIZE);
	memcpy(LAT, (float []){ 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699, 24.9699 }, sizeof(float) * SIZE);
	memcpy(SOILM, (float []){ 0.09508198, 0.09217762, 0.09338263, 0.1172753, 0.1491531, 0.1688348, 0.1636439, 0.1466109, 0.1253012, 0.122324, 0.1337502, 0.1340503, 0.164309, 0.1921154, 0.1945109, 0.1770736, 0.1356585, 0.1313382, 0.1664971, 0.1913277, 0.2086558, 0.2300829, 0.2300689, 0.2078186, 0.2377395 }, sizeof(float) * SIZE);
	memcpy(SOILT, (float []){ 303.537, 303.9827, 302.1665, 300.2706, 298.6183, 297.6171, 298.619, 299.5035, 301.3833, 307.6646, 309.4813, 305.3076, 305.4087, 305.0209, 304.9799, 307.18, 310.0703, 312.6512, 310.1442, 309.9953, 308.3934, 309.0668, 309.3318, 311.3359, 308.7086 }, sizeof(float) * SIZE);
	memcpy(RSTYP, (float []){ 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6 }, sizeof(float) * SIZE);

	// Call FORTRAN
	//printf("run_megsea_c\n");
	//printf("SDATE=%d\n",SDATE);
    run_megsea_c(&SDATE, &STIME, &MXREC, &NCOLS, &NROWS, &TSTEP, &NRTYP, TEMP, PRECADJ, CTF, LAIc, LAT, SOILM, SOILT, RSTYP, &LSOIL, GAMNO, GAMSM);
	
	/*int i;
	printf("GAMNO: ");
	for(i = 0; i < SIZE; i++) {
        printf("%f ", GAMNO[i]);
    }
    printf("\n");
	printf("GAMSM: ");
	for(i = 0; i < SIZE; i++) {
        printf("%f ", GAMSM[i]);
    }
    printf("\n");*/
	
	// Free memory
	free(TEMP);
	free(PRECADJ);
	free(CTF);
	free(LAIc);
	free(LAT);
	free(SOILM);
	free(SOILT);
	free(RSTYP);
	free(GAMNO);
	free(GAMSM);
}