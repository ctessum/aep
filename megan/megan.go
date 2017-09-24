package megan

/* 
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h> 

#cgo LDFLAGS: -L ./fortran -lmegan -lm -lgfortran

void run_megsea_c(int* SDATE, int* STIME, int* MXREC, int* NCOLS, int* NROWS, int* TSTEP, float* TEMP, float* PRECADJ, float* CTF, float* LAIc, float* LAT, float* SOILM, float* SOILT, float* RSTYP, _Bool* LSOIL, float* GAMNO, float* GAMSM);
void run_megcan_c(int* SDATE, int* STIME, int* MXREC, int* NCOLS, int* NROWS, int* TSTEP, int* Layers, float* LAT, float* LONG, float* LAIc, float* TEMP, float* PPFD, float* WIND, float* PRES, float* QV, float* CTF, float* SunleafTK, float* ShadeleafTK, float* SunPPFD, float* ShadePPFD, float* SunFrac);
void run_megvea_c(int* SDATE, int* STIME, int* MXREC, int* NCOLS, int* NROWS, int* TSTEP, int* NEMIS, int* Layers, int* N_MaxT, int* N_MinT, int* N_MaxWS, _Bool* GAMBD_YN, _Bool* GAMAQ_YN, _Bool* GAMCO2_YN, _Bool* GAMHW_YN, _Bool* GAMHT_YN, _Bool* GAMLT_YN, _Bool* GAMSM_YN, float* GAMSM, float* AQI, float* LDFMAP, float* LAIp, float* LAIc, float* SunT, float* ShaT, float* SunP, float* ShaP, float* SunF, float* Max_temp, float* Max_wind, float* Min_temp, float* D_TEMP, float* D_PPFD, float* ISOP, float* MBO, float* MT_PINE, float* MT_ACYC, float* MT_CAMP, float* MT_SABI, float* MT_AROM, float* MT_OXY, float* SQT_HR, float* SQT_LR, float* MEOH, float* ACTO, float* ETOH, float* ACID, float* LVOC, float* OXPROD, float* STRESS, float* OTHER, float* CO, float* NO);
void run_mgn2mech_c(int* SDATE, int* STIME, int* MXREC, int* NCOLS, int* NROWS, int* TSTEP, int* NVAR, _Bool* CONVERSION, _Bool* TONPHR, char* MECHANISM, int* MECHANISM_len, float* garea, float* ISOP_in, float* MBO_in, float* MT_PINE_in, float* MT_ACYC_in, float* MT_CAMP_in, float* MT_SABI_in, float* MT_AROM_in, float* MT_OXY_in, float* SQT_HR_in, float* SQT_LR_in, float* MEOH_in, float* ACTO_in, float* ETOH_in, float* ACID_in, float* LVOC_in, float* OXPROD_in, float* STRESS_in, float* OTHER_in, float* CO_in, float* NO_in, float* EF, float* GAMNO, float* ISOP, float* TERP, float* PAR, float* XYL, float* OLE, float* NR, float* MEOH, float* CH4, float* NH3, float* NO, float* ALD2, float* ETOH, float* FORM, float* ALDX, float* TOL, float* IOLE, float* CO, float* ETHA, float* ETH, float* ETHY, float* PRPA, float* BENZ, float* ACET, float* KET, float* AACD, float* FACD, float* HCN, float* ISPD, float* N2O, float* SESQ, float* TRS, float* CH3BR, float* CH3CL, float* CH3I, float* ISP, float* TRP, float* XYLA, float* SQT, float* TOLA );

*/
import "C"
import "unsafe"
import "errors"
import "fmt"

/*
 n = number of time steps (1st dimension)
*/
type Megsea_output struct {
	NOEmissionActivity 		[]float64   // Final NO emission activity
	SoilMoistureActivity	[]float64   // Soil moisture activity for isoprene
}

/*
 n = number of time steps (1st dimension)
 m = number of canopy layers (2nd dimension)
*/
type Megcan_output struct {
	SunleafTK 	[][]float64	  // Leaf temperature for sun leaves [K] (weighted by canopy type)
	ShadeleafTK [][]float64   // Leaf temperature for shade leaves [K] (weighted by canopy type)
	SunPPFD 	[][]float64   // PPFD on a sun leaf [umol/m2/s] (weighted by canopy type)
	ShadePPFD 	[][]float64   // PPFD on a shade leaf [umol/m2/s] (weighted by canopy type)
	SunFrac 	[][]float64   // Fraction of sun leaves (weighted by canopy type)
}

type Megvea_output struct {
	ISOP 	[]float64  	// isoprene
	MBO 	[]float64  	// MBO
	MT_PINE []float64  	// monoterpenes: pines (alpha and beta)
	MT_ACYC []float64  	// monoterpenes: acyclic, 3 = (e.g., myrcene, ocimenes)
	MT_CAMP []float64  	// monoterpenes: carene, camphene, others
	MT_SABI []float64  	// monoterpenes: sabinene, limonene, terpinenes, others
	MT_AROM []float64  	// C10 aromatic: cymenes, cymenenes
	MT_OXY 	[]float64  	// C8-C13 oxygenated (e.g., camphor)
	SQT_HR 	[]float64  	// Highly reactive SQT (e.g., caryophyllene)
	SQT_LR 	[]float64  	// less reactive SQT  (e.g., longifolene, copaene) and salates
	MEOH 	[]float64  	// methanol
	ACTO 	[]float64  	// acetone
	ETOH 	[]float64  	// acetaldehyde and ethanol
	ACID 	[]float64  	// organic acids: formic acid, acetic acid, pyruvic acid
	LVOC 	[]float64  	// C2 to C4 HC (e.g., ethene, ethane)
	OXPROD 	[]float64  	// oxidation products: aldehydes 
	STRESS 	[]float64  	// Stress compounds (e.g., linalool)
	OTHER 	[]float64  	// other VOC (e.g., indole, pentane, methyl bromide)
	CO 		[]float64  	// carbon monoxide
	NO 		[]float64  	// Nitric oxide
}

type Mgn2mech_output struct {
	ISOP  []float64
	TERP  []float64
	PAR   []float64
	XYL   []float64
	OLE   []float64
	NR    []float64
	MEOH  []float64
	CH4   []float64
	NH3   []float64
	NO    []float64
	ALD2  []float64
	ETOH  []float64
	FORM  []float64
	ALDX  []float64
	TOL   []float64
	IOLE  []float64
	CO    []float64
	ETHA  []float64
	ETH   []float64
	ETHY  []float64
	PRPA  []float64
	BENZ  []float64
	ACET  []float64
	KET   []float64
	AACD  []float64
	FACD  []float64
	HCN   []float64
	ISPD  []float64
	N2O   []float64
	SESQ  []float64
	TRS   []float64
	CH3BR []float64
	CH3CL []float64
	CH3I  []float64
	ISP   []float64
	TRP   []float64
	XYLA  []float64
	SQT   []float64
	TOLA  []float64
}
 

func CFloat_to_Float64(in []C.float) []float64 {
	out := make([]float64, len(in))
	for i := range in {
		out[i] = float64(in[i])
	}
	return out
}

func Float64_to_CFloat(in []float64) []C.float {
	out := make([]C.float, len(in))
	for i := range in {
		out[i] = C.float(in[i])
	}
	return out
}

func Convert1Dto2D_Cfloat(in []C.float, n int, m int) [][]float64 {
	return Convert1Dto2D(CFloat_to_Float64(in), n, m)
}

func Convert1Dto2D(in []float64, n int, m int) [][]float64 {
	out := make([][]float64, n)
    for i := range out {
        out[i] = make([]float64, m)
		copy(out[i], in[i*m : (i+1)*m])
    }
	return out
}

func Convert2Dto1D_Cfloat(in [][]float64, ) []C.float {
	return Float64_to_CFloat(Convert2Dto1D(in))
}

func Convert2Dto1D(in [][]float64) []float64 {
	n := len(in)
	m := len(in[0])
	out := make([]float64, n * m)
    for i := range in {
		copy(out[i*m : (i+1)*m], in[i])
    }
	return out
}

func allEquals(values []int) bool {
    for i := 1; i < len(values); i++ {
        if values[i] != values[0] {
            return false
        }
    }
    return true
}

/* Computes isoprene soil moisture activity and soil NO emission 
   activity factor using MCIP output variables.
*/
func SoilMoistureAndNOEmissionActivityFactors(
	start_date int, // Start date (YYYYDDD) 
	start_time int, // Start time (HHMMSS)
	time_increment int, // Time increment (HHMMSS)
	use_PX_version_of_MCIP bool, // true: using PX version of MCIP (cf. soilnox.F)
	temperature []float64, // Temperautre (K) per timestep
	soil_moisture []float64, // Soil moisture (M**3/M**3) per timestep
	soil_temperature []float64, // Soil temperature (K) per timestep
	precipitation_adjustment []float64, // Precip adjustment per timestep
	leaf_area_index []float64, // Leaf area index [m2 per m2 ground area] per timestep
	lattitude []float64, // Lattitude per timestep
	soil_type []float64, // Soil type per timestep (between 1 and NRTYP, cf. MEGSEA.EXT)
	canopy_type_factor []float64, // Canopy type factor
) (output Megsea_output, err error) {
	var (
		// Date and time parameters used to compute the day of the growing season
		SDATE C.int = C.int(start_date) 
		STIME C.int = C.int(start_time) 
		TSTEP C.int = C.int(time_increment) 
		
		// Input/Output dimensions
		NROWS C.int = 1 // Number of rows (HARDCODED)
		NCOLS C.int = 1 // Number of columns (HARDCODED)
		MXREC C.int = C.int(len(temperature)) // Total number of timesteps
		
		LSOIL C.bool = C.bool(use_PX_version_of_MCIP) 

		// Calculated values
		output_size int = int(NROWS * NCOLS * MXREC)
	)	
	
	// Check that all input slices have the same length
	if !allEquals([]int{len(temperature), len(soil_moisture), len(soil_temperature), len(precipitation_adjustment), len(leaf_area_index), len(lattitude), len(soil_type)}) {
		return Megsea_output{}, errors.New("All input slices must have the same length")
	}	
	
	TEMP := Float64_to_CFloat(temperature)
	SOILM := Float64_to_CFloat(soil_moisture)	
	SOILT := Float64_to_CFloat(soil_temperature)	
	PRECADJ := Float64_to_CFloat(precipitation_adjustment)	
	LAIc := Float64_to_CFloat(leaf_area_index)	
	LAT := Float64_to_CFloat(lattitude)	
	RSTYP := Float64_to_CFloat(soil_type)
	CTF := Float64_to_CFloat(canopy_type_factor)
	
	var GAMNO, GAMSM []C.float	
	GAMNO = make([]C.float, output_size) // Final NO emission activity
	GAMSM = make([]C.float, output_size) // Soil moisture activity for isoprene

	// Call FORTRAN
    C.run_megsea_c(&SDATE, &STIME, &MXREC, &NCOLS, &NROWS, &TSTEP, &TEMP[0], &PRECADJ[0], &CTF[0], &LAIc[0], &LAT[0], &SOILM[0], &SOILT[0], &RSTYP[0], &LSOIL, &GAMNO[0], &GAMSM[0])
	
	//fmt.Println("GAMNO: %v", GAMNO)
	//fmt.Println("GAMSM: %v", GAMSM)

	return Megsea_output{CFloat_to_Float64(GAMNO), CFloat_to_Float64(GAMSM)}, nil
}

/* Computes isoprene soil moisture activity and soil NO emission 
   activity factor using MCIP output variables.
*/
func ConvertAboveCanopyMeteorologyToWithinCanopyMeteorology(
	start_date int, // Start date (YYYYDDD) 
	start_time int, // Start time (HHMMSS)
	time_increment int, // Time increment (HHMMSS)
	latitude float64, // Latitude
	longitude float64, // Longitude
	leaf_area_index []float64, // Leaf area index [m2 per m2 ground area] per timestep
	temperature []float64, // Temperautre (K) per timestep
	incoming_photosynthetic_active_radiation []float64, // Incoming photosynthetic active radiation [umol/m2/s1]
	wind_speed []float64, // Wind speed [m s-1]
	pressure []float64, // Pressure [Pa]
	water_vapor_mixing_ratio []float64, // Water vapor mixing ratio [KG/KG] (NOT CERTAIN, TO BE CONFIRMED, QV variable in MEGCAN)
	canopy_type_factor []float64, // Canopy type factor
) (output Megcan_output, err error) {
	var (
		// Date and time parameters used to compute the solar angle
		SDATE C.int = C.int(start_date) 
		STIME C.int = C.int(start_time) 
		TSTEP C.int = C.int(time_increment) 
		
		// Input/Output dimensions
		NROWS C.int = 1 // Number of rows (HARDCODED)
		NCOLS C.int = 1 // Number of columns (HARDCODED)
		MXREC C.int = C.int(len(temperature)) // Total number of timesteps
			
		Layers C.int = 5 // Number of layers in canopy model (HARDCODED, defined in MEGCAN.EXT)
		
		// Calculated values
		output_size int = int(MXREC * NROWS * NCOLS * Layers)
	)
	
	// Check that all input slices have the same length
	if !allEquals([]int{len(leaf_area_index), len(temperature), len(incoming_photosynthetic_active_radiation), len(wind_speed), len(pressure), len(water_vapor_mixing_ratio)}) {
		return Megcan_output{}, errors.New("All input slices must have the same length")
	}
	
	LAT := Float64_to_CFloat([]float64{latitude})
	LONG := Float64_to_CFloat([]float64{longitude})
	LAIc := Float64_to_CFloat(leaf_area_index)
	TEMP := Float64_to_CFloat(temperature)
	PPFD := Float64_to_CFloat(incoming_photosynthetic_active_radiation)
	WIND := Float64_to_CFloat(wind_speed)
	PRES := Float64_to_CFloat(pressure)
	QV := Float64_to_CFloat(water_vapor_mixing_ratio)
	CTF := Float64_to_CFloat(canopy_type_factor)
	
	// Multipy incoming photosynthetic active radiation by 4.5 (cf. megcan.f, line 430) --> NEEDED?
	for i, _ := range PPFD {
        PPFD[i] *= C.float(4.5)
    }
	
	var SunleafTK, ShadeleafTK, SunPPFD, ShadePPFD, SunFrac []C.float
	SunleafTK = make([]C.float, output_size) 	// Leaf temperature for sun leaves [K] (weighted by canopy type)
	ShadeleafTK = make([]C.float, output_size) 	// Leaf temperature for shade leaves [K] (weighted by canopy type)
	SunPPFD = make([]C.float, output_size) 		// PPFD on a sun leaf [umol/m2/s] (weighted by canopy type)
	ShadePPFD = make([]C.float, output_size)	// PPFD on a shade leaf [umol/m2/s] (weighted by canopy type)
	SunFrac = make([]C.float, output_size) 		// Fraction of sun leaves (weighted by canopy type)
	
	// Call FORTRAN
    C.run_megcan_c(&SDATE, &STIME, &MXREC, &NCOLS, &NROWS, &TSTEP, &Layers, &LAT[0], &LONG[0], &LAIc[0], &TEMP[0], &PPFD[0], &WIND[0], &PRES[0], &QV[0], &CTF[0], &SunleafTK[0], &ShadeleafTK[0], &SunPPFD[0], &ShadePPFD[0], &SunFrac[0])
	
	//fmt.Printf("SunleafTK: %v\n", SunleafTK)
	//fmt.Printf("ShadeleafTK: %v\n", ShadeleafTK)
	//fmt.Printf("SunPPFD: %v\n", SunPPFD)
	//fmt.Printf("ShadePPFD: %v\n", ShadePPFD)
	//fmt.Printf("SunFrac: %v\n", SunFrac)	
	
	timestep_count := int(MXREC)
	canopy_layers := int(Layers)
	return Megcan_output{Convert1Dto2D_Cfloat(SunleafTK, timestep_count, canopy_layers), 
						 Convert1Dto2D_Cfloat(ShadeleafTK, timestep_count, canopy_layers), 
						 Convert1Dto2D_Cfloat(SunPPFD, timestep_count, canopy_layers), 
						 Convert1Dto2D_Cfloat(ShadePPFD, timestep_count, canopy_layers), 
						 Convert1Dto2D_Cfloat(SunFrac, timestep_count, canopy_layers)}, nil
}

/* Calculate Vegetation emission activity (EA) for each emission
   class as the product of EA for individual drivers
*/
func CalculateVariousEmissionActivityFactors(
	start_date int, // Start date (YYYYDDD) 
	start_time int, // Start time (HHMMSS)
	time_increment int, // Time increment (HHMMSS)
	use_EA_for_bidirectional_exchange_LAI_response bool, // Use Vegetation Emission Activity algorithm for bidirectional exchange LAI response
	use_EA_for_response_to_air_pollution bool, // Use Vegetation Emission Activity algorithm for response to air pollution
	use_EA_for_CO2_response bool, // Use Vegetation Emission Activity algorithm for CO2 response (only applied to isoprene)
	use_EA_for_response_to_high_wind_storms bool, // Use Vegetation Emission Activity algorithm for response to high wind storms
	use_EA_for_resposne_to_high_temperature bool, // Use Vegetation Emission Activity algorithm for resposne to high temperature
	use_EA_for_response_to_low_temperature bool, // Use Vegetation Emission Activity algorithm for response to low temperature
	use_EA_for_response_to_soil_moisture bool, // Use Vegetation Emission Activity algorithm for response to soil moisture (multiplied with LDF)
	soil_moisture_activity []float64, // Soil moisture activity for isoprene
	air_quality_index []float64, // Air quality index (i.e.W126)
	light_dependent_fraction_map []float64, // Light dependent fraction map
	previous_time_step_LAI []float64, // Previous time step LAI
	current_time_step_LAI []float64, // Current time step LAI
	sunleafTK [][]float64, // Leaf temperature for sun leaves [K] (weighted by canopy type)
	shadeleafTK [][]float64, // Leaf temperature for shade leaves [K] (weighted by canopy type)
	sunPPFD [][]float64, // PPFD on a sun leaf [umol/m2/s] (weighted by canopy type)
	shadePPFD [][]float64, // PPFD on a shade leaf [umol/m2/s] (weighted by canopy type)
	sunFrac [][]float64, // Fraction of sun leaves (weighted by canopy type)
	max_temperature []float64, // Maximum temperature of previous n days (K)
	max_wind_speed []float64, // Maximum wind speed of previous n days (m/s)
	min_temperature []float64, // Minimum temperature of previous n days (K)	
	daily_average_temperature []float64, // Daily average temperature (K)
	daily_average_PPFD []float64, // Daily average PPFD (umol/m2.s)
) (output Megvea_output, err error) {
	var (
		// Date and time parameters used to compute the solar angle
		SDATE C.int = C.int(start_date) 
		STIME C.int = C.int(start_time) 
		TSTEP C.int = C.int(time_increment) 
		
		// Input/Output dimensions
		NROWS C.int = 1 // Number of rows (HARDCODED)
		NCOLS C.int = 1 // Number of columns (HARDCODED)
		MXREC C.int = C.int(len(soil_moisture_activity)) // Total number of timesteps
		
		NEMIS C.int = 20 // Number of emission classes (HARDCODED, defined in MEGVEA.EXT)
		
		Layers C.int = C.int(len(sunleafTK[0])) // Canopy vertical layers, default is 5 (2nd dimension of sunleafTK, shadeleafTK, sunPPFD, shadePPFD, sunFrac)
		N_MaxT C.int = C.int(len(max_temperature)) // Number of past days for maximum temperature (dimension of max_temperature)
		N_MinT C.int = C.int(len(min_temperature)) // Number of past days for minimum temperature (dimension of min_temperature)
		N_MaxWS C.int = C.int(len(max_wind_speed)) // Number of past days for maximum wind speed (dimension of max_wind_speed)
		
		GAMBD_YN C.bool = C.bool(use_EA_for_bidirectional_exchange_LAI_response)
		GAMAQ_YN C.bool = C.bool(use_EA_for_response_to_air_pollution)
		GAMCO2_YN C.bool = C.bool(use_EA_for_CO2_response)
		GAMHW_YN C.bool = C.bool(use_EA_for_response_to_high_wind_storms) 
		GAMHT_YN C.bool = C.bool(use_EA_for_resposne_to_high_temperature)
		GAMLT_YN C.bool = C.bool(use_EA_for_response_to_low_temperature) 
		GAMSM_YN C.bool = C.bool(use_EA_for_response_to_soil_moisture)

		// Calculated values
		output_size int = int(MXREC * NCOLS * NROWS)
	)
	
	// Check that light_dependent_fraction_map has a length equal to NEMIS * NROWS * NCOLS
	if len(light_dependent_fraction_map) != int(NEMIS * NROWS * NCOLS) {
		return Megvea_output{}, errors.New(fmt.Sprintf("light_dependent_fraction_map length must be %v", NEMIS * NROWS * NCOLS))
	}
	// Check that air_quality_index has a length equal to NROWS * NCOLS
	if len(air_quality_index) != int(NROWS * NCOLS) {
		return Megvea_output{}, errors.New(fmt.Sprintf("air_quality_index length must be %v", NROWS * NCOLS))
	}
	// Check that all other input slices have the same length MXREC * NROWS * NCOLS
	if !allEquals([]int{len(soil_moisture_activity), len(previous_time_step_LAI), len(current_time_step_LAI), len(sunleafTK), len(shadeleafTK), len(sunPPFD), len(shadePPFD), len(sunFrac), int(MXREC * NROWS * NCOLS)}) {
		return Megvea_output{}, errors.New(fmt.Sprintf("The length of the input slices soil_moisture_activity, previous_time_step_LAI, current_time_step_LAI, sunleafTK, shadeleafTK, sunPPFD, shadePPFD and sunFrac must be %v", MXREC * NROWS * NCOLS))
	}
	
	GAMSM := Float64_to_CFloat(soil_moisture_activity)
	AQI := Float64_to_CFloat(air_quality_index)
	LDFMAP := Float64_to_CFloat(light_dependent_fraction_map)
	LAIp := Float64_to_CFloat(previous_time_step_LAI)
	LAIc := Float64_to_CFloat(current_time_step_LAI)
	
	SunT := Convert2Dto1D_Cfloat(sunleafTK)
	ShaT := Convert2Dto1D_Cfloat(shadeleafTK)
	SunP := Convert2Dto1D_Cfloat(sunPPFD)
	ShaP := Convert2Dto1D_Cfloat(shadePPFD)
	SunF := Convert2Dto1D_Cfloat(sunFrac)
	
	Max_temp := Float64_to_CFloat(max_temperature)
	Max_wind := Float64_to_CFloat(max_wind_speed)
	Min_temp := Float64_to_CFloat(min_temperature)
	D_TEMP := Float64_to_CFloat(daily_average_temperature)
	D_PPFD := Float64_to_CFloat(daily_average_PPFD)
	
	var ISOP, MBO, MT_PINE, MT_ACYC, MT_CAMP, MT_SABI, MT_AROM, MT_OXY, SQT_HR, SQT_LR, MEOH, ACTO, ETOH, ACID, LVOC, OXPROD, STRESS, OTHER, CO, NO []C.float
	ISOP = make([]C.float, output_size)   
	MBO = make([]C.float, output_size)    
	MT_PINE = make([]C.float, output_size)
	MT_ACYC = make([]C.float, output_size)
	MT_CAMP = make([]C.float, output_size)
	MT_SABI = make([]C.float, output_size)
	MT_AROM = make([]C.float, output_size)
	MT_OXY = make([]C.float, output_size) 
	SQT_HR = make([]C.float, output_size) 
	SQT_LR = make([]C.float, output_size) 
	MEOH = make([]C.float, output_size)   
	ACTO = make([]C.float, output_size)   
	ETOH = make([]C.float, output_size)   
	ACID = make([]C.float, output_size)   
	LVOC = make([]C.float, output_size)   
	OXPROD = make([]C.float, output_size) 
	STRESS = make([]C.float, output_size) 
	OTHER = make([]C.float, output_size)  
	CO = make([]C.float, output_size)     
	NO = make([]C.float, output_size)     
	
	// Call FORTRAN
    C.run_megvea_c(&SDATE, &STIME, &MXREC, &NCOLS, &NROWS, &TSTEP, &NEMIS, &Layers, &N_MaxT, &N_MinT, &N_MaxWS, &GAMBD_YN, &GAMAQ_YN, &GAMCO2_YN, &GAMHW_YN, &GAMHT_YN, &GAMLT_YN, &GAMSM_YN, &GAMSM[0], &AQI[0], &LDFMAP[0], &LAIp[0], &LAIc[0], &SunT[0], &ShaT[0], &SunP[0], &ShaP[0], &SunF[0], &Max_temp[0], &Max_wind[0], &Min_temp[0], &D_TEMP[0], &D_PPFD[0], &ISOP[0], &MBO[0], &MT_PINE[0], &MT_ACYC[0], &MT_CAMP[0], &MT_SABI[0], &MT_AROM[0], &MT_OXY[0], &SQT_HR[0], &SQT_LR[0], &MEOH[0], &ACTO[0], &ETOH[0], &ACID[0], &LVOC[0], &OXPROD[0], &STRESS[0], &OTHER[0], &CO[0], &NO[0])
	
	//fmt.Printf("NON_DIMGARMA: %v\n", NON_DIMGARMA)
	
	return Megvea_output{CFloat_to_Float64(ISOP), CFloat_to_Float64(MBO), CFloat_to_Float64(MT_PINE), CFloat_to_Float64(MT_ACYC), CFloat_to_Float64(MT_CAMP), CFloat_to_Float64(MT_SABI), CFloat_to_Float64(MT_AROM), CFloat_to_Float64(MT_OXY), CFloat_to_Float64(SQT_HR), CFloat_to_Float64(SQT_LR), CFloat_to_Float64(MEOH), CFloat_to_Float64(ACTO), CFloat_to_Float64(ETOH), CFloat_to_Float64(ACID), CFloat_to_Float64(LVOC), CFloat_to_Float64(OXPROD), CFloat_to_Float64(STRESS), CFloat_to_Float64(OTHER), CFloat_to_Float64(CO), CFloat_to_Float64(NO)}, nil
}

func RunMgn2mech() Mgn2mech_output {
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
	
	/*inper := []C.float{0,0,0,0,0,0,0,0,0,0,0,0,0,0.06770491,0.1506119,0.01119377,0,0,0,0.196123,0.1344049,0.1061472,0.1205298,0.1168384,0.2244172,0,0,0,0,0,0,0,0,0,0,0,0,0,0.06770491,0.1506119,0.01119377,0,0,0,0.196123,0.1344049,0.1061472,0.1205298,0.1168384,0.2244172,0,0,0,0,0,0,0,0,0,0,0,0,0,0.06933573,0.1202633,0.01123249,0,0,0,0.134856,0.1982512,0.2188023,0.2465584,0.2356006,0.3204671,0,0,0,0,0,0,0,0,0,0,0,0,0,0.08496049,0.1642323,0.01366621,0,0,0,0.1953614,0.2000265,0.1960313,0.219812,0.2091313,0.3202239,0,0,0,0,0,0,0,0,0,0,0,0,0,0.05284291,0.08925098,0.008574537,0,0,0,0.09848323,0.1572113,0.1770349,0.1996476,0.1909046,0.2545713,0,0,0,0,0,0,0,0,0,0,0,0,0,0.05447638,0.09219893,0.008838497,0,0,0,0.101865,0.1615902,0.1816995,0.2048965,0.1959141,0.2616284,0,0,0,0,0,0,0,0,0,0,0,0,0,0.08056435,0.1449134,0.01302163,0,0,0,0.1659329,0.217198,0.2321275,0.2612407,0.2493507,0.3501392,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03801988,0.06323272,0.006174963,0,0,0,0.06910365,0.1156103,0.1315724,0.1484378,0.1419873,0.1873812,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03663687,0.08102604,0.007240801,0,0,0,0.1088041,0.1389914,0.1483657,0.1817109,0.1929337,0.2755488,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03296247,0.07232749,0.006560703,0,0,0,0.0965313,0.1286724,0.1389317,0.170283,0.1809583,0.2555993,0,0,0,0,0,0,0,0,0,0,0,0,0,0.09560993,0.1912228,0.01457586,0,0,0,0.2235541,0.1625942,0.1300601,0.1399062,0.1269294,0.2372466,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03786996,0.06298337,0.006150613,0,0,0,0.06883116,0.1151544,0.1310535,0.1478524,0.1414274,0.1866423,0,0,0,0,0,0,0,0,0,0,0,0,0,0.05224184,0.1083723,0.008910099,0,0,0,0.136238,0.135417,0.1317498,0.1524265,0.1508592,0.2336758,0,0,0,0,0,0,0,0,0,0,0,0,0,0.06889792,0.1429243,0.01175087,0,0,0,0.1796743,0.1785914,0.1737551,0.2010241,0.198957,0.3081778,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03786996,0.06298337,0.006150613,0,0,0,0.06883116,0.1151544,0.1310535,0.1478524,0.1414274,0.1866423,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03786996,0.06298337,0.006150613,0,0,0,0.06883116,0.1151544,0.1310535,0.1478524,0.1414274,0.1866423,0,0,0,0,0,0,0,0,0,0,0,0,0,0.0847534,0.1627171,0.01363934,0,0,0,0.1928948,0.2023743,0.2003172,0.2247161,0.21388,0.3242324,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03786996,0.06298337,0.006150613,0,0,0,0.06883116,0.1151544,0.1310535,0.1478524,0.1414274,0.1866423,0,0,0,0,0,0,0,0,0,0,0,0,0,0.09411947,0.1882419,0.01434864,0,0,0,0.2200691,0.1600596,0.1280326,0.1377252,0.1249507,0.2335482,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}*/
	
	ISOP_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06770491, 0.1506119, 0.01119377, 0, 0, 0, 0.196123, 0.1344049, 0.1061472, 0.1205298, 0.1168384, 0.2244172}
	MBO_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06770491, 0.1506119, 0.01119377, 0, 0, 0, 0.196123, 0.1344049, 0.1061472, 0.1205298, 0.1168384, 0.2244172}
	MT_PINE_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06933573, 0.1202633, 0.01123249, 0, 0, 0, 0.134856, 0.1982512, 0.2188023, 0.2465584, 0.2356006, 0.3204671}
	MT_ACYC_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.08496049, 0.1642323, 0.01366621, 0, 0, 0, 0.1953614, 0.2000265, 0.1960313, 0.219812, 0.2091313, 0.3202239}
	MT_CAMP_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.05284291, 0.08925098, 0.008574537, 0, 0, 0, 0.09848323, 0.1572113, 0.1770349, 0.1996476, 0.1909046, 0.2545713}
	MT_SABI_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.05447638, 0.09219893, 0.008838497, 0, 0, 0, 0.101865, 0.1615902, 0.1816995, 0.2048965, 0.1959141, 0.2616284}
	MT_AROM_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.08056435, 0.1449134, 0.01302163, 0, 0, 0, 0.1659329, 0.217198, 0.2321275, 0.2612407, 0.2493507, 0.3501392}
	MT_OXY_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.03801988, 0.06323272, 0.006174963, 0, 0, 0, 0.06910365, 0.1156103, 0.1315724, 0.1484378, 0.1419873, 0.1873812}
	SQT_HR_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.03663687, 0.08102604, 0.007240801, 0, 0, 0, 0.1088041, 0.1389914, 0.1483657, 0.1817109, 0.1929337, 0.2755488}
	SQT_LR_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.03296247, 0.07232749, 0.006560703, 0, 0, 0, 0.0965313, 0.1286724, 0.1389317, 0.170283, 0.1809583, 0.2555993}
	MEOH_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.09560993, 0.1912228, 0.01457586, 0, 0, 0, 0.2235541, 0.1625942, 0.1300601, 0.1399062, 0.1269294, 0.2372466}
	ACTO_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.03786996, 0.06298337, 0.006150613, 0, 0, 0, 0.06883116, 0.1151544, 0.1310535, 0.1478524, 0.1414274, 0.1866423}
	ETOH_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.05224184, 0.1083723, 0.008910099, 0, 0, 0, 0.136238, 0.135417, 0.1317498, 0.1524265, 0.1508592, 0.2336758}
	ACID_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06889792, 0.1429243, 0.01175087, 0, 0, 0, 0.1796743, 0.1785914, 0.1737551, 0.2010241, 0.198957, 0.3081778}
	LVOC_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.03786996, 0.06298337, 0.006150613, 0, 0, 0, 0.06883116, 0.1151544, 0.1310535, 0.1478524, 0.1414274, 0.1866423}
	OXPROD_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.03786996, 0.06298337, 0.006150613, 0, 0, 0, 0.06883116, 0.1151544, 0.1310535, 0.1478524, 0.1414274, 0.1866423}
	STRESS_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0847534, 0.1627171, 0.01363934, 0, 0, 0, 0.1928948, 0.2023743, 0.2003172, 0.2247161, 0.21388, 0.3242324}
	OTHER_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.03786996, 0.06298337, 0.006150613, 0, 0, 0, 0.06883116, 0.1151544, 0.1310535, 0.1478524, 0.1414274, 0.1866423}
	CO_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.09411947, 0.1882419, 0.01434864, 0, 0, 0, 0.2200691, 0.1600596, 0.1280326, 0.1377252, 0.1249507, 0.2335482}
	NO_in := []C.float{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	
	
	EF := []C.float{8.769534, 0.0224195, 0.3402439, 0.01646902, 0.007909732, 0.008048705, 0.00181239, 0.0019196, 0.00901152, 0.0150626, 1.03454, 0.143244, 0.2013025, 0.060192, 0.3470387, 0.02525556, 0.03697741, 0.00553815, 0.7958, 1.06345e-05}
	GAMNO := []C.float{0.6491075, 0.6640053, 0.6189721, 0.5696779, 0.5248248, 0.5029572, 0.523435, 0.546968, 0.6322287, 0.7603613, 0.7641032, 0.699164, 0.6803498, 0.6906353, 0.6858764, 0.6965074, 0.7716522, 0.8314256, 0.8096505, 0.8013604, 0.7773128, 0.7740801, 0.8019726, 0.8279191, 0.8342465}

	/*fmt.Printf("MECHANISM: %v\n", MECHANISM)
	fmt.Printf("inper: %v\n", inper)
	fmt.Printf("EF: %v\n", EF)
	fmt.Printf("GAMNO: %v\n", GAMNO)*/
	
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
    C.run_mgn2mech_c(&SDATE, &STIME, &MXREC, &NCOLS, &NROWS, &TSTEP, &NVAR, &CONVERSION, &TONPHR, MECHANISM, &MECHANISM_len, &garea, &ISOP_in[0], &MBO_in[0], &MT_PINE_in[0], &MT_ACYC_in[0], &MT_CAMP_in[0], &MT_SABI_in[0], &MT_AROM_in[0], &MT_OXY_in[0], &SQT_HR_in[0], &SQT_LR_in[0], &MEOH_in[0], &ACTO_in[0], &ETOH_in[0], &ACID_in[0], &LVOC_in[0], &OXPROD_in[0], &STRESS_in[0], &OTHER_in[0], &CO_in[0], &NO_in[0], &EF[0], &GAMNO[0], &ISOP[0], &TERP[0], &PAR[0], &XYL[0], &OLE[0], &NR[0], &MEOH[0], &CH4[0], &NH3[0], &NO[0], &ALD2[0], &ETOH[0], &FORM[0], &ALDX[0], &TOL[0], &IOLE[0], &CO[0], &ETHA[0], &ETH[0], &ETHY[0], &PRPA[0], &BENZ[0], &ACET[0], &KET[0], &AACD[0], &FACD[0], &HCN[0], &ISPD[0], &N2O[0], &SESQ[0], &TRS[0], &CH3BR[0], &CH3CL[0], &CH3I[0], &ISP[0], &TRP[0], &XYLA[0], &SQT[0], &TOLA[0])
	
	//fmt.Printf("ISOP: %v\n", ISOP)
	
	return Mgn2mech_output{CFloat_to_Float64(ISOP), CFloat_to_Float64(TERP), CFloat_to_Float64(PAR), CFloat_to_Float64(XYL), CFloat_to_Float64(OLE), CFloat_to_Float64(NR), CFloat_to_Float64(MEOH), CFloat_to_Float64(CH4), CFloat_to_Float64(NH3), CFloat_to_Float64(NO), CFloat_to_Float64(ALD2), CFloat_to_Float64(ETOH), CFloat_to_Float64(FORM), CFloat_to_Float64(ALDX), CFloat_to_Float64(TOL), CFloat_to_Float64(IOLE), CFloat_to_Float64(CO), CFloat_to_Float64(ETHA), CFloat_to_Float64(ETH), CFloat_to_Float64(ETHY), CFloat_to_Float64(PRPA), CFloat_to_Float64(BENZ), CFloat_to_Float64(ACET), CFloat_to_Float64(KET), CFloat_to_Float64(AACD), CFloat_to_Float64(FACD), CFloat_to_Float64(HCN), CFloat_to_Float64(ISPD), CFloat_to_Float64(N2O), CFloat_to_Float64(SESQ), CFloat_to_Float64(TRS), CFloat_to_Float64(CH3BR), CFloat_to_Float64(CH3CL), CFloat_to_Float64(CH3I), CFloat_to_Float64(ISP), CFloat_to_Float64(TRP), CFloat_to_Float64(XYLA), CFloat_to_Float64(SQT), CFloat_to_Float64(TOLA)}
}