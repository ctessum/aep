/*
Copyright (C) 2017 the AEP authors.
This file is part of AEP.
AEP is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
AEP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with AEP.  If not, see <http://www.gnu.org/licenses/>.
*/

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
void run_mgn2mech_c(int* SDATE, int* STIME, int* MXREC, int* NCOLS, int* NROWS, int* TSTEP, int* NVAR, _Bool* CONVERSION, _Bool* TONPHR, char* MECHANISM, int* MECHANISM_len, float* garea, float* ISOP_in, float* MBO_in, float* MT_PINE_in, float* MT_ACYC_in, float* MT_CAMP_in, float* MT_SABI_in, float* MT_AROM_in, float* MT_OXY_in, float* SQT_HR_in, float* SQT_LR_in, float* MEOH_in, float* ACTO_in, float* ETOH_in, float* ACID_in, float* LVOC_in, float* OXPROD_in, float* STRESS_in, float* OTHER_in, float* CO_in, float* NO_in, float* EF, float* GAMNO, float* outer);

*/
import "C"
import "unsafe"
import "errors"
import "fmt"
import "reflect"

/*
 n = number of time steps (1st dimension)
*/
type SoilMoistureAndNOEmissionActivity struct { // MEGSEA output
	NOEmissionActivity 		float64   // Final NO emission activity
	SoilMoistureActivity	float64   // Soil moisture activity for isoprene
}

/*
 n = number of time steps (1st dimension)
 m = number of canopy layers (2nd dimension)
*/
type WithinCanopyMeteorology struct { // MEGCAN output
	SunleafTK 	[]float64	  // Leaf temperature for sun leaves [K] (weighted by canopy type)
	ShadeleafTK []float64   // Leaf temperature for shade leaves [K] (weighted by canopy type)
	SunPPFD 	[]float64   // PPFD on a sun leaf [umol/m2/s] (weighted by canopy type)
	ShadePPFD 	[]float64   // PPFD on a shade leaf [umol/m2/s] (weighted by canopy type)
	SunFrac 	[]float64   // Fraction of sun leaves (weighted by canopy type)
}

/*
 Emission activity for each of the 20 emission types
 n = number of time steps (1st dimension)
*/
type EmissionActivityPerEmissionType struct { // MEGVEA output
	ISOP 	float64  	// isoprene
	MBO 	float64  	// MBO
	MT_PINE float64  	// monoterpenes: pines (alpha and beta)
	MT_ACYC float64  	// monoterpenes: acyclic, 3 = (e.g., myrcene, ocimenes)
	MT_CAMP float64  	// monoterpenes: carene, camphene, others
	MT_SABI float64  	// monoterpenes: sabinene, limonene, terpinenes, others
	MT_AROM float64  	// C10 aromatic: cymenes, cymenenes
	MT_OXY 	float64  	// C8-C13 oxygenated (e.g., camphor)
	SQT_HR 	float64  	// Highly reactive SQT (e.g., caryophyllene)
	SQT_LR 	float64  	// less reactive SQT  (e.g., longifolene, copaene) and salates
	MEOH 	float64  	// methanol
	ACTO 	float64  	// acetone
	ETOH 	float64  	// acetaldehyde and ethanol
	ACID 	float64  	// organic acids: formic acid, acetic acid, pyruvic acid
	LVOC 	float64  	// C2 to C4 HC (e.g., ethene, ethane)
	OXPROD 	float64  	// oxidation products: aldehydes 
	STRESS 	float64  	// Stress compounds (e.g., linalool)
	OTHER 	float64  	// other VOC (e.g., indole, pentane, methyl bromide)
	CO 		float64  	// carbon monoxide
	NO 		float64  	// Nitric oxide
}

/* Computes isoprene soil moisture activity and soil NO emission 
   activity factor using MCIP output variables.
*/
func SoilMoistureAndNOEmissionActivityFactors( // MEGSEA
	date int, // Start date (YYYYDDD) 
	time int, // Start time (HHMMSS)
	use_PX_version_of_MCIP bool, // true: using PX version of MCIP (cf. soilnox.F)
	temperature float64, // Temperautre (K) per timestep
	soil_moisture float64, // Soil moisture (M**3/M**3) per timestep
	soil_temperature float64, // Soil temperature (K) per timestep
	precipitation_adjustment float64, // Precip adjustment per timestep
	leaf_area_index float64, // Leaf area index [m2 per m2 ground area] per timestep
	lattitude float64, // Lattitude per timestep
	soil_type float64, // Soil type per timestep (between 1 and NRTYP, cf. MEGSEA.EXT)
	canopy_type_factor []float64, // Canopy type factor
) (output SoilMoistureAndNOEmissionActivity, err error) {
	var (
		// Date and time parameters used to compute the day of the growing season
		SDATE C.int = C.int(date) 
		STIME C.int = C.int(time) 
		TSTEP C.int = C.int(0) // Time increment (HHMMSS) (HARDCODED)
		
		// Input/Output dimensions
		NROWS C.int = 1 // Number of rows (HARDCODED)
		NCOLS C.int = 1 // Number of columns (HARDCODED)
		MXREC C.int = 1 // Total number of timesteps (HARDCODED)
		
		LSOIL C.bool = C.bool(use_PX_version_of_MCIP) 

		// Calculated values
		output_size int = int(NROWS * NCOLS * MXREC)
	)	
	
	TEMP := Float64_to_CFloat(temperature)
	SOILM := Float64_to_CFloat(soil_moisture)	
	SOILT := Float64_to_CFloat(soil_temperature)	
	PRECADJ := Float64_to_CFloat(precipitation_adjustment)	
	LAIc := Float64_to_CFloat(leaf_area_index)	
	LAT := Float64_to_CFloat(lattitude)	
	RSTYP := Float64_to_CFloat(soil_type)
	CTF := Float64_to_CFloat_array(canopy_type_factor)
	
	var GAMNO, GAMSM []C.float	
	GAMNO = make([]C.float, output_size) // Final NO emission activity
	GAMSM = make([]C.float, output_size) // Soil moisture activity for isoprene

	// Call FORTRAN
    C.run_megsea_c(&SDATE, &STIME, &MXREC, &NCOLS, &NROWS, &TSTEP, &TEMP[0], &PRECADJ[0], &CTF[0], &LAIc[0], &LAT[0], &SOILM[0], &SOILT[0], &RSTYP[0], &LSOIL, &GAMNO[0], &GAMSM[0])
	
	//fmt.Println("GAMNO: %v", GAMNO)
	//fmt.Println("GAMSM: %v", GAMSM)

	return SoilMoistureAndNOEmissionActivity{CFloat_to_Float64(GAMNO), CFloat_to_Float64(GAMSM)}, nil
}

/* Computes isoprene soil moisture activity and soil NO emission 
   activity factor using MCIP output variables.
*/
func ConvertAboveCanopyMeteorologyToWithinCanopyMeteorology( // MEGCAN
	date int, // Start date (YYYYDDD) 
	time int, // Start time (HHMMSS)
	latitude float64, // Latitude
	longitude float64, // Longitude
	leaf_area_index float64, // Leaf area index [m2 per m2 ground area] per timestep
	temperature float64, // Temperautre (K) per timestep
	incoming_photosynthetic_active_radiation float64, // Incoming photosynthetic active radiation [umol/m2/s1]
	wind_speed float64, // Wind speed [m s-1]
	pressure float64, // Pressure [Pa]
	water_vapor_mixing_ratio float64, // Water vapor mixing ratio [KG/KG] (NOT CERTAIN, TO BE CONFIRMED, QV variable in MEGCAN)
	canopy_type_factor []float64, // Canopy type factor
) (output WithinCanopyMeteorology, err error) {
	var (
		// Date and time parameters used to compute the solar angle
		SDATE C.int = C.int(date) 
		STIME C.int = C.int(time) 
		TSTEP C.int = C.int(0) // Time increment (HHMMSS) (HARDCODED)
		
		// Input/Output dimensions
		NROWS C.int = 1 // Number of rows (HARDCODED)
		NCOLS C.int = 1 // Number of columns (HARDCODED)
		MXREC C.int = 1 // Total number of timesteps (HARDCODED)
			
		Layers C.int = 5 // Number of layers in canopy model (HARDCODED, defined in MEGCAN.EXT)
		
		// Calculated values
		output_size int = int(MXREC * NROWS * NCOLS * Layers)
	)
	
	LAT := Float64_to_CFloat(latitude)
	LONG := Float64_to_CFloat(longitude)
	LAIc := Float64_to_CFloat(leaf_area_index)
	TEMP := Float64_to_CFloat(temperature)
	PPFD := Float64_to_CFloat(incoming_photosynthetic_active_radiation)
	WIND := Float64_to_CFloat(wind_speed)
	PRES := Float64_to_CFloat(pressure)
	QV := Float64_to_CFloat(water_vapor_mixing_ratio)
	CTF := Float64_to_CFloat_array(canopy_type_factor)
	
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
	
	return WithinCanopyMeteorology{CFloat_to_Float64_array(SunleafTK), 
								   CFloat_to_Float64_array(ShadeleafTK), 
								   CFloat_to_Float64_array(SunPPFD), 
								   CFloat_to_Float64_array(ShadePPFD), 
								   CFloat_to_Float64_array(SunFrac)}, nil
}

/* Calculate Vegetation emission activity (EA) for each emission
   class as the product of EA for individual drivers
*/
func CalculateVariousEmissionActivityFactors( // MEGVEA
	date int, // Start date (YYYYDDD) 
	time int, // Start time (HHMMSS)
	use_EA_for_bidirectional_exchange_LAI_response bool, // Use Vegetation Emission Activity algorithm for bidirectional exchange LAI response
	use_EA_for_response_to_air_pollution bool, // Use Vegetation Emission Activity algorithm for response to air pollution
	use_EA_for_CO2_response bool, // Use Vegetation Emission Activity algorithm for CO2 response (only applied to isoprene)
	use_EA_for_response_to_high_wind_storms bool, // Use Vegetation Emission Activity algorithm for response to high wind storms
	use_EA_for_resposne_to_high_temperature bool, // Use Vegetation Emission Activity algorithm for resposne to high temperature
	use_EA_for_response_to_low_temperature bool, // Use Vegetation Emission Activity algorithm for response to low temperature
	use_EA_for_response_to_soil_moisture bool, // Use Vegetation Emission Activity algorithm for response to soil moisture (multiplied with LDF)
	soil_moisture_activity float64, // Soil moisture activity for isoprene
	air_quality_index float64, // Air quality index (i.e.W126)
	light_dependent_fraction_map []float64, // Light dependent fraction map
	previous_time_step_LAI float64, // Previous time step LAI
	current_time_step_LAI float64, // Current time step LAI
	sunleafTK []float64, // Leaf temperature for sun leaves [K] (weighted by canopy type)
	shadeleafTK []float64, // Leaf temperature for shade leaves [K] (weighted by canopy type)
	sunPPFD []float64, // PPFD on a sun leaf [umol/m2/s] (weighted by canopy type)
	shadePPFD []float64, // PPFD on a shade leaf [umol/m2/s] (weighted by canopy type)
	sunFrac []float64, // Fraction of sun leaves (weighted by canopy type)
	max_temperature []float64, // Maximum temperature of previous n days (K)
	max_wind_speed []float64, // Maximum wind speed of previous n days (m/s)
	min_temperature []float64, // Minimum temperature of previous n days (K)	
	daily_average_temperature float64, // Daily average temperature (K)
	daily_average_PPFD float64, // Daily average PPFD (umol/m2.s)
) (output EmissionActivityPerEmissionType, err error) {
	var (
		// Date and time parameters used to compute the solar angle
		SDATE C.int = C.int(date) 
		STIME C.int = C.int(time) 
		TSTEP C.int = C.int(0) // Time increment (HHMMSS) (HARDCODED)
		
		// Input/Output dimensions
		NROWS C.int = 1 // Number of rows (HARDCODED)
		NCOLS C.int = 1 // Number of columns (HARDCODED)
		MXREC C.int = 1 // Total number of timesteps (HARDCODED)
		
		NEMIS C.int = 20 // Number of emission classes (HARDCODED, defined in MEGVEA.EXT)
		
		Layers C.int = C.int(len(sunleafTK)) // Canopy vertical layers, default is 5 (2nd dimension of sunleafTK, shadeleafTK, sunPPFD, shadePPFD, sunFrac)
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
		return EmissionActivityPerEmissionType{}, errors.New(fmt.Sprintf("light_dependent_fraction_map length must be %v", NEMIS * NROWS * NCOLS))
	}
	
	GAMSM := Float64_to_CFloat(soil_moisture_activity)
	AQI := Float64_to_CFloat(air_quality_index)
	LDFMAP := Float64_to_CFloat_array(light_dependent_fraction_map)
	LAIp := Float64_to_CFloat(previous_time_step_LAI)
	LAIc := Float64_to_CFloat(current_time_step_LAI)
	
	SunT := Float64_to_CFloat_array(sunleafTK)
	ShaT := Float64_to_CFloat_array(shadeleafTK)
	SunP := Float64_to_CFloat_array(sunPPFD)
	ShaP := Float64_to_CFloat_array(shadePPFD)
	SunF := Float64_to_CFloat_array(sunFrac)
	
	Max_temp := Float64_to_CFloat_array(max_temperature)
	Max_wind := Float64_to_CFloat_array(max_wind_speed)
	Min_temp := Float64_to_CFloat_array(min_temperature)
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
	
	return EmissionActivityPerEmissionType{CFloat_to_Float64(ISOP), CFloat_to_Float64(MBO), CFloat_to_Float64(MT_PINE), CFloat_to_Float64(MT_ACYC), CFloat_to_Float64(MT_CAMP), CFloat_to_Float64(MT_SABI), CFloat_to_Float64(MT_AROM), CFloat_to_Float64(MT_OXY), CFloat_to_Float64(SQT_HR), CFloat_to_Float64(SQT_LR), CFloat_to_Float64(MEOH), CFloat_to_Float64(ACTO), CFloat_to_Float64(ETOH), CFloat_to_Float64(ACID), CFloat_to_Float64(LVOC), CFloat_to_Float64(OXPROD), CFloat_to_Float64(STRESS), CFloat_to_Float64(OTHER), CFloat_to_Float64(CO), CFloat_to_Float64(NO)}, nil
}

/* Chemical speciation and MECHANISM conversion.
   The output is converted from 20 to 201 species which
   are then lumped according to the MECHANISM assigned
*/
func ChemicalSpeciationAndMechanismConversion( // MGN2MECH
	date int, // Start date (YYYYDDD) 
	time int, // Start time (HHMMSS)
	use_mechanism_conversion bool, // Mechanism conversion flag
	output_in_ton_per_hr bool, // Output in tons/hr flag
	area_per_grid_cell float64, // Area per grid cell (m2)
	mechanism Mechanism, // Mechanism
	input_emission_activity EmissionActivityPerEmissionType, // Emission Activity Per Emission Type
	emission_factor []float64, // Emission factor  
	soil_moisture_and_NO_emission_activity SoilMoistureAndNOEmissionActivity, // Soil Moisture And NO Emission Activity
) (output interface{}, err error) {
	var (
		// Date and time parameters used to compute the solar angle
		SDATE C.int = C.int(date) 
		STIME C.int = C.int(time) 
		TSTEP C.int = C.int(0) // Time increment (HHMMSS) (HARDCODED)
		
		// Input/Output dimensions
		NROWS C.int = 1 // Number of rows (HARDCODED)
		NCOLS C.int = 1 // Number of columns (HARDCODED)
		MXREC C.int = 1 // Total number of timesteps (HARDCODED)
		N_MGN_SPC int = 20 // Number of emission types  (HARDCODED, cf. SPC_NOCONVER.EXT)

		mechanism_species_count = GetNumberOfMechanismSpecies(mechanism) // Number of mechanism species (cf. SPC_"MECHANISM".EXT file)
		NVAR C.int = C.int(mechanism_species_count) 
		CONVERSION C.bool = C.bool(use_mechanism_conversion) 
		TONPHR C.bool = C.bool(output_in_ton_per_hr)
		garea C.float = C.float(area_per_grid_cell)
		
		// Calculated values
		output_size int = int(MXREC * NCOLS * NROWS)
	)
	
	// Check that emission_factor has a length equal to N_MGN_SPC
	if len(emission_factor) != N_MGN_SPC {
		return Species_CB6X{}, errors.New(fmt.Sprintf("emission_factor length must be %v", N_MGN_SPC))
	}
	
	// Convert GO string to C string
	mechanism_string := fmt.Sprintf("%v", mechanism)
	MECHANISM := C.CString(mechanism_string)
	defer C.free(unsafe.Pointer(MECHANISM))
	var MECHANISM_len C.int = C.int(len(mechanism_string))
	
	var output_data []C.float
	output_data = make([]C.float, output_size * mechanism_species_count) 
	
	// Call FORTRAN
    C.run_mgn2mech_c(&SDATE, &STIME, &MXREC, &NCOLS, &NROWS, &TSTEP, &NVAR, &CONVERSION, &TONPHR, MECHANISM, &MECHANISM_len, &garea, 
	&Float64_to_CFloat(input_emission_activity.ISOP)[0], 
	&Float64_to_CFloat(input_emission_activity.MBO)[0], 
	&Float64_to_CFloat(input_emission_activity.MT_PINE)[0], 
	&Float64_to_CFloat(input_emission_activity.MT_ACYC)[0], 
	&Float64_to_CFloat(input_emission_activity.MT_CAMP)[0], 
	&Float64_to_CFloat(input_emission_activity.MT_SABI)[0], 
	&Float64_to_CFloat(input_emission_activity.MT_AROM)[0], 
	&Float64_to_CFloat(input_emission_activity.MT_OXY)[0], 
	&Float64_to_CFloat(input_emission_activity.SQT_HR)[0], 
	&Float64_to_CFloat(input_emission_activity.SQT_LR)[0], 
	&Float64_to_CFloat(input_emission_activity.MEOH)[0], 
	&Float64_to_CFloat(input_emission_activity.ACTO)[0], 
	&Float64_to_CFloat(input_emission_activity.ETOH)[0], 
	&Float64_to_CFloat(input_emission_activity.ACID)[0], 
	&Float64_to_CFloat(input_emission_activity.LVOC)[0], 
	&Float64_to_CFloat(input_emission_activity.OXPROD)[0], 
	&Float64_to_CFloat(input_emission_activity.STRESS)[0], 
	&Float64_to_CFloat(input_emission_activity.OTHER)[0], 
	&Float64_to_CFloat(input_emission_activity.CO)[0], 
	&Float64_to_CFloat(input_emission_activity.NO)[0], 
	&Float64_to_CFloat_array(emission_factor)[0], 
	&Float64_to_CFloat(soil_moisture_and_NO_emission_activity.NOEmissionActivity)[0], // GAMNO
	&output_data[0])

	species := FillSpeciesData(GetMechanismSpecies(mechanism), output_data, int(MXREC)) // Fill species data from output_data
	
	return species, nil
}

func FillSpeciesData(species interface{}, data []C.float, timestep_count int) interface{} {
	// Get pointer to species
	ptr := reflect.New(reflect.TypeOf(species))
	temp := ptr.Elem()
	temp.Set(reflect.ValueOf(species))

	// Iterate over species fields and set data
	for i := 0; i < temp.NumField(); i++ {
		f := temp.Field(i)
		species_data := CFloat_to_Float64(data[i*timestep_count : (i+1)*timestep_count]) // data for the i'th species 
		f.Set(reflect.ValueOf(species_data))
		//fmt.Printf("%d: %s %s = %v\n", i, typeOfT.Field(i).Name, f.Type(), f.Interface())
	}
	
	return temp.Interface()
}