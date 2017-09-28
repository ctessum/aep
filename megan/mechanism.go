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

type Mechanism int

// Available mechanisms
const (
	SAPRCII Mechanism = iota
	SAPRC99
	RADM2
	RACM
	CBMZ
	SAPRC99X
	SAPRC99Q
	CB05
	CB6
	SOAX
	CB6X
	RACM2
	CRIv2
	SAPRC07
)

// Extracted from SPC_"MECHANISM".EXT files
func (m Mechanism) String() string {
    switch m {
    case SAPRCII:
        return "SAPRCII"
    case SAPRC99:
        return "SAPRC99"
    case RADM2:
        return "RADM2"
    case RACM:
        return "RACM"
    case CBMZ:
        return "CBMZ"
    case SAPRC99X:
        return "SAPRC99X"
    case SAPRC99Q:
        return "SAPRC99Q"
    case CB05:
        return "CB05"
	case CB6:
        return "CB6"
	case SOAX:
        return "SOAX"
	case CB6X:
        return "CB6X"
	case RACM2:
        return "RACM2"
	case CRIv2:
        return "CRIv2"
	case SAPRC07:
        return "SAPRC07"
    }
    return "Unknown"
}

// Extracted from SPC_"MECHANISM".EXT files
func GetNumberOfMechanismSpecies(m Mechanism) int {
	switch m {
    case SAPRCII:
        return 31
    case SAPRC99:
        return 28
    case RADM2:
        return 21
    case RACM:
        return 23
    case CBMZ:
        return 37
    case SAPRC99X:
        return 27
    case SAPRC99Q:
        return 27
    case CB05:
        return 29
	case CB6:
        return 34
	case SOAX:
        return 5
	case CB6X:
        return 39
	case RACM2:
        return 38
	case CRIv2:
        return 28
	case SAPRC07:
        return 44
    }
    return -1
} 

func GetMechanismSpecies(m Mechanism) interface{} {
	switch m {
    case SAPRCII:
        return Species_SAPRCII{}
    case SAPRC99:
        return Species_SAPRC99{}
    case RADM2:
        return Species_RADM2{}
    case RACM:
        return Species_RACM{}
    case CBMZ:
        return Species_CBMZ{}
    case SAPRC99X:
        return Species_SAPRC99X{}
    case SAPRC99Q:
        return Species_SAPRC99Q{}
    case CB05:
        return Species_CB05{}
	case CB6:
        return Species_CB6{}
	case SOAX:
        return Species_SOAX{}
	case CB6X:
        return Species_CB6X{}
	case RACM2:
        return Species_RACM2{}
	case CRIv2:
        return Species_CRIv2{}
	case SAPRC07:
        return Species_SAPRC07{}
    }
    return nil
} 

/*
 n = number of time steps (1st dimension)
*/
type Species_SAPRCII struct { // cf. SPC_SAPRCII.EXT
	ISOPRENE  	float64
	TRP1  		float64
	BCARL   	float64
	AHUMUL   	float64
	SSQT   		float64
	MEOH    	float64
	ACET  		float64
	CH4   		float64
	NO   		float64
	NO2    		float64
	NH3  		float64
	CCHO  		float64
	HCOOH  		float64
	HCHO  		float64
	CCO_OH   	float64
	BALD  		float64
	MEK    		float64
	RCO_OH  	float64
	CO   		float64
	ETHENE 		float64
	ALK1  		float64
	ALK2 		float64
	ALK3 		float64
	ALK4   		float64
	ALK5  		float64
	ARO1  		float64
	ARO2   		float64
	OLE1  		float64
	OLE2   		float64
	RCHO  		float64
	NONR   		float64
}

/*
 n = number of time steps (1st dimension)
*/
type Species_SAPRC99 struct { // cf. SPC_SAPRC99.EXT
	ISOPRENE  float64
	TRP1      float64
	MEOH      float64
	ACET      float64
	CH4       float64
	NO        float64
	NO2       float64
	NH3       float64
	CCHO      float64
	HCOOH     float64
	HCHO      float64
	CCO_OH    float64
	BALD      float64
	MEK       float64
	RCO_OH    float64
	CO        float64
	ETHENE    float64
	ALK1      float64
	ALK2      float64
	ALK3      float64
	ALK4      float64
	ALK5      float64
	ARO1      float64
	ARO2      float64
	OLE1      float64
	OLE2      float64
	RCHO      float64
	NONR      float64
}

/*
 n = number of time steps (1st dimension)
*/
type Species_RADM2 struct { // cf. SPC_RADM2.EXT
	ISO   float64
	CH4   float64
	ETH   float64
	HC3   float64
	HC5   float64
	HC8   float64
	OL2   float64
	OLI   float64
	OLT   float64
	ALD   float64
	KET   float64
	TOL   float64
	HCHO  float64
	ORA1  float64
	ORA2  float64
	CO    float64
	SO2   float64
	NO    float64
	HNO3  float64
	NO2   float64
	NR    float64
}

/*
 n = number of time steps (1st dimension)
*/
type Species_RACM struct { // cf. SPC_RACM.EXT
	ISO   float64
	CH4   float64
	ETH   float64
	HC3   float64
	HC5   float64
	HC8   float64
	OL2   float64
	OLI   float64
	OLT   float64
	ALD   float64
	KET   float64
	TOL   float64
	HCHO  float64
	ORA1  float64
	ORA2  float64
	API   float64
	LIM   float64
	CO    float64
	SO2   float64
	NO    float64
	HNO3  float64
	NO2   float64
	NR    float64
}

/*
 n = number of time steps (1st dimension)
*/
type Species_CBMZ struct { // cf. SPC_CBMZ.EXT
	ISOP     float64
	NO       float64
	NO2      float64
	CO       float64
	CH3OH    float64
	ANOL     float64
	ALD2     float64
	HCHO     float64
	HCOOH    float64
	RCOOH    float64
	CH4      float64
	C2H6     float64
	PAR      float64
	ETH      float64
	OLET     float64
	OLEI     float64
	CRES     float64
	AONE     float64
	TOL      float64
	XYL      float64
	DMS      float64
	NH3      float64
	NR       float64
	OLI      float64
	TERP     float64
	SESQ     float64
	C2H5OH   float64
	KET      float64
	OL2      float64
	OLT      float64
	ISOPRD   float64
	HCN      float64
	CH3BR    float64
	CH3CL    float64
	CH3I     float64
	TRS      float64
	N2O      float64
}

/*
 n = number of time steps (1st dimension)
*/
type Species_SAPRC99X struct { // cf. SPC_SAPRC99X.EXT
	ISOP   float64
	TERP   float64
	OLE2   float64
	ARO2   float64
	ALK5   float64
	XC     float64
	OLE1   float64
	MEOH   float64
	ACET   float64
	CH4    float64
	NH3    float64
	NO     float64
	CCHO   float64
	ALK3   float64
	HC2H   float64
	HCHO   float64
	CO2H   float64
	BALD   float64
	MEK    float64
	RCHO   float64
	ALK4   float64
	ARO1   float64
	BACL   float64
	CO     float64
	ALK1   float64
	ETHE   float64
	ALK2   float64
}

/*
 n = number of time steps (1st dimension)
*/
type Species_SAPRC99Q struct { // cf. SPC_SAPRC99Q.EXT
	ISOPRENE  float64
	TRP1      float64
	OLE2      float64
	ARO2      float64
	ALK5      float64
	XC        float64
	OLE1      float64
	MEOH      float64
	ACET      float64
	CH4       float64
	NH3       float64
	NO        float64
	CCHO      float64
	ALK3      float64
	HCOOH     float64
	HCHO      float64
	CCO_OH    float64
	BALD      float64
	MEK       float64
	RCHO      float64
	ALK4      float64
	ARO1      float64
	BACL      float64
	CO        float64
	ALK1      float64
	ETHENE    float64
	ALK2      float64
}

/*
 n = number of time steps (1st dimension)
*/
type Species_CB05 struct { // cf. SPC_CB05.EXT
	ISOP    float64
	TERP    float64
	PAR     float64
	XYL     float64
	OLE     float64
	NR      float64
	MEOH    float64
	CH4     float64
	NH3     float64
	NO      float64
	ALD2    float64
	ETOH    float64
	FORM    float64
	ALDX    float64
	TOL     float64
	IOLE    float64
	CO      float64
	ETHA    float64
	ETH     float64
	AACD    float64
	FACD    float64
	HCN     float64
	ISPD    float64
	N2O     float64
	SESQ    float64
	TRS     float64
	CH3BR   float64
	CH3CL   float64
	CH3I    float64
}

/*
 n = number of time steps (1st dimension)
*/
type Species_CB6 struct { // cf. SPC_CB6.EXT
	ISOP    float64
	TERP    float64
	PAR     float64
	XYL     float64
	OLE     float64
	NR      float64
	MEOH    float64
	CH4     float64
	NH3     float64
	NO      float64
	ALD2    float64
	ETOH    float64
	FORM    float64
	ALDX    float64
	TOL     float64
	IOLE    float64
	CO      float64
	ETHA    float64
	ETH     float64
	ETHY    float64
	PRPA    float64
	BENZ    float64
	ACET    float64
	KET     float64
	AACD    float64
	FACD    float64
	HCN     float64
	ISPD    float64
	N2O     float64
	SESQ    float64
	TRS     float64
	CH3BR   float64
	CH3CL   float64
	CH3I    float64
}

/*
 n = number of time steps (1st dimension)
*/
type Species_SOAX struct { // cf. SPC_SOAX.EXT
	ISP     float64
	TRP     float64
	XYLA    float64
	SQT     float64
	TOLA    float64
}

/*
 n = number of time steps (1st dimension)
*/
type Species_CB6X struct { // cf. SPC_CB6X.EXT
	ISOP  float64
	TERP  float64
	PAR   float64
	XYL   float64
	OLE   float64
	NR    float64
	MEOH  float64
	CH4   float64
	NH3   float64
	NO    float64
	ALD2  float64
	ETOH  float64
	FORM  float64
	ALDX  float64
	TOL   float64
	IOLE  float64
	CO    float64
	ETHA  float64
	ETH   float64
	ETHY  float64
	PRPA  float64
	BENZ  float64
	ACET  float64
	KET   float64
	AACD  float64
	FACD  float64
	HCN   float64
	ISPD  float64
	N2O   float64
	SESQ  float64
	TRS   float64
	CH3BR float64
	CH3CL float64
	CH3I  float64
	ISP   float64
	TRP   float64
	XYLA  float64
	SQT   float64
	TOLA  float64
}

/*
 n = number of time steps (1st dimension)
*/
type Species_RACM2 struct { // cf. SPC_RACM2.EXT
	ISO   float64
	CH4   float64
	ETH   float64
	HC3   float64
	HC5   float64
	HC8   float64
	OL2   float64
	OLI   float64
	OLT   float64
	ALD   float64
	KET   float64
	TOL   float64
	HCHO  float64
	ORA1  float64
	ORA2  float64
	API   float64
	LIM   float64
	CO    float64
	SO2   float64
	NO    float64
	HNO3  float64
	NO2   float64
	NR    float64
	SESQ  float64
	MOH   float64
	ACT   float64
	EOH   float64
	ACD   float64
	ETE   float64
	MVK   float64
	HCN   float64
	MACR  float64
	CH3BR float64
	CH3CL float64
	CH3I  float64
	TRS   float64
	NH3   float64
	N2O   float64
}

/*
 n = number of time steps (1st dimension)
*/
type Species_CRIv2 struct { // cf. SPC_CRIv2.EXT
	C5H8       float64
	TBUT2ENE   float64
	APINENE    float64
	BPINENE    float64
	SESQ       float64
	CH3OH      float64
	CH3COCH3   float64
	C2H5OH     float64
	CH3CHO     float64
	HCOOH      float64
	CH3CO2H    float64
	C2H6       float64
	C2H4       float64
	C3H8       float64
	C3H6       float64
	HCHO       float64
	UCARB10    float64
	HCN        float64
	CH4        float64
	CH3BR      float64
	CH3CL      float64
	CH3I       float64
	DMS        float64
	NC4H10     float64
	CO         float64
	NO         float64
	NH3        float64
	N2O        float64
}

/*
 n = number of time steps (1st dimension)
*/
type Species_SAPRC07 struct { // cf. SPC_SAPRC07.EXT
	CH4   float64
	ALK1  float64
	ALK2  float64
	ALK3  float64
	ALK4  float64
	ALK5  float64
	ETHE  float64
	OLE1  float64
	OLE2  float64
	ISOP  float64
	TERP  float64
	BENZ  float64
	ARO1  float64
	ARO2  float64
	ACYE  float64
	HCHO  float64
	CCHO  float64
	RCHO  float64
	BALD  float64
	ACET  float64
	MEK   float64
	PRD2  float64
	MEOH  float64
	FACD  float64
	AACD  float64
	PACD  float64
	GLY   float64
	MGLY  float64
	BACL  float64
	CRES  float64
	MACR  float64
	MVK   float64
	IPRD  float64
	RNO3  float64
	CO    float64
	SESO  float64
	HCN   float64
	CH3BR float64
	CH3CL float64
	CH3I  float64
	TRS   float64
	NO    float64
	NH3   float64
	N2O   float64
}
