/*
Copyright (C) 2012-2014 the AEP authors.
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

package aep

import (
	"fmt"
	"strconv"
	"time"

	"github.com/ctessum/geom/proj"
)

const (
	months = "jan feb mar apr may jun jul aug sep oct nov dec"
)

// Website gives the location of the AEP website.
const Website = "http://github.com/ctessum/aep/"

// Version give the current version of AEP.
// The versioning scheme is at: http://semver.org/.
const Version = "0.1.0"

// Context is a container for the configuration and report info
type Context struct {
	outputDir          string // Output directory
	ShapefileDir       string // Directory where input shapefiles are stored
	GriddedSrgs        string // Directory where gridded spatial surrogate shapefiles are to be created
	Sector             string // Name of the sector
	SectorType         string // Type of sector (point, area)
	RunSpeciate        bool   // Whether to speciate data
	RunSpatialize      bool   // Whether to spatialize data
	RunTemporal        bool   // Whether to temporalize data
	StartDate          string // Date for which to begin processing emissions
	startDate          time.Time
	EndDate            string // Date for which to end processing emissions
	endDate            time.Time
	Tstep              string // Timestep between emissions outputs. Currently, it is recommended to set this to "1h".
	tStep              time.Duration
	TstepsPerFile      int    // Number of output timesteps in each file. Currently, it is recommended to set this to "24".
	OutputType         string // Type of output file. Currently the only available option is "WRF".
	SccDesc            string // name of file with SCC code descriptions
	SicDesc            string // name of file with SIC code descriptions
	NaicsDesc          string // name of file with NAICS code descriptions
	SpecRefFile        string // Location of the speciation code reference file.
	SpecRefComboFile   string // Location of the county specific speciation code reference file, if any.
	SpecProFile        string // Location of the SPECIATE database in sqlite format.
	SpecType           string // Type of speciation to perform. Either "mol" or "mass".
	ChemicalMechanism  string // name for chemical species grouping scheme (needs to be present in SPECIATE database as columns with "_GROUP" and "_FACTOR" appended)
	MechAssignmentFile string // Path to the mechanism assignment file. Can be downloaded from http://www.cert.ucr.edu/~carter/emitdb/
	MechanismMWFile    string // Path to the mechanism molecular weight file. Can be compiled from information from http://www.cert.ucr.edu/~carter/emitdb/
	SpeciesInfoFile    string // Path to the VOC species info file. Can be downloaded from http://www.cert.ucr.edu/~carter/emitdb/
	//PolsToKeep             map[string]*PolHolder // List and characteristics of pollutants to extract from the inventory and process
	InputProj4             string // Proj4 specification of the spatial projection of the input emissions data.
	inputSr                *proj.SR
	GridRefFile            string             // Location of the gridding reference file
	SrgSpecFile            string             // Location of the surrogate specification file
	TemporalRefFile        string             // Location of the temporal reference file
	TemporalProFile        string             // Location of the temporal profile file
	HolidayFile            string             // Location of the file specifying which days are holidays
	InventoryFreq          InventoryFrequency // The temporal frequency of the inventory data files. Currently the options are "annual", "monthly", and "cem".
	RunPeriods             []Period           // Periods (annual, or individual months, etc.) to be run for this sector
	MatchFullSCC           bool               // Whether to only match codes which are identical, or to accept partial matches.
	DebugLevel             int                // Sets the volume of output printed to the screen. Set to 0 for least output, 3 for most output. Also, if DebugLevel > 0, any errors encountered will cause the entire program to crash with a stack trace, rather than just printing an error message and continuing.
	Ncpus                  int                // Number of processors available for use
	InputUnits             string             // Units of emissions in input file
	InputConv              float64
	ForceWesternHemisphere bool     // If all data is in the western hemisphere, fix any errors where the minus sign was left out of the longitude.
	InvFileNames           []string // List of input files. "[month]" can be used as a wildcard for the month name.
	CEMFileNames           []string // List of input files with CEM data (needs to be a whole year's worth of data
	EarthRadius            float64  // in meters
	WPSnamelist            string   // Path to WPS namelist file
	WRFnamelist            string   // Path to WPS namelist file
	OldWRFout              string   // Path to old WRF output files for plume rise calculations.
	//[DOMAIN] and [DATE] can be used as wildcards. If kemit > 1 in the WRF namelist.input file, these files
	// will be needed to calculate plume rise for point sources. Otherwise, these files will not be necessary.
	RegenerateSpatialData bool   // Whether or not to delete surrogates and ShapefileDir generated by previous run and start over. If the model has not been previously run with the current "SimuationName", this does nothing. If the model did not sucessfully complete the last time it was run, the model may crash again if this is set to `false`.
	SimulationName        string // Name for the simulation: user choice
	currentTime           time.Time
	ErrorFlag             bool // whether this sector has already encountered an error
	testMode              bool // testMode is set by a command line flag and is used for testing the program. In ensures that SpecType is set to "mass" and turns off the conversion from VOCs to TOGs so that speciated emissions totals can be compared to total emissions in the inventory
	msgchan               chan string
	slaves                []string // Addresses for available slaves when in distributed mode.

	CheckSrgs bool // Should we check the surrogate shapefiles before running?
}

// Period specifies the time period of the emissions data.
type Period int

// The Periods are the months of the year, annual, or Cem which is hourly
// continuous emissions monitoring data.
const (
	Jan Period = iota + 1
	Feb
	Mar
	Apr
	May
	Jun
	Jul
	Aug
	Sep
	Oct
	Nov
	Dec
	Annual
	Cem
)

func (p Period) String() string {
	switch p {
	case Jan:
		return "Jan"
	case Feb:
		return "Feb"
	case Mar:
		return "Mar"
	case Apr:
		return "Apr"
	case May:
		return "May"
	case Jun:
		return "Jun"
	case Jul:
		return "Jul"
	case Aug:
		return "Aug"
	case Sep:
		return "Sep"
	case Oct:
		return "Oct"
	case Nov:
		return "Nov"
	case Dec:
		return "Dec"
	case Annual:
		return "Annual"
	default:
		panic(fmt.Sprintf("unknown period: %d", int(p)))
	}
}

// TimeInterval returns the start and the end of the receiver period
// in the given year.
func (p Period) TimeInterval(year string) (start, end time.Time, err error) {
	switch p {
	case Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec:
		start, err = time.Parse("Jan 2006", fmt.Sprintf("%s %s", p, year))
		if err != nil {
			return
		}
		if p == Dec {
			var intYear int64
			intYear, err = strconv.ParseInt(year, 0, 32)
			if err != nil {
				return
			}
			nextYear := fmt.Sprintf("%04d", intYear+1)
			end, err = time.Parse("Jan 2006", fmt.Sprintf("Jan %s", nextYear))
			if err != nil {
				return
			}
		} else {
			end, err = time.Parse("Jan 2006", fmt.Sprintf("%s %s", p+1, year))
			if err != nil {
				return
			}
		}
	case Annual:
		start, err = time.Parse("Jan 2006",
			fmt.Sprintf("Jan %s", year))
		if err != nil {
			return
		}
		var intYear int64
		intYear, err = strconv.ParseInt(year, 0, 32)
		if err != nil {
			return
		}
		nextYear := fmt.Sprintf("%04d", intYear+1)
		end, err = time.Parse("Jan 2006", fmt.Sprintf("Jan %s", nextYear))
		if err != nil {
			return
		}
	default:
		panic(fmt.Sprintf("unknown period: %d", int(p)))
	}
	return
}

// PeriodFromTimeInterval returns the period associated with the given
// time interval.
func PeriodFromTimeInterval(start, end time.Time) (Period, error) {
	const (
		monthHoursLow, monthHoursHigh = 24.0 * 27, 24.0 * 32
		yearHoursLow, yearHoursHigh   = 8700.0, 8800.0
	)
	duration := end.Sub(start).Hours()
	if yearHoursLow <= duration && duration <= yearHoursHigh {
		return Annual, nil
	}
	if !(monthHoursLow <= duration && duration <= monthHoursHigh) {
		return -1, fmt.Errorf("aep: invalid period time interval %v -- %v", start, end)
	}
	// The period is a month.
	return Period(start.Month()), nil
}
