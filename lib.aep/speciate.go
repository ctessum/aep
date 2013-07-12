package aep

import (
	"bufio"
	"database/sql"
	"errors"
	"fmt"
	_ "github.com/mattn/go-sqlite3"
	"os"
	"strconv"
	"strings"
	"sync"
)

const tolerance = 1.e-4 // fractional difference between two numbers where they can be considered the same

var (
	SpecProfChan    = make(chan *SpecProfRequest)
	SpecRefFileLock = new(sync.Mutex)
)

// SpecRef reads the SMOKE gsref file, which maps SCC codes to chemical speciation profiles.
func (c *RunData) SpecRef() (specRef map[string]map[string]string, err error) {
	specRef = make(map[string]map[string]string)
	// map[SCC][pol]code
	var record string
	fid, err := os.Open(c.SpecRefFile)
	if err != nil {
		msg := "SpecRef: " + err.Error() + "\nFile= " +
			c.SpecRefFile + "\nRecord= " + record
		err = errors.New(msg)
		return
	} else {
		defer fid.Close()
	}
	buf := bufio.NewReader(fid)
	for {
		record, err = buf.ReadString('\n')
		if err != nil {
			if err.Error() == "EOF" {
				err = nil
				break
			} else {
				msg := record + "\n" + err.Error() + "\nFile= " +
					c.SpecRefFile + "\nRecord= " + record
				err = errors.New(msg)
				return
			}
		}
		// Get rid of comments at end of line.
		if i := strings.Index(record, "!"); i != -1 {
			record = record[0:i]
		}

		if record[0] != '#' && record[0] != '/' && record[0] != '\n' {
			// for point sources, only match to SCC code.
			splitLine := strings.Split(record, ";")
			SCC := strings.Trim(splitLine[0], "\"")
			if len(SCC) == 8 {
				SCC = "00" + SCC
			}
			code := strings.Trim(splitLine[1], "\"")
			pol := strings.Trim(splitLine[2], "\"\n")

			if _, ok := specRef[SCC]; !ok {
				specRef[SCC] = make(map[string]string)
			}
			specRef[SCC][pol] = code
		}
	}
	return
}

// SpecRefCombo reads the SMOKE gspro_combo file, which maps location
// codes to chemical speciation profiles for mobile sources.
func (c *RunData) SpecRefCombo(runPeriod string) (specRef map[string]map[string]map[string]float64, err error) {
	specRef = make(map[string]map[string]map[string]float64)
	// map[pol][FIPS][code]frac
	var record string
	fid, err := os.Open(c.SpecRefComboFile)
	if err != nil {
		msg := "SpecRefCombo: " + err.Error() + "\nFile= " +
			c.SpecRefComboFile + "\nRecord= " + record
		err = errors.New(msg)
		return
	} else {
		defer fid.Close()
	}

	periods := map[string]string{"0": "annual", "1": "jan", "2": "feb",
		"3": "mar", "4": "apr", "5": "may", "6": "jun", "7": "jul",
		"8": "aug", "9": "sep", "10": "oct", "11": "nov", "12": "dec"}

	buf := bufio.NewReader(fid)
	for {
		record, err = buf.ReadString('\n')
		if err != nil {
			if err.Error() == "EOF" {
				err = nil
				break
			} else {
				msg := record + "\n" + err.Error() + "\nFile= " +
					c.SpecRefComboFile + "\nRecord= " + record
				err = errors.New(msg)
				return
			}
		}
		// Get rid of comments at end of line.
		if i := strings.Index(record, "!"); i != -1 {
			record = record[0:i]
		}

		if record[0] != '#' && record[0] != '/' && record[0] != '\n' {
			// for point sources, only match to SCC code.
			splitLine := strings.Split(record, ";")
			pol := strings.Trim(splitLine[0], "\" ")
			// The FIPS number here is 6 characters instead of the usual 5.
			// The first character is a country code.
			FIPS := strings.Trim(splitLine[1], "\" ")

			period, ok := periods[splitLine[2]]
			if !ok {
				err = fmt.Errorf("Missing or mislabeled period in %v.",
					c.SpecRefComboFile)
				panic(err)
			}
			if period == runPeriod {
				if _, ok := specRef[pol]; !ok {
					specRef[pol] = make(map[string]map[string]float64)
				}
				if _, ok := specRef[pol][FIPS]; !ok {
					specRef[pol][FIPS] = make(map[string]float64)
				}
				for i := 4; i < len(splitLine); i += 2 {
					code := strings.Trim(splitLine[i], "\n\" ")
					frac, err := strconv.ParseFloat(strings.Trim(splitLine[i+1], "\n\" "), 64)
					if err != nil {
						panic(err)
					}
					specRef[pol][FIPS][code] = frac
				}
			}
		}
	}
	return
}

type SpecProfRequest struct {
	SpecType            string
	PolNameOrProfNumber string
	ProfileChan         chan map[string]*SpecHolder
}

func newSpecProfRequest(spectype, name string) *SpecProfRequest {
	data := new(SpecProfRequest)
	data.SpecType = spectype
	data.PolNameOrProfNumber = name
	data.ProfileChan = make(chan map[string]*SpecHolder)
	return data
}

func (c *RunData) SpecProfiles(e *ErrCat) {
	db, err := sql.Open("sqlite3", c.SpecProFile)
	if err != nil {
		e.Add(err)
		return
	}
	VOCprofiles := make(map[string]map[string]*SpecHolder)
	NOxprofiles := make(map[string]map[string]*SpecHolder)
	PM25profiles := make(map[string]map[string]*SpecHolder)
	individualProfiles := make(map[string]map[string]*SpecHolder)
	for request := range SpecProfChan {
		profile := make(map[string]*SpecHolder)
		switch request.SpecType {
		case "VOC":
			if _, ok := VOCprofiles[request.PolNameOrProfNumber]; ok {
				request.ProfileChan <- VOCprofiles[request.PolNameOrProfNumber]
			} else {
				total, convFac := c.getVOCtoTOGfactor(db,
					request.PolNameOrProfNumber)

				rows, err := db.Query("select species_id,weight_per from " +
					"gas_species where p_number=\"" + fmt.Sprint(
					request.PolNameOrProfNumber) + "\"")
				if err != nil {
					panic(err)
				}
				totalWeight := 0.
				for rows.Next() {
					var specID int
					var weightPercent float64
					err = rows.Scan(&specID, &weightPercent)
					if err != nil {
						panic(err)
					}
					totalWeight += weightPercent

					specName, tempMW, groupString :=
						c.getSpeciesInfoFromID(db, specID)
					MW := handleMW(specName, tempMW, weightPercent)
					groupFactors := c.handleGroupString(specName, groupString)

					x := new(SpecHolder)
					switch c.SpecType {
					case "mol":
						x.Factor = convFac * weightPercent / MW / total
						x.Units = "mol/gram"
					case "mass":
						x.Factor = convFac * weightPercent / total
						x.Units = "gram/gram"
					default:
						err = fmt.Errorf("Invalid specType %v. Options are "+
							"\"mass\" and \"mol\"", c.SpecType)
					}
					profile[specName] = new(SpecHolder)
					profile[specName].Units = x.Units
					profile[specName].Factor += x.Factor
					profile[specName].groupFactors = groupFactors
				}
				rows.Close()
				request.ProfileChan <- profile
				VOCprofiles[request.PolNameOrProfNumber] = profile
				if c.testMode && AbsBias(totalWeight, total) > tolerance {
					err := fmt.Errorf("For Gas speciation profile %v, "+
						"sum of species weights (%v) is not equal to total (%v)",
						request.PolNameOrProfNumber, totalWeight, total)
					panic(err)
				}
			}
		case "NOx":
			if _, ok := NOxprofiles[request.PolNameOrProfNumber]; ok {
				request.ProfileChan <- NOxprofiles[request.PolNameOrProfNumber]
			} else {
				total := c.getNOxTotal(db,
					request.PolNameOrProfNumber)

				rows, err := db.Query("select species_id,weight_per from " +
					"\"other gases_species\" where p_number=\"" + fmt.Sprint(
					request.PolNameOrProfNumber) + "\"")
				if err != nil {
					panic(err)
				}
				totalWeight := 0.
				for rows.Next() {
					var specID int
					var weightPercent float64
					err = rows.Scan(&specID, &weightPercent)
					if err != nil {
						panic(err)
					}
					totalWeight += weightPercent

					specName, tempMW, groupString :=
						c.getSpeciesInfoFromID(db, specID)
					MW := handleMW(specName, tempMW, weightPercent)
					groupFactors := c.handleGroupString(specName, groupString)

					x := new(SpecHolder)
					switch c.SpecType {
					case "mol":
						x.Factor = weightPercent / MW / total
						x.Units = "mol/gram"
					case "mass":
						x.Factor = weightPercent / total
						x.Units = "gram/gram"
					default:
						err = fmt.Errorf("Invalid specType %v. Options are "+
							"\"mass\" and \"mol\"", c.SpecType)
					}
					profile[specName] = new(SpecHolder)
					profile[specName].Units = x.Units
					profile[specName].Factor += x.Factor
					profile[specName].groupFactors = groupFactors
				}
				rows.Close()
				request.ProfileChan <- profile
				NOxprofiles[request.PolNameOrProfNumber] = profile
				if c.testMode && AbsBias(totalWeight, total) > tolerance {
					err := fmt.Errorf("For NOx speciation profile %v, "+
						"sum of species weights (%v) is not equal to total (%v)",
						request.PolNameOrProfNumber, totalWeight, total)
					panic(err)
				}
			}
		case "PM2.5":
			if _, ok := PM25profiles[request.PolNameOrProfNumber]; ok {
				request.ProfileChan <- PM25profiles[request.PolNameOrProfNumber]
			} else {
				total := c.getPM25Total(db,
					request.PolNameOrProfNumber)
				rows, err := db.Query("select species_id,weight_per from " +
					"pm_species where p_number=\"" + fmt.Sprint(
					request.PolNameOrProfNumber) + "\"")
				if err != nil {
					panic(err)
				}
				totalWeight := 0.
				for rows.Next() {
					var specID int
					var weightPercent float64
					err = rows.Scan(&specID, &weightPercent)
					if err != nil {
						panic(err)
					}
					totalWeight += weightPercent

					specName, _, groupString :=
						c.getSpeciesInfoFromID(db, specID)

					profile[specName] = new(SpecHolder)
					profile[specName].Units = "gram/gram"
					profile[specName].Factor += weightPercent / total
					profile[specName].groupFactors =
						c.handleGroupString(specName, groupString)
				}
				rows.Close()
				request.ProfileChan <- profile
				PM25profiles[request.PolNameOrProfNumber] = profile
				if c.testMode && AbsBias(totalWeight, total) > tolerance {
					err := fmt.Errorf("For PM2.5 speciation profile %v, "+
						"sum of species weights (%v) is not equal to total (%v)",
						request.PolNameOrProfNumber, totalWeight, total)
					panic(err)
				}
			}
		case "individualGas":
			if _, ok := individualProfiles[request.PolNameOrProfNumber]; ok {
				request.ProfileChan <- individualProfiles[request.PolNameOrProfNumber]
			} else {
				specName := request.PolNameOrProfNumber
				tempMW, groupString :=
					c.getSpeciesInfoFromName(db, specName)
				MW := handleMW(specName, tempMW, 100.)
				groupFactors := c.handleGroupString(specName, groupString)

				x := new(SpecHolder)
				switch c.SpecType {
				case "mol":
					x.Factor = 1. / MW
					x.Units = "mol/gram"
				case "mass":
					x.Factor = 1.
					x.Units = "gram/gram"
				default:
					err = fmt.Errorf("Invalid specType %v. Options are \"mass\" "+
						"and \"mol\"", c.SpecType)
				}
				x.groupFactors = groupFactors
				profile[request.PolNameOrProfNumber] = x
				request.ProfileChan <- profile
				individualProfiles[request.PolNameOrProfNumber] = profile
			}
		default:
			panic("Unknown speciation type " + request.SpecType)
		}
	}
}

func (c *RunData) getVOCtoTOGfactor(db *sql.DB, ProfileNumber string) (
	total, convFac float64) {
	var tempConvFac sql.NullFloat64
	cmd := fmt.Sprintf("select TOTAL,VOCtoTOG from GAS_PROFILE where"+
		" P_NUMBER=\"%v\"", ProfileNumber)
	err := db.QueryRow(cmd).Scan(&total, &tempConvFac)
	if err != nil {
		panic(fmt.Errorf("Speciation SQL problem.\nCommand=%v\n"+
			"Error=%v.", cmd, err.Error()))
	}
	if tempConvFac.Valid && !c.testMode { // in testMode, set to 1 for checksums
		convFac = tempConvFac.Float64
	} else {
		convFac = 1.
		msg := fmt.Sprintf("VOC to TOG conversion factor is missing for "+
			"SPECIATE pollutant ID %v. Setting it to 1.0.",
			ProfileNumber)
		c.Log(msg, 1)
	}
	return
}

func (c *RunData) getPM25Total(db *sql.DB, ProfileNumber string) (
	total float64) {
	cmd := fmt.Sprintf("select TOTAL from PM_PROFILE where"+
		" P_NUMBER=%v", ProfileNumber)
	err := db.QueryRow(cmd).Scan(&total)
	if err != nil {
		panic(fmt.Errorf("Speciation SQL problem.\nCommand=%v\n"+
			"Error=%v.", cmd, err.Error()))
	}
	return
}

func (c *RunData) getNOxTotal(db *sql.DB, ProfileNumber string) (
	total float64) {
	cmd := fmt.Sprintf("select TOTAL from \"OTHER GASES_PROFILE\" where"+
		" P_NUMBER=%v and MASTER_POL=\"NOx\"", ProfileNumber)
	err := db.QueryRow(cmd).Scan(&total)
	if err != nil {
		panic(fmt.Errorf("Speciation SQL problem.\nCommand=%v\n"+
			"Error=%v.", cmd, err.Error()))
	}
	return
}

func (c *RunData) getSpeciesInfoFromID(db *sql.DB, specID int) (
	specName string, tempMW sql.NullFloat64, groupString sql.NullString) {
	err := db.QueryRow(fmt.Sprintf("SELECT name,spec_mw,%v_group "+
		"FROM species_properties WHERE ID=%v", c.SpeciesGroupName, specID)).
		Scan(&specName, &tempMW, &groupString)
	if err != nil {
		panic(fmt.Errorf("Problem retrieving species "+
			"properties for SPECIATE pollutant ID=%v: %v",
			specID, err.Error()))
	}
	return
}

func (c *RunData) getSpeciesInfoFromName(db *sql.DB, specName string) (
	tempMW sql.NullFloat64, groupString sql.NullString) {
	err := db.QueryRow(fmt.Sprintf("SELECT spec_mw,%v_group "+
		"FROM species_properties WHERE name=\"%v\"", c.SpeciesGroupName, specName)).
		Scan(&tempMW, &groupString)
	if err != nil {
		panic(fmt.Errorf("Problem retrieving species "+
			"properties for SPECIATE pollutant Name=%v: %v",
			specName, err.Error()))
	}
	return
}

func handleMW(specName string, tempMW sql.NullFloat64, weightPercent float64) (
	MW float64) {
	if tempMW.Valid {
		MW = tempMW.Float64
	} else if weightPercent < 1. {
		MW = 100. // If database doesn't have a molecular weight
		// but the species is less than 1% of total mass,
		// give it a default MW of 100
	} else {
		panic(fmt.Errorf("Species %v does not have a "+
			"molecular weight in SPECIATE database "+
			"(mass fraction = %v).",
			specName, weightPercent))
	}
	return
}

func (c *RunData) handleGroupString(specName string, groupString sql.NullString) (
	groupFactors map[string]float64) {
	if groupString.Valid && groupString.String != "" {
		groupFactors = make(map[string]float64)
		if c.testMode {
			groupFactors[strings.Split(strings.Split(
				groupString.String, "&")[0], ":")[0]] = 1.0
			return
		}
		for _, groupCombo := range strings.Split(groupString.String, "&") {
			groupComboList := strings.Split(groupCombo, ":")
			group := groupComboList[0]
			switch len(groupComboList) {
			case 2:
				var err error
				groupFactors[group], err = strconv.ParseFloat(
					groupComboList[1], 64)
				if err != nil {
					panic(err)
				}
			case 1:
				groupFactors[group] = 1.0
			default:
				panic(fmt.Errorf("Problem parsing speciation group %v "+
					"for species name %v", groupString, specName))
			}
		}
	}
	return
}

type SpecRef struct {
	sRef      map[string]map[string]string             // map[SCC][pol]code
	sRefCombo map[string]map[string]map[string]float64 // map[pol][FIPS][code]frac
}

func (c *RunData) NewSpecRef() (sp *SpecRef, err error) {
	sp = new(SpecRef)
	SpecRefFileLock.Lock()
	sp.sRef, err = c.SpecRef()
	SpecRefFileLock.Unlock()
	if err != nil {
		return
	}
	return
}

type SpecHolder struct {
	Factor       float64
	Units        string
	groupFactors map[string]float64
}

// A default speciation profile for when we don't have any other
// information.
func (sp *SpecRef) DefaultProfile(pol string) (profile map[string]*SpecHolder) {
	// We don't know the molecular weight, so don't
	// perform any conversion
	profile = make(map[string]*SpecHolder)
	profile[pol] = new(SpecHolder)
	profile[pol].Factor = 1.
	profile[pol].Units = "gram/gram"
	return
}

func (sp *SpecRef) GetProfileSingleSpecies(pol string, c *RunData) (
	profile map[string]*SpecHolder, droppedNotInGroup map[string]*SpecHolder) {

	request := newSpecProfRequest("individualGas", pol)
	SpecProfChan <- request
	tempProfile := <-request.ProfileChan

	if len(tempProfile[pol].groupFactors) > 0 {
		profile = make(map[string]*SpecHolder)
		for groupName, groupFactor := range tempProfile[pol].groupFactors {
			// If pollutant is part of a species group, add to the group
			if _, ok := profile[groupName]; !ok {
				profile[groupName] = tempProfile[pol]
				profile[groupName].Factor *= groupFactor
			} else {
				profile[groupName].Factor += tempProfile[pol].Factor * groupFactor
			}
		}
	} else {
		droppedNotInGroup = make(map[string]*SpecHolder)
		droppedNotInGroup = tempProfile
	}
	return
}

func (sp *SpecRef) GetProfileAggregateSpecies(number, specType string,
	doubleCountPols []string, c *RunData) (
	profile map[string]*SpecHolder,
	droppedDoubleCount map[string]*SpecHolder,
	droppedNotInGroup map[string]*SpecHolder) {

	profile = make(map[string]*SpecHolder)
	droppedDoubleCount = make(map[string]*SpecHolder)
	droppedNotInGroup = make(map[string]*SpecHolder)

	request := newSpecProfRequest(specType, number)
	SpecProfChan <- request
	tempProfile := <-request.ProfileChan

	for specName, data := range tempProfile {
		if len(data.groupFactors) > 0 &&
			!IsStringInArray(doubleCountPols, specName) {
			// If pollutant is part of a species group, and it isn't
			// double counting an explicit emissions record,
			// add to the group
			for groupName, groupFactor := range data.groupFactors {
				if _, ok := profile[groupName]; !ok {
					profile[groupName] = new(SpecHolder)
					profile[groupName].Units = data.Units
				} else if data.Units != profile[groupName].Units {
					err := fmt.Errorf("Units %v and %v don't match", data.Units,
						profile[groupName].Units)
					panic(err)
				}
				profile[groupName].Factor += data.Factor * groupFactor
			}
		} else if len(data.groupFactors) == 0 {
			// ungrouped pollutants
			droppedNotInGroup[specName] = data
		} else {
			// double counted pollutants
			droppedDoubleCount[specName] = data
		}
	}
	return
}

// Match SCC in record to speciation profile. If none matches exactly, find a
// more general SCC that matches.
func (sp *SpecRef) getSccFracs(record *ParsedRecord, pol string, c *RunData,
	period string) (
	specFactors, doubleCountSpecFactors,
	ungroupedSpecFactors map[string]*SpecHolder) {
	var ok bool
	var err error
	var ref map[string]string
	SCC := record.SCC
	if c.MatchFullSCC {
		var matchedSCC string
		matchedSCC, err = MatchCode2(SCC, sp.sRef)
		if err != nil {
			err = fmt.Errorf("In Speciate, SCC code " + SCC +
				" is not in specRef file and there is no default.")
			panic(err)
		} else {
			ref = sp.sRef[matchedSCC]
		}
	} else {
		ref, ok = sp.sRef[SCC]
		if !ok {
			err = fmt.Errorf("In Speciate, SCC code " + SCC +
				" is not in specRef file. Setting matchFullSCC to " +
				"to 'false' will allow a default value to be used.")
			panic(err)
		}
	}

	if c.PolsToKeep[cleanPol(pol)].SpecProf != nil {
		specFactors = c.PolsToKeep[cleanPol(pol)].SpecProf
	} else if c.PolsToKeep[cleanPol(pol)].SpecNames != nil {
		// For explicit species, convert the value to moles
		// if required and add the emissions to a species group
		// if one exists
		specFactors, ungroupedSpecFactors = sp.GetProfileSingleSpecies(
			c.PolsToKeep[cleanPol(pol)].SpecNames[0], c)
	} else {
		specType := c.PolsToKeep[cleanPol(pol)].SpecType
		switch specType {
		case "VOC":
			code, ok := ref[pol]
			if ok {
				if code == "COMBO" { // for location specific speciation profiles
					if sp.sRefCombo == nil {
						sp.sRefCombo, err = c.SpecRefCombo(period)
						if err != nil {
							return
						}
					}
					countryCode := GetCountryCode(record.Country)
					codes := sp.sRefCombo[pol][countryCode+record.FIPS]
					specFactors = make(map[string]*SpecHolder)
					doubleCountSpecFactors = make(map[string]*SpecHolder)
					ungroupedSpecFactors = make(map[string]*SpecHolder)
					for code2, frac := range codes {
						tempSpecFactors := make(map[string]*SpecHolder)
						tempDoubleCountSpecFactors := make(map[string]*SpecHolder)
						tempUngroupedSpecFactors := make(map[string]*SpecHolder)
						tempSpecFactors, tempDoubleCountSpecFactors,
							tempUngroupedSpecFactors =
							sp.GetProfileAggregateSpecies(code2, specType,
								record.DoubleCountPols, c)
						for pol, val := range tempSpecFactors {
							if _, ok := specFactors[pol]; !ok {
								specFactors[pol] = new(SpecHolder)
								specFactors[pol].Factor = val.Factor * frac
								specFactors[pol].Units = val.Units
							} else {
								specFactors[pol].Factor += val.Factor * frac
								if specFactors[pol].Units != val.Units {
									panic("Units error!")
								}
							}
						}
						for pol, val := range tempDoubleCountSpecFactors {
							if _, ok := doubleCountSpecFactors[pol]; !ok {
								doubleCountSpecFactors[pol] = new(SpecHolder)
								doubleCountSpecFactors[pol].Factor =
									val.Factor * frac
								doubleCountSpecFactors[pol].Units =
									val.Units
							} else {
								doubleCountSpecFactors[pol].Factor +=
									val.Factor * frac
								if doubleCountSpecFactors[pol].Units !=
									val.Units {
									panic("Units error!")
								}
							}
						}
						for pol, val := range tempUngroupedSpecFactors {
							if _, ok := ungroupedSpecFactors[pol]; !ok {
								ungroupedSpecFactors[pol] = new(SpecHolder)
								ungroupedSpecFactors[pol].Factor =
									val.Factor * frac
								ungroupedSpecFactors[pol].Units = val.Units
							} else {
								ungroupedSpecFactors[pol].Factor +=
									val.Factor * frac
								if ungroupedSpecFactors[pol].Units != val.Units {
									panic("Units error!")
								}
							}
						}
					}
				} else {
					specFactors, doubleCountSpecFactors,
						ungroupedSpecFactors = sp.GetProfileAggregateSpecies(
						code, specType, record.DoubleCountPols, c)
				}
			} else { // no speciation profile reference is found
				specFactors = sp.DefaultProfile(pol)
			}
		case "PM2.5", "NOx":
			if code, ok := ref[pol]; ok {
				specFactors, doubleCountSpecFactors,
					ungroupedSpecFactors = sp.GetProfileAggregateSpecies(
					code, specType, record.DoubleCountPols, c)
			} else { // no speciation profile reference is found
				specFactors = sp.DefaultProfile(pol)
			}
		default:
			panic("In PolsToKeep, either `SpecNames', `SpecProf', or `SpecType" +
				" needs to be specified. `SpecType' can only be VOC, " +
				"PM2.5, and NOx")
		}
	}
	if c.testMode { // fractions only add up to 1 in test mode.
		fracSum := 0.
		for _, data := range specFactors {
			fracSum += data.Factor
		}
		for _, data := range doubleCountSpecFactors {
			fracSum += data.Factor
		}
		for _, data := range ungroupedSpecFactors {
			fracSum += data.Factor
		}
		if AbsBias(fracSum, 1.0) > tolerance {
			err = fmt.Errorf("Sum of speciation fractions (%v) for pollutant %v "+
				"is not equal to 1.", fracSum, pol)
			panic(err)
		}
	}
	return
}

type SpecTotals struct {
	Kept          map[string]*specValUnits
	DoubleCounted map[string]*specValUnits
	Ungrouped     map[string]*specValUnits
}

func newSpeciationTotalHolder() *SpecTotals {
	out := new(SpecTotals)
	out.Kept = make(map[string]*specValUnits)
	out.DoubleCounted = make(map[string]*specValUnits)
	out.Ungrouped = make(map[string]*specValUnits)
	return out
}

func (h *SpecTotals) AddKept(pol string, data *specValUnits) {
	t := *h
	if _, ok := t.Kept[pol]; !ok {
		t.Kept[pol] = new(specValUnits)
		t.Kept[pol].Units = data.Units
	} else {
		if t.Kept[pol].Units != data.Units {
			err := fmt.Errorf("Units problem: %v! = %v",
				t.Kept[pol].Units, data.Units)
			panic(err)
		}
	}
	t.Kept[pol].Val += data.Val
	*h = t
}

func (h *SpecTotals) AddDoubleCounted(pol string, val float64, units string) {
	t := *h
	if _, ok := t.DoubleCounted[pol]; !ok {
		t.DoubleCounted[pol] = new(specValUnits)
		t.DoubleCounted[pol].Units = units
	} else {
		if t.DoubleCounted[pol].Units != units {
			err := fmt.Errorf("Units problem: %v! = %v",
				t.DoubleCounted[pol].Units, units)
			panic(err)
		}
	}
	t.DoubleCounted[pol].Val += val
	*h = t
}

func (h *SpecTotals) AddUngrouped(pol string, val float64, units string) {
	t := *h
	if _, ok := t.Ungrouped[pol]; !ok {
		t.Ungrouped[pol] = new(specValUnits)
		t.Ungrouped[pol].Units = units
	} else {
		if t.Ungrouped[pol].Units != units {
			err := fmt.Errorf("Units problem: %v! = %v",
				t.Ungrouped[pol].Units, units)
			panic(err)
		}
	}
	t.Ungrouped[pol].Val += val
	*h = t
}

// Check if there is a speciation record for this
// SCC/pollutant combination.If not, check if the pollutant
// is in the list of pollutants to drop. If it is not in the
// drop list, transfer it to the speciated record without any
// speciating. Otherwise, add it to the totals of dropped
// pollutants. If the record is a speciatable record, multiply the
// annual emissions by the speciation factors. Multiply all
// speciated emissions by a conversion factor from the input
// units to g/year.
func (c *RunData) Speciate(InputChan chan *ParsedRecord,
	OutputChan chan *ParsedRecord, period string) {
	defer c.ErrorRecoverCloseChan(InputChan)
	c.Log("Speciating "+period+" "+c.Sector+"...", 0)

	sp, err := c.NewSpecRef()
	if err != nil {
		panic(err)
	}

	totals := newSpeciationTotalHolder()
	polFracs := make(map[string]*SpecHolder)
	doubleCountPolFracs := make(map[string]*SpecHolder)
	ungroupedPolFracs := make(map[string]*SpecHolder)
	for record := range InputChan {
		newAnnEmis := make(map[string]*specValUnits)
		for pol, AnnEmis := range record.ANN_EMIS {
			polFracs, doubleCountPolFracs, ungroupedPolFracs =
				sp.getSccFracs(record, pol, c, period)
			for newpol, factor := range polFracs {
				if _, ok := newAnnEmis[newpol]; ok {
					err = fmt.Errorf("Possible double counting of"+
						" pollutant %v SCC %v", newpol, record.SCC)
					panic(err)
				}
				newAnnEmis[newpol] = new(specValUnits)
				newAnnEmis[newpol].Val = AnnEmis.Val *
					c.InputConv * factor.Factor
				newAnnEmis[newpol].Units = strings.Replace(
					factor.Units, "/gram", "/year", -1)
				totals.AddKept(newpol, newAnnEmis[newpol])
			}
			for droppedpol, factor := range doubleCountPolFracs {
				totals.AddDoubleCounted(droppedpol, AnnEmis.Val*
					c.InputConv*factor.Factor, strings.Replace(
					factor.Units, "/gram", "/year", -1))
			}
			for droppedpol, factor := range ungroupedPolFracs {
				totals.AddUngrouped(droppedpol, AnnEmis.Val*
					c.InputConv*factor.Factor, strings.Replace(
					factor.Units, "/gram", "/year", -1))
			}
		}
		record.ANN_EMIS = newAnnEmis
		OutputChan <- record
	}
	Report.SectorResults[c.Sector][period].
		SpeciationResults = totals

	c.msgchan <- "Finished speciating " + period + " " + c.Sector
	close(OutputChan)
}
