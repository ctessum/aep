package main

import (
	"bufio"
	"database/sql"
	"errors"
	"fmt"
	_ "github.com/mattn/go-sqlite3"
	"os"
	"strconv"
	"strings"
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

		if record[0] != '#' && record[0] != '/' {
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
func (c *RunData) SpecRefCombo() (specRef map[string]map[string]map[string]map[string]float64, err error) {
	specRef = make(map[string]map[string]map[string]map[string]float64)
	// map[pol][FIPS][period][code]frac
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

		if record[0] != '#' && record[0] != '/' {
			// for point sources, only match to SCC code.
			splitLine := strings.Split(record, ";")
			pol := strings.Trim(splitLine[0], "\" ")
			// The FIPS number here is 6 characters instead of the usual 5.
			// The first character is a country code.
			FIPS := strings.Trim(splitLine[1], "\" ")

			periods := map[string]string{"0": "annual", "1": "jan", "2": "feb",
				"3": "mar", "4": "apr", "5": "may", "6": "jun", "7": "jul",
				"8": "aug", "9": "sep", "10": "oct", "11": "nov", "12": "dec"}
			period, ok := periods[splitLine[2]]
			if !ok {
				err = fmt.Errorf("Missing or mislabeled period in %v.",
					c.SpecRefComboFile)
				panic(err)
			}
			if _, ok := specRef[pol]; !ok {
				specRef[pol] = make(map[string]map[string]map[string]float64)
			}
			if _, ok := specRef[pol][FIPS]; !ok {
				specRef[pol][FIPS] = make(map[string]map[string]float64)
			}
			if _, ok := specRef[pol][FIPS][period]; !ok {
				specRef[pol][FIPS][period] = make(map[string]float64)
			}
			for i := 4; i < len(splitLine); i += 2 {
				code := strings.Trim(splitLine[i], "\n\" ")
				frac, err := strconv.ParseFloat(strings.Trim(splitLine[i+1], "\n\" "), 64)
				if err != nil {
					panic(err)
				}
				specRef[pol][FIPS][period][code] = frac
			}
		}
	}
	return
}

type SpecProf struct {
	db         *sql.DB
	sRef       map[string]map[string]string                        // map[SCC][pol]code
	sRefCombo  map[string]map[string]map[string]map[string]float64 // map[pol][FIPS][period][code]frac
	sPro       map[string]map[string]*specHolder                   //map[code][NewPol]fracs
	droppedPro map[string]map[string]*specHolder                   //map[code][NewPol]fracs
}

func (c *RunData) NewSpecProf() (sp *SpecProf, err error) {
	sp = new(SpecProf)
	sp.db, err = sql.Open("sqlite3", c.SpecProFile)

	sp.sRef, err = c.SpecRef()
	if err != nil {
		return
	}
	sp.sRefCombo, err = c.SpecRefCombo()
	if err != nil {
		return
	}
	sp.sPro = make(map[string]map[string]*specHolder)
	sp.droppedPro = make(map[string]map[string]*specHolder)
	return
}

type specHolder struct {
	factor float64
	units  string
}

// A default speciation profile for when we don't have any other
// information.
func (sp *SpecProf) DefaultProfile(pol string) (profile map[string]*specHolder) {
	// We don't know the molecular weight, so don't 
	// perform any conversion
	profile = make(map[string]*specHolder)
	profile[pol] = new(specHolder)
	profile[pol].factor = 1.
	profile[pol].units = "gram/gram"
	return
}

func (sp *SpecProf) GetProfileSingleSpecies(pol string, c *RunData) (
	profile map[string]*specHolder) {
	var tempMW sql.NullFloat64
	var MW float64
	var group sql.NullString
	var tempgroupfactor sql.NullFloat64
	var groupfactor float64
	err := sp.db.QueryRow("select spec_mw,"+c.SpeciesGroupName+"_group,"+
		c.SpeciesGroupName+"_factor from "+
		"species_properties where NAME=?", pol).Scan(
		&tempMW, &group, &tempgroupfactor)
	if err != nil {
		panic(err)
	}
	if tempMW.Valid {
		MW = tempMW.Float64
	} else {
		err = fmt.Errorf("Species %v does not have a molecular weight"+
			" in SPECIATE database.", pol)
		panic(err)
	}
	if tempgroupfactor.Valid {
		groupfactor = tempgroupfactor.Float64
	} else {
		groupfactor = 1.0
	}
	x := new(specHolder)
	switch c.SpecType {
	case "mol":
		x.factor = 1. / MW
		x.units = "mol/gram"
	case "mass":
		x.factor = 1.
		x.units = "gram/gram"
	default:
		err = fmt.Errorf("Invalid specType %v. Options are \"mass\" "+
			"and \"mol\"", c.SpecType)
	}
	profile = make(map[string]*specHolder)
	if group.Valid { // If pollutant is part of a species group, add to the group
		profile[group.String] = x
		profile[group.String].factor += x.factor * groupfactor
	} else {
		profile[pol] = x
	}
	return
}

func (sp *SpecProf) GetProfileGas(number string,
	doubleCountPols []string, c *RunData) (
	profile map[string]*specHolder,
	droppedProfile map[string]*specHolder) {

	profile = make(map[string]*specHolder)
	droppedProfile = make(map[string]*specHolder)
	var err error
	var total float64
	var tempConvFac sql.NullFloat64
	var convFac float64
	fmt.Println(number, "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
	err = sp.db.QueryRow("select TOTAL,VOCtoTOG from GAS_PROFILE where"+
		" P_NUMBER=?", number).Scan(&total, &tempConvFac)
	if err != nil {
		panic(err)
	}
	if tempConvFac.Valid {
		convFac = tempConvFac.Float64
	} else {
		convFac = 0.
	}

	rows, err := sp.db.Query("select species_id,weight_per from " +
		"gas_species where p_number=\"" + fmt.Sprint(number) + "\"")
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
		var specName string
		var tempMW sql.NullFloat64
		var MW float64
		var group sql.NullString
		var tempgroupfactor sql.NullFloat64
		var groupfactor float64
		err = sp.db.QueryRow("select name,spec_mw,"+c.SpeciesGroupName+"_group,"+
			c.SpeciesGroupName+"_factor from "+
			"species_properties where ID=?", specID).Scan(
			&specName, &tempMW, &group, &tempgroupfactor)
		if err != nil {
			panic(err)
		}
		if tempMW.Valid {
			MW = tempMW.Float64
		} else if weightPercent < 1. {
			MW = 100. // If database doesn't have a molecular weight, but the species
			// less than 1% of total mass, give it a default MW of 100
		} else {
			err = fmt.Errorf("Species %v (ID %v) does not have a molecular weight"+
				" in SPECIATE database (mass fraction = %v).", specName, specID,
				weightPercent)
			panic(err)
		}
		if tempgroupfactor.Valid {
			groupfactor = tempgroupfactor.Float64
		} else {
			groupfactor = 1.0
		}

		x := new(specHolder)
		switch c.SpecType {
		case "mol":
			x.factor = convFac * weightPercent /
				100. / MW
			x.units = "mol/gram"
		case "mass":
			x.factor = convFac * weightPercent /
				100.
			x.units = "gram/gram"
		default:
			err = fmt.Errorf("Invalid specType %v. Options are \"mass\" "+
				"and \"mol\"", c.SpecType)
		}
		if group.Valid && IsStringInArray(doubleCountPols, specName) {
			// If pollutant is part of a species group, and it isn't
			// double counting an explicit emissions record,
			// add to the group
			if _, ok := sp.sPro[number][group.String]; !ok {
				profile[group.String] = new(specHolder)
			} else if x.units != sp.sPro[number][group.String].units {
				err = fmt.Errorf("Units %v and %v don't match", x.units,
					profile[group.String].units)
			}
			fmt.Println(x.factor, groupfactor)
			profile[group.String].factor += x.factor * groupfactor
		} else if !group.Valid {
			// Add ungrouped pollutants to the outputs
			profile[specName] = x
		} else {
			// Add double counted pollutants to the droppedProfile
			droppedProfile[specName] = x
		}
	}
	rows.Close()

	if AbsBias(totalWeight, total) > 0.001 {
		err = fmt.Errorf("For Gas speciation profile %v, sum of species weights"+
			" (%v) does not match profile total (%v)", totalWeight, total)
		panic(err)
	}
	return
}

type reportData struct {
	totals map[string]float64
	units  map[string]string
}

// Match SCC in record to speciation profile. If none matches exactly, find a
// more general SCC that matches.
func (sp *SpecProf) getSccFracs(record *ParsedRecord, pol string, c *RunData,
	period string) (
	specFactors, droppedSpecFactors map[string]*specHolder) {
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

	if c.PolsToKeep[pol].SpecNames != nil {
		// For explicit species, convert the value to moles 
		// if required and add the emissions to a species group 
		// if one exists
		specFactors = sp.GetProfileSingleSpecies(
			c.PolsToKeep[pol].SpecNames[0], c)
	} else {
		switch c.PolsToKeep[pol].SpecType {
		case "VOC":
			code, ok := ref[pol]
			if ok {
				if code == "COMBO" { // for location specific speciation profiles
					countryCode := GetCountryCode(record.Country)
					codes := sp.sRefCombo[pol][countryCode+record.FIPS][period]
					specFactors = make(map[string]*specHolder)
					droppedSpecFactors = make(map[string]*specHolder)
					for code2, frac := range codes {
						tempSpecFactors := make(map[string]*specHolder)
						tempDroppedSpecFactors := make(map[string]*specHolder)
						tempSpecFactors, ok = sp.sPro[code2]
						if !ok || record.DoubleCountPols != nil {
							tempSpecFactors, tempDroppedSpecFactors =
								sp.GetProfileGas(code2, record.DoubleCountPols, c)
							if record.DoubleCountPols == nil {
								sp.sPro[code2] = tempSpecFactors
							}
						}
						for pol, val := range tempSpecFactors {
							if _, ok := specFactors[pol]; !ok {
								specFactors[pol] = new(specHolder)
								specFactors[pol].factor = val.factor * frac
								specFactors[pol].units = val.units
							} else {
								specFactors[pol].factor += val.factor * frac
								if specFactors[pol].units != val.units {
									panic("Units error!")
								}
							}
						}
						for pol, val := range tempDroppedSpecFactors {
							if _, ok := specFactors[pol]; !ok {
								droppedSpecFactors[pol] = new(specHolder)
								droppedSpecFactors[pol].factor = val.factor * frac
								droppedSpecFactors[pol].units = val.units
							} else {
								droppedSpecFactors[pol].factor += val.factor * frac
								if droppedSpecFactors[pol].units != val.units {
									panic("Units error!")
								}
							}
						}
					}
				} else {
					specFactors, ok = sp.sPro[code]
					if !ok || record.DoubleCountPols != nil {
						specFactors, droppedSpecFactors = sp.GetProfileGas(
							code, record.DoubleCountPols, c)
						if record.DoubleCountPols == nil {
							sp.sPro[code] = specFactors
						}
					}
				}
			} else { // no speciation profile reference is found
				specFactors = sp.DefaultProfile(pol)
			}
		case "PM2.5":
			panic("PM2.5 speciation not yet implemented")
		case "NOx":
			panic("NOx speciation not yet implemented")
		case "SOx":
			panic("SOx speciation not yet implemented")
		default:
			panic("In PolsToKeep, either `SpecNames' or `SpecType" +
				" needs to be specified. `SpecType' can only be VOC, " +
				"PM2.5, NOx, or SOx")
		}
	}
	return
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
func (c *RunData) speciate(MesgChan chan string, InputChan chan *ParsedRecord,
	OutputChan chan *ParsedRecord, period string) {
	defer c.ErrorRecoverCloseChan(MesgChan, InputChan)
	c.Log("Speciating "+period+" "+c.Sector+"...", 0)

	sp, err := c.NewSpecProf()
	if err != nil {
		panic(err)
	}

	r := new(reportData)
	r.totals = make(map[string]float64)
	r.units = make(map[string]string)
	totalDropped := make(map[string]float64)
	polFracs := make(map[string]*specHolder)
	droppedPolFracs := make(map[string]*specHolder)
	for record := range InputChan {
		newAnnEmis := make(map[string]*specValUnits)
		for pol, AnnEmis := range record.ANN_EMIS {
			polFracs, droppedPolFracs = sp.getSccFracs(
				record, pol, c, period)
			for newpol, factor := range polFracs {
				if _, ok := newAnnEmis[newpol]; ok {
					err = fmt.Errorf("Possible double counting of"+
						" pollutant %v SCC %v", newpol, record.SCC)
					panic(err)
				}
				newAnnEmis[newpol] = new(specValUnits)
				newAnnEmis[newpol].val = AnnEmis.val *
					c.InputConv * factor.factor
				newAnnEmis[newpol].units = strings.Replace(
					factor.units, "/gram", "/year", -1)
				r.totals[newpol] += newAnnEmis[newpol].val
			}
			for droppedpol, factor := range droppedPolFracs {
				totalDropped[droppedpol] += AnnEmis.val *
					c.InputConv * factor.factor
			}
		}
		record.ANN_EMIS = newAnnEmis
		OutputChan <- record
	}
	c.SpeciationReport(r, totalDropped, period)
	MesgChan <- "Finished speciating " + period + " " + c.Sector
	if OutputChan != TotalReportChan {
		close(OutputChan)
	}
}
