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

const tolerance = 1.e-9 // fractional difference between two numbers where they can be considered the same

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

		if record[0] != '#' && record[0] != '/' {
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

type SpecProf struct {
	db        *sql.DB
	sRef      map[string]map[string]string             // map[SCC][pol]code
	sRefCombo map[string]map[string]map[string]float64 // map[pol][FIPS][code]frac
}

func (c *RunData) NewSpecProf() (sp *SpecProf, err error) {
	sp = new(SpecProf)
	sp.db, err = sql.Open("sqlite3", c.SpecProFile)

	sp.sRef, err = c.SpecRef()
	if err != nil {
		return
	}
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
	profile map[string]*specHolder, droppedNotInGroup map[string]*specHolder) {
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
	if tempgroupfactor.Valid && !c.testMode {
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
	if group.Valid { // If pollutant is part of a species group, add to the group
		profile = make(map[string]*specHolder)
		profile[group.String] = x
		profile[group.String].factor *= groupfactor
	} else {
		droppedNotInGroup = make(map[string]*specHolder)
		droppedNotInGroup[pol] = x
	}
	return
}

func (sp *SpecProf) GetProfileGas(number string,
	doubleCountPols []string, c *RunData) (
	profile map[string]*specHolder,
	droppedDoubleCount map[string]*specHolder,
	droppedNotInGroup map[string]*specHolder) {

	profile = make(map[string]*specHolder)
	droppedDoubleCount = make(map[string]*specHolder)
	droppedNotInGroup = make(map[string]*specHolder)
	var err error
	var total float64
	var tempConvFac sql.NullFloat64
	var convFac float64
	err = sp.db.QueryRow("select TOTAL,VOCtoTOG from GAS_PROFILE where"+
		" P_NUMBER=?", number).Scan(&total, &tempConvFac)
	if err != nil {
		panic(err)
	}
	if tempConvFac.Valid && !c.testMode {
		convFac = tempConvFac.Float64
	} else {
		convFac = 1.
		msg := fmt.Sprintf("VOC to TOG conversion factor is missing for "+
			"SPECIATE pollutant ID %v. Setting it to 1.0.", number)
		c.Log(msg, 1)
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
				100. / MW * 100 / total
			x.units = "mol/gram"
		case "mass":
			x.factor = convFac * weightPercent /
				100. * 100 / total
			x.units = "gram/gram"
		default:
			err = fmt.Errorf("Invalid specType %v. Options are \"mass\" "+
				"and \"mol\"", c.SpecType)
		}
		if group.Valid && !IsStringInArray(doubleCountPols, specName) {
			// If pollutant is part of a species group, and it isn't
			// double counting an explicit emissions record,
			// add to the group
			if _, ok := profile[group.String]; !ok {
				profile[group.String] = new(specHolder)
				profile[group.String].units = x.units
			} else if x.units != profile[group.String].units {
				err = fmt.Errorf("Units %v and %v don't match", x.units,
					profile[group.String].units)
			}
			profile[group.String].factor += x.factor * groupfactor
		} else if !group.Valid {
			// ungrouped pollutants
			droppedNotInGroup[specName] = x
		} else {
			// double counted pollutants
			droppedDoubleCount[specName] = x
		}
	}
	rows.Close()

	if c.testMode && AbsBias(totalWeight, total) > tolerance {
		err = fmt.Errorf("For Gas speciation profile %v, sum of species weights"+
			" (%v) is not equal to total (%v)", number, totalWeight, total)
		panic(err)
	}
	return
}

// Match SCC in record to speciation profile. If none matches exactly, find a
// more general SCC that matches.
func (sp *SpecProf) getSccFracs(record *ParsedRecord, pol string, c *RunData,
	period string) (
	specFactors, doubleCountSpecFactors,
	ungroupedSpecFactors map[string]*specHolder) {
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
		specFactors, ungroupedSpecFactors = sp.GetProfileSingleSpecies(
			c.PolsToKeep[pol].SpecNames[0], c)
	} else {
		switch c.PolsToKeep[pol].SpecType {
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
					specFactors = make(map[string]*specHolder)
					doubleCountSpecFactors = make(map[string]*specHolder)
					ungroupedSpecFactors = make(map[string]*specHolder)
					for code2, frac := range codes {
						tempSpecFactors := make(map[string]*specHolder)
						tempDoubleCountSpecFactors := make(map[string]*specHolder)
						tempUngroupedSpecFactors := make(map[string]*specHolder)
						tempSpecFactors, tempDoubleCountSpecFactors,
							tempUngroupedSpecFactors =
							sp.GetProfileGas(code2, record.DoubleCountPols, c)
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
						for pol, val := range tempDoubleCountSpecFactors {
							if _, ok := doubleCountSpecFactors[pol]; !ok {
								doubleCountSpecFactors[pol] = new(specHolder)
								doubleCountSpecFactors[pol].factor = val.factor * frac
								doubleCountSpecFactors[pol].units = val.units
							} else {
								doubleCountSpecFactors[pol].factor += val.factor * frac
								if doubleCountSpecFactors[pol].units != val.units {
									panic("Units error!")
								}
							}
						}
						for pol, val := range tempUngroupedSpecFactors {
							if _, ok := ungroupedSpecFactors[pol]; !ok {
								ungroupedSpecFactors[pol] = new(specHolder)
								ungroupedSpecFactors[pol].factor = val.factor * frac
								ungroupedSpecFactors[pol].units = val.units
							} else {
								ungroupedSpecFactors[pol].factor += val.factor * frac
								if ungroupedSpecFactors[pol].units != val.units {
									panic("Units error!")
								}
							}
						}
					}
				} else {
					specFactors, doubleCountSpecFactors,
						ungroupedSpecFactors = sp.GetProfileGas(
						code, record.DoubleCountPols, c)
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
	if c.testMode { // fractions only add up to 1 in test mode.
		fracSum := 0.
		for _, data := range specFactors {
			fracSum += data.factor
		}
		for _, data := range doubleCountSpecFactors {
			fracSum += data.factor
		}
		for _, data := range ungroupedSpecFactors {
			fracSum += data.factor
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
func (c *RunData) speciate(MesgChan chan string, InputChan chan *ParsedRecord,
	OutputChan chan *ParsedRecord, period string) {
	defer c.ErrorRecoverCloseChan(MesgChan, InputChan)
	c.Log("Speciating "+period+" "+c.Sector+"...", 0)

	sp, err := c.NewSpecProf()
	if err != nil {
		panic(err)
	}

	totals := newSpeciationTotalHolder()
	polFracs := make(map[string]*specHolder)
	doubleCountPolFracs := make(map[string]*specHolder)
	ungroupedPolFracs := make(map[string]*specHolder)
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
					c.InputConv * factor.factor
				newAnnEmis[newpol].Units = strings.Replace(
					factor.units, "/gram", "/year", -1)
				totals.AddKept(newpol, newAnnEmis[newpol])
			}
			for droppedpol, factor := range doubleCountPolFracs {
				totals.AddDoubleCounted(droppedpol, AnnEmis.Val*
					c.InputConv*factor.factor, strings.Replace(
					factor.units, "/gram", "/year", -1))
			}
			for droppedpol, factor := range ungroupedPolFracs {
				totals.AddUngrouped(droppedpol, AnnEmis.Val*
					c.InputConv*factor.factor, strings.Replace(
					factor.units, "/gram", "/year", -1))
			}
		}
		record.ANN_EMIS = newAnnEmis
		OutputChan <- record
	}
	Report.SectorResults[c.Sector][period].
		SpeciationResults = totals

	MesgChan <- "Finished speciating " + period + " " + c.Sector
	if OutputChan != TotalReportChan {
		close(OutputChan)
	}
}
