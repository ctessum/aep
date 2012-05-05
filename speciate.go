package main

import (
	"os"
	"path/filepath"
	"bufio"
	"strconv"
	"strings"
	"fmt"
	"sort"
)

// SCCdesc reads the smoke sccdesc file, which gives descriptions for each SCC code.
func SCCdesc(filename string) (map[string]string, os.Error) {
	sccDesc := make(map[string]string)
	var record string
	fid, err := os.Open(filename)
	buf := bufio.NewReader(fid)
	defer fid.Close()
	if err != nil {
		return sccDesc, os.NewError("SCCdesc: " + err.String() + "\nFile= " + filename + "\nRecord= " + record)
	}
	for {
		record, err = buf.ReadString('\n')
		if err != nil {
			if err.String() == "EOF" {
				err = nil
				break
			} else {
				return sccDesc, os.NewError(filename + "\n" + record + "\n" + err.String() + "\nFile= " + filename + "\nRecord= " + record)
			}
		}
		// Get rid of comments at end of line.
		if i := strings.Index(record, "!"); i != -1 {
			record = record[0:i]
		}
		if record[0] != '#' {
			splitLine := strings.Split(record, ";")
			sccDesc[strings.Trim(splitLine[0], "\"")] = strings.Trim(splitLine[1], "\"")
		}
	}
	return sccDesc, err
}
// SpecRef reads the SMOKE gsref file, which maps SCC codes to chemical speciation profiles.
func (c *RunData) SpecRef(filename string) (map[string]map[string]string, os.Error) {
	specRef := make(map[string]map[string]string)
	var record string
	fid, err := os.Open(filename)
	buf := bufio.NewReader(fid)
	defer fid.Close()
	if err != nil {
		return specRef, os.NewError("SpecRef: " + err.String() + "\nFile= " + filename + "\nRecord= " + record)
	}
	//	// For point sources, don't use beginning of file
	//	readSection := false
	//	if c.sectorType != "point" {
	//		readSection = true
	//	}
	for {
		record, err = buf.ReadString('\n')
		if err != nil {
			if err.String() == "EOF" {
				err = nil
				break
			} else {
				return specRef, os.NewError(filename + "\n" + record + "\n" + err.String() + "\nFile= " + filename + "\nRecord= " + record)
			}
		}
		// Get rid of comments at end of line.
		if i := strings.Index(record, "!"); i != -1 {
			record = record[0:i]
		}

		//		// For non-point sources, don't use end of file
		//		if strings.Index(record, "/POINT DEFN/") != -1 {
		//			if c.sectorType != "point" {
		//				readSection = false
		//				break
		//			} else {
		//				readSection = true
		//				continue
		//			}
		//		}
		//		if record[0] != '#' && readSection == true {
		if record[0] != '#' && record[0] != '/' {
			// for point sources, only match to SCC code.
			splitLine := strings.Split(record, ";")
			SCC := strings.Trim(splitLine[0], "\"")
			if len(SCC) == 8 {
				SCC = "00" + SCC
			}
			code := strings.Trim(splitLine[1], "\"")
			pol := strings.Trim(splitLine[2], "\"\n")

			_, ok := specRef[SCC]
			if !ok {
				y := make(map[string]string)
				y[pol] = code
				specRef[SCC] = y
			} else {
				specRef[SCC][pol] = code
			}
		}
	}
	return specRef, err
}

type cnvHolder struct {
	newpol string
	factor float64
}

// SpecConv reads the SMOKE gscnv file, which gives ratios (usually VOC to TOG) for chemical speciation profiles.
func SpecConv(filename string) (map[string]map[string]cnvHolder, os.Error) {
	specConv := make(map[string]map[string]cnvHolder)
	var record string
	fid, err := os.Open(filename)
	buf := bufio.NewReader(fid)
	defer fid.Close()
	if err != nil {
		return specConv, os.NewError("SpecConv: " + err.String() + "\nFile= " + filename + "\nRecord= " + record)
	}
	for {
		record, err = buf.ReadString('\n')
		if err != nil {
			if err.String() == "EOF" {
				err = nil
				break
			} else {
				return specConv, os.NewError(filename + "\n" + record + "\n" + err.String() + "\nFile= " + filename + "\nRecord= " + record)
			}
		}
		// Get rid of comments at end of line.
		if i := strings.Index(record, "!"); i != -1 {
			record = record[0:i]
		}
		if record[0] != '#' {
			splitLine := strings.Split(record, ";")
			code := strings.Trim(splitLine[2], "\"")
			pol := strings.Trim(splitLine[0], "\"")
			newpol := strings.Trim(splitLine[1], "\"")
			factor := strings.Trim(splitLine[3], "\"\n")

			var x cnvHolder
			x.newpol = newpol
			x.factor, err = strconv.Atof64(factor)
			if err != nil {
				return specConv, os.NewError("SpecConv: " + err.String() + "\nFile= " + filename + "\nRecord= " + record)
			}
			_, ok := specConv[code]
			if !ok {
				y := make(map[string]cnvHolder)
				y[pol] = x
				specConv[code] = y
			} else {
				specConv[code][pol] = x
			}
		}
	}
	return specConv, err
}

type proHolder struct {
	molfrac  float64
	moldiv   float64
	massfrac float64
}

// SpecPro reads the SMOKE gspro file, which gives the speciation fractions for each chemical speciation profile.
func SpecPro(filename string) (map[string]map[string]map[string]proHolder, os.Error) {
	specPro := make(map[string]map[string]map[string]proHolder)
	var record string
	fid, err := os.Open(filename)
	buf := bufio.NewReader(fid)
	defer fid.Close()
	if err != nil {
		return specPro, os.NewError("SpecPro: " + err.String() + "\nFile= " + filename + "\nRecord= " + record)
	}
	for {
		record, err = buf.ReadString('\n')
		if err != nil {
			if err.String() == "EOF" {
				err = nil
				break
			} else {
				return specPro, os.NewError(filename + "\n" + record + "\n" + err.String() + "\nFile= " + filename + "\nRecord= " + record)
			}
		}
		// Get rid of comments at end of line.
		if i := strings.Index(record, "!"); i != -1 {
			record = record[0:i]
		}
		if record[0] != '#' {
			splitLine := strings.Split(record, ";")
			code := strings.Trim(splitLine[0], "\"")
			oldpol := strings.Trim(splitLine[1], "\"")
			newpol := strings.Trim(splitLine[2], "\"")
			molfrac := strings.Trim(splitLine[3], "\"")
			moldiv := strings.Trim(splitLine[4], "\"")
			massfrac := strings.Trim(splitLine[5], "\"\n")

			var x proHolder
			x.molfrac, err = strconv.Atof64(molfrac)
			if err != nil {
				return specPro, os.NewError("SpecPro: " + err.String() + "\nFile= " + filename + "\nRecord= " + record)
			}
			x.moldiv, err = strconv.Atof64(moldiv)
			if err != nil {
				return specPro, os.NewError("SpecPro: " + err.String() + "\nFile= " + filename + "\nRecord= " + record)
			}
			x.massfrac, err = strconv.Atof64(massfrac)
			if err != nil {
				return specPro, os.NewError("SpecPro: " + err.String() + "\nFile= " + filename + "\nRecord= " + record)
			}

			if _, ok := specPro[code]; !ok {
				z := make(map[string]proHolder)
				z[oldpol] = x
				y := make(map[string]map[string]proHolder)
				y[newpol] = z
				specPro[code] = y
			} else if _, ok := specPro[code][oldpol]; !ok {
				y := make(map[string]proHolder)
				y[newpol] = x
				specPro[code][oldpol] = y
			} else {
				specPro[code][oldpol][newpol] = x
			}
		}
	}
	// Normalize speciation fractions in case they don't sum to one.
	for code, oldpols := range specPro {
		for oldpol, newpols := range oldpols {
			molTotal := 0.
			massTotal := 0.
			for _, data := range newpols {
				molTotal += data.molfrac
				massTotal += data.massfrac
			}
			for newpol, data := range newpols {
				data.molfrac /= molTotal
				data.massfrac /= massTotal
				specPro[code][oldpol][newpol] = data
			}
		}
	}
	return specPro, err
}
// specSynonyms reads a file of species names that should be replaced when matching records with speciation profiles. The file should be in the format `oldname;newname' (semicolon deliminited).
func specSynonyms(filename string) (map[string]string, os.Error) {
	specSyns := make(map[string]string)
	var record string
	fid, err := os.Open(filename)
	buf := bufio.NewReader(fid)
	defer fid.Close()
	if err != nil {
		return specSyns, os.NewError("SpecSyns: " + err.String() + "\nFile= " + filename + "\nRecord= " + record)
	}
	for {
		record, err = buf.ReadString('\n')
		if err != nil {
			if err.String() == "EOF" {
				err = nil
				break
			} else {
				return specSyns, os.NewError(filename + "\n" + record + "\n" + err.String() + "\nFile= " + filename + "\nRecord= " + record)
			}
		}
		// Get rid of comments at end of line.
		if i := strings.Index(record, "!"); i != -1 {
			record = record[0:i]
		}
		if record[0] != '#' {
			splitLine := strings.Split(record, ";")
			specSyns[strings.Trim(splitLine[0], "\"")] = strings.Trim(splitLine[1], "\"")
		}
	}
	return specSyns, err
}
func polTrans(pol string, specSynonym map[string]string) (polout string) {
	polout, ok := specSynonym[pol]
	if !ok {
		polout = pol
	}
	return
}

type specHolder struct {
	factor float64
	units  string
}

func (c *RunData) SpecFractions(refFile, convFile, proFile, synFile, specType string) os.Error {
	c.specFrac = make(map[string]map[string]map[string]specHolder)

	sRef, err := c.SpecRef(refFile)
	if err != nil {
		return err
	}
	sConv, err := SpecConv(convFile)
	if err != nil {
		return err
	}
	sPro, err := SpecPro(proFile)
	if err != nil {
		return err
	}
	sSyn, err := specSynonyms(synFile)
	if err != nil {
		return err
	}

	var x specHolder
	for SCC, polRef := range sRef {
		for pol, code := range polRef {
			var cnvfactor float64
			var cnvPol string

			// set up map tables
			if _, ok := c.specFrac[SCC]; !ok {
				z := make(map[string]specHolder)
				y := make(map[string]map[string]specHolder)
				y[pol] = z
				c.specFrac[SCC] = y
			} else if _, ok := c.specFrac[SCC][pol]; !ok {
				y := make(map[string]specHolder)
				c.specFrac[SCC][pol] = y
			}

			// test if speciation code is in specConv data, assign pollutant conversion to 1.0 if not.
			cnvData, ok1 := sConv[code]
			_, ok2 := sConv[code][pol]
			if !(ok1 && ok2) {
				cnvfactor = 1.0
				cnvPol = pol
			} else {
				cnvfactor = cnvData[polTrans(pol, sSyn)].factor
				cnvPol = cnvData[polTrans(pol, sSyn)].newpol
			}
			procode, ok1 := sPro[code]
			poldata, ok2 := procode[polTrans(cnvPol, sSyn)]
			if !(ok1 && ok2) {
				x.factor = 1.0
				x.units = "g/year"
				c.specFrac[SCC][pol][cnvPol] = x
			} else {
				for newpol, fracdata := range poldata {
					if specType == "mol" {
						x.factor = cnvfactor * fracdata.molfrac / fracdata.moldiv
						// if speciation profile divider (moldiv) = 1, assume that it is a mass to mass
						// speciation. Otherwise, assume a mass to mol speciation.
						if fracdata.moldiv == 1. {
							x.units = "g/year"
						} else {
							x.units = "mol/year"
						}
					} else if specType == "mass" {
						x.factor = cnvfactor * fracdata.massfrac
						x.units = "g/year"
					} else {
						return os.NewError("In SpecFractions, speciation type " + specType + " is unknown. Please choose `mol' or `mass'")
					}
					c.specFrac[SCC][pol][newpol] = x
				}
			}
		}
	}
	return err
}

type specValUnits struct {
	val   float64
	units string
}

type reportData struct {
	totals map[string]float64
	units  map[string]string
}

// check to make sure units in this record are the same as the 
// other units for this pollutant in the other records that have
// been summed.
func (r *reportData) checkUnits(x specValUnits, newpol string) {
	if r.units[newpol] != "" {
		if r.units[newpol] != x.units {
			panic("Units for pol " + newpol + " are not consistent between records.")
		}
	} else {
		r.units[newpol] = x.units
	}
}

func (c *RunData) Speciate(MesgChan chan string, InvSpecChan chan ParsedRecord, period string) {
	defer func() {
		if err := recover(); err != nil {
			// Handle error
			c.ErrorReport(err)
			MesgChan <- c.sector + " failed!"
		}
	}()
	r := new(reportData)
	r.totals = make(map[string]float64)
	r.units = make(map[string]string)
	totalDropped := make(map[string]float64)
	var x specValUnits
	for record := range InvSpecChan {
		sccFracs, ok := c.specFrac[record.SCC]
		if !ok {
			panic("In Speciate, SCC code " + record.SCC + " is not in specRef file.")
		}
		for pol, AnnEmis := range record.ANN_EMIS {
			// check if there is a speciation record for this 
			// SCC/pollutant combination. If there is no record,
			// check if there is a record for the SCC "0000000000"
			// which is a unversal record. If not, check if the pollutant
			// is in the list of pollutants to drop. If it is not in the
			// drop list, transfer it to the speciated record without any
			// speciating. Otherwise, add it to the totals of dropped 
			// pollutants. If the is a speciation record, multiply the 
			// annual emissions by the speciation factors. Multiply all
			// speciated emissions by a conversion factor from the input
			// units to g/year.
			if polFracs, ok := sccFracs[pol]; ok {
				for newpol, frac := range polFracs {
					x.val = AnnEmis * frac.factor * c.inputConv
					x.units = frac.units
					r.checkUnits(x, newpol)
					record.SpecAnnEmis[newpol] = x
					r.totals[newpol] += record.SpecAnnEmis[newpol].val
				}
			} else if strings.Index(c.PolsToDrop, pol) != -1 {
				totalDropped[pol] += AnnEmis * c.inputConv
			} else if polfracs, ok := c.specFrac["0000000000"][pol]; ok {
				for newpol, frac := range polfracs {
					x.val = AnnEmis * frac.factor * c.inputConv
					x.units = frac.units
					r.checkUnits(x, newpol)
					record.SpecAnnEmis[newpol] = x
					r.totals[newpol] += record.SpecAnnEmis[newpol].val
				}
			} else {
				newpol := pol
				x.val = AnnEmis * c.inputConv
				x.units = "g/year"
				r.checkUnits(x, newpol)
				record.SpecAnnEmis[newpol] = x
				r.totals[newpol] += record.SpecAnnEmis[newpol].val
			}
		}
	}
	c.speciationReport(r, totalDropped, period)
	MesgChan <- "Finished speciating " + period + " " + c.sector
}

func (c *RunData) speciationReport(r *reportData,  totalDropped map[string]float64, period string) {
	err := os.MkdirAll(c.sectorLogs, uint32(0776))
	if err != nil {
		panic(err)
	}
	repStr := ""
	if c.SpecRep != nil {
		repStr += "\n\n"
	} else {
		c.SpecRep, err = os.Create(filepath.Join(c.sectorLogs, c.sector+"_speciation.csv"))
		if err != nil {
			panic(err)
		}
	}
	var polNamesTemp string
	for pol, _ := range r.totals {
		if strings.Index(polNamesTemp, pol) == -1 {
			polNamesTemp += " " + pol
		}
	}
	var droppedPolNamesTemp string
	for pol, _ := range totalDropped {
		if strings.Index(droppedPolNamesTemp, pol) == -1 {
			droppedPolNamesTemp += " " + pol
		}
	}
	polNames := strings.Split(polNamesTemp, " ")
	sort.Strings(polNames)
	droppedPolNames := strings.Split(droppedPolNamesTemp, " ")
	sort.Strings(droppedPolNames)

	repStr += "Speciated totals\n"
	repStr += fmt.Sprintf("Sector: %s\n", c.sector)
	repStr += fmt.Sprintf("Time period: %s\n", period)
	for _, pol := range polNames {
		repStr += fmt.Sprintf("%s,", pol)
	}
	repStr += "\n"
	for _, pol := range polNames {
		repStr += fmt.Sprintf("%s,", r.units[pol])
	}
	repStr += "\n"
	for _, pol := range polNames {
		repStr += fmt.Sprintf("%e,", r.totals[pol])
	}
	repStr += "\n\nTotals of dropped pollutants (g/year)\n"
	for _, pol := range droppedPolNames {
		repStr += fmt.Sprintf("%s,", pol)
	}
	repStr += "\n"
	for _, pol := range droppedPolNames {
		repStr += fmt.Sprintf("%e,", totalDropped[pol])
	}
	repStr += "\n"
	fmt.Fprint(c.SpecRep, repStr)
	return
}
