package aep

import (
	"encoding/csv"
	"fmt"
	"io"
	"strconv"
	"strings"
	"time"
)

const comboCode = "COMBO"

// SpecRef holds information to map chemical speciation profiles to SCC codes.
type SpecRef struct {
	sr     map[SCC]map[Pollutant]string                                       // map[SCC][pol]code
	combo  map[Country]map[Pollutant]map[Period]map[string]map[string]float64 // map[country][Pollutant][Period][FIPS][code]frac
	srSCCs map[SCC]empty
}

// Get returns the speciation reference code(s) for the given SCC and pollutant
// at the given time and in the given FIPS location. The returned value is a map
// of different codes and the fraction of speciation that should be attributed to
// each of them. The fractions are guaranteed to sum to one. matchSCC and match
// specify whether exact matches of the corresponding codes are required, or
// whether more general codes will do if the exact codes are not present.
func (sr *SpecRef) Get(SCC string, pol Pollutant, t time.Time, FIPS string, matchSCC, matchFIPS bool) (map[string]float64, error) {
	if sr.srSCCs == nil {
		sr.srSCCs = make(map[string]empty)
		for scc := range sr.sr {
			sr.srSCCs[scc] = empty{}
		}
	}

	sccToUse := matchSCC(SCC, sr.srSCCs, matchSCC)

	code, ok := sr.sr[sccToUse][pol]
	if !ok {
		return nil, fmt.Errorf("aep.SpecRef.Get: no reference for SCC '%s' and pollutant %s",
			SCC, pol)
	}
	if code != comboCode {
		return map[string]float64{code: 1.}, nil
	}
}

func (sr *SpecRef) add(SCC SCC, pol Pollutant, code string) {
	if _, ok := sr.sr[SCC]; !ok {
		sr.sr[SCC] = make(map[Pollutant]string)
	}
	sr.sr[SCC][pol] = code
}

// Load reads the SMOKE gsref file, which maps SCC codes to chemical
// speciation profiles.
func (sr *SpecRef) Load(f io.Reader) (SpecRef, error) {
	specRef := make(SpecRef)

	r := csv.NewReader(f)
	r.Comma = ';'
	r.Comment = '#'
	for {
		record, err := r.Read()
		if err != nil {
			if err == io.EOF {
				break
			}
			return nil, fmt.Errorf("aep.ReadSpecRef: %v", err)
		}
		record = trimComment(record)

		scc := ParseSCC(record[0])
		code := trimString(record[1])
		pol := splitPol(record[2])

		specRef.add(scc, pol, code)
	}
	return specRef, nil
}

// trimComment gets rid of comments at the end of a line.
func trimComment(record []string) []string {
	if i := strings.Index(record[len(record)-1], "!"); i != -1 {
		record[len(record)-1] = record[len(record)-1][0:i]
	}
	return record
}

func (sr *SpecRef) addCombo(country Country, pol Pollutant, p Period, FIPS, code string, frac float64) {
	if _, ok := sr.combo[country]; !ok {
		sr.combo[country] = make(map[Pollutant]map[Period]map[string]map[string]float64)
	}
	if _, ok := sr.combo[country][pol]; !ok {
		sr.combo[country][pol] = make(map[Period]map[string]map[string]float64)
	}
	if _, ok := sr.combo[country][pol][p]; !ok {
		sr.combo[country][pol][p] = make(map[string]map[string]float64)
	}
	if _, ok := sr.combo[country][pol][p][FIPS]; !ok {
		sr.combo[country][pol][p][FIPS] = make(map[string]float64)
	}
	sr.combo[country][pol][p][FIPS][code] = frac
}

// LoadCombo reads the SMOKE gspro_combo file,
// which holds information to map chemical speciation profiles to
// SCC codes in cases where different FIPS codes have different speciation
// profiles.
func (sr *SpecRef) LoadCombo(f io.Reader) error {

	r := csv.NewReader(f)
	for {
		record, err = r.Read()
		if err != nil {
			if err == io.EOF {
				break
			}
			return fmt.Errorf("aep.ReadSpecRefCombo: %v", err)
		}
		record = trimComment(record)

		pol := splitPol(splitLine[0])
		// The FIPS number here is 6 characters instead of the usual 5.
		// The first character is a country code.
		FIPS := trimString(splitLine[1])
		country := getCountryFromID(FIPS[0])
		FIPS = FIPS[1:FIPS[len(FIPS)]]

		period, err := periodFromString(splitLine[2])
		if err != nil {
			return fmt.Errorf("aep.ReadSpecRefCombo: %v", err)
		}
		for i := 4; i < len(splitLine); i += 2 {
			code := strings.Trim(splitLine[i], "\n\" ")
			frac, err := strconv.ParseFloat(strings.Trim(splitLine[i+1], "\n\" "), 64)
			if err != nil {
				return fmt.Errorf("aep.ReadSpecRefCombo: %v", err)
			}
			specRef.addCombo(country, pol, period, FIPS, code, frac)
		}
	}
	return nil
}
