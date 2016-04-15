/*
Copyright (C) 2012-2014 Regents of the University of Minnesota.
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
	"math"
	"strings"
)

// SCC is the US EPA's Source Classification Code:
// https://www.cmascenter.org/smoke/documentation/3.6.5/html/ch02s03s05.html.
type SCC struct {
	Tier1, Tier2, Tier3, Tier4 int
}

func cleanSCC(scc string) string {
	scc = trimString(scc)
	if len(scc) == 8 {
		scc = "00" + scc
	} else if len(scc) == 7 {
		scc = "00" + scc + "0"
	} else if len(scc) == 6 {
		scc = "00" + scc + "00"
	} else if len(scc) == 5 {
		scc = "00" + scc + "000"
	} else if len(scc) == 4 {
		scc = "00" + scc + "0000"
	} else if len(scc) == 3 {
		scc = "00" + scc + "00000"
	} else if len(scc) == 2 {
		scc = "00" + scc + "000000"
	}
	return scc
}

// ParseSCC takes and SCC in string format and returns an SCC object.
func ParseSCC(scc string) SCC {
	scc = cleanSCC
	if scc[0:2] == "00" { // 8 digit code
		return SCC{
			Tier1: scc[2:3],
			Tier2: scc[3:5],
			Tier3: scc[5:8],
			Tier4: scc[8:10],
		}
	}
	return SCC{ // 10 digit code
		Tier1: scc[0:2],
		Tier2: scc[2:4],
		Tier3: scc[4:7],
		Tier4: scc[7:10],
	}
}

func (s SCC) String() string {
	return s.Tier1 + s.Tier2 + s.Tier3 + s.Tier4
}

// matchSCC matches scc with one of sccs. If matchFull is true, an exact
// match must be made, otherwise the closest more general SCC is returned.
func matchSCC(scc string, sccs map[string]empty, matchFull bool) string {

}

// For cases where a specific code needs to be matched with a more
// general code. For instance, if code is "10101" and matchmap is
// {"10102":"xxx","10100":"yyyy"}, "10100" will be returned as the
// closest match to the input code. "10102" will never be returned,
// even if the "10100" item didn't exist. Returns an error if there
// is no match.
func MatchCode(code string, matchmap map[string]interface{}) (
	matchedCode string, matchVal interface{}, err error) {
	var ok bool
	l := len(code)
	for i := l - 1; i >= -1; i-- {
		matchedCode = code[0:i+1] + strings.Repeat("0", l-i-1)
		matchVal, ok = matchmap[matchedCode]
		if ok {
			return
		}
	}
	err = fmt.Errorf("No matching code for %v", code)
	return
}
func MatchCodeDouble(code1, code2 string,
	matchmap map[string]map[string]interface{}) (
	matchedCode1, matchedCode2 string, matchVal interface{}, err error) {
	l1 := len(code1)
	l2 := len(code2)
	for i := l1 - 1; i >= -1; i-- {
		matchedCode1 = code1[0:i+1] + strings.Repeat("0", l1-i-1)
		match1, ok := matchmap[matchedCode1]
		if ok {
			for i := l2 - 1; i >= -1; i-- {
				matchedCode2 = code2[0:i+1] + strings.Repeat("0", l2-i-1)
				matchVal, ok = match1[matchedCode2]
				if ok {
					return
				}
			}
		}
	}
	err = fmt.Errorf("No matching codes for %v, %v", code1, code2)
	return
}

func IsStringInArray(a []string, s string) bool {
	for _, val := range a {
		if val == s {
			return true
		}
	}
	return false
}

func absBias(a, b float64) (o float64) {
	o = math.Abs(a-b) / b
	return
}

type Country int

const (
	USA               Country = 0
	Canada                    = 1
	Mexico                    = 2
	Cuba                      = 3
	Bahamas                   = 4
	Haiti                     = 5
	DominicanRepublic         = 6
	Unknown                   = -1
)

func (c Country) String() string {
	switch c {
	case USA:
		return "USA"
	case Canada:
		return "CA"
	case Mexico:
		return "MEXICO"
	case Cuba:
		return "CUBA"
	case Bahamas:
		return "BAHAMAS"
	case Haiti:
		return "HAITI"
	case DominicanRepublic:
		return "DOMINICANREPUBLIC"
	default:
		panic(fmt.Errorf("Unknown country %v", c))
	}
}

func getCountryCode(country Country) string {
	return fmt.Sprintf("%d", country)
}
func getCountryFromID(code string) Country {
	switch code {
	case "0":
		return USA
	case "1":
		return Canada
	case "2":
		return Mexico
	case "3":
		return Cuba
	case "4":
		return Bahamas
	case "5":
		return Haiti
	case "6":
		return DominicanRepublic
	default:
		err := fmt.Errorf("Unknown country code %v.", code)
		panic(err)
	}
}

func countryFromName(name string) (Country, error) {
	switch name {
	case "USA", "US":
		return USA, nil
	case "CANADA", "CA", "CAN":
		return Canada, nil
	case "MEXICO", "MEX":
		return Mexico, nil
	case "CUBA":
		return Cuba, nil
	case "BAHAMAS":
		return Bahamas, nil
	case "HAITI":
		return Haiti, nil
	case "DOMINICANREPUBLIC":
		return DominicanRepublic, nil
	default:
		return Unknown, fmt.Errorf("Unkown country '%s'", name)
	}

}
