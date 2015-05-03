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

func getCountryCode(country string) (code string) {
	switch country {
	case "USA":
		code = "0"
	case "CA": // Canada
		code = "1"
	case "CANADA":
		code = "1"
	case "MEXICO":
		code = "2"
	case "CUBA":
		code = "3"
	case "BAHAMAS":
		code = "4"
	case "HAITI":
		code = "5"
	case "DOMINICANREPUBLIC":
		code = "6"
	default:
		err := fmt.Errorf("Unknown country %v.", country)
		panic(err)
	}
	return
}
func getCountryName(code string) (country string) {
	switch code {
	case "0":
		country = "USA"
	case "1":
		country = "CA" // Canada
	case "2":
		country = "MEXICO"
	case "3":
		country = "CUBA"
	case "4":
		country = "BAHAMAS"
	case "5":
		country = "HAITI"
	case "6":
		country = "DOMINICANREPUBLIC"
	default:
		err := fmt.Errorf("Unknown country code %v.", code)
		panic(err)
	}
	return
}