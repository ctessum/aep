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

import (
	"C"
	"fmt"
	"log"
	"os/exec"
	"strings"
	"encoding/csv"
	"strconv"
    "math"
)

func run_command(command string) string {
	//fmt.Printf("Running command %v\n", command)
	out, err := exec.Command("/bin/csh", "-c", command).Output()
    if err != nil {
        log.Fatal(err)
    }
	return string(out)
}

func approximately_equal(a float64, b float64, epsilon float64, variable string) bool {
	var val float64
	if math.Abs(a) < math.Abs(b) {
		val = b
	} else {
		val = a
	}
	if !(math.Abs(a - b) <= math.Abs(val) * epsilon) {
		fmt.Printf(variable + " - %v != %v (eps=%v)\n", a, b, epsilon)
		return false
	}
	return true
}

func arrays_approximately_equal(a []float64, b []float64, epsilon float64, variable string) bool {
    if a == nil && b == nil { 
        return true; 
    }
    if a == nil || b == nil { 
		fmt.Printf(variable + " - a or b is nil\n")
        return false; 
    }
    if len(a) != len(b) {
		fmt.Printf(variable + " - a (%v) and b (%v) have different length\n", len(a), len(b))
        return false
    }
	res := true
    for i := range a {
		if !approximately_equal(a[i], b[i], epsilon, variable) {
			res = false
		}
    }
    return res
}

func arrays_approximately_equal_2d(a [][]float64, b [][]float64, epsilon float64, variable string) bool {
    if a == nil && b == nil { 
        return true; 
    }
    if a == nil || b == nil { 
		fmt.Printf(variable + " - a or b is nil\n")
        return false; 
    }
    if len(a) != len(b) {
		fmt.Printf(variable + " - a (%v) and b (%v) have different length\n", len(a), len(b))
        return false
    }
	res := true
    for i := range a {
		if len(a[i]) != len(b[i]) {
			fmt.Printf(variable + " - a[%v] (%v) and b[%v] (%v) have different length\n", i, len(a[i]), i, len(b[i]))
			return false
		}
		for j:= range a[i] {
			if !approximately_equal(a[i][j], b[i][j], epsilon, variable) {
				res = false
			}
		}	
    }
    return res
}

func string_to_float(str []string) []float64 {
	var res []float64
	res = make([]float64, len(str))
	for i := range str {
		if v, err := strconv.ParseFloat(str[i], 64); err == nil {
			res[i] = v
		}
	}
	return res
}

func parse_netcdf_file(variable, file string) []float64 {
	// Use ncdump to extract data from NETCDF file
	cmd := "ncdump -v " + variable + " " + file
	res := run_command(cmd)
	
	// Convert ncdump output to csv string
	csv_string := strings.Split(strings.Split(res, variable + " =")[1], ";")[0] // extract comma separated values from ncdump output
	csv_string = strings.Replace(csv_string, "\n", "", -1) // remove newline characters
	csv_string = strings.Replace(csv_string, " ", "", -1) // remove space characters

	// Convert csv string to slice
    reader := csv.NewReader(strings.NewReader(csv_string))
	str_array, _ := reader.Read()
	float_array := string_to_float(str_array)
	
	return float_array
}

func CFloat_to_Float64(in []C.float) float64 {
	return float64(in[0])
}

func CFloat_to_Float64_array(in []C.float) []float64 {
	out := make([]float64, len(in))
	for i := range in {
		out[i] = float64(in[i])
	}
	return out
}

func Float64_to_CFloat(in float64) []C.float {
	out := make([]C.float, 1)
	out[0] = C.float(in)
	return out
}

func Float64_to_CFloat_array(in []float64) []C.float {
	out := make([]C.float, len(in))
	for i := range in {
		out[i] = C.float(in[i])
	}
	return out
}

func Convert1Dto2D_Cfloat(in []C.float, n int, m int) [][]float64 {
	return Convert1Dto2D(CFloat_to_Float64_array(in), n, m)
}

func Convert1Dto2D(in []float64, n int, m int) [][]float64 {
	out := make([][]float64, n)
    for i := range out {
        out[i] = make([]float64, m)
		copy(out[i], in[i*m : (i+1)*m])
    }
	return out
}

func Convert2Dto1D_Cfloat(in [][]float64, ) []C.float {
	return Float64_to_CFloat_array(Convert2Dto1D(in))
}

func Convert2Dto1D(in [][]float64) []float64 {
	n := len(in)
	m := len(in[0])
	out := make([]float64, n * m)
    for i := range in {
		copy(out[i*m : (i+1)*m], in[i])
    }
	return out
}

func allEquals(values []int) bool {
    for i := 1; i < len(values); i++ {
        if values[i] != values[0] {
            return false
        }
    }
    return true
}