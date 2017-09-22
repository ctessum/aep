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

func approximately_equal(a float64, b float64, epsilon float64) bool {
	var val float64
	if math.Abs(a) < math.Abs(b) {
		val = b
	} else {
		val = a
	}
	return math.Abs(a - b) <= math.Abs(val) * epsilon
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
		if !approximately_equal(a[i], b[i], epsilon) {
			fmt.Printf(variable + " - %v != %v (eps=%v)\n", a[i], b[i], epsilon)
			res = false
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