package main

import (
	"bitbucket.org/ctessum/gonetcdf"
	"bitbucket.org/ctessum/grassutils"
	"math"
	"fmt"
	"github.com/skelterjohn/go.matrix"
	"strings"
)

func MatrixSum(mat *matrix.SparseMatrix) (sum float64) {
	for j := 0; j < mat.Rows(); j++ {
		for i := 0; i < mat.Cols(); i++ {
			sum += mat.Get(j, i)
		}
	}
	return
}

// For cases where a specific code needs to be matched with a more
// general code. For instance, if code is "10101" and matchmap is
// {"10000":"xxx","10100":"yyyy"}, "10100" will be returned as the
// closest match to the input code. Returns an error if there is
// no match.
func MatchCode2(code string, matchmap map[string]map[string]string) (
	matchedCode string, err error) {
	l := len(code)
	for i := l - 1; i >= 0; i-- {
		matchedCode = code[0:i+1] + strings.Repeat("0", l-i-1)
		_, ok := matchmap[matchedCode]
		if ok {
			return
		}
	}
	err = fmt.Errorf("No matching code for %v", code)
	return
}
func MatchCode3(code string, matchmap map[string]string) (
	matchedCode string, err error) {
	l := len(code)
	for i := l - 1; i >= 0; i-- {
		matchedCode = code[0:i+1] + strings.Repeat("0", l-i-1)
		_, ok := matchmap[matchedCode]
		if ok {
			return
		}
	}
	err = fmt.Errorf("No matching code for %v", code)
	return
}
func MatchCode4(code string, matchmap map[string]map[string][3]string) (
	matchedCode string, err error) {
	l := len(code)
	for i := l - 1; i >= 0; i-- {
		matchedCode = code[0:i+1] + strings.Repeat("0", l-i-1)
		_, ok := matchmap[matchedCode]
		if ok {
			return
		}
	}
	err = fmt.Errorf("No matching code for %v", code)
	return
}
func MatchCode5(code string, matchmap map[string][3]string) (
	matchedCode string, err error) {
	l := len(code)
	for i := l - 1; i >= 0; i-- {
		matchedCode = code[0:i+1] + strings.Repeat("0", l-i-1)
		_, ok := matchmap[matchedCode]
		if ok {
			return
		}
	}
	err = fmt.Errorf("No matching code for %v", code)
	return
}

func Nc2sparse(nc *gonetcdf.NCfile, varname string, region *grassutils.Region) (
	mat *matrix.SparseMatrix, err error) {

	mat = matrix.ZerosSparse(region.NY, region.NX)
	vardims, err := nc.VarSize(varname)
	if err != nil {
		return
	}
	if region.NY != vardims[0] || region.NX != vardims[1] {
		err = fmt.Errorf("netCDF surrogate dimensions (%v x %v) do not match region dimensions (%v x %v)",
			region.NY, region.NX, vardims[0], vardims[1])
		return
	}
	vals, err := nc.GetVarDouble(varname)
	if err != nil {
		return
	}
	for j := 0; j < region.NY; j++ {
		for i := 0; i < region.NX; i++ {
			mat.Set(j, i, vals[region.NX*j+i])
		}
	}
	return
}

func Sparse2Nc(fname string, varname string, mat *matrix.SparseMatrix,
	region *grassutils.Region) (err error) {
	nc, err := gonetcdf.Open(fname, "write")
	if err != nil {
		panic(err)
	}
	err = nc.ReDef()
	if err != nil {
		panic(err)
	}
	err = nc.DefVar(varname, "double", []string{"ny", "nx"})
	if err != nil {
		panic(err)
	}
	err = nc.EndDef()
	if err != nil {
		panic(err)
	}
	vardims, err := nc.VarSize(varname)
	if err != nil {
		panic(err)
	}
	if region.NY != vardims[0] || region.NX != vardims[1] {
		err = fmt.Errorf("netCDF surrogate dimensions (%v x %v) do not match region dimensions (%v x %v)",
			region.NY, region.NX, vardims[0], vardims[1])
		panic(err)
	}

	temp := make([]float64, region.NY*region.NX)
	for j := 0; j < region.NY; j++ {
		for i := 0; i < region.NX; i++ {
			temp[region.NX*j+i] = mat.Get(j, i)
		}
	}
	err = nc.PutVarDouble(varname, temp)
	if err != nil {
		panic(err)
	}
	err = nc.Close()
	if err != nil {
		panic(err)
	}
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

func AbsBias(a, b float64) (o float64) {
	o = math.Abs(a-b) / b
	return
}
