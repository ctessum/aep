package main

import (
	"encoding/json"
	"os"
	"io/ioutil"
	"fmt"
)

func ReadConfigFile(*filepath string) (err os.Error) {
	fmt.Println("success")
	var (
		file  *os.File
		bytes []byte
	)
	if file, err = os.Open(filepath); err != nil {
		return err
	}
	reader := bufio.NewReader(file)
	if bytes, err = ioutil.Readall(reader); err != nil {
		return err
	}
	fmt.Println("success")

	return err
}

type dirstruct struct {
	SmkHome    string
	input      string
	ancilliary string
	logs       string
}

type settingsStruct struct {
	startDate  string
	endDate    string
	tStepSec   int
	outputType string
	specType   string
	inputUnits string
}

type configfile struct {
	dirs            dirstruct
	defaultSettings settingsStruct
}
