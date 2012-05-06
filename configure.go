package main

import (
	"encoding/json"
	"os"
	"io/ioutil"
	"fmt"
	"bufio"
"time"
)

func ReadConfigFile(filepath *string,sectors []string) (err error) {
	var (
		file  *os.File
		bytes []byte
	)
	if file, err = os.Open(*filepath); err != nil {
		return err
	}
	reader := bufio.NewReader(file)
	if bytes, err = ioutil.ReadAll(reader); err != nil {
		return err
	}

	var temp interface{}
	if err = json.Unmarshal(bytes,&temp); err != nil {
		return err
}

	c := temp.(map[string]interface{})

	
	SectorConfig := make([]RunData,len(sectors))

	// First, fill in all the default values
	for krd, rd := range SectorConfig {
fmt.Println(krd,rd)
    for k, v := range c {
        switch vv := v.(type) {
		case map[string]interface{}:
            fmt.Println(k, "is a map:")
            for i, u := range vv {
                fmt.Println(i, u)
            }
        }
    }
}

//	fmt.Println(c.Dirs.SmkHome)
//	fmt.Println(c.DefaultSettings.OutputType)
	return err
}


// type RunData is a container for the configuration and report info
type RunData struct {
	sector         string
	sectorType     string
	SmkHome        string
	input          string
	logs           string
	sectorLogs     string
	ancilliary     string
	startDate      *time.Time
	endDate        *time.Time
	tStepSec       int
	inventoryMonth string
	outputType     string
	srgDesc        string
	specSynonyms   string
	specRef        string
	specConv       string
	specPro        string
	specType       string
	specFrac       map[string]map[string]map[string]specHolder
	PolsToDrop     string
	caseName       string
	inventoryFreq  string
	inputUnits     string
	inputConv      float64
	invFileNames   string
	invFilePaths   string
	currentTime    *time.Time
	// report files
	InvRep  *os.File
	SpecRep *os.File
}

