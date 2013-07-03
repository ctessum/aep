package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

var (
	TotalReportChan = make(chan *ParsedRecord)
	Report          = new(ReportHolder)
	Status          *StatusHolder
)

func main() {
	log.Println("\n",
		"-------------------------------------\n",
		"             Welcome!\n",
		"Program (A)ir (E)missions (P)rocessor\n",
		"   Copyright 2012 Chris Tessum\n",
		"-------------------------------------\n")

	// Read from configuration file and prepare sectors for processing
	var sectorFlag *string = flag.String("sectors", "all", "List of sectors to process, in quotes, separated by spaces")
	var configFile *string = flag.String("config", "none", "Path to configuration file")
	var reportOnly *bool = flag.Bool("reportonly", false, "Run html report server for results of previous run (do not calculate new results)")
	var testmode *bool = flag.Bool("testmode", false, "Run model with mass speciation and no VOC to TOG conversion so that results can be validated by go test")
	flag.Parse()

	if *configFile == "none" {
		fmt.Println("Please set `-config' flag and run again: ie: aep -config=/path/to/config_file")
		fmt.Println("For more information try typing `aep --help'")
		return
	}
	e := new(ErrCat) // error report

	// create list of sectors
	sectors := strings.Split(*sectorFlag, " ")

	// parse configuration file
	ConfigAll := ReadConfigFile(configFile, testmode, e)

	if *reportOnly {
		// The reportServer function will run forever (go to localhost:8080 in web browser to view report).
		file := filepath.Join(ConfigAll.Dirs.Logs, "Report.json")
		f, err := os.Open(file)
		if err != nil {
			panic(err)
		}
		reader := bufio.NewReader(f)
		bytes, err := ioutil.ReadAll(reader)
		if err != nil {
			panic(err)
		}
		err = json.Unmarshal(bytes, Report)
		if err != nil {
			panic(err)
		}
		Report.ReportOnly = *reportOnly
		ReportServer()
	}

	Report.ReportOnly = *reportOnly
	Report.Config = ConfigAll.DefaultSettings
	Report.SectorResults = make(map[string]map[string]*Results)

	// track status of all of the running sectors
	Status = NewStatus()

	// Start server for html report (go to localhost:6060 in web browser to view report)
	// Here, we run the report server in the background while the rest of the program is running.
	go ReportServer()

	runChan := make(chan string, 1)

	runtime.GOMAXPROCS(ConfigAll.DefaultSettings.Ncpus)
	if ConfigAll.DefaultSettings.CreateTotalInventoryReport {
		// set up TotalInventoryReport subroutine
		go ConfigAll.DefaultSettings.TotalInventoryReport(runChan)
	} else {
		go DiscardRecords(runChan)
	}

	// Start server for retrieving profiles from the
	// SPECIATE database
	go ConfigAll.DefaultSettings.SpecProfiles(e)

	// Set up spatial environment
	if ConfigAll.DefaultSettings.Spatialize {
		ConfigAll.DefaultSettings.SpatialSetup(e)
	}

	// Set up temporal environment
	if ConfigAll.DefaultSettings.RunTemporal {
		ConfigAll.DefaultSettings.TemporalSetup(e)
	}

	e.Report() // Print errors, if any

	// run sector subroutines
	var n, numAnnualSectors, numMonthlySectors int
	for sector, c := range ConfigAll.Sectors {
		if sectors[0] == "all" || IsStringInArray(sectors, sector) {
			Report.SectorResults[sector] = make(map[string]*Results)
			go c.Run(runChan)
			switch c.InventoryFreq {
			case "annual":
				numAnnualSectors++
			case "monthly":
				numMonthlySectors++
			}
			n++
		}
	}
	// run temporal subroutine
	if ConfigAll.DefaultSettings.RunTemporal {
		go ConfigAll.DefaultSettings.Temporal(numAnnualSectors, numMonthlySectors)
	}

	// wait for calculations to complete
	for i := 0; i < n; i++ {
		message := <-runChan
		log.Println(message)
	}
	close(TotalReportChan)
	message := <-runChan
	log.Println(message)

	// Write out the report
	b, err := json.MarshalIndent(Report, "", "  ")
	if err != nil {
		panic(err)
	}
	f, err := os.Create(filepath.Join(
		ConfigAll.Dirs.Logs, "Report.json"))
	if err != nil {
		panic(err)
	}
	_, err = f.Write(b)
	if err != nil {
		panic(err)
	}
	f.Close()

	log.Println("\n",
		"------------------------------------\n",
		"           AEP Completed!\n",
		"   Check above for error messages.\n",
		"------------------------------------\n")
}

func (c *RunData) nextTime() {
	c.currentTime = c.currentTime.Add(c.tStep)
}
func (c *RunData) CurrentMonth() (month string) {
	month = strings.ToLower(c.currentTime.Format("Jan"))
	return
}

func (c RunData) Run(runChan chan string) {
	if c.InventoryFreq == "annual" {
		Report.SectorResults[c.Sector]["annual"] = new(Results)
		c.RunPeriod("annual")
	} else {
		for c.currentTime.Before(c.endDate) {
			month := c.CurrentMonth()
			if c.InventoryFreq == "monthly" && month != c.inventoryMonth {
				Report.SectorResults[c.Sector][month] = new(Results)
				c.RunPeriod(month)
				c.inventoryMonth = month
			}
			// either advance to next date or end loop
			c.nextTime()
		}
	}

	if Status.Sectors[c.Sector] != "Failed!" {
		Status.Sectors[c.Sector] = "Finished"
	}
	runChan <- "Finished processing " + c.Sector
}

// Set up the correct subroutines to run for each sector and period
func (c RunData) RunPeriod(period string) {
	Status.Sectors[c.Sector] = "Running " + period
	c.msgchan = make(chan string, 1)
	n := 0 // number of subroutines that we need to wait to finish

	// only run inventory
	if c.Speciate == false && c.Spatialize == false {
		go c.inventory(TotalReportChan, period)
		n++
	}

	// speciate but don't spatialize
	if c.Speciate == true && c.Spatialize == false {
		ChanFromInventory := make(chan *ParsedRecord, 1)
		go c.inventory(ChanFromInventory, period)
		go c.speciate(ChanFromInventory, TotalReportChan, period)
		n += 2
	}

	// speciate and spatialize
	if c.Speciate == true && c.Spatialize == true {
		ChanFromInventory := make(chan *ParsedRecord, 1)
		go c.inventory(ChanFromInventory, period)
		SpecSpatialChan := make(chan *ParsedRecord, 1)
		go c.speciate(ChanFromInventory, SpecSpatialChan, period)
		if c.RunTemporal { // only run temporal if spatializing
			SpatialTemporalChan := make(chan *ParsedRecord, 1)
			go c.spatialize(SpecSpatialChan, SpatialTemporalChan, period)
			go c.SectorTemporal(SpatialTemporalChan, TotalReportChan, period)
			n++
		} else {
			go c.spatialize(SpecSpatialChan, TotalReportChan, period)
		}
		n += 3
	}
	// spatialize but don't speciate
	if c.Speciate == false && c.Spatialize == true {
		ChanFromInventory := make(chan *ParsedRecord, 1)
		go c.inventory(ChanFromInventory, period)
		if c.RunTemporal { // only run temporal if spatializing
			SpatialTemporalChan := make(chan *ParsedRecord, 1)
			go c.spatialize(ChanFromInventory, SpatialTemporalChan, period)
			go c.SectorTemporal(SpatialTemporalChan, TotalReportChan, period)
			n++
		} else {
			go c.spatialize(ChanFromInventory, TotalReportChan, period)
		}
		n += 2
	}

	// wait for calculations to complete
	for i := 0; i < n; i++ {
		message := <-c.msgchan
		c.Log(message, 0)
	}

	return
}

func DiscardRecords(msgChan chan string) {
	for _ = range TotalReportChan {
		continue
	}
	msgChan <- "Finished processing all records."
}
