package main

import (
	"bitbucket.org/ctessum/aep/lib.aep"
	"flag"
	"fmt"
	"log"
	"runtime"
	"strings"
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
	e := new(aep.ErrCat) // error report

	// create list of sectors
	sectors := strings.Split(*sectorFlag, " ")

	// parse configuration file
	ConfigAll := aep.ReadConfigFile(configFile, testmode, e)

	// go to localhost:6060 in web browser to view report
	if *reportOnly {
		// The reportServer function will run forever.
		ConfigAll.DefaultSettings.ReportServer(*reportOnly)
	} else {
		// The reportServer function will run until the program is finished.
		go ConfigAll.DefaultSettings.ReportServer(*reportOnly)
	}

	runChan := make(chan string, 1)

	runtime.GOMAXPROCS(ConfigAll.DefaultSettings.Ncpus)

	// Start server for retrieving profiles from the
	// SPECIATE database
	if ConfigAll.DefaultSettings.RunSpeciate {
		go ConfigAll.DefaultSettings.SpecProfiles(e)
	}

	// Set up spatial environment
	if ConfigAll.DefaultSettings.RunSpatialize {
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
		if sectors[0] == "all" || aep.IsStringInArray(sectors, sector) {
			go Run(c, runChan)
			switch c.InventoryFreq {
			case "annual":
				numAnnualSectors++
			case "monthly":
				numMonthlySectors++
			}
			n++
		}
	}
	// run temporal and output subroutines
	if ConfigAll.DefaultSettings.RunTemporal {
		outchan := make(chan *aep.OutputDataChan)
		go ConfigAll.DefaultSettings.Temporal(numAnnualSectors, numMonthlySectors,
			outchan, runChan)
		go ConfigAll.DefaultSettings.Output(outchan, runChan)
		n+=2
	}

	// wait for calculations to complete
	for i := 0; i < n; i++ {
		message := <-runChan
		log.Println(message)
	}

	log.Println("\n",
		"------------------------------------\n",
		"           AEP Completed!\n",
		"   Check above for error messages.\n",
		"------------------------------------\n")
}

func Run(c *aep.RunData, runChan chan string) {
	if c.InventoryFreq == "annual" {
		RunPeriod(c, "annual")
	} else {
		var inventoryMonth string
		for {
			month := c.CurrentMonth()
			if c.InventoryFreq == "monthly" && month != inventoryMonth {
				RunPeriod(c, month)
				inventoryMonth = month
			}
			// either advance to next date or end loop
			keepGoing := c.NextTime()
			if !keepGoing {
				break
			}
		}
	}

	if aep.Status.Sectors[c.Sector] != "Failed!" {
		aep.Status.Sectors[c.Sector] = "Finished"
	}
	runChan <- "Finished processing " + c.Sector
}

// Set up the correct subroutines to run for each sector and period
func RunPeriod(c *aep.RunData, period string) {
	aep.Status.Sectors[c.Sector] = "Running " + period
	msgchan := c.MessageChan()
	n := 0 // number of subroutines that we need to wait to finish

	discardChan := make(chan *aep.ParsedRecord)
	go DiscardRecords(discardChan)

	// only run inventory
	if c.RunSpeciate == false && c.RunSpatialize == false {
		go c.Inventory(discardChan, period)
		n++
	}

	// speciate but don't spatialize
	if c.RunSpeciate == true && c.RunSpatialize == false {
		ChanFromInventory := make(chan *aep.ParsedRecord, 1)
		go c.Inventory(ChanFromInventory, period)
		go c.Speciate(ChanFromInventory, discardChan, period)
		n += 2
	}

	// speciate and spatialize
	if c.RunSpeciate == true && c.RunSpatialize == true {
		ChanFromInventory := make(chan *aep.ParsedRecord, 1)
		go c.Inventory(ChanFromInventory, period)
		SpecSpatialChan := make(chan *aep.ParsedRecord, 1)
		go c.Speciate(ChanFromInventory, SpecSpatialChan, period)
		if c.RunTemporal { // only run temporal if spatializing
			SpatialTemporalChan := make(chan *aep.ParsedRecord, 1)
			go c.Spatialize(SpecSpatialChan, SpatialTemporalChan, period)
			go c.SectorTemporal(SpatialTemporalChan, discardChan, period)
			n++
		} else {
			go c.Spatialize(SpecSpatialChan, discardChan, period)
		}
		n += 3
	}
	// spatialize but don't speciate
	if c.RunSpeciate == false && c.RunSpatialize == true {
		ChanFromInventory := make(chan *aep.ParsedRecord, 1)
		go c.Inventory(ChanFromInventory, period)
		if c.RunTemporal { // only run temporal if spatializing
			SpatialTemporalChan := make(chan *aep.ParsedRecord, 1)
			go c.Spatialize(ChanFromInventory, SpatialTemporalChan, period)
			go c.SectorTemporal(SpatialTemporalChan, discardChan, period)
			n++
		} else {
			go c.Spatialize(ChanFromInventory, discardChan, period)
		}
		n += 2
	}

	// wait for calculations to complete
	for i := 0; i < n; i++ {
		message := <-msgchan
		c.Log(message, 0)
	}

	return
}

func DiscardRecords(inputChan chan *aep.ParsedRecord) {
	for _ = range inputChan {
		continue
	}
}
