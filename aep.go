package main

import (
	"flag"
	"fmt"
	"log"
	"runtime"
	"strings"
)

var (
	TotalReportChan = make(chan *ParsedRecord)
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
	ConfigAll := ReadConfigFile(configFile, e)

	runChan := make(chan string, 1)

	runtime.GOMAXPROCS(ConfigAll.DefaultSettings.Ncpus)
	// set up TotalInventoryReport subroutine
	go ConfigAll.DefaultSettings.TotalInventoryReport(runChan)

	// start surrogate generator subroutine
	if ConfigAll.DefaultSettings.Spatialize {
		ConfigAll.DefaultSettings.SpatialSetup(e)
		err := ConfigAll.DefaultSettings.SurrogateSpecification()
		e.Add(err)
		err = ConfigAll.DefaultSettings.GridRef()
		e.Add(err)
	}

	e.Report() // Print errors, if any

	// run subroutines
	n := 0
	for sector, c := range ConfigAll.Sectors {
		if sectors[0] == "all" || IsStringInArray(sectors, sector) {
			go c.Run(runChan)
			n++
		}
	}
	// wait for calculations to complete
	for i := 0; i < n; i++ {
		message := <-runChan
		log.Println(message)
	}
	close(TotalReportChan)
	message := <-runChan
	log.Println(message)

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
		c.RunPeriod("annual")
	}
	for c.currentTime.Before(c.endDate) {
		month := c.CurrentMonth()
		if c.InventoryFreq == "monthly" && month != c.inventoryMonth {
			c.RunPeriod(month)
			c.inventoryMonth = month
		}
		// either advance to next date or end loop
		//fmt.Println(c.currentTime.Format("Mon Jan 2 15:04:05 2006"))
		c.nextTime()
	}

	runChan <- "Finished processing " + c.Sector
}

// Set up the correct subroutines to run for each sector and period
func (c RunData) RunPeriod(period string) {
	MesgChan := make(chan string, 1)
	n := 0 // number of subroutines that we need to wait to finish

	// only run inventory
	if c.Speciate == false && c.Spatialize == false {
		go c.inventory(MesgChan, TotalReportChan, period)
		n++
	}

	// speciate but don't spatialize
	if c.Speciate == true && c.Spatialize == false {
		ChanFromInventory := make(chan *ParsedRecord, 1)
		go c.inventory(MesgChan, ChanFromInventory, period)
		go c.speciate(MesgChan, ChanFromInventory, TotalReportChan, period)
		n += 2
	}

	// speciate and spatialize
	if c.Speciate == true && c.Spatialize == true {
		ChanFromInventory := make(chan *ParsedRecord, 1)
		go c.inventory(MesgChan, ChanFromInventory, period)
		SpecSpatialChan := make(chan *ParsedRecord, 1)
		go c.speciate(MesgChan, ChanFromInventory, SpecSpatialChan, period)
		go c.spatialize(MesgChan, SpecSpatialChan, TotalReportChan, period)
		n += 3
	}
	// spatialize but don't speciate
	if c.Speciate == false && c.Spatialize == true {
		ChanFromInventory := make(chan *ParsedRecord, 1)
		go c.inventory(MesgChan, ChanFromInventory, period)
		go c.spatialize(MesgChan, ChanFromInventory, TotalReportChan, period)
		n += 2
	}

	// wait for calculations to complete
	for i := 0; i < n; i++ {
		message := <-MesgChan
		c.Log(message, 0)
	}

	return
}
