package main

import (
	"flag"
	"fmt"
	"strings"
//	"os"
)


func (c *RunData) ErrorReport(error interface{}) {
	err := "ERROR REPORT\n"
	err += "Sector: " + c.sector + "\n"
	err += "Message:\n"
	err += fmt.Sprintf("%v", error) + "\n"
	fmt.Print(err)
	return
}

func main() {
	// Read from configuration file and prepare sectors for processing
	var sectorFlag *string = flag.String("sectors", "none", "List of sectors to process, in quotes, separated by spaces")
	var configFile *string = flag.String("config", "none", "Path to configuration file")
	flag.Parse()
	if *sectorFlag == "none" || *configFile == "none" {
		fmt.Println("Please set `-sectors' and `-config' flags and run again: ie: GoSmoke -config=config_file -sectors=\"sector list\"")
		fmt.Println("For more information try typing `GoSmoke --help'")
		return
	}

	// create list of sectors
	sectors := strings.Split(*sectorFlag, " ")

	// parse configuration file
	ConfigAll, err := ReadConfigFile(configFile, sectors)
	if err != nil {
		panic(err)
	}

	e := new(ErrCat) // error report

	runChan := make(chan string, len(sectors))
	for _, c := range ConfigAll {

		// Load speciation fractions
		err = c.SpecFractions(c.specRef, c.specConv, c.specPro, 
			c.specSynonyms, c.specType)
		e.Add(err)


// run subroutines
		go c.Run(runChan)
	}
	e.Report() // Print errors, if any
// wait for calculations to complete
	for i := 0; i < len(sectors); i++ {
		message := <-runChan
		fmt.Println(message)
	}
}

func (c *RunData) nextTime() {
	c.currentTime = c.currentTime.Add(c.tStep)
}
func (c *RunData) CurrentMonth() (month string) {
	month = strings.ToLower(c.currentTime.Format("Jan"))
	return
}

func (c *RunData) Run(runChan chan string) {
	MesgChan := make(chan string, 100)
	if c.inventoryFreq == "annual" {
		InvSpecChan := make(chan ParsedRecord, 1)
		go c.Inventory(MesgChan, InvSpecChan, "annual")
		go c.Speciate(MesgChan, InvSpecChan, "annual")
	 // wait for calculations to complete
		for i := 0; i < 2; i++ {
			message := <-MesgChan
			fmt.Println(message)
		}
	}
	for {
		month := c.CurrentMonth()
		if c.inventoryFreq == "monthly" && month != c.inventoryMonth {
			InvSpecChan := make(chan ParsedRecord, 1)
			go c.Inventory(MesgChan, InvSpecChan, month)
			go c.Speciate(MesgChan, InvSpecChan, month)
		 // wait for calculations to complete
			for i := 0; i < 2; i++ {
				message := <-MesgChan
				fmt.Println(message)
			}
			c.inventoryMonth = month
		}
	// either advance to next date or end loop
	fmt.Println(c.currentTime.Format("Mon Jan 2 15:04:05 2006"))
		if c.currentTime.Before(c.endDate) {
			c.nextTime()
		} else {
			break
		}
	}

	runChan <- "Finished processing " + c.sector
}
