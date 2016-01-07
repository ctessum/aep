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

package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/ctessum/aep"
	"github.com/ctessum/geom/proj"
	//"time"
)

func main() {
	log.Println("\n",
		"----------------------------------------------------------\n",
		"                        Welcome!\n",
		"             Program (A)ir (E)missions (P)rocessor\n",
		"Copyright 2012-2014 Regents of the University of Minnesota\n",
		"----------------------------------------------------------\n")

	// Read from configuration file and prepare sectors for processing
	var sectorFlag = flag.String("sectors", "all", "List of sectors to process, in quotes, separated by spaces")
	var configFile = flag.String("config", "none", "Path to configuration file")
	var reportOnly = flag.Bool("reportonly", false, "Run html report server for results of previous run (do not calculate new results)")
	var testmode = flag.Bool("testmode", false, "Run model with mass speciation and no VOC to TOG conversion so that results can be validated by go test")
	var seq = flag.Bool("seq", false, "Run sectors in sequence instead of parallel to conserve memory")
	var slavesFlag = flag.String("slaves", "", "List of addresses of available slaves, in quotes, separated by spaces.")
	var masterAddress = flag.String("masteraddress", "", "What is the address of the master? Leave empty if this is not a slave.")
	flag.Parse()

	if *configFile == "none" {
		fmt.Println("Please set `-config' flag and run again: ie: aep -config=/path/to/config_file")
		fmt.Println("For more information try typing `aep --help'")
		return
	}
	e := new(aep.ErrCat) // error report

	// create list of sectors
	sectors := strings.Split(*sectorFlag, " ")

	// Are we doing distributed computing,
	// and is this the master?
	var slaves []string
	if *slavesFlag != "" {
		// create list of slaves
		slaves = strings.Split(*slavesFlag, " ")
	}

	// parse configuration file
	ConfigAll := aep.ReadConfigFile(configFile, testmode, slaves, e)

	if *masterAddress != "" {
		// Set up a server to accept RPC requests;
		// this will block and run forever.
		aep.DistributedServer(ConfigAll.DefaultSettings)
	}
	c := ConfigAll.DefaultSettings

	// go to localhost:6060 in web browser to view report
	if *reportOnly {
		// The reportServer function will run forever.
		c.ReportServer(*reportOnly)
	} else {
		// The reportServer function will run until the program is finished.
		go c.ReportServer(*reportOnly)
	}
	defer c.WriteReport()

	// Start server for retrieving profiles from the
	// SPECIATE database
	if c.RunSpeciate {
		go c.SpecProfiles(e)
	}

	var err error
	var wrfConfig *aep.WRFconfigData
	if c.RunSpatialize || c.RunTemporal {
		wrfConfig, err = aep.ParseWRFConfig(c.WPSnamelist, c.WRFnamelist)
		e.Add(err)
	}

	// Set up spatial environment
	var grids []*aep.GridDef
	var sp *aep.SpatialProcessor
	if c.RunSpatialize {
		grids, err = wrfConfig.Grids(
			filepath.Join(c.ShapefileDir, "world_timezones.shp"), "TZID") // TODO: shapefile shouldn't be hard coded.
		e.Add(err)

		srgf, err := os.Open(c.SrgSpecFile)
		e.Add(err)
		srgSpecs, err := aep.ReadSrgSpec(srgf, c.ShapefileDir, c.CheckSrgs)
		e.Add(err)
		gref, err := os.Open(c.GridRefFile)
		e.Add(err)
		gridRef, err := aep.ReadGridRef(gref)
		inputSR, err := proj.FromProj4(c.InputProj4)
		e.Add(err)
		sp = aep.NewSpatialProcessor(srgSpecs, grids, gridRef, inputSR, c.MatchFullSCC)
		sp.DiskCachePath = ConfigAll.Dirs.GriddedSrgs
	}

	// Set up temporal and output processors
	var temporal *aep.TemporalProcessor
	if ConfigAll.DefaultSettings.RunTemporal {
		temporal = c.NewTemporalProcessor(sp, grids)
	}

	e.Report() // Print errors, if any

	runChan := make(chan string, 1)
	if *seq { // run sectors in sequence to conserve memory
		for sector, c := range ConfigAll.Sectors {
			if sectors[0] == "all" || aep.IsStringInArray(sectors, sector) {
				go Run(c, runChan, sp, temporal)
				message := <-runChan
				log.Println(message)
			}
		}
	} else { // run sectors in parallel
		// run sector subroutines
		n := 0
		for sector, c := range ConfigAll.Sectors {
			if sectors[0] == "all" || aep.IsStringInArray(sectors, sector) {
				go Run(c, runChan, sp, temporal)
				n++
			}
		}
		// wait for calculations to complete
		for i := 0; i < n; i++ {
			message := <-runChan
			log.Println(message)
		}
	}

	// run output subroutines
	if ConfigAll.DefaultSettings.RunTemporal {
		outputter := wrfConfig.NewOutputter(temporal,
			filepath.Join(ConfigAll.Dirs.Output, c.OutputType), c.OldWRFout)
		st, err := c.StartTime()
		e.Add(err)
		et, err := c.EndTime()
		e.Add(err)
		ts, err := c.TimeStep()
		e.Add(err)
		outputter.Output(temporal, st, et, ts)
	}

	log.Println("\n",
		"------------------------------------\n",
		"           AEP Completed!\n",
		"   Check above for error messages.\n",
		"------------------------------------\n")
}

func Run(c *aep.Context, runChan chan string,
	sp *aep.SpatialProcessor, temporal *aep.TemporalProcessor) {

	log.Println("Running " + c.Sector + "...")
	aep.Status.Sectors[c.Sector] = "Running"
	msgchan := c.MessageChan()

	discardChan := make(chan *aep.ParsedRecord)
	go DiscardRecords(discardChan)

	// Read in emissions records.
	er, err := aep.NewEmissionsReader(c.PolsToKeep, c.InventoryFreq,
		c.InputUnits, c.SectorType)

	filenames := make(map[aep.Period][]string)
	files := make(map[aep.Period][]aep.ReadSeekCloser)
	for _, p := range c.RunPeriods {
		filenames[p] = make([]string, len(c.InvFileNames))
		files[p] = make([]aep.ReadSeekCloser, len(c.InvFileNames))
		for i, filetemplate := range c.InvFileNames {
			filenames[p][i], files[p][i], err = er.OpenFileFromTemplate(filetemplate, p)
			if err != nil {
				panic(err)
			}
		}
	}
	recs := make(map[aep.Period]map[string]*aep.ParsedRecord)
	for _, p := range c.RunPeriods {
		recs[p], _, err = er.ReadFiles(files[p], filenames[p], p)
		if err != nil {
			panic(err)
		}
	}

	if c.RunSpeciate == true {
		recChan := make(chan *aep.ParsedRecord, 1)
		go c.Speciate(recChan, discardChan)
		for _, precs := range recs {
			for _, rec := range precs {
				fmt.Printf("qqqq %#v\n", rec.ANN_EMIS[aep.Annual]["PEC"])
				recChan <- rec
			}
		}
		close(recChan)
		// wait for calculations to complete
		message := <-msgchan
		c.Log(message, 1)
	}

	if c.RunTemporal {
		recChan := make(chan *aep.ParsedRecord, 1)
		temporal.NewSector(c, recChan, discardChan, sp)
		for _, precs := range recs {
			for _, rec := range precs {
				fmt.Printf("seeeeeeeeeeessssssss %#v\n", rec.ANN_EMIS[aep.Annual]["PEC"])
				recChan <- rec
			}
		}
		close(recChan)
		// wait for calculations to complete
		message := <-msgchan
		c.Log(message, 1)
	}

	if aep.Status.Sectors[c.Sector] != "Failed!" {
		aep.Status.Sectors[c.Sector] = "Finished"
	}
	runChan <- "Finished processing " + c.Sector
}

func DiscardRecords(inputChan chan *aep.ParsedRecord) {
	for _ = range inputChan {
		continue
	}
}
