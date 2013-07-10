package aep

import (
	"fmt"
	"time"
	"path/filepath"
)

type Outputter interface {
	WriteTimesteps(chan *TimeStep)
}

func (c *RunData) NewOutputter(startTime time.Time,
	polsAndUnits map[string]string) Outputter {
	var o Outputter
	switch c.OutputType {
	case "wrf":
		filebase := filepath.Join(c.outputDir, c.OutputType,
			"wrfchemi_[DOMAIN]_[DATE]")
		o = c.NewWRFoutput(filebase,polsAndUnits,startTime)
	default:
		panic(fmt.Errorf("Output type `%v' not yet supported.", c.OutputType))
	}
	return o
}

type OutputDataChan struct {
	tstepChan    chan *TimeStep
	startTime    time.Time
	polsAndUnits map[string]string
}

func (c *RunData) Output(outputChanChan chan *OutputDataChan, msgChan chan string) {
	if c.OutputType == "none" {
		for _ = range outputChanChan {
			continue
		}
	} else {
		for out := range outputChanChan {
			writer := c.NewOutputter(out.startTime, out.polsAndUnits)
			writer.WriteTimesteps(out.tstepChan)
		}
	}
	msgChan <- fmt.Sprintf("File output of type `%v' completed.",c.OutputType)
}