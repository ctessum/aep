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

package aep

import (
	"fmt"
	"path/filepath"
	"time"
)

type Outputter interface {
	WriteTimesteps(chan timeStep)
}

func (c *RunData) NewOutputter(startTime time.Time,
	polsAndUnits map[string]string) Outputter {
	var o Outputter
	switch c.OutputType {
	case "wrf":
		filebase := filepath.Join(c.outputDir, c.OutputType,
			"wrfchemi_[DOMAIN]_[DATE]")
		o = c.NewWRFoutput(filebase, polsAndUnits, startTime)
	default:
		panic(fmt.Errorf("Output type `%v' not yet supported.", c.OutputType))
	}
	return o
}

type OutputDataChan struct {
	tstepChan    chan timeStep
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
	msgChan <- fmt.Sprintf("File output of type `%v' completed.", c.OutputType)
}
