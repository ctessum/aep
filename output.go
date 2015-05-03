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
)

type Outputter interface {
	Output()
	PlumeRise(gridIndex int, r *ParsedRecord) (kPlume int)
	Kemit() int
}

func (c *Context) NewOutputter(tp *TemporalProcessor) Outputter {
	var o Outputter
	switch c.OutputType {
	case "wrf":
		o = c.NewWRFOutputter(tp)
	default:
		panic(fmt.Errorf("Output type `%v' not yet supported.", c.OutputType))
	}
	return o
}

func (c *Context) NextTime() (keepGoing bool) {
	c.currentTime = c.currentTime.Add(c.tStep)
	keepGoing = c.currentTime.Before(c.endDate)
	return
}