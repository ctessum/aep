/*
Copyright (C) 2012-2014 the AEP authors.
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
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
	"sort"
	"strings"
	"text/tabwriter"

	"github.com/ctessum/unit"
)

// Status holds information on the progress or status of a job.
type Status struct {
	// Name and Code hold information about the job
	Name, Code string

	// Status holds information about the status of the job.
	Status string

	// Progress holds information about how close the job is to completion.
	Progress float64
}

type statuses []Status

func (s statuses) Len() int           { return len(s) }
func (s statuses) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }
func (s statuses) Less(i, j int) bool { return s[i].Name < s[j].Name }

// An InventoryReport report holds information about raw inventory data.
type InventoryReport struct {
	Files []*InventoryFile
}

// AddData adds file(s) to the report.
func (ir *InventoryReport) AddData(files ...*InventoryFile) {
	ir.Files = append(ir.Files, files...)
}

// TotalsTable returns a table of the total emissions in this report, where
// the rows are the files and the columns are the pollutants, arranged
// alphabetically.
func (ir *InventoryReport) TotalsTable() Table {
	return ir.table(getTotals)
}

// DroppedTotalsTable returns a table of the total emissions in this report, where
// the rows are the files and the columns are the pollutants, arranged
// alphabetically.
func (ir *InventoryReport) DroppedTotalsTable() Table {
	return ir.table(getDroppedTotals)
}

func (ir *InventoryReport) table(df func(*InventoryFile) map[Pollutant]*unit.Unit) Table {
	t := make([][]string, len(ir.Files)+1)

	// get pollutants
	pols := make(map[string]int)
	dims := make(map[string]unit.Dimensions)
	for _, f := range ir.Files {
		for pol, val := range df(f) {
			pols[pol.Name] = 0
			if d, ok := dims[pol.Name]; !ok {
				dims[pol.Name] = val.Dimensions()
			} else {
				if !val.Dimensions().Matches(d) {
					panic(fmt.Errorf("dimensions mismatch: '%v' != '%v'", val.Dimensions(), d))
				}
			}
		}
	}
	for pol := range pols {
		t[0] = append(t[0], pol)
	}
	sort.Strings(t[0])
	t[0] = append([]string{"Group", "File"}, t[0]...)
	for i, pol := range t[0] {
		pols[pol] = i
	}
	for i, pol := range t[0] {
		if dims[pol] != nil {
			t[0][i] += fmt.Sprintf(" (%s)", dims[pol].String())
		}
	}
	for i, f := range ir.Files {
		t[i+1] = make([]string, len(pols))
		t[i+1][0], t[i+1][1] = f.Group, f.Name
		for pol, val := range df(f) {
			t[i+1][pols[pol.Name]] = fmt.Sprintf("%g", val.Value())
		}
	}

	return t
}

// A Table holds a text representation of report data.
type Table [][]string

// Tabbed creates a tab-separated table.
func (t Table) Tabbed(w io.Writer) (n int, err error) {
	ww := new(tabwriter.Writer)
	ww.Init(w, 0, 2, 0, '\t', 0)
	var nn int
	for _, l := range t {
		for _, r := range l {
			nn, err = fmt.Fprint(ww, r+"\t")
			if err != nil {
				return
			}
			n += nn
		}
		nn, err = fmt.Fprint(ww, "\n")
		if err != nil {
			return
		}
		n += nn
	}
	return
}

// SCCDescription reads a SMOKE sccdesc file, which gives descriptions
// for each SCC code. The returned data is in the form map[SCC]description.
func SCCDescription(r io.Reader) (map[string]string, error) {
	sccDesc := make(map[string]string)
	buf := bufio.NewReader(r)
	for {
		record, err := buf.ReadString('\n')
		if err != nil {
			if err == io.EOF {
				break
			} else {
				return sccDesc, fmt.Errorf("In SCCdescription: %s; record: %s", err, record)
			}
		}
		// Get rid of comments at end of line.
		if i := strings.Index(record, "!"); i != -1 {
			record = record[0:i]
		}
		if record[0] != '#' {
			splitLine := strings.Split(record, ",")
			SCC := strings.Trim(splitLine[0], "\"")
			// Add zeros to 8 digit SCCs so that all SCCs are 10 digits
			// If SCC is less than 8 digits, add 2 zeros to the front and the rest to
			// the end.
			if len(SCC) == 8 {
				SCC = "00" + SCC
			} else if len(SCC) == 7 {
				SCC = "00" + SCC + "0"
			} else if len(SCC) == 6 {
				SCC = "00" + SCC + "00"
			} else if len(SCC) == 5 {
				SCC = "00" + SCC + "000"
			} else if len(SCC) == 4 {
				SCC = "00" + SCC + "0000"
			} else if len(SCC) == 3 {
				SCC = "00" + SCC + "00000"
			} else if len(SCC) == 2 {
				SCC = "00" + SCC + "000000"
			}
			sccDesc[SCC] = cleanDescription(splitLine[1])
		}
	}
	return sccDesc, nil
}

// SICDesc reads an SIC description file, which gives descriptions for each SIC code.
func (c *Context) SICDesc() (map[string]string, error) {
	sicDesc := make(map[string]string)
	var record string
	fid, err := os.Open(c.SicDesc)
	if err != nil {
		return sicDesc, errors.New("SICdesc: " + err.Error() + "\nFile= " + c.SicDesc + "\nRecord= " + record)
	}
	defer fid.Close()
	buf := bufio.NewReader(fid)
	for {
		record, err = buf.ReadString('\n')
		if err != nil {
			if err == io.EOF {
				err = nil
				break
			} else {
				return sicDesc, errors.New(c.SicDesc + "\n" + record + "\n" + err.Error() + "\nFile= " + c.SicDesc + "\nRecord= " + record)
			}
		}
		if record[0] != '#' {
			sicDesc[strings.Trim(record[0:4], " ")] =
				cleanDescription(record[5:])
		}
	}
	return sicDesc, err
}

// NAICSDesc reads a NAICS description file, which gives descriptions for each NAICS code.
func (c *Context) NAICSDesc() (map[string]string, error) {
	naicsDesc := make(map[string]string)
	var record string
	fid, err := os.Open(c.NaicsDesc)
	if err != nil {
		return naicsDesc, errors.New("NAICSdesc: " + err.Error() + "\nFile= " + c.NaicsDesc + "\nRecord= " + record)
	}
	defer fid.Close()
	buf := bufio.NewReader(fid)
	for {
		record, err = buf.ReadString('\n')
		if err != nil {
			if err.Error() == "EOF" {
				err = nil
				break
			} else {
				return naicsDesc, errors.New(record + "\n" + err.Error() + "\nFile= " + c.NaicsDesc + "\nRecord= " + record)
			}
		}
		if record[0] != '#' {
			splitLine := strings.Split(record, ",")
			naicsDesc[strings.Trim(splitLine[0], "\"")] =
				cleanDescription(splitLine[1])
		}
	}
	return naicsDesc, err
}

func cleanDescription(d string) string {
	return "\"" + strings.Replace(strings.Trim(d, "\n"), "\"", "", -1) + "\""
}
