package main

import (
	"bitbucket.org/ctessum/aep/sparse"
	"bufio"
	"fmt"
	"os"
	"strings"
)

var (
	temporalRef = make(map[string]map[string][3]string)
)

// TemporalRef reads the SMOKE tref file, which maps FIPS and SCC
// codes to grid surrogates. Although the tref file allows the
// specification of code by pollutant name, that functionality is
// not included here.
func (c *RunData) TemporalRef() (err error) {
	var record string
	fid, err := os.Open(c.TemporalRefFile)
	if err != nil {
		err = fmt.Errorf("termporalRef: %v \nFile= %v\nRecord= ",
			err.Error(), c.TemporalRefFile, record)
		return
	} else {
		defer fid.Close()
	}
	buf := bufio.NewReader(fid)
	for {
		record, err = buf.ReadString('\n')
		if err != nil {
			if err.Error() == "EOF" {
				err = nil
				break
			} else {
				err = fmt.Errorf("TemporalRef: %v \nFile= %v\nRecord= ",
					err.Error(), c.TemporalRefFile, record)
				return
			}
		}
		// Get rid of comments at end of line.
		if i := strings.Index(record, "!"); i != -1 {
			record = record[0:i]
		}

		if record[0] != '#' && record[0] != '\n' {
			splitLine := strings.Split(record, ";")
			SCC := splitLine[0]
			if len(SCC) == 0 {
				SCC = "0000000000"
			} else if len(SCC) == 8 {
				SCC = "00" + SCC
			}
			monthCode := splitLine[1]
			weekCode := splitLine[2]
			diurnalCode := splitLine[3]
			FIPS := splitLine[5][1:]
			if len(FIPS) == 0 {
				FIPS = "00000"
			} else if len(FIPS) == 6 {
				FIPS = FIPS[1:]
			}

			if _, ok := temporalRef[SCC]; !ok {
				temporalRef[SCC] = make(map[string][3]string)
			}
			temporalRef[SCC][FIPS] = [3]string{
				monthCode, weekCode, diurnalCode}
		}
	}
	return
}

func (c *RunData) AreaTemporalAggregator(InputChan chan *ParsedRecord,
	period string) {
	var err error
	defer c.ErrorRecoverCloseChan(InputChan)
	c.Log("Aggregating by temporal profile "+period+" "+c.Sector+"...", 0)

	data := make(map[[3]string]map[string][]*sparse.SparseArray)

	for record := range InputChan {
		var matchedSCC string
		if !c.MatchFullSCC {
			matchedSCC, err = MatchCode4(record.SCC, temporalRef)
			if err != nil {
				panic(err)
			}
		} else {
			matchedSCC = record.SCC
		}
		matchedFIPS, err := MatchCode5(record.FIPS, temporalRef[matchedSCC])
		if err != nil {
			panic(err)
		}
		temporalCodes := temporalRef[matchedSCC][matchedFIPS]

		// Create matricies if they don't exist
		if _, ok := data[temporalCodes]; !ok {
			data[temporalCodes] = make(map[string][]*sparse.SparseArray)
		}
		// Add data from record into matricies.

		for pol, vals := range record.ANN_EMIS {
			if _, ok := data[temporalCodes][pol]; !ok {
				data[temporalCodes][pol] =
					make([]*sparse.SparseArray, len(grids))
				for i, grid := range grids {
					data[temporalCodes][pol][i] =
						sparse.ZerosSparse(grid.Ny, grid.Nx)
				}
			}
			for i, _ := range grids {
				data[temporalCodes][pol][i].AddSparse(vals.gridded[i])
			}
		}
	}
}
