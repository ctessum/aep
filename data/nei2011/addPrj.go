package main

import (
	"log"
	"os"
	"path/filepath"
	"strings"
)

// prj is the wkt string that specifies the projection that these shapfiles have.
const prj = `PROJCS["Lambert_Conformal_Conic_2SP",GEOGCS["GCS_unnamed ellipse",DATUM["D_unknown",SPHEROID["Unknown",6370997,0]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Lambert_Conformal_Conic_2SP"],PARAMETER["standard_parallel_1",33],PARAMETER["standard_parallel_2",45],PARAMETER["latitude_of_origin",40],PARAMETER["central_meridian",-97],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["Meter",1]]`

// This program check whether the shapefiles in the shapefile directory have .prj
// files associated with them. If they do not, a .prj file containing the default
// projection is added.
func main() {

	err := filepath.Walk(filepath.Join(os.ExpandEnv("${nei2011Dir}"), "emiss_shp2010"), func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if filepath.Ext(path) == ".shp" {
			newPath := strings.TrimSuffix(path, "shp") + "prj"
			if _, err := os.Stat(newPath); err != nil {
				log.Println(newPath)
				f, err := os.Create(newPath)
				if err != nil {
					return err
				}
				if _, err := f.WriteString(prj); err != nil {
					return err
				}
			}
		}
		return nil
	})

	if err != nil {
		log.Fatal(err)
	}
}
