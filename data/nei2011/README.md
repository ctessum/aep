# Retrieving the 2011 NEI data

Data for the air quality modeling version of the U.S. EPA's 2011 National emissions inventory is available for download from an [EPA FTP server](ftp://ftp.epa.gov/EmisInventory/2011v6/). A description of the included data is available [here](ftp://ftp.epa.gov/EmisInventory/2011v6/v2platform/README_2011v6.2_package.txt), and a technical description of the inventory is available [here](https://www.epa.gov/air-emissions-modeling/2011-version-62-technical-support-document).

This repository includes two tools designed to simplify the process of downloading the data and preparing it for use. They both assume that the `${nei2011Dir}` environment variable has been set to the desired location for the data download.

* The bash script `download.sh` downloads, unzips, and reorganizes the emissions data, shapefiles for creating spatial surrogates, and other supporting data required to process the NEI data. It may take awhile.

* The Go language script `addPrj.go` searches among the downloaded shapefiles for those that don't have a `.prj` file associated with them containing spatial projection information adds such a file as needed. After installing the Go compiler, this script can be run using the command `go run addPrj.go`.

This repository also includes the additional file `surrogate_specification_all_edited.csv`. This file is a version of the downloaded file `${nei2011Dir}/US_SpatialSurrogates_SurrogateToolScripts.30Sep2013/surrogate_specification_all.csv` that has been edited to replace missing shapefiles with existing replacements. Improvements to this file or advice regarding the locations of the missing files are welcome.
