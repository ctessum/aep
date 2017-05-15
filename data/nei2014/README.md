# Retrieving the 2014 NEI data

Data for the air quality modeling version of the U.S. EPA's 2011 National emissions inventory is available for download from an [EPA FTP server](ftp://ftp.epa.gov/EmisInventory/2014platform/v1/). A description of the included data is available [here](ftp://ftp.epa.gov/EmisInventory/2014platform/v1/README_2014v1_nata_package.txt).

This repository includes a script—```download.go```—that downloads the data and preparing it for use. After [installing the Go language compiler](https://golang.org/doc/install), the script can be run with the command ```go run download.go -dir="/path/to/download"``` where ```/path/to/download``` is the location of the directory where the data should be downloaded to. Downloading the data may take a while.

This repository also includes the additional file `surrogate_specification_2014.csv`. This file is combined and edited version of surrogate specification files that can be downloaded from the FTP site which has been edited to replace missing shapefiles with existing replacements and combine US, Canada, and Mexico surrogates in one place. Improvements to this file or advice regarding the locations of the missing files are welcome.

Finally, this directory in includes a configuration file—```cstref_2014.toml```—that specifies how the files can be used to processed the 2014 NEI. The configuration file assumes that a ```$nei2014Dir``` environment variable has been set to the directory where the data files were downloaded to (```/path/to/download``` in the example above).
