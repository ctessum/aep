# **A**ir **E**missions **P**rocessor program

## Installation

1. Install the [Go compiler](http://golang.org/doc/install). Also make sure to set the [GOPATH](http://golang.org/doc/code.html#GOPATH) environment variable.

2. Install the required (non-Go) dependencies. These are:
	* [PostGIS](http://postgis.net/) and its dependencies.
	* [GEOS](http://trac.osgeo.org/geos/) and [proj4](http://trac.osgeo.org/proj/). These libraries are requirements for PostGIS, but they are also directly used by the AEP program. If these libraries are in a nonstandard location, be sure to set the CGO\_CFLAGS and CGO\_LDFLAGS environment variables to ensure that the Go compiler can find the libraries.
	* The [git](http://git-scm.com/) and [mercurial](http://mercurial.selenic.com/) version control programs.

3. Download and install the main program:

		hg clone https://ctessum@bitbucket.org/ctessum/aep $GOPATH/src/bitbucket.org/ctessum/aep // download the main program
		go get bitbucket.org/ctessum/aep // download its (Go-language) dependencies and compile and install it.
	Normally we could skip the first command, but since the AEP program is in a password protected repository, it needs to be downloaded separately.

## Use

1. First, it is important for the PostGIS server to be up and running. 

2. Once PostGIS is up and running, you can load the shapefiles into your PostGIS database. To do this first run:
		
		$GOPATH/src/bitbucket.org/ctessum/aep/test/loadshp.sh
	This script will download several shapefiles and load them automatically. Then, download some more shapefiles from [here](https://bitbucket.org/ctessum/aep/downloads), change to the directory where you downloaded the shapefiles to, and run:

		$GOPATH/src/bitbucket.org/ctessum/aep/test/loadshp.sh
	Both of these scripts may need to be edited to specify the name of the PostGIS database you have created.

3. Download the input emissions files for the test case from [here](https://bitbucket.org/ctessum/aep/downloads) and extract them into the directory 

		$GOPATH/src/bitbucket.org/ctessum/aep/test/Minneapolis2005

4. Run the program: 

		aep -config=$GOPATH/src/bitbucket.org/ctessum/aep/test/Minneapolis2005.json. 
	While the program is running, you can open a web browser and navigate to localhost:6060 to view status and diagnostic information.
	After the program finishes running, output can be found in the directory 

		$GOPATH/src/bitbucket.org/ctessum/aep/test/Output

5. Once you have run the test case, you can try running the program for your domain and time period of interest.
	* Because the test case emissions only have records specific to Minneapolis, you will need to Download the full emissions inventory files from [here](ftp://ftp.epa.gov/EmisInventory/2005v4_2/2005emis)
	* You can start with the configuration file at 

			$GOPATH/src/bitbucket.org/ctessum/aep/test/2005_nei.json 
		and edit it to point to the emissions files you have downloaded as well as the namelist,input and namelist.wps files for your WRF domain, and the WRF output files from a previous simulation which are required to calculate emissions plume rise.


## Additional information

### Configuration file

The configuration file needs to be a valid [json](http://en.wikipedia.org/wiki/JSON) file with the following format:

	{
		"Dirs": {
			DirInfo
		},
		"DefaultSettings": {
			RunData
		},
		"sectors": {
			"sectorname1": {
				RunData
			},
			"sectorname2": {
				RunData
			}
		}
	}
Refer directly to the source code for the fields that make up the 
[DirInfo](https://bitbucket.org/ctessum/aep/src/default/lib.aep/configure.go#cl-71)
and [RunData](https://bitbucket.org/ctessum/aep/src/default/lib.aep/configure.go#cl-78) data holders. Only fields where the first letter is capitalized are valid options for the configuration file.

Within the configuration file, you can use the variables `[Home]`, `[Input]`, and `[Ancilliary]` to represent the directories specified in the `Dirs` section of the file.You can also use environment variables such as `$GOPATH`. When specifiying the locations of the `OldWRFout` files, you can use the variables `[DATE]` and `[DOMAIN]` which will be replaced with the relevant dates and domains while the program is being run.

Some of the fields in the configuration file have automatic default values associated with them. Additionally, some can only be specified in the `DefaultSettings` section of the file; for these variables, settings specified for individual sectors will be ignored. Refer to the [source code](https://bitbucket.org/ctessum/aep/src/default/lib.aep/configure.go#cl-175) to further understand this behavior.
