**A**ir **E**missions **P**rocessor program
===========================================

Installation
------------

1. Install the [Go compiler](http://golang.org/doc/install). Also make sure to set the [GOPATH](http://golang.org/doc/code.html#GOPATH) environment variable.

2. Install the required (non-Go) dependencies. These are:
	1. [PostGIS](http://postgis.net/) and its dependencies.
	2. [GEOS](http://trac.osgeo.org/geos/) and [proj4](http://trac.osgeo.org/proj/). These libraries are requirements for PostGIS, but they are also directly used by the AEP program. If these libraries are in a nonstandard location, be sure to set the CGO\_CFLAGS and CGO\_LDFLAGS environment variables to ensure that the Go compiler can find the libraries.
	3. The [git](http://git-scm.com/) and [mercurial](http://mercurial.selenic.com/) version control programs.

3. Download and install the main program:
		hg clone https://ctessum@bitbucket.org/ctessum/aep $GOPATH/bitbucket.org/ctessum/aep // download the main program
		go get bitbucket.org/ctessum/aep // download its (Go-language) dependencies and compile and install it.
		Normally we could skip the first step, but since the AEP program is in a password protected repository, it needs to be downloaded separately

Use
---

1. First, it is important for the PostGIS server to be up and running. One way to (partially) check this is to type "psql" in a command line and see what happens. 

2. Once PostGIS is up and running, you can load the shapefiles into the server. To do this first run:
	$GOPATH/bitbucket.org/ctessum/aep/test/loadshp.sh
This script will download several shapefiles and load them automatically. Then, download some more shapefiles from [here](https://bitbucket.org/ctessum/aep/downloads), change to the directory where you downloaded the shapefiles to, and run:
	$GOPATH/bitbucket.org/ctessum/aep/test/loadshp.sh
Both of these scripts may need to be edited for your specific PostGIS setup.

3. Download the input emissions files for the test case from [here](https://bitbucket.org/ctessum/aep/downloads) and extract them into the directory $GOPATH/bitbucket.org/ctessum/aep/test/Minneapolis2005

4. Run the program: aep -config=$GOPATH/bitbucket.org/ctessum/aep/test/Minneapolis2005.json. Output can be found in the directory $GOPATH/bitbucket.org/ctessum/aep/test/Output


3. Download the emissions inventory files from [here](ftp://ftp.epa.gov/EmisInventory/2005v4_2/2005emis)
	
