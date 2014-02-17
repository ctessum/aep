#!/bin/bash

wkdir="${GOPATH}/src/bitbucket.org/ctessum/aep/test/inputdata/shapefiles"
mkdir -p $wkdir
cd $wkdir

# Get administrative boundary and 
# timezone locations.
# Conditions for use of this shapefile are specified at:
# http://www.baruch.cuny.edu/geoportal/data/esri/esri_intl.htm
wget http://www.baruch.cuny.edu/geoportal/data/esri/world/timezone.zip
unzip timezone.zip
rm timezone.zip

wget http://www.baruch.cuny.edu/geoportal/data/esri/world/admin.zip
unzip admin.zip
rm admin.zip

# get locations of U.S. states.
wget http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_040_00_20m.zip
unzip gz_2010_us_040_00_20m.zip
rm gz_2010_us_040_00_20m.zip

shpdir="ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/"

prj="PROJCS[\"Lambert_Conformal_Conic\",GEOGCS[\"GCS_unnamed ellipse\",DATUM[\"D_unknown\",SPHEROID[\"Unknown\",6370997,0]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.017453292519943295]],PROJECTION[\"Lambert_Conformal_Conic\"],PARAMETER[\"standard_parallel_1\",33],PARAMETER[\"standard_parallel_2\",45],PARAMETER[\"latitude_of_origin\",40],PARAMETER[\"central_meridian\",-97],PARAMETER[\"false_easting\",0],PARAMETER[\"false_northing\",0],UNIT[\"Meter\",1]]"

for x in airport-area airport_point military_air mines_nlcd mines_usg nav_water_activity pophu2k ports2004 rural_land us_ag2k us_dryclean us_for2k us_gas_sta us_golf us_heat us_lowres us_lu2k us_lw2k us_nat_gas us_nav_h20 us_nf us_oil us_ph90 us_ph90 us_rail2k us_timb us_urban us_wwtp usrds_2000 vi_pophu2k vi_rds2k
do
	echo $x 
	wget ${shpdir}/${x}.*
	echo $prj > ${x}.prj # add .prj file because the EPA shapefiles don't have them.
done



