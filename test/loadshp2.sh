#!/bin/bash

dbase=gis
schema=aepshp

# Load administrative boundary and 
# timezone locations into PostGIS.
# Conditions for use of this shapefile are specified at:
# http://www.baruch.cuny.edu/geoportal/data/esri/esri_intl.htm
wget http://www.baruch.cuny.edu/geoportal/data/esri/world/timezone.zip
unzip timezone.zip
rm timezone.zip
shp2pgsql -s 4326 -g geom -c timezone.shp ${schema}.timezone > timezone.sql
psql -f timezone.sql $dbase
psql -c "VACUUM ANALYZE ${schema}.timezone;" $dbase
rm -rf timezone*

wget http://www.baruch.cuny.edu/geoportal/data/esri/world/admin.zip
unzip admin.zip
rm admin.zip
shp2pgsql -W LATIN1 -s 4326 -g geom -c admin.shp ${schema}.adminBoundaries > admin.sql
psql -f admin.sql $dbase
psql -c "VACUUM ANALYZE ${schema}.adminBoundaries;" $dbase
#rm -rf admin*

# Load locations of U.S. states into PostGIS.
wget http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_040_00_20m.zip
unzip gz_2010_us_040_00_20m.zip
rm gz_2010_us_040_00_20m.zip
cd gz_2010_us_040_00_20m
shp2pgsql -s 4269 -g geom -c gz_2010_us_040_00_20m.shp ${schema}.states > states.sql
psql -f states.sql $dbase
psql -c "VACUUM ANALYZE ${schema}.states;" $dbase
cd ..
rm -rf gz_2010_us_040_00_20m
