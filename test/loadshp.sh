#!/bin/bash
# This script will work to import the shapefiles found at ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/ into PostGIS. If this script is used with different shapefiles, the projection info will probably have to be changed.


dbase=gis
schema=aepshp

psql -c "INSERT INTO spatial_ref_sys (srid, auth_name, auth_srid, proj4text) VALUES (500000,'EPA',500000,'+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +no_defs +a=6370997 +b=6370997 +to_meter=1');" $dbase

psql -c "DROP SCHEMA ${schema} CASCADE;" $dbase
psql -c "CREATE SCHEMA ${schema};" $dbase

for x in `ls *.shp`
do 
	echo ------------------------------$x------------------------------
	y=`echo $x | cut -d . -f 1`
	shp2pgsql -W LATIN1 -s 500000 -g geom -c $x ${schema}.$y > $y.sql
	#shp2pgsql -D -W LATIN1 -s 500000 -g geom -c $x ${schema}.$y > $y.sql
	psql -f $y.sql $dbase
	psql -c "VACUUM ANALYZE ${schema}.\"$y\";" $dbase
	rm $y.sql
done
