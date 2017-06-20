# Copyright © 2017 the AEP authors.
# This file is part of AEP.

# AEP is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# AEP is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with AEP.  If not, see <http://www.gnu.org/licenses/>.


#!/bin/bash

# this script downloads files for the 2011 NEI v6.

cd ${nei2011Dir}

# emissions data

wget ftp://ftp.epa.gov/EmisInventory/2011v6/v2platform/2011emissions/2011eh_v6_11g_inputs_point.zip
unzip 2011eh_v6_11g_inputs_point.zip
rm 2011eh_v6_11g_inputs_point.zip
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v2platform/2011emissions/2011eh_v6_11g_inputs_nonpoint.zip
unzip 2011eh_v6_11g_inputs_nonpoint.zip
rm 2011eh_v6_11g_inputs_nonpoint.zip
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v2platform/2011emissions/2011eh_v6_11g_inputs_oth.zip
unzip 2011eh_v6_11g_inputs_oth.zip
rm 2011eh_v6_11g_inputs_oth.zip
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v2platform/2011emissions/2011eh_v6_11g_inputs_nonroad_part1.zip
unzip 2011eh_v6_11g_inputs_nonroad_part1.zip
rm 2011eh_v6_11g_inputs_nonroad_part1.zip
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v2platform/2011emissions/2011eh_v6_11g_inputs_nonroad_part2.zip
unzip 2011eh_v6_11g_inputs_nonroad_part2.zip
rm 2011eh_v6_11g_inputs_nonroad_part2.zip
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v2platform/2011emissions/2011eh_v6_11g_inputs_onroad.zip
unzip 2011eh_v6_11g_inputs_onroad.zip
rm 2011eh_v6_11g_inputs_onroad.zip

wget ftp://ftp.epa.gov/EmisInventory/2011v6/v2platform/2011emissions/onroad/2011eh_onroad_SMOKE_MOVES_MOVES2014_no_speciated_pm.zip
unzip 2011eh_onroad_SMOKE_MOVES_MOVES2014_no_speciated_pm.zip
rm 2011eh_onroad_SMOKE_MOVES_MOVES2014_no_speciated_pm.zip

# ancilliary data

wget ftp://ftp.epa.gov/EmisInventory/2011v6/v2platform/ancillary_data/ge_dat_for_2011v2_other.zip
unzip ge_dat_for_2011v2_other.zip
rm ge_dat_for_2011v2_other.zip
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v2platform/ancillary_data/ge_dat_for_2011v2_gridding.zip
unzip ge_dat_for_2011v2_gridding.zip
rm ge_dat_for_2011v2_gridding.zip

#shapefiles

wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.acs.tar.zip
unzip 2010shapefiles.acs.tar.zip
tar -xvf 2010shapefiles.acs.tar
rm 2010shapefiles.acs.tar.zip 2010shapefiles.acs.tar
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.census_full.tar.zip
unzip 2010shapefiles.census_full.tar.zip
tar -xvf 2010shapefiles.census_full.tar
rm 2010shapefiles.census_full.tar.zip 2010shapefiles.census_full.tar
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.census_split.tar.zip
unzip 2010shapefiles.census_split.tar.zip
tar -xvf 2010shapefiles.census_split.tar
rm 2010shapefiles.census_split.tar.zip 2010shapefiles.census_split.tar
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.extended_idle.zip
unzip 2010shapefiles.extended_idle.tar.zip
rm 2010shapefiles.extended_idle.zip
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.fema.tar.zip
unzip 2010shapefiles.fema.tar.zip
tar -xvf 2010shapefiles.fema.tar
rm 2010shapefiles.fema.tar.zip 2010shapefiles.fema.tar
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.ihs.zip
unzip 2010shapefiles.ihs.zip
rm 2010shapefiles.ihs.zip
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.ladco.tar.zip
unzip 2010shapefiles.ladco.tar.zip
tar -xvf 2010shapefiles.ladco.tar
rm 2010shapefiles.ladco.tar.zip 2010shapefiles.ladco.tar
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.mexico.tar.zip
unzip 2010shapefiles.mexico.tar.zip
tar -xvf 2010shapefiles.mexico.tar
rm 2010shapefiles.mexico.tar.zip 2010shapefiles.mexico.tar
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.misc.tar.zip
unzip 2010shapefiles.misc.tar.zip
tar -xvf 2010shapefiles.misc.tar
rm 2010shapefiles.misc.tar.zip 2010shapefiles.misc.tar
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.nlcd_agri_forest.tar.zip
unzip 2010shapefiles.nlcd_agri_forest.tar.zip
tar -xvf 2010shapefiles.nlcd_agri_forest.tar
rm 2010shapefiles.nlcd_agri_forest.tar.zip 2010shapefiles.nlcd_agri_forest.tar
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.nlcd_devwaterland.tar.zip
unzip 2010shapefiles.nlcd_devwaterland.tar.zip
tar -xvf 2010shapefiles.nlcd_devwaterland.tar
rm 2010shapefiles.nlcd_devwaterland.tar.zip 2010shapefiles.nlcd_devwaterland.tar
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.ntad.tar.zip
unzip 2010shapefiles.ntad.tar.zip
tar -xvf 2010shapefiles.ntad.tar
rm 2010shapefiles.ntad.tar.zip 2010shapefiles.ntad.tar
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.ntad_ipcd.zip
unzip 2010shapefiles.ntad_ipcd.zip
rm 2010shapefiles.ntad_ipcd.zip
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.offshore.tar.zip
unzip 2010shapefiles.offshore.tar.zip
tar -xvf 2010shapefiles.offshore.tar
rm 2010shapefiles.offshore.tar.zip 2010shapefiles.offshore.tar
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.tiger.tar.zip
unzip 2010shapefiles.tiger.tar.zip
tar -xvf 2010shapefiles.tiger.tar
rm 2010shapefiles.tiger.tar.zip 2010shapefiles.tiger.tar
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/2010shapefiles.usgs.tar.zip
unzip 2010shapefiles.usgs.tar.zip
tar -xvf 2010shapefiles.usgs.tar
rm 2010shapefiles.usgs.tar.zip 2010shapefiles.usgs.tar
#
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/US_SpatialSurrogate_Documentation_v091113.docx
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/US_SpatialSurrogate_Workbook_v093013.xlsx
wget ftp://ftp.epa.gov/EmisInventory/2011v6/v1platform/spatial_surrogates/shapefiles/US_SpatialSurrogates_SurrogateToolScripts.30Sep2013.zip
unzip US_SpatialSurrogates_SurrogateToolScripts.30Sep2013.zip
rm US_SpatialSurrogates_SurrogateToolScripts.30Sep2013.zip
#
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2006/us/cty_pophu2k_revised.zip
unzip cty_pophu2k_revised.zip
rm cty_pophu2k_revised.zip
mv cty_pophu2k_revised emiss_shp2010
#
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/us_ag2k.dbf
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/us_ag2k.shp
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/us_ag2k.shx
mv us_ag2k* emiss_shp2010
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/mines_nlcd.dbf
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/mines_nlcd.shp
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/mines_nlcd.shx
mv mines_nlcd* emiss_shp2010
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2006/us/99_county_lu2k.tar.gz
tar -xvf 99_county_lu2k.tar.gz
mv county_lu2k.* emiss_shp2010/
rm 99_county_lu2k.tar.gz
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/airport-area.dbf
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/airport-area.shp
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/airport-area.shx
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/airport-area.sbn
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/airport-area.sbx
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/airport-area.prj
mv airport-area* emiss_shp2010
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/military_air.dbf
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/military_air.shp
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/military_air.shx
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/military_air.sbn
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/military_air.sbx
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/military_air.prj
mv military_air* emiss_shp2010
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/nav_water_activity.dbf
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/nav_water_activity.shp
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/nav_water_activity.shx
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/nav_water_activity.sbn
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/nav_water_activity.sbx
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/nav_water_activity.prj
mv nav_water_activity* emiss_shp2010
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/us_golf.dbf
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/us_golf.shp
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/us_golf.shx
mv us_golf* emiss_shp2010
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/us_wwtp.dbf
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/us_wwtp.shp
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/us_wwtp.shx
mv us_wwtp* emiss_shp2010
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/us_timb.dbf
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/us_timb.shp
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/us_timb.shx
mv us_timb* emiss_shp2010
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/canada/Canadian_Province_Shapefiles.tar.gz
tar -xvf Canadian_Province_Shapefiles.tar.gz
rm Canadian_Province_Shapefiles.tar.gz
mv province_lam_FIPS* emiss_shp2010
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/canada/can_rds.dbf
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/canada/can_rds.prj
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/canada/can_rds.sbn
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/canada/can_rds.sbx
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/canada/can_rds.shp
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/canada/can_rds.shx
mv can_rds* emiss_shp2010
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/canada/can_socio_econ.dbf
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/canada/can_socio_econ.prj
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/canada/can_socio_econ.sbn
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/canada/can_socio_econ.sbx
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/canada/can_socio_econ.shp
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/canada/can_socio_econ.shx
mv can_socio_econ* emiss_shp2010
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/us_oil.dbf
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/us_oil.shp
wget ftp://ftp.epa.gov/EmisInventory/emiss_shp2003/us/us_oil.shx
mv us_oil* emiss_shp2010
#
# organize shapefiles
#
mv ihs emiss_shp2010
mv ladco emiss_shp2010
mv mexico emiss_shp2010
mv ntad_ipcd emiss_shp2010
mv offshore emiss_shp2010
mv tiger emiss_shp2010
mv cty_pophu2k_revised.* emiss_shp2010/
