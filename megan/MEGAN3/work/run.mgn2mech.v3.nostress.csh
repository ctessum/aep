#! /bin/csh -f
########################################################################
source ./setcase.csh
## Directory setups
setenv PROMPTFLAG N

# Program directory
setenv PROG   mgn2mech
setenv EXEDIR /disk8/MEGAN3/source_code/MEGAN3/src/MGN2MECH
setenv EXE    $EXEDIR/$PROG

# Input map data directory
setenv INPDIR $MGNINP/MAP

# Intermediate file directory
setenv INTDIR $MGNINT

# Output directory
setenv OUTDIR $MGNOUT

# MCIP input directory
setenv METDIR $MGNINP/MGNMET

# Log directory
setenv LOGDIR $MGNLOG/$PROG
if ( ! -e $LOGDIR ) mkdir -p $LOGDIR
########################################################################

foreach mech ( CB6X )
foreach dom ( 36 12)
foreach scen ( J4 )
set JD = 2013145
while ($JD <= 2013198)
########################################################################
# Set up time and date to process
setenv SDATE $JD        #start date
setenv STIME 0
setenv RLENG 250000
setenv TSTEP 10000
########################################################################

########################################################################
# Set up for MECHCONV
setenv RUN_SPECIATE   Y    # run MG2MECH

setenv RUN_CONVERSION Y    # run conversions?
                           # run conversions MEGAN to model mechanism
                           # units are mole/s

setenv SPCTONHR       N    # speciation output unit in tonnes per hour
                           # This will convert 138 species to tonne per
                           # hour or mechasnim species to tonne per hour.
                           
# If RUN_CONVERSION is set to "Y", one of mechanisms has to be selected.
#setenv MECHANISM    RADM2
#setenv MECHANISM    RACM
#setenv MECHANISM    CBMZ
#setenv MECHANISM    CB05
setenv MECHANISM    $mech
#setenv MECHANISM    SOAX
#setenv MECHANISM    SAPRC99
#setenv MECHANISM    SAPRC99Q
#setenv MECHANISM    SAPRC99X

# Grid name
setenv GDNAM3D tceq_${dom}km 

# EFMAPS NetCDF input file
setenv EFMAPS  $INPDIR/EFMAPS3.$GDNAM3D.$scen.ncf

# MEGAN ER filename
setenv MGNERS $INTDIR/MGNERS.$GDNAM3D.$scen.${SDATE}.nostress.ncf

# MEGSEA filename
setenv MGNSEA $INTDIR/MGNSEA.$GDNAM3D.${SDATE}.ncf

# Output filename
setenv MGNOUT $OUTDIR/MEGANv3.$GDNAM3D.$scen.$MECHANISM.$SDATE.nostress.ncf

########################################################################
## Run speciation and mechanism conversion
if ( $RUN_SPECIATE == 'Y' ) then
   rm -f $MGNOUT
   $EXE | tee $LOGDIR/log.run.$PROG.$GDNAM3D.$scen.$MECHANISM.$SDATE.nostress.txt
endif

@ JD++
end  # End while JD

end # scen
end # dom
end # mech
