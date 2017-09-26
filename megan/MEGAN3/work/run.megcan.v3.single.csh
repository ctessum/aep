#! /bin/csh -f
########################################################################
source setcase.csh
## Directory setups
setenv PROMPTFLAG N

# Program directory
setenv PROG   megcan
setenv EXEDIR ${PWD}/../bin
setenv EXE    $EXEDIR/$PROG

# Input map data directory
setenv INPDIR $MGNINP/MAP

# MCIP input directory
setenv METDIR $MGNINP/MGNMET

# Output directory
setenv OUTDIR $MGNINT

# Log directory
setenv LOGDIR $MGNLOG/megcan
mkdir -p $LOGDIR
########################################################################

foreach dom ( 12 )
set JD = 2013145
while ($JD <= 2013145)
########################################################################
# Set up time and date to process
setenv SDATE $JD        #start date
setenv STIME 0
setenv RLENG 10000

########################################################################

########################################################################
# Set up for MEGAN
setenv RUN_MEGAN   Y       # Run megan?

# Grid definition
setenv GRIDDESC $cwd/GRIDDESC.single
setenv GDNAM3D tceq_${dom}km 

# CANTYP
setenv CANTYP $INPDIR/CT3_$GDNAM3D.single.nc

# LAIS46
setenv LAIS46 $INPDIR/LAI3_$GDNAM3D.single.nc

# MGNMET
setenv MGNMET $METDIR/MET.MEGAN.$GDNAM3D.rad45.${SDATE}.single.nc

# Output
setenv CANMET $OUTDIR/CANMET.$GDNAM3D.${SDATE}.single.nc

########################################################################
## Run MEGAN
if ( $RUN_MEGAN == 'Y' ) then
   rm -f $CANMET
   $EXE | tee $LOGDIR/log.run.$PROG.$GDNAM3D.$SDATE.txt
endif

@ JD++
end  # End while JD

end  # dom
