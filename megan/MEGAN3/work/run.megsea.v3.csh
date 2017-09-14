#! /bin/csh -f
########################################################################
source setcase.csh
## Directory setups
setenv PRJ USA 
setenv PROMPTFLAG N

# Program directory
setenv PROG   megsea
setenv EXEDIR /disk8/MEGAN3/source_code/delivery/for_xiaoyan/MEGAN3/bin
setenv EXE    $EXEDIR/$PROG

# Input map data directory
setenv INPDIR $MGNINP

# Met data directory
setenv METDIR $MGNINP/MGNMET

# Output directory
setenv OUTDIR $MGNINT

# Log directory
setenv LOGDIR $MGNLOG/megsea
mkdir -p $LOGDIR

########################################################################

foreach dom ( 36 12)
set JD = 2013145
while ($JD <= 2013198)
########################################################################
# Set up time and date to process
setenv SDATE $JD        #start date
setenv STIME 0
setenv RLENG 250000

########################################################################

########################################################################
# Set up for MEGAN
setenv RUN_MEGAN   Y       # Run megan?

# Grid definition
setenv GRIDDESC $cwd/GRIDDESC
setenv GDNAM3D tceq_${dom}km 

# CANTYP
setenv CANTYP $INPDIR/MAP/CT3_$GDNAM3D.ncf

# LAIS46
setenv LAIS46 $INPDIR/MAP/LAI3_$GDNAM3D.ncf

# MGNMET
setenv MGNMET $METDIR/MET.MEGAN.$GDNAM3D.rad45.${JD}.ncf

# Output
setenv MGNSEA $MGNINT/MGNSEA.$GDNAM3D.${SDATE}.ncf

########################################################################
## Run MEGAN
if ( $RUN_MEGAN == 'Y' ) then
   rm -f $MGNSEA
   $EXE | tee $LOGDIR/log.run.$PROG.$GDNAM3D.$SDATE.txt
endif

@ JD++
end  # End while JD

end # dom
