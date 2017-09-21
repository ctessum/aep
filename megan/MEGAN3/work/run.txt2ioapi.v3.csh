#! /bin/csh -f
########################################################################
## Common setups
source setcase.csh

foreach dom ( 36km 12km )
foreach scen ( J0 J4 )

setenv PROMPTFLAG N
setenv PROG   txt2ioapi
setenv EXEDIR /home/Paul-Antoine/src/MEGAN3/bin
setenv EXEC   $EXEDIR/$PROG
setenv GRIDDESC $cwd/GRIDDESC
setenv GDNAM3D tceq_$dom

## File setups
## Inputs
setenv EFSTXTF $MGNINP/MAP/grid_EF.${GDNAM3D}.$scen.csv
setenv CTTXTF $MGNINP/MAP/CT3.${GDNAM3D}.csv
setenv LAITXTF $MGNINP/MAP/LAI3.${GDNAM3D}.csv
setenv W126TXTF $MGNINP/MAP/grid_W126.$GDNAM3D.csv
setenv LDFTXTF $MGNINP/MAP/grid_LDF.$GDNAM3D.$scen.csv
## Outputs
setenv EFMAPS  $MGNINP/MAP/EFMAPS3.${GDNAM3D}.$scen.ncf
setenv CANTYP  $MGNINP/MAP/CT3_${GDNAM3D}.ncf
setenv LAIS46  $MGNINP/MAP/LAI3_${GDNAM3D}.ncf
setenv W126FILE $MGNINP/MAP/W126_${GDNAM3D}.ncf
setenv LDFILE $MGNINP/MAP/LDF_${GDNAM3D}.$scen.ncf

## Run control
setenv RUN_EFS T       # [T|F]
setenv RUN_LAI T       # [T|F]
setenv RUN_CANTYP T    # [T|F]
setenv RUN_W126 T      # [T|F]
setenv RUN_LDF T       # [T|F]
########################################################################


## Run TXT2IOAPI
rm -f $LAIS46
rm -f $EFMAPS
rm -f $CANTYP
rm -f $W126FILE
rm -f $LDFILE
if ( ! -e $MGNLOG/$PROG ) mkdir -p $MGNLOG/$PROG
if ( $RUN_EFS == T || $RUN_LDF == T ) then
$EXEC | tee $MGNLOG/$PROG/log.run.$PROG.${GDNAM3D}.$scen.txt
else
$EXEC | tee $MGNLOG/$PROG/log.run.$PROG.${GDNAM3D}.txt
endif

end
end
