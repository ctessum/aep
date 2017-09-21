! julday:  a function routine that converts gregorian day, month and year
!          to julian day.
!
! These routines originally came from the Harvard Ocean Modeling System, as
! did the idea for using a modified Julian day (the true Julian day  minus
! a constant) as a running time variable. The earlier use of modified
! julian days is with astronomers, as detailed in Numerical Recipes, by
! Press and others. The common offset day is may 23, 1968 which is julian
! day 2440000. All time routines here use this day as the offset for julian
! day calculations. 
!
! http://www.ccpo.odu.edu/~klinck/SOGLOBEC/cruise/timedate/Readme
!
      integer function julday(mm,id,iyyy)
        parameter (igreg=15+31*(10+12*1582))
        if (iyyy.eq.0) pause 'there is no year zero.'
        if (iyyy.lt.0) iyyy=iyyy+1
        if (mm.gt.2) then
          jy=iyyy
          jm=mm+1
        else
          jy=iyyy-1
          jm=mm+13
        endif
        julday=int(365.25*jy)+int(30.6001*jm)+id+1720995
        if (id+31*(mm+12*iyyy).ge.igreg) then
          ja=int(0.01*jy)
          julday=julday+2-ja+int(0.25*ja)
        endif
        return
      end function julday