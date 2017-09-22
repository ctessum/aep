       subroutine gregorian (julian,iday,month,iyear,iyday)
!
!=======================================================================
!                                                                    ===
!  This routine converts Julian day number to calendar (Gregorian)   ===
!  date.                                                             ===
!                                                                    ===
!  On Input:                                                         ===
!                                                                    ===
!     JULIAN   Julian day (integer)                                  ===
!                                                                    ===
!  On Ouput:                                                         ===
!                                                                    ===
!     IDAY     day of the month (integer)                            ===
!     IYDAY    year day (integer)                                    ===
!     IYEAR    year (integer)                                        ===
!     MONTH    month of the year (integer)                           ===
!                                                                    ===
!=======================================================================
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
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
       integer iday,igreg,ileap,iyd(13),iyday,iydl(13),iyear,ja,jalpha,
     &         jb,jc,jd,je,julian,month
       data iyd  /1,32,60,91,121,152,182,213,244,274,305,335,366/
       data iydl /1,32,61,92,122,153,183,214,245,275,306,336,367/
       parameter (igreg=2299161)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
       if (julian.ge.igreg) then
         jalpha=int((float(julian-1867216)-0.25)/36524.25)
         ja=julian+1+jalpha-int(0.25*float(jalpha))
       else
         ja=julian
       endif
       jb=ja+1524
       jc=int(6680.+(float(jb-2439870)-122.1)/365.25)
       jd=365*jc+int(0.25*float(jc))
       je=int(float(jb-jd)/30.6001)
       iday=jb-jd-int(30.6001*float(je))
       month=je-1
       if (month.gt.12) month=month-12
       iyear=jc-4715
       if (month.gt.2) iyear=iyear-1
       if (iyear.le.0)iyear=iyear-1
       ileap=mod(iyear,4)
       if (ileap.eq.0) then
           iyday=iydl(month)+iday-1
       else
           iyday=iyd(month)+iday-1
       endif
       return
       end
