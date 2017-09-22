      INTEGER FUNCTION Seconds(TIME)
        
        IMPLICIT NONE
        
        INTEGER, INTENT(IN) :: TIME           !  time (encoded  HHMMSS)
        INTEGER :: MMSS, HH, MM, SS
        
        HH = NINT( DFLOAT( TIME ) / 10000. )
        MMSS = TIME - HH * 10000
        MM = NINT( DFLOAT( MMSS ) / 100. )
        SS = MMSS - MM * 100
        Seconds = HH * 3600 + MM * 60 + SS ! Number of seconds in TIME
        
        RETURN     
      END FUNCTION
      
      SUBROUTINE IncrementDateTime  ( JDATE , JTIME, DTIME )
      
        IMPLICIT  NONE
        
        INTEGER, INTENT(INOUT) :: JDATE           !  date (encoded YYYYDDD)
        INTEGER, INTENT(INOUT) :: JTIME           !  time (encoded  HHMMSS)
        INTEGER, INTENT(IN   ) :: DTIME           !  time increment (encoded HHMMSS)
        
        INTEGER, EXTERNAL :: julday, Seconds
        
        INTEGER :: YEAR, DAY, JULIANIN, JULIAN_TOTAL
        INTEGER :: JTIME_seconds, DTIME_seconds
        REAL*8 :: fracday_sum, fracday_remaining, julian_tmp
        INTEGER :: iday,month,iyear,iyday
        INTEGER :: hour, minute, second

        ! Get input julian day
        YEAR = NINT( DFLOAT( JDATE ) / 1000. )
        DAY = JDATE - YEAR * 1000        
        JULIANIN = julday( 1, 1, YEAR ) + DAY - 1
        
        ! Get fraction of day to add up to the julian day
        JTIME_seconds = Seconds(JTIME) ! Number of seconds in JTIME
        DTIME_seconds = Seconds(DTIME) ! Number of seconds in JTIME
        fracday_sum = DFLOAT(JTIME_seconds + DTIME_seconds) / 86400. ! fraction of day
        
        ! Get total julian day with added fraction of day
        julian_tmp = DFLOAT(JULIANIN) + fracday_sum
        JULIAN_TOTAL = INT(julian_tmp) 
        fracday_remaining = julian_tmp - JULIAN_TOTAL
        
        ! Convert julian day to gragorian date
        call gregorian (JULIAN_TOTAL,iday,month,iyear,iyday)
        JDATE = iyear * 1000 + iyday
        
        ! Convert remaining fraction of day to time
        JTIME_seconds = NINT(fracday_remaining * 86400)
        hour = NINT( DFLOAT(JTIME_seconds) / 3600. )
        minute = NINT( (JTIME_seconds - 3600. * hour) / 60. )
        second =  JTIME_seconds - 3600*hour - 60*minute
        JTIME = hour * 10000 + minute * 100 + second
        
        RETURN
      END SUBROUTINE IncrementDateTime