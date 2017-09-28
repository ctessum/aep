      PROGRAM RUN_MEGSEA_MODULE

!***********************************************************************
!   This program computes soil NO emission activity factor and isoprene
!   soil moisture activity using MCIP output variables.
!
!  DESCRIPTION:
!
!     Uses new NO algorithm NO = Normalized*Tadj*Padj*Fadj*Cadj
!     to estimate NO emissions
!     Information needed to estimate NO emissions
!     Julian Day          (integer)    JDATE
!     Surface Temperature (MCIP field) TA    (K)
!     Soil Moisture       (MCIP field) SOILM (M**3/M**3) (LSOIL)
!          (ratio of volume of water per volume of soil)
!     Soil Temperature    (MCIP field) SOILT (K)         (LSOIL)
!     Soil Type           (MCIP field) ISLTYP            (LSOIL)
!
!     saturation values for soil types (constants)       (LSOIL)
!     FOR PX Version, the Temperature adjustment factor accounts for wet
!     and dry soils
!                and  the precipitation adjustment factor accounts for
!                saturated soils
!     FOR the non-PX version, the basic algorithm remains with a
!     temperature adjustment factor (dry soil)
!                     and no adjustment for saturated soils
!
!     The following arrays are updated after each call to SOILNOX
!     PULTYPE   type of NO emission pulse
!     PULSEDATE julian date for the beginning of an NO pulse
!     PULSETIME        time for the beginning of an NO pulse
!
!     The calculation are based on the following paper by J.J. Yienger
!     and H. Levy II
!     J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol
!     100,11447-11464,1995
!
!     The Temperature Adjustment Factor is based on section 4.2 for wet
!     and dry soils with the following modification (PX version):
!       Instead of classifying soils as either 'wet' or 'dry', the wet
!       and dry adjustment is calculated at each grid cell.  A linear 
!       interpolation between the wet and dry adjustment factor is made 
!       using the relative amount of soil moisture in the top layer (1cm)
!       as the interpolating factor.  The relative amount of soil moisture 
!       is determined by taking the MCIP soil moisture field and dividing by the
!       saturation value defined for each soil type in the PX version of MCIP
!       the soil temperature is used in PX version
!
!     The Precipation Adjustment factor is based on section 4.1 with the
!     following modifications.
!       The rainrate is computed from the MCIP directly using a 24 hr daily total.
!       THe types of Pulses as described in YL95 were used to estimate
!       the NO emission rate.
!
!    Also see the following paper for more information:
!    Proceedings of the Air and Waste Management Association/U.S. Environmental Protection
!    Agency EMission Inventory Conference, Raleigh October 26-28, 1999 Raleigh NC
!    by Tom Pierce and Lucille Bender
!
!    REFERENCES
!
!    JACQUEMIN B. AND NOILHAN J. (1990), BOUND.-LAYER METEOROL., 52, 93-134.
!    J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,11447-11464,1995
!    T. Pierce and L. Bender, Examining the Temporal Variability of Ammonia and 
!      Nitric Oxide Emissions from Agricultural Proc Proceedings of the Air and Waste 
!      Management Association/U.S. Environmental Protection Agency EMission Inventory 
!      Conference, Raleigh October 26-28, 1999 Raleigh NC
!  PRECONDITIONS REQUIRED:
!     Normalized NO emissions, Surface Temperature, Soil Moisture, Soil type,
!     NO emission pulse type, soil moisture from previous time step, julian date
!     of NO emission pulse start, time of NO emission pulse start, soil type, 
!     SOIL TYPES, Land use data
!
!  SUBROUTINES AND FUNCTIONS CALLED (directly or indirectly):
!     FERTILIZER_ADJ computes fertlizer adjustment factor
!     VEG_ADJ        computes vegatation adjustment factor
!     GROWSEASON     computes day of growing season
!
! HISTORY:
!   07/21/11: Imported from SMOKE-BEIS v3.14 for MEGEAN v2.10 (Tan)
!   03/19/17: Make as an indpendent program (MEGSEA) (Ling Huang)
!   03/31/17: Add calculation for soil moisture activity (Ling Huang)
!*********************************************************************

      USE SOILNOX_FX
      USE MEGSEA

      IMPLICIT NONE

! INCLUDE FILES
      INCLUDE 'PARMS3.EXT'   ! I/O API parameters
      INCLUDE 'IODECL3.EXT'  ! I/O API function declarations
      INCLUDE 'FDESC3.EXT'   ! I/O API file desc. data structures
      INCLUDE 'MEGSEA.EXT'   ! wilting point info.

!...  EXTERNAL FUNCTIONS and their descriptions:
      INTEGER, EXTERNAL       ::   ENVINT
      INTEGER, EXTERNAL       ::   ENVYN
      LOGICAL      DSCGRID
      EXTERNAL     DSCGRID

!...  Program I/O files: From run script

! Program name
      CHARACTER*16  :: PROGNAME = 'MEGSEA'

! Netcdf file
      CHARACTER*16  :: MGNMET = 'MGNMET'       ! Meteorology file  
      CHARACTER*16  :: LAIS46 = 'LAIS46'       ! LAI file  
      CHARACTER*16  :: CANTYP = 'CANTYP'     ! canopy type file logical name

! output file
      CHARACTER*16  :: MGNSEA = 'MGNSEA'       ! Emission activity 

!...  Parameters for file units
      INTEGER  LOGDEV                          ! Logfile unit number

!...  External parameters
! From run script
      INTEGER       SDATE          ! Start date YYYYDDD
      INTEGER       STIME          ! Start time HHMMSS
      INTEGER       RLENG          ! Run length HHMMSS

! I/O API file parameters
      INTEGER :: JDATE        ! Date YYYYDDD from inpname
      INTEGER :: JTIME        ! Time HHMMSS from inpname
      INTEGER :: NCOLS        ! Number of columns
      INTEGER :: NROWS        ! Number of rows
      INTEGER :: NLAYS        ! Number of vertical layers
      INTEGER :: MXREC        ! Total number of timesteps
      INTEGER :: TSTEP        ! Time step

!...  Internal parameters
! Internal parameters (status and buffer)
      INTEGER       IOS                    ! i/o status
      CHARACTER*256 MESG                   ! message buffer

! Local variables and their descriptions:
      CHARACTER*16  :: GDNAM
      CHARACTER*16  :: CNAME        ! Coord name

!     variable from LAI file
      REAL, ALLOCATABLE :: LAIc( :,:,: )    ! Current time step LAI
      REAL, ALLOCATABLE :: LAT ( :,:,: )    ! Latitude

!     variable from MGNMET
      REAL, ALLOCATABLE :: TEMP (:,:,:)   ! Temperautre (K)
      REAL, ALLOCATABLE :: PRECADJ (:,:,:)   
      INTEGER, ALLOCATABLE :: SLTYP ( :,:,: ) ! soil type
      REAL, ALLOCATABLE :: SOILM ( :,:,: ) ! soil moisture
      REAL, ALLOCATABLE :: SOILT ( :,:,: ) ! soil temperature
 
      REAL, ALLOCATABLE    :: RSTYP( :,:,: )

      REAL, ALLOCATABLE :: CTF( :, :, : ) ! Canopy type factor arra

!     output variable
      REAL, ALLOCATABLE :: CFNO ( :,: )      ! Emission activity for crop
      REAL, ALLOCATABLE :: CFNOG ( :,: )     ! Emission activity for grass
      REAL, ALLOCATABLE :: GAMSM ( :,:,: )     ! Soil moisture activity for isoprene
      REAL, ALLOCATABLE :: GAMNO ( :,:,: )     ! Final NO emission activity

      INTEGER :: LAIp_I,LAIc_I
      INTEGER :: GDAY, GLEN
      INTEGER :: MXLAI,MXCT
      REAL :: t1,wilt,TMO1,TMO2

      LOGICAL :: LSOIL = .TRUE.

! loop indices
      INTEGER :: IDATE, ITIME
      INTEGER :: T,I,J,I_CT

!***********************************************************************

!--=====================================================================
!...  Begin program
!--=====================================================================

!----------------------------------------------------------------
!.....1) File set up and assign I/O parameters
!----------------------------------------------------------------
!...  Initialize log file unit
      LOGDEV = INIT3()
           !  Now I/O API is set up, and LOGUNIT is the unit number
           !  for the log file (or it 6 for st'd output).

!...  Get input parameters from run script
      MESG = 'Model start date (YYYYDDD)'
      SDATE = ENVINT( 'SDATE', MESG, JDATE, IOS )

      MESG = 'Model start time (HHMMSS)'
      STIME = ENVINT( 'STIME', MESG, JTIME, IOS )

      MESG = 'Model run length (HHMMSS)'
      RLENG = ENVINT( 'RLENG', MESG, MXREC*10000, IOS )

      CALL ENVSTR( 'GDNAM3D', MESG, 'ASACA36km', GDNAM, IOS )
      IF( .NOT. DSCGRID( GDNAM, CNAME, GDTYP3D,
     &              P_ALP3D, P_BET3D, P_GAM3D, XCENT3D, YCENT3D,
     &              XORIG3D, YORIG3D, XCELL3D, YCELL3D,
     &              NCOLS3D, NROWS3D, NTHIK3D ) ) THEN
         MESG = 'Could not get grid description.'
         CALL M3EXIT ( PROGNAME, 0, 0, MESG, 2 )
      ENDIF

!...  Open files


! Canopy type file
      WRITE(MESG,1030) 'Checking up canopy type file',0,0,0
      CALL M3MESG( MESG )
      IF ( .NOT. OPEN3( CANTYP, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (CANTYP, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      ! Check grid
      IF ( .NOT. FILCHK3 ( CANTYP,
     &              GRDDED3, NCOLS3D, NROWS3D, 1, NTHIK3D))  THEN
         MESG = 'CANTYP has differenet grid definition'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. DESC3( CANTYP ) ) THEN
         CALL NAMEVAL (CANTYP, MESG)  ! get input file name and path
         MESG = 'Could not get description of '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      MXCT = MXREC3D
      PRINT*,'MXCT=',MXCT

! LAI file
      WRITE(MESG,1030) 'Checking up LAI file',0,0,0
      CALL M3MESG( MESG )
      IF ( .NOT. OPEN3( LAIS46, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (LAIS46, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      ! Check grid
      IF ( .NOT. FILCHK3 ( LAIS46,
     &              GRDDED3, NCOLS3D, NROWS3D, 1, NTHIK3D))  THEN
         MESG = 'LAIS46 has differenet grid definition'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. DESC3( LAIS46 ) ) THEN
         CALL NAMEVAL (LAIS46, MESG)  ! get input file name and path
         MESG = 'Could not get description of '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      MXLAI = MXREC3D
      PRINT*,'MXLAI=',MXLAI

! Met file
      IF ( .NOT. OPEN3( MGNMET, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (MGNMET, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      ! Check grid
      IF ( .NOT. FILCHK3 ( MGNMET,
     &              GRDDED3, NCOLS3D, NROWS3D, 1, NTHIK3D))  THEN
         MESG = 'MGNMET has differenet grid definition'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. DESC3( MGNMET ) ) THEN
         CALL NAMEVAL (MGNMET, MESG)  ! get input file name and path
         MESG = 'Could not get description of '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF

      NCOLS = NCOLS3D
      NROWS = NROWS3D
      TSTEP = TSTEP3D

!...  Get input parameters from run script
      MESG = 'Model start date (YYYYDDD)'
      SDATE = ENVINT( 'SDATE', MESG, JDATE, IOS )

      MESG = 'Model start time (HHMMSS)'
      STIME = ENVINT( 'STIME', MESG, JTIME, IOS )

      MESG = 'Model run length (HHMMSS)'
      RLENG = ENVINT( 'RLENG', MESG, MXREC*10000, IOS )

!...  Check start date, start time, end date, end time in MGNMET
      WRITE(MESG,1030) 'Checking up MGNMET',0,0,0
      CALL M3MESG( MESG )
      IDATE = SDATE; ITIME = STIME
      IF ( .NOT. CHECK3( MGNMET, 'TEMP2', IDATE, ITIME ) ) THEN
         MESG = 'Starting time not on met file'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      CALL NEXTIME ( IDATE, ITIME, RLENG-10000 )
      IF ( .NOT. CHECK3( MGNMET, 'TEMP2', IDATE, ITIME ) ) THEN
         MESG = 'Ending time not on met file'
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IDATE = SDATE; ITIME = STIME

!...  Set output parameters that are different from met file and open file
      SDATE3D = SDATE                ! From run-script
      STIME3D = STIME                ! From run-script
      MXREC3D = RLENG / 10000
      MXREC = MXREC3D
      NLAYS3D = 1
      NVARS3D = 2

      VNAME3D(1) = 'GAMNO'
      UNITS3D(1) = ' '
      VTYPE3D(1) = M3REAL
      VDESC3D(1) = ' '

      VNAME3D(2) = 'GAMSM'
      UNITS3D(2) = ' '
      VTYPE3D(2) = M3REAL
      VDESC3D(2) = ' '


      IF ( .NOT. OPEN3( MGNSEA, FSCREA3, PROGNAME ) ) THEN
         CALL NAMEVAL (MGNSEA, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF

!----------------------------------------------------------------
!.....2) Calculate emission activity 
!----------------------------------------------------------------
!...  Allocate memory
      ALLOCATE ( TEMP    ( MXREC, NCOLS, NROWS ), STAT = IOS )
      CALL CHECKMEM    ( IOS, 'TEMP',        PROGNAME )
      ALLOCATE ( PRECADJ    ( MXREC, NCOLS, NROWS ), STAT = IOS )
      CALL CHECKMEM    ( IOS, 'PRECADJ',        PROGNAME )
      ALLOCATE ( LAIc    ( MXREC, NCOLS, NROWS ), STAT = IOS )
      CALL CHECKMEM    ( IOS, 'LAIc',        PROGNAME )
      ALLOCATE ( LAT    ( MXREC, NCOLS, NROWS ), STAT = IOS )
      CALL CHECKMEM    ( IOS, 'LAT',        PROGNAME )
      ALLOCATE ( SOILT    ( MXREC, NCOLS, NROWS ), STAT = IOS )
      CALL CHECKMEM    ( IOS, 'SOILT',  PROGNAME )
      ALLOCATE ( SOILM ( MXREC, NCOLS, NROWS ), STAT = IOS )
      CALL CHECKMEM    ( IOS, 'SOILM',  PROGNAME )
      ALLOCATE ( SLTYP ( MXREC, NCOLS, NROWS ), STAT = IOS )
      CALL CHECKMEM    ( IOS, 'SLTYP',  PROGNAME )
      ALLOCATE ( RSTYP ( MXREC, NCOLS, NROWS ), STAT = IOS )
      CALL CHECKMEM    ( IOS, 'RSTYP',  PROGNAME )
      ALLOCATE ( CFNO  ( NCOLS, NROWS ), STAT = IOS ) ! TEMP OUTPUT
      CALL CHECKMEM    ( IOS, 'CFNO',   PROGNAME )
      ALLOCATE ( CFNOG ( NCOLS, NROWS ), STAT = IOS ) ! TEMP OUTPUT
      CALL CHECKMEM    ( IOS, 'CFNOG',  PROGNAME )
      ALLOCATE ( GAMNO ( MXREC, NCOLS, NROWS ), STAT = IOS )
      CALL CHECKMEM    ( IOS, 'GAMNO',  PROGNAME )
      ALLOCATE ( GAMSM ( MXREC, NCOLS, NROWS ), STAT = IOS )
      CALL CHECKMEM    ( IOS, 'GAMSM',  PROGNAME )
      ALLOCATE ( CTF( NRTYP, NCOLS, NROWS ), STAT = IOS )
      CALL CHECKMEM ( IOS, 'CTF', PROGNAME )
      
      PRINT*,'NRTYP=',NRTYP
      
!...  Read CANTYP (DOES NOT DEPEND ON T)
      DO I_CT = 1, MXCT
         IF ( .NOT. READ3(CANTYP,'CTS',1,0,(I_CT-1)*10000,
     &        CTF(I_CT,:,:))) THEN
            MESG = 'Error reading CTS'
            CALL M3EXIT(PROGNAME,IDATE,ITIME,MESG,2)
         ENDIF
      ENDDO

!...  Start the loop over the time period
      IDATE = SDATE
      ITIME = STIME
      PRINT*,'MXREC=',MXREC
      DO T = 1, MXREC

        WRITE(MESG,*) 'Processing at :', IDATE, ITIME
        CALL M3MESG( MESG )
! ...  Initialize hourly variables
        TEMP(T,:,:) = 0.
        LAIc(T,:,:)  = 0.
        CFNO = 0.
        CFNOG = 0.
        GAMNO(T,:,:) = 0.

        IF ( .NOT. READ3(MGNMET,'TEMP2',  ALLAYS3,IDATE,ITIME,TEMP(T,:,:)))
     &  THEN
          MESG = 'Error reading temperature'
          CALL M3EXIT(PROGNAME,IDATE,ITIME,MESG,2)
        ENDIF

        IF ( .NOT. READ3(MGNMET,'PREC_ADJ',ALLAYS3,IDATE,ITIME,PRECADJ(T,:,:)))
     &     THEN
          MESG = 'Error reading precipitation adjustment'
          CALL M3EXIT(PROGNAME,IDATE,ITIME,MESG,2)
        ENDIF



! Read LAIS46 file 
        ! Find LAIc from date
        CALL FINDLAI(IDATE,MXLAI,LAIp_I,LAIc_I)
        WRITE(MESG,*) 'Found LAI current period for YYYYJJJ : ',
     &  IDATE,LAIc_I
        CALL M3MESG( MESG )

        WRITE(MESG,'(I0.2)') LAIc_I
        IF ( .NOT. READ3(LAIS46,'LAI'//TRIM(MESG),ALLAYS3,0,0,LAIc(T,:,:))) 
     &  THEN
          MESG = 'Error reading LAI at current time step'
          CALL M3EXIT(PROGNAME,IDATE,ITIME,MESG,2)
        ENDIF

        IF ( .NOT. READ3(LAIS46,'LAT',1,0,0,LAT(T,:,:))) THEN
          MESG = 'Error reading LAT'
          CALL M3EXIT(PROGNAME,IDATE,ITIME,MESG,2)
        ENDIF


        
        IF ( READ3(MGNMET,'SOIM1', ALLAYS3,IDATE,ITIME,SOILM(T,:,:)) .AND.
     &       READ3(MGNMET,'SOIT1', ALLAYS3,IDATE,ITIME,SOILT(T,:,:)) .AND.
     &       READ3(MGNMET,'SLTYP', ALLAYS3,IDATE,ITIME,RSTYP(T,:,:)) ) THEN

          MESG = 'Using SOIL parameters in NOx adjustment'
          CALL M3MESG( MESG )
          LSOIL = .TRUE.
          SLTYP(T,:,:) = INT(RSTYP(T,:,:))
        ELSE
          MESG = 'SOIL parameters are not available'
          CALL M3MESG( MESG )
          LSOIL = .FALSE.
        ENDIF
        CALL NEXTIME( IDATE, ITIME, TSTEP )
      ENDDO ! End loop for time step (T)
      

      
!      IDATE = SDATE
!      ITIME = STIME
      PRINT*,'MXREC=',MXREC
!      DO T = 1, MXREC
!! ... Calculate emission activity factor for NO
!        WRITE(MESG,1030) 'Estimating soil NOx adj: ',T,IDATE,ITIME
!        CALL M3MESG( MESG )
!        
!        CALL SOILNOX(IDATE,ITIME,NCOLS,NROWS,
!     &               TEMP(T,:,:),LSOIL,SLTYP(T,:,:), SOILM(T,:,:), SOILT(T,:,:),
!     &               LAIc(T,:,:), LAT(T,:,:), PRECADJ(T,:,:),
!     &               CFNO, CFNOG )
!        
!
!        DO I = 1,NCOLS
!          DO J = 1,NROWS
!            CALL GROWSEASON(IDATE,LAT(T,I,J),GDAY,GLEN)
!            IF (GDAY .EQ. 0) THEN
!             ! non growing season
!             ! CFNOG for everywhere
!               GAMNO(T,I,J) = CFNOG(I,J)
!
!             ELSE IF (GDAY .GT. 0 .AND. GDAY .LE. 366) THEN
!             ! growing season
!             ! CFNOG for everywhere except crops
!             TMO1 = 0.
!             TMO2 = 0.
!             DO I_CT = 1,5
!               TMO1 = TMO1 + CTF(I_CT,I,J)
!               TMO2 = TMO2 + CTF(I_CT,I,J) * CFNOG(I,J)
!             ENDDO
!             ! CFNO for crops
!             TMO1 = TMO1 + CTF(6,I,J)
!             TMO2 = TMO2 + CTF(6,I,J) * CFNO(I,J)
!             IF (TMO1 .EQ. 0.0) THEN
!                GAMNO(T,I,J) = 0.0
!             ELSE
!                GAMNO(T,I,J) = TMO2 / TMO1
!             ENDIF
!
!           ELSE
!             ! bad GDAY
!             WRITE(MESG,*) 'Bad GDAY ',GDAY
!             CALL M3EXIT(PROGNAME,IDATE,ITIME,MESG,2)
!           ENDIF
!
!           ENDDO  !NROWS
!        ENDDO  !NCOLS
!
!        WRITE(MESG,1030) 'Finished soil NOx adj: ',T,IDATE,ITIME
!        CALL M3MESG( MESG )
!
!! ... Calculate soil moisture activity factor for isoprene emissions
!        IF ( LSOIL ) THEN
!        WRITE(MESG,1030) 'Calculating soil moisture activity
!     &                                    factor: ',T,IDATE,ITIME
!
!        CALL M3MESG( MESG )
!        SLTYP(T,:,:) = INT(RSTYP(T,:,:))
!           DO I = 1, NCOLS
!             DO J = 1, NROWS
!               wilt = WWLT(SLTYP(T,I,J))
!               t1 = wilt + d1
!               IF ( SOILM(T,I,J) < wilt ) THEN
!                   GAMSM(T,I,J) = 0
!               ELSE IF ( SOILM(T,I,J) >= wilt .AND. SOILM(T,I,J) < t1 ) THEN
!                   GAMSM(T,I,J) = (SOILM(T,I,J) - wilt)/d1
!               ELSE
!                   GAMSM(T,I,J) = 1
!               END IF
!             END DO ! NROWS
!           END DO ! NCOLS
!         ENDIF
!        CALL NEXTIME( IDATE, ITIME, TSTEP )
!      ENDDO ! End loop for time step (T)
      
      CALL RUN_MEGSEA(SDATE, STIME, MXREC, NCOLS, NROWS, TSTEP,
     &                      TEMP, PRECADJ, CTF, LAIc, LAT, SOILM, SOILT, RSTYP,
     &                      LSOIL, GAMNO, GAMSM)

!----------------------------------------------------------------
!.....3) Write out the calculated EA 
!----------------------------------------------------------------
      IDATE = SDATE
      ITIME = STIME
      PRINT*,'MXREC=',MXREC
      DO T = 1, MXREC
!... Write emission to file
        WRITE(MESG,1030) 'Writing emission at ',T,IDATE,ITIME
        CALL M3MESG( MESG )

! #1
        IF ( .NOT. WRITE3(MGNSEA,VNAME3D(1),IDATE,ITIME,
     &               GAMNO(T,:,:))) THEN
          CALL NAMEVAL (MGNSEA, MESG)  ! get input file name and path
          MESG = 'Error writing to file: '//TRIM(MESG)
          CALL M3EXIT(PROGNAME,IDATE,ITIME,MESG,2)
        ENDIF

! #2
        IF ( .NOT. WRITE3(MGNSEA,VNAME3D(2),IDATE,ITIME,
     &               GAMSM(T,:,:))) THEN
          CALL NAMEVAL (MGNSEA, MESG)  ! get input file name and path
          MESG = 'Error writing to file: '//TRIM(MESG)
          CALL M3EXIT(PROGNAME,IDATE,ITIME,MESG,2)
        ENDIF

       CALL NEXTIME( IDATE, ITIME, TSTEP )
      ENDDO ! End loop for time step (T)

!... Exit and close file
      CALL M3EXIT(PROGNAME,0,0,' ',0)

      DEALLOCATE ( TEMP    )
      DEALLOCATE ( PRECADJ )
      DEALLOCATE ( LAIc    )
      DEALLOCATE ( LAT     )
      DEALLOCATE ( CFNO )
      DEALLOCATE ( CFNOG )
      DEALLOCATE ( SOILM )
      DEALLOCATE ( SOILT )
      DEALLOCATE ( SLTYP )
      DEALLOCATE ( RSTYP )
      DEALLOCATE ( GAMSM )
      DEALLOCATE ( GAMNO )
      DEALLOCATE ( CTF )
!
! ... Exit and close file
      CALL M3EXIT(PROGNAME,0,0,' ',0)

!--=====================================================================
!...  FORMAT
!--=====================================================================
1030  FORMAT (A30,I8,X,I8,X,I8)

!--=====================================================================
!...  End program
!--=====================================================================

      END PROGRAM RUN_MEGSEA_MODULE

