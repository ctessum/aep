      MODULE MEGSEA

!***********************************************************************
!   This subroutine computes soil NO emission activity factor and isoprene
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

      IMPLICIT NONE
      
      CONTAINS
      
      SUBROUTINE RUN_MEGSEA(SDATE, STIME, MXREC, NCOLS, NROWS, TSTEP,
     &                      TEMP, PRECADJ, CTF, LAIc, LAT, SOILM, SOILT, RSTYP,
     &                      LSOIL, GAMNO, GAMSM)
                                                           
! INCLUDE FILES                                            
      INCLUDE 'MEGSEA.EXT'   ! wilting point info.

!...  Program I/O files: From run script

! Program name
!      CHARACTER*16  :: PROGNAME = 'MEGSEA'

!...  External parameters
! From run script
      INTEGER       SDATE          ! Start date YYYYDDD
      INTEGER       STIME          ! Start time HHMMSS

! I/O API file parameters
      INTEGER :: NCOLS        ! Number of columns
      INTEGER :: NROWS        ! Number of rows
      INTEGER :: MXREC        ! Total number of timesteps
      INTEGER :: TSTEP        ! Time step

!...  Internal parameters
! Internal parameters (status and buffer)
      INTEGER       IOS                    ! i/o status
      CHARACTER*256 MESG                   ! message buffer

!     variable from LAI file
      REAL :: LAIc(MXREC, NCOLS, NROWS)    ! Current time step LAI
      REAL :: LAT (MXREC, NCOLS, NROWS)    ! Latitude

!     variable from MGNMET
      !REAL, ALLOCATABLE :: TEMP (:,:,:)   ! Temperautre (K)
      REAL :: TEMP(MXREC, NCOLS, NROWS)   ! Temperautre (K)
      REAL :: PRECADJ (MXREC, NCOLS, NROWS) 
      INTEGER, ALLOCATABLE :: SLTYP ( :,:,: ) ! soil type (TEMPORARY VARIABLE)
      REAL :: SOILM (MXREC, NCOLS, NROWS) ! soil moisture
      REAL :: SOILT (MXREC, NCOLS, NROWS) ! soil temperature
 
      REAL    :: RSTYP(MXREC, NCOLS, NROWS)

      REAL :: CTF( NRTYP, NCOLS, NROWS ) ! Canopy type factor arra

!     output variable
      REAL, ALLOCATABLE :: CFNO ( :,: )      ! Emission activity for crop (TEMPORARY VARIABLE)
      REAL, ALLOCATABLE :: CFNOG ( :,: )     ! Emission activity for grass (TEMPORARY VARIABLE)
      REAL :: GAMSM (MXREC, NCOLS, NROWS)     ! Soil moisture activity for isoprene
      REAL :: GAMNO (MXREC, NCOLS, NROWS)     ! Final NO emission activity

      INTEGER :: GDAY, GLEN
      REAL :: t1,wilt,TMO1,TMO2

      LOGICAL :: LSOIL

! loop indices
      INTEGER :: IDATE, ITIME
      INTEGER :: T,I,J,I_CT

!***********************************************************************

      !PRINT*,'SDATE=',SDATE
      !PRINT*,'STIME=',STIME
      !PRINT*,'MXREC=',MXREC
      !PRINT*,'NCOLS=',NCOLS
      !PRINT*,'NROWS=',NROWS
      !PRINT*,'TSTEP=',TSTEP
      !PRINT*,'TEMP=',TEMP
      !PRINT*,'PRECADJ=',PRECADJ
      !PRINT*,'CTF=',CTF
      !PRINT*,'LAIc=',LAIc
      !PRINT*,'LAT=',LAT
      !PRINT*,'SOILM=',SOILM
      !PRINT*,'SOILT=',SOILT
      !PRINT*,'RSTYP=',RSTYP
      !PRINT*,'LSOIL=',LSOIL

! ... Allocate memory
      ALLOCATE ( SLTYP ( MXREC, NCOLS, NROWS ), STAT = IOS )
      !CALL CHECKMEM    ( IOS, 'SLTYP',  PROGNAME )
      ALLOCATE ( CFNO  ( NCOLS, NROWS ), STAT = IOS ) ! TEMP OUTPUT
      !CALL CHECKMEM    ( IOS, 'CFNO',   PROGNAME )
      ALLOCATE ( CFNOG ( NCOLS, NROWS ), STAT = IOS ) ! TEMP OUTPUT
      !CALL CHECKMEM    ( IOS, 'CFNOG',  PROGNAME )
      
      CFNO = 0.
      CFNOG = 0.
      
      IDATE = SDATE
      ITIME = STIME
      !PRINT*,'MXREC=',MXREC
      DO T = 1, MXREC
! ... Calculate emission activity factor for NO
        WRITE(MESG,1030) 'Estimating soil NOx adj: ',T,IDATE,ITIME
!        CALL M3MESG( MESG ) ! REPLACE
        
        SLTYP(T,:,:) = INT(RSTYP(T,:,:))
        
        CALL SOILNOX(IDATE,ITIME,NCOLS,NROWS,
     &               TEMP(T,:,:),LSOIL,SLTYP(T,:,:), SOILM(T,:,:), SOILT(T,:,:),
     &               LAIc(T,:,:), LAT(T,:,:), PRECADJ(T,:,:),
     &               CFNO, CFNOG )
     
        DO I = 1,NCOLS
          DO J = 1,NROWS
            CALL GROWSEASON(IDATE,LAT(T,I,J),GDAY,GLEN)
            IF (GDAY .EQ. 0) THEN
             ! non growing season
             ! CFNOG for everywhere
               GAMNO(T,I,J) = CFNOG(I,J)

            ELSE IF (GDAY .GT. 0 .AND. GDAY .LE. 366) THEN
             ! growing season
             ! CFNOG for everywhere except crops
             TMO1 = 0.
             TMO2 = 0.
             DO I_CT = 1,5
               TMO1 = TMO1 + CTF(I_CT,I,J)
               TMO2 = TMO2 + CTF(I_CT,I,J) * CFNOG(I,J)
             ENDDO
             ! CFNO for crops
             TMO1 = TMO1 + CTF(6,I,J)
             TMO2 = TMO2 + CTF(6,I,J) * CFNO(I,J)
             IF (TMO1 .EQ. 0.0) THEN
                GAMNO(T,I,J) = 0.0
             ELSE
                GAMNO(T,I,J) = TMO2 / TMO1
             ENDIF

            ELSE
             ! bad GDAY
             WRITE(MESG,*) 'Bad GDAY ',GDAY
!             CALL M3EXIT(PROGNAME,IDATE,ITIME,MESG,2) ! REPLACE
            ENDIF

           ENDDO  !NROWS
        ENDDO  !NCOLS

        WRITE(MESG,1030) 'Finished soil NOx adj: ',T,IDATE,ITIME
!        CALL M3MESG( MESG ) ! REPLACE

! ... Calculate soil moisture activity factor for isoprene emissions
        IF ( LSOIL ) THEN
        WRITE(MESG,1030) 'Calculating soil moisture activity
     &                                    factor: ',T,IDATE,ITIME

!        CALL M3MESG( MESG ) ! REPLACE
        SLTYP(T,:,:) = INT(RSTYP(T,:,:))
           DO I = 1, NCOLS
             DO J = 1, NROWS
               wilt = WWLT(SLTYP(T,I,J))
               t1 = wilt + d1
               IF ( SOILM(T,I,J) < wilt ) THEN
                   GAMSM(T,I,J) = 0
               ELSE IF ( SOILM(T,I,J) >= wilt .AND. SOILM(T,I,J) < t1 ) THEN
                   GAMSM(T,I,J) = (SOILM(T,I,J) - wilt)/d1
               ELSE
                   GAMSM(T,I,J) = 1
               END IF
             END DO ! NROWS
           END DO ! NCOLS
         ENDIF
        CALL IncrementDateTime ( IDATE, ITIME, TSTEP )
      ENDDO ! End loop for time step (T)
      
! ... Free memory
      DEALLOCATE ( CFNO )
      DEALLOCATE ( CFNOG )
      DEALLOCATE ( SLTYP )
      

!--=====================================================================
!...  FORMAT
!--=====================================================================
1030  FORMAT (A30,I8,X,I8,X,I8)

!--=====================================================================
!...  End subroutine
!--=====================================================================
      END SUBROUTINE RUN_MEGSEA
      
      END MODULE MEGSEA
      
      SUBROUTINE RUN_MEGSEA_C(SDATE, STIME, MXREC, NCOLS, NROWS, TSTEP, NRTYP,
     &                      TEMP, PRECADJ, CTF, LAIc, LAT, SOILM, SOILT, RSTYP,
     &                      LSOIL, GAMNO, GAMSM) BIND(C,name='run_megsea_c')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_BOOL, C_FLOAT
      USE MEGSEA
      
      ! input parameters
      integer(c_int), intent(in)  :: SDATE, STIME, MXREC, NCOLS, NROWS, TSTEP, NRTYP
      logical(c_bool), intent(in)  :: LSOIL

      ! input arrays
      real(c_float), intent(in)  :: TEMP(MXREC, NCOLS, NROWS)
      real(c_float), intent(in)  :: PRECADJ(MXREC, NCOLS, NROWS)
      real(c_float), intent(in)  :: CTF( NRTYP, NCOLS, NROWS )
      real(c_float), intent(in)  :: LAIc(MXREC, NCOLS, NROWS)
      real(c_float), intent(in)  :: LAT(MXREC, NCOLS, NROWS)
      real(c_float), intent(in)  :: SOILM(MXREC, NCOLS, NROWS)
      real(c_float), intent(in)  :: SOILT(MXREC, NCOLS, NROWS)
      real(c_float), intent(in)  :: RSTYP(MXREC, NCOLS, NROWS)
      
      ! output arrays
      real(c_float), intent(out)  :: GAMNO(MXREC, NCOLS, NROWS)
      real(c_float), intent(out)  :: GAMSM(MXREC, NCOLS, NROWS)
      
      !print*,'SDATE=',SDATE
      
      CALL RUN_MEGSEA(SDATE, STIME, MXREC, NCOLS, NROWS, TSTEP,
     &                      TEMP, PRECADJ, CTF, LAIc, LAT, SOILM, SOILT, RSTYP,
     &                      logical(LSOIL, 4), GAMNO, GAMSM)
      
      END SUBROUTINE RUN_MEGSEA_C

