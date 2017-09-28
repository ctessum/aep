      MODULE MEGVEA

! PURPOSE: Calculate Vegetation emission activity (EA) for each emission
!		class as the product of EA for individual drivers
!		calculated by the following functions
!
! Vegetation Emission Activity (EA) algorithm FUNCTIONS
!
!   GAMTLD: EA Temperature response (light dependent emission) 
!   GAMTLI: EA Temperature response (light independent emission)
!   GAMP: EA Light response
!   GAMTP: combines GAMLD, GAMLI, GAMP to get canopy average
!
!   GAMLA: EA leaf age response 
!   GAMBD: EA bidirectional exchange LAI response
!
!   CDEA: Canopy depth emission response
!
!   GAMHW: EA response to high wind storms
!   GAMHT: EA resposne to high temperature
!   GAMLT: EA response to low temperature
!
!   GAMAQ: EA response to air pollution
!
!   GAMCO2: EA CO2 response (only applied to isoprene)
!   GAMSM: EA response to soil moisture (multiplied with LDF)
!
! INCLUDE FILES
!     'PARMS3.EXT'   ! I/O API parameters
!     'IODECL3.EXT'  ! I/O API function declarations
!     'FDESC3.EXT'   ! I/O API file desc. data structures
!     'MEGVEA.EXT'    ! coefficients
!
!  INPUT Files
!	Single value for each location
!		LDF: Light dependent fraction (for categories other than
!		monoterpene, use constant values from MEGVEA.EXT)
!		AQ:  Air Quality indicator
!	Time series for each location
!		LAI: Leaf Area Index 
!		GAMSM: emission activity response to soil moisture
!		MaxT: Daily maximum temperature (K)
!		MinT: Daily minimum temperature (K)
!		MaxWS: Daily mximum wind speed (m/s)
!               D_TEMP: Daily average temperature (K)
!               D_PPFD: Daily averaged PPFD (umol/m2.s)
!	Hourly time series for each canopy layer in each location
!		sunfrac: fraction of leaves that are sunlit
!		SUNT: leaf Temperature of sunlit leaves (K)
!		SUNP: sun leaf PPFD visible light (micromol/m2/s)
!		SHAT: leaf Temperature of shade leaves (K)
!		SHAP: shade leaf PPFD visible light (micromol/m2/s)
!	
!  OUTPUT Files
!	Hourly time series for each location 
!		Emission activity for each of the 20 emission types
!
!
! HISTORY:
!   Based on code initiated by Alex Guenther in 1990s
!   Coded in FORTRAN as
!   MEGAN: Jack Chen 11/04
!   MEGANv2.04: Tan 11/21/06
!   MEGANv2.1: X. Wang 11/04/2007
!       Modified by Julia Lee-Taylor 03/18/2008
!       Modified by Xuemei Wang 09/30/2008
!       Modified by Tan 07/28/2011
!   MEGAN3.0:
!   Alex Guenther and Ling Huang Feb 2017

      IMPLICIT NONE
      
      CONTAINS
      
      SUBROUTINE RUN_MEGVEA(SDATE, STIME, MXREC, NCOLS, NROWS, TSTEP,
     &                      NEMIS, Layers, N_MaxT, N_MinT, N_MaxWS, 
     &                      GAMBD_YN, GAMAQ_YN, GAMCO2_YN, GAMHW_YN, 
     &                      GAMHT_YN, GAMLT_YN, GAMSM_YN, GAMSM, AQI, 
     &                      LDFMAP, LAIp, LAIc, SunT, ShaT, 
     &                      SunP, ShaP, SunF, Max_temp, Max_wind, 
     &                      Min_temp, D_TEMP, D_PPFD, NON_DIMGARMA)
     
     
     
     

! INCLUDE FILES
      !INCLUDE 'PARMS3.EXT'   ! I/O API parameters
      !INCLUDE 'IODECL3.EXT'  ! I/O API function declarations
      !INCLUDE 'FDESC3.EXT'   ! I/O API file desc. data structures
      INCLUDE 'MEGVEA.EXT'    ! coefficients

!...  EXTERNAL FUNCTIONS and their descriptions:
      INTEGER, EXTERNAL       ::   ENVINT
      INTEGER, EXTERNAL       ::   ENVYN
      LOGICAL      DSCGRID
      EXTERNAL     DSCGRID

!...  Program I/O files: From run script
! Program name
!      CHARACTER*16  :: PROGNAME = 'MEGVEA'
! Netcdf file
!      CHARACTER*16  :: LAIS46   = 'LAIS46'     ! LAI file logical name
!      CHARACTER*16  :: AQFILE   = 'AQFILE'     ! Air quality index file
!      CHARACTER*16  :: SMFILE   = 'SMFILE'     ! Soil moisture acitvity file
!      CHARACTER*16  :: CANMET   = 'CANMET'     ! Canopy meteorology file
!      CHARACTER*16  :: DailyMET = 'DailyMET'   ! Daily meteorology file
!      CHARACTER*16  :: LDFILE   = 'LDFILE'     ! Light dependent fraction file
                                               ! for certain MT/SQT

! output file
!      CHARACTER*16  :: MGNERS = 'MGNERS'       ! Emission activity 

!...  Parameters for file units
!      INTEGER  LOGDEV                          ! Logfile unit number

!...  External parameters
! From run script
      INTEGER       SDATE          ! Start date YYYYDDD
      INTEGER       STIME          ! Start time HHMMSS
      !INTEGER       RLENG          ! Run length HHMMSS
      INTEGER       Layers         ! Vertical layers
      INTEGER       N_MaxT         ! number of past days for maximum temperature
      INTEGER       N_MinT         ! number of past days for minimum temperature
      INTEGER       N_MaxWS        ! number of past days for maximum wind speed

      LOGICAL       GAMBD_YN       ! flag for GAMBD
      LOGICAL       GAMAQ_YN       ! flag for GAMAQ
      LOGICAL       GAMCO2_YN      ! flag for GAMCO2
      LOGICAL       GAMHW_YN       ! flag for GAMHW
      LOGICAL       GAMHT_YN       ! flag for GAMHT
      LOGICAL       GAMLT_YN       ! flag for GAMLT
      LOGICAL       GAMSM_YN       ! flag for GAMSM

! I/O API file parameters
      !INTEGER :: JDATE        ! Date YYYYDDD from inpname
      !INTEGER :: JTIME        ! Time HHMMSS from inpname
      INTEGER :: NCOLS        ! Number of columns
      INTEGER :: NROWS        ! Number of rows
      !INTEGER :: NLAYS        ! Number of vertical layers
      INTEGER :: MXREC        ! Total number of timesteps
      INTEGER :: TSTEP        ! Time step

!...  Internal parameters
! Internal parameters (status and buffer)
      INTEGER       IOS                    ! i/o status
!      CHARACTER*256 MESG                   ! message buffer

! Parameter for output species
      INTEGER :: NEMIS ! number of emission classes
      !INTEGER, PARAMETER :: NEMIS = NCLASS
                          ! number of emission classes

! Local variables and their descriptions:
!      CHARACTER*16  :: GDNAM
!      CHARACTER*16  :: CNAME        ! Coord name
      
      REAL :: NON_DIMGARMA ( MXREC, NEMIS, NCOLS, NROWS ) ! output
      REAL :: GAMSM( MXREC, NCOLS, NROWS )      ! emission activity response to soil moisture (input)
      REAL :: AQI( NCOLS, NROWS )        ! air quality index (i.e.W126) (input)
      REAL :: LDFMAP( NEMIS, NCOLS, NROWS )     ! light depenedent fraction map (input)
      REAL :: LAIp( MXREC, NCOLS, NROWS )    ! Previous time step LAI (input)
      REAL :: LAIc( MXREC, NCOLS, NROWS )    ! Current time step LAI (input)

!     variable from CANMET
      REAL :: SunT ( NCOLS, NROWS, Layers, MXREC )   ! Sun leaf temperature (K) (input)
      REAL :: ShaT ( NCOLS, NROWS, Layers, MXREC )   ! Shade leaf temperature (K) (input)
      REAL :: SunP ( NCOLS, NROWS, Layers, MXREC )   ! Sun leaf PPFD (umol/m2.s) (input)
      REAL :: ShaP ( NCOLS, NROWS, Layers, MXREC )   ! Shade leaf PPFD (umol/m2.s) (input)
      REAL :: SunF ( NCOLS, NROWS, Layers, MXREC )   ! Sun leaf fraction (input)

!     variable from DailyMET
      REAL :: Max_temp( N_MaxT, NCOLS, NROWS ) ! input
      REAL :: Max_wind( N_MaxWS, NCOLS, NROWS ) ! input
      REAL :: Min_temp( N_MinT, NCOLS, NROWS ) ! input
      REAL :: D_TEMP( NCOLS, NROWS )    ! Daily averagye temperature (K) (input)
      REAL :: D_PPFD( NCOLS, NROWS )    ! Daily averagye PPFD (umol/m2.s) (input)

!     temporary arrays
      REAL, ALLOCATABLE :: ER( :,: )      ! Output emission buffer (temporary)
      REAL, ALLOCATABLE :: GAMLA( :,: )      ! EA leaf age response (temporary)
      REAL, ALLOCATABLE :: GAMAQ( :,: )      ! EA response to air pollution (temporary)
      REAL, ALLOCATABLE :: GAMBD( :,: )      ! EA bidirectional exchange LAI response (temporary)
      REAL, ALLOCATABLE :: GAMHT( :,: )      ! EA response to high temperature (temporary)
      REAL, ALLOCATABLE :: GAMLT( :,: )      ! EA response to low temperature (temporary)
      REAL, ALLOCATABLE :: GAMHW( :,: )      ! EA response to high wind speed (temporary)
      REAL, ALLOCATABLE :: GAMCO2( :,: )     ! EA response to CO2 (temporary)
      REAL, ALLOCATABLE :: GAMTP( :,: )      ! combines GAMLD, GAMLI, GAMP to get canopy average (temporary)
      REAL, ALLOCATABLE :: CDEA (:,:,:)   ! Emission response to canopy depth (temporary)
      REAL, ALLOCATABLE :: VPGWT(:), Ea1L(:), Ea2L(:) ! temporary
      REAL, ALLOCATABLE :: MaxT( :, : )    ! Maximum temperature of previous n days (K) (input)
      REAL, ALLOCATABLE :: MinT( :, : )    ! Minimum temperature of previous n days (K) (input)
      REAL, ALLOCATABLE :: MaxWS( :, : )   ! Maximum wind speed of previous n days (m/s) (input)
      REAL :: GAMP, GAMTLD, GAMTLI

! loop indices
      INTEGER :: IDATE, ITIME
      INTEGER :: S, T, I, J, K

!***********************************************************************

!--=====================================================================
!...  Begin program
!--=====================================================================
     
      !PRINT*,'SDATE=',SDATE
      !PRINT*,'STIME=',STIME
      !PRINT*,'MXREC=',MXREC
      !PRINT*,'NCOLS=',NCOLS
      !PRINT*,'NROWS=',NROWS
      !PRINT*,'TSTEP=',TSTEP
      !PRINT*,'NEMIS=',NEMIS
      !PRINT*,'Layers=',Layers
      !PRINT*,'N_MaxT=',N_MaxT
      !PRINT*,'N_MinT=',N_MinT
      !PRINT*,'N_MaxWS=',N_MaxWS
      !PRINT*,'GAMBD_YN=',GAMBD_YN
      !PRINT*,'GAMAQ_YN=',GAMAQ_YN
      !PRINT*,'GAMCO2_YN=',GAMCO2_YN
      !PRINT*,'GAMHW_YN=',GAMHW_YN
      !PRINT*,'GAMHT_YN=',GAMHT_YN
      !PRINT*,'GAMLT_YN=',GAMLT_YN
      !PRINT*,'GAMSM_YN=',GAMSM_YN
      !PRINT*,'GAMSM=',GAMSM
      !PRINT*,'AQI=',AQI
      !PRINT*,'LDFMAP=',LDFMAP
      !PRINT*,'LAIp=',LAIp
      !PRINT*,'LAIc=',LAIc
      !PRINT*,'SunT=',SunT
      !PRINT*,'ShaT=',ShaT
      !PRINT*,'SunP=',SunP
      !PRINT*,'ShaP=',ShaP
      !PRINT*,'SunF=',SunF
      !PRINT*,'Max_temp=',Max_temp
      !PRINT*,'Max_wind=',Max_wind
      !PRINT*,'Min_temp=',Min_temp
      !PRINT*,'D_TEMP=',D_TEMP
      !PRINT*,'D_PPFD=',D_PPFD
      !PRINT*,'MaxT=',MaxT
      !PRINT*,'MinT=',MinT
      !PRINT*,'MaxWS=',MaxWS
      
      MaxT = MAXVAL(Max_temp,1)
      MinT = MINVAL(Min_temp,1)
      MaxWS = MAXVAL(Max_wind,1)

!----------------------------------------------------------------
!.....2) Calculate emission activity 
!----------------------------------------------------------------
!...  Allocate memory
      ALLOCATE ( ER ( NCOLS, NROWS ), STAT = IOS )
      ALLOCATE ( GAMLA   ( NCOLS, NROWS ), STAT = IOS )
      ALLOCATE ( GAMAQ   ( NCOLS, NROWS ), STAT = IOS )
      ALLOCATE ( GAMBD   ( NCOLS, NROWS ), STAT = IOS )
      ALLOCATE ( GAMHT   ( NCOLS, NROWS ), STAT = IOS )
      ALLOCATE ( GAMLT   ( NCOLS, NROWS ), STAT = IOS )
      ALLOCATE ( GAMHW   ( NCOLS, NROWS ), STAT = IOS )
      ALLOCATE ( GAMCO2  ( NCOLS, NROWS ), STAT = IOS )
      ALLOCATE ( GAMTP   ( NCOLS, NROWS ), STAT = IOS )
      ALLOCATE ( CDEA  ( NCOLS, NROWS, Layers ), STAT = IOS )
      ALLOCATE ( VPGWT  ( Layers ), STAT = IOS )
      ALLOCATE ( Ea1L  ( Layers ), STAT = IOS )
      ALLOCATE ( Ea2L  ( Layers ), STAT = IOS )
      ALLOCATE ( MaxT  (  NCOLS, NROWS ), STAT = IOS )
      ALLOCATE ( MinT  (  NCOLS, NROWS ), STAT = IOS )
      ALLOCATE ( MaxWS (  NCOLS, NROWS ), STAT = IOS )

      !...  Start the loop over the time period
      IDATE = SDATE
      ITIME = STIME
      DO T = 1, MXREC
        ! Go over all the emission classes
        DO S = 1, NEMIS

! leaf age activity factor
          CALL GAMMA_A( NCOLS, NROWS, S,
     &                    LAIp(T,:,:), LAIc(T,:,:), D_TEMP, GAMLA )

! Emission response to canopy depth
          CALL GAMMA_CD( NCOLS, NROWS, Layers, LAIc(T,:,:), CDEA )

! EA bidirectional exchange LAI response
          IF ( GAMBD_YN ) THEN
             CALL GAMMA_LAIbidir(NCOLS, NROWS, LAIc(T,:,:), GAMBD)
          ELSE
             GAMBD = 1.0
          ENDIF

! emission activity response to air quality
          IF ( GAMAQ_YN ) THEN
             CALL GAMMA_AQ(NCOLS, NROWS, S, AQI, GAMAQ)
          ELSE
             GAMAQ = 1.0
          ENDIF

! EA response to high temperature
          IF ( GAMHT_YN ) THEN
             CALL GAMMA_HT(NCOLS, NROWS, S, MaxT, GAMHT)
          ELSE
             GAMHT = 1.0
          ENDIF

! EA response to low temperature
          IF ( GAMLT_YN ) THEN
             CALL GAMMA_LT(NCOLS, NROWS, S, MinT, GAMLT)
          ELSE
             GAMLT = 1.0
          ENDIF

! EA response to high wind speed
          IF ( GAMHW_YN ) THEN 
             CALL GAMMA_HW(NCOLS, NROWS, S, MaxWS, GAMHW)
          ELSE
             GAMHW = 1.0
          ENDIF

! EA response to CO2 only for isoprene
          IF ( S .EQ. 1 ) THEN
            IF ( GAMCO2_YN ) THEN
              CALL GAMMA_CO2(NCOLS, NROWS, GAMCO2)
            ELSE
                 GAMCO2 = 1.0
            ENDIF
          ENDIF

! EA response to canopy temperature/light

        IF ( Layers .EQ. 5 ) THEN
          VPGWT(1) = 0.1184635
          VPGWT(2) = 0.2393144
          VPGWT(3) = 0.284444444
          VPGWT(4) = 0.2393144
          VPGWT(5) = 0.1184635
        ELSE
          DO K = 1,Layers
          VPGWT(K) =1/Layers
          ENDDO
        ENDIF
        
        !PRINT*,'SunT(T)=',SunT(:,:,:,T)
        !PRINT*,'ShaT(T)=',ShaT
        !PRINT*,'SunP(T)=',SunP
        !PRINT*,'ShaP(T)=',ShaP
        !PRINT*,'SunF(T)=',SunF

          DO I = 1, NCOLS
            DO J = 1, NROWS

              DO K = 1, Layers
                Ea1L(K)  = CDEA(I,J,K) *
     &                GAMTLD(SunT(I,J,K,T),D_TEMP(I,J),S) *
     &                GAMP(SunP(I,J,K,T),D_PPFD(I,J)) *  SunF(I,J,K,T) +
     &                GAMTLD(ShaT(I,J,K,T),D_TEMP(I,J),S) * 
     &                GAMP(ShaP(I,J,K,T),D_PPFD(I,J))
     &                * (1-SunF(I,J,K,T))

                Ea2L(K) = GAMTLI(SunT(I,J,K,T),S)* SunF(I,J,K,T)+
     &              GAMTLI(ShaT(I,J,K,T),S)*(1-SunF(I,J,K,T))

              ENDDO   ! ENDDO canopy layers
            GAMTP(I,J)=SUM((Ea1L(:)*LDFMAP(S,I,J) + 
     &                      Ea2L(:)*(1-LDFMAP(S,I,J)))* VPGWT(:))
            ENDDO   ! NROWS
          ENDDO ! NCOLS

!          write(MESG,*) 'GAMSM(100,100)',GAMSM(100,100)
!          CALL M3MESG( MESG )
 
! ... Calculate emission activity factor
!       GAMCO2 only applied to isoprene
          IF ( S .EQ. 1 ) THEN
          ER(:,:) = LAIc(T,:,:) * GAMTP * GAMCO2 * GAMLA * 
     &         GAMHW * GAMAQ * GAMHT * GAMLT * GAMSM(T,:,:) * LDFMAP(S,:,:)
!       GAMBD only applied to ethanol and acetaldehyde
          ELSE IF ( S .EQ. 13 ) THEN
          ER(:,:) = LAIc(T,:,:) * GAMTP * GAMBD * GAMLA * 
     &         GAMHW * GAMAQ * GAMHT * GAMLT * GAMSM(T,:,:) * LDFMAP(S,:,:)
!       For CO (S=19), LDFMAP not applied
          ELSE IF ( S .EQ. 19 ) THEN
          ER(:,:) = LAIc(T,:,:) * GAMTP * GAMLA * 
     &         GAMHW * GAMAQ * GAMHT * GAMLT * GAMSM(T,:,:)
          ELSE
          ER(:,:) = LAIc(T,:,:) * GAMTP * GAMLA * 
     &         GAMHW * GAMAQ * GAMHT * GAMLT * GAMSM(T,:,:) * LDFMAP(S,:,:)
          ENDIF
          WHERE(ER(:,:).GT.0.0)
           NON_DIMGARMA (T,S,:,:) = ER(:,:)
          ELSEWHERE
           NON_DIMGARMA (T,S,:,:) = 0.0
          ENDWHERE

        ENDDO  ! End loop of species (S)

       CALL IncrementDateTime( IDATE, ITIME, TSTEP )
      ENDDO ! End loop for time step (T)

      DEALLOCATE ( ER )
      DEALLOCATE ( GAMLA )
      DEALLOCATE ( GAMAQ )
      DEALLOCATE ( GAMBD )
      DEALLOCATE ( GAMHT )
      DEALLOCATE ( GAMLT )
      DEALLOCATE ( GAMHW )
      DEALLOCATE ( GAMCO2 )
      DEALLOCATE ( GAMTP )
      DEALLOCATE ( CDEA )
      DEALLOCATE ( VPGWT )
      DEALLOCATE ( Ea1L )
      DEALLOCATE ( Ea2L )
      DEALLOCATE ( MaxT )
      DEALLOCATE ( MinT )
      DEALLOCATE ( MaxWS )
!--=====================================================================
!...  FORMAT
!--=====================================================================
1000  FORMAT( A )
1010  FORMAT( 43( A, :, I8, :, 1X ) )
1020  FORMAT (A40,I8,X,I8,X,I8)
1030  FORMAT (A20,I8,X,I8,X,I8)

!--=====================================================================
!...  End program
!--=====================================================================
      END SUBROUTINE RUN_MEGVEA
      
      END MODULE MEGVEA

!----------------------------------------------------------------
!
!   SUBROUTINE GAMMA_CD
!       Emission response to canopy depath
!----------------------------------------------------------------
      SUBROUTINE GAMMA_CD(NCOLS,NROWS,Layers,LAI,CDEA)
  
      IMPLICIT NONE
      INCLUDE 'MEGVEA.EXT'

      INTEGER,INTENT(IN) :: NCOLS,NROWS,Layers
      REAL,DIMENSION(NCOLS,NROWS),INTENT(IN) :: LAI
      REAL,DIMENSION(NCOLS,NROWS,Layers),INTENT(OUT) :: CDEA
      REAL,DIMENSION(Layers) :: Cdepth
      REAL :: LAIdepth
      INTEGER :: I,J,K

      IF ( Layers .EQ. 5 ) THEN
        Cdepth (1)   = 0.0469101
        Cdepth (2)   = 0.2307534
        Cdepth (3)   = 0.5
        Cdepth (4)   = 0.7692465
        Cdepth (5)   = 0.9530899
      ELSE
        DO K = 1,Layers
          Cdepth(K) =(K - 0.5) /Layers
        ENDDO
      ENDIF
 
      DO I = 1, NCOLS
        DO J = 1, NROWS
          DO K = 1, Layers
            LAIdepth = LAI(I,J) * Cdepth(K)
            IF ( LAIdepth .GT. 3 ) THEN
               LAIdepth = 3.0
            ENDIF
            CDEA(I,J,K) = CCD1 * LAIdepth + CCD2
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END SUBROUTINE GAMMA_CD
!----------------------------------------------------------------

!----------------------------------------------------------------
!
!   FUNCTION GAMTLD
!       EA Temperature response (light dependent emission)
!----------------------------------------------------------------
      FUNCTION GAMTLD(T1,T24,S)

      IMPLICIT NONE
      INCLUDE 'MEGVEA.EXT'
      REAL,PARAMETER :: Ct2 = 230
      INTEGER :: S
      REAL :: T1,T24,T240,Topt, X, Eopt, GAMTLD

      T240 = T24

      IF (T1 < 260) THEN
        GAMTLD = 0
      ELSE
      ! Temperature at which maximum emission occurs
        Topt = 312.5 + 0.6 * (T240 - 297)
        X    = ((1 / Topt) - (1 / T1)) / 0.00831
      ! Maximum emission (relative to emission at 30 C)
        Eopt = Cleo(S) * EXP(0.05 * (T24 - 297)) *
     &         Exp(0.05*(T240-297))
        GAMTLD= Eopt * Ct2 * Exp(Ct1(S) * X) /
     &       (Ct2 - Ct1(S) * (1 - EXP(Ct2 * X)))
      ENDIF

      END FUNCTION GAMTLD
!----------------------------------------------------------------

!----------------------------------------------------------------
!
!   FUNCTION GAMTLI
!       EA Temperature response (light independent emission)
!----------------------------------------------------------------


      FUNCTION GAMTLI(temp,S)

      IMPLICIT NONE
      INCLUDE 'MEGVEA.EXT'

      REAL :: temp, GAMTLI
      REAL,PARAMETER :: Ts = 303.15
      INTEGER :: S

      GAMTLI= exp( beta(S)*(temp-Ts) )

      END FUNCTION GAMTLI
!----------------------------------------------------------------


!----------------------------------------------------------------
!
!   FUNCTION GAMP
!       EA Light response
!----------------------------------------------------------------

      FUNCTION GAMP(PPFD1,PPFD24)

      IMPLICIT NONE
      INCLUDE 'MEGVEA.EXT'
      REAL :: PPFD1, PPFD24, Alpha, C1, GAMP

      IF (PPFD24 < 0.01) THEN
        GAMP= 0
      ELSE
        Alpha  = 0.004
!        C1     = 0.0468 * EXP(0.0005 * (PPFD24 - PSTD))
!     &          * (PPFD24 ** 0.6)
        C1 = 1.03
        GAMP= (Alpha * C1 * PPFD1) /
     &          ((1 + Alpha**2. * PPFD1**2.)**0.5)
      ENDIF

      END FUNCTION GAMP

!----------------------------------------------------------------
!
!   SUBROUTINE GAMMA_HT
!   EA response to high temperature
!
!----------------------------------------------------------------

      SUBROUTINE GAMMA_HT(NCOLS, NROWS, S, MaxT, GAMHT)

      IMPLICIT NONE
      INCLUDE 'MEGVEA.EXT'

      INTEGER,INTENT(IN) :: NCOLS, NROWS, S
      REAL,DIMENSION(NCOLS,NROWS),INTENT(IN) :: MaxT
      REAL,DIMENSION(NCOLS,NROWS),INTENT(OUT) :: GAMHT
      INTEGER :: I,J
      REAL :: THTK, t1

      DO I = 1,NCOLS
        DO J = 1,NROWS
         THTK = 273.15 + THT(S)
         t1 = THTK + DTHT(S)
          IF (MaxT(I,J) <= THTK) THEN
            GAMHT(I,J) = 1.0
          ELSE IF ( MaxT(I,J) > THTK .AND. MaxT(I,J) < t1) THEN
                 GAMHT(I,J) = 1.0 + (CHT(S) - 1.0)* (MaxT(I,J) - 
     &                                             THTK)/DTHT(S)
          ELSE
                 GAMHT(I,J) = CHT(S)
          ENDIF
        END DO
      END DO

      RETURN
      END SUBROUTINE GAMMA_HT
!----------------------------------------------------------------

!----------------------------------------------------------------
!
!   SUBROUTINE GAMMA_LT
!   EA response to low temperature
!
!----------------------------------------------------------------

      SUBROUTINE GAMMA_LT(NCOLS, NROWS, S, MinT, GAMLT)

      IMPLICIT NONE
      INCLUDE 'MEGVEA.EXT'

      INTEGER,INTENT(IN) :: NCOLS, NROWS, S
      REAL,DIMENSION(NCOLS,NROWS),INTENT(IN) :: MinT
      REAL,DIMENSION(NCOLS,NROWS),INTENT(OUT) :: GAMLT
      INTEGER :: I,J
      REAL :: TLTK, t1

      DO I = 1,NCOLS
        DO J = 1,NROWS
          TLTK = 273.15 + TLT(S)
          t1 = TLTK - DTLT(S)
          IF (MinT(I,J) >= TLTK) THEN
            GAMLT(I,J) = 1.0
          ELSE IF ( MinT(I,J) < TLTK .AND. MinT(I,J) > t1) THEN
                 GAMLT(I,J) = 1.0 + (CLT(S) - 1.0)* (TLTK - 
     &                                            MinT(I,J))/DTLT(S)
          ELSE
                 GAMLT(I,J) = CLT(S)
          ENDIF
        END DO
      END DO

      RETURN
      END SUBROUTINE GAMMA_LT
!----------------------------------------------------------------


!----------------------------------------------------------------
!
!   SUBROUTINE GAMMA_HW
!   EA response to high wind speed
!
!----------------------------------------------------------------

      SUBROUTINE GAMMA_HW(NCOLS, NROWS, S, MaxWS, GAMHW)

      IMPLICIT NONE
      INCLUDE 'MEGVEA.EXT'

      INTEGER,INTENT(IN) :: NCOLS, NROWS, S
      REAL,DIMENSION(NCOLS,NROWS),INTENT(IN) :: MaxWS
      REAL,DIMENSION(NCOLS,NROWS),INTENT(OUT) :: GAMHW
      INTEGER :: I,J
      REAL :: t1

      DO I = 1,NCOLS
        DO J = 1,NROWS
          t1 = THW(S) + DTHW(S)
          IF (MaxWS(I,J) <= THW(S)) THEN
            GAMHW(I,J) = 1.0
          ELSE IF ( MaxWS(I,J) > THW(S) .AND. MaxWS(I,J) < t1) THEN
                 GAMHW(I,J) = 1.0 + (CHW(S) - 1.0)* (MaxWs(I,J) - 
     &                                                 THW(S))/ DTHW(S)
          ELSE
                 GAMHW(I,J) = CHW(S)
          ENDIF
        END DO
      END DO

      RETURN
      END SUBROUTINE GAMMA_HW
!----------------------------------------------------------------



!----------------------------------------------------------------
!
!   SUBROUTINE GAMMA_AQ
!   EA response to air quality
!
!----------------------------------------------------------------

      SUBROUTINE GAMMA_AQ(NCOLS, NROWS, S, AQI, GAMAQ)

      IMPLICIT NONE
      INCLUDE 'MEGVEA.EXT'

      INTEGER, INTENT(IN) :: NCOLS, NROWS, S
      REAL, DIMENSION(NCOLS,NROWS),INTENT(IN) :: AQI
      REAL, DIMENSION(NCOLS,NROWS),INTENT(OUT) :: GAMAQ
      INTEGER :: I,J
      REAL :: t1

      DO I = 1, NCOLS
        DO J = 1, NROWS
          t1 = TAQ(S) + DTAQ(S)
          IF (AQI(I,J) <= TAQ(S)) THEN
            GAMAQ(I,J) = 1.0
          ELSE IF ( AQI(I,J) > TAQ(S) .AND. AQI(I,J) < t1) THEN
                 GAMAQ(I,J) = 1.0 + (CAQ(S) - 1.0)* (AQI(I,J) - 
     &                                             TAQ(S))/DTAQ(S)
          ELSE
                 GAMAQ(I,J) = CAQ(S)
          ENDIF
        ENDDO
      ENDDO

      RETURN
      END SUBROUTINE GAMMA_AQ
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
! Subroutine GAMMA_CO2
!-----------------------------------------------------------------------
!From Alex Guenther 2017-03-11
      SUBROUTINE GAMMA_CO2( NCOLS, NROWS, GAMCO2 )

      IMPLICIT NONE
      INCLUDE 'MEGVEA.EXT'

      INTEGER :: NCOLS, NROWS
      REAL,DIMENSION(NCOLS,NROWS) :: Ci,CO2temp
      REAL,DIMENSION(NCOLS,NROWS),INTENT(OUT) :: GAMCO2
    
      CO2temp = CO2
      Ci = 0.7* CO2

      WHERE (CO2temp.eq.400.0)
      GAMCO2 = 1.0
      ELSEWHERE
      GAMCO2 = ISmax- ((ISmax*Ci**CO2h) /(Cstar**CO2h+Ci**CO2h))
      ENDWHERE

      RETURN
      END SUBROUTINE GAMMA_CO2

!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!
! Subroutine GAMMA_LAIbidir(gam_LAIbidir,LAI)
!-----------------------------------------------------------------------
!From Alex Guenther 2010-01-26
!If lai < 2 Then
!gammaLAIbidir= 0.5 * lai
!ElseIf lai <= 6 Then
!gammaLAIbidir= 1 - 0.0625 * (lai - 2)
!Else
!gammaLAIbidir= 0.75
!End If
!
!     SUBROUTINE GAMMA_LAIbidir returns the gam_LAIbidir values
!    Xuemei Wang-2010-01-28
!
!-----------------------------------------------------------------------
      SUBROUTINE GAMMA_LAIbidir(NCOLS, NROWS,LAI,GAMBD)

       IMPLICIT NONE
       INTEGER,INTENT(IN) :: NCOLS, NROWS
       INTEGER :: I,J
       REAL,DIMENSION(NCOLS, NROWS),INTENT(IN) ::  LAI
       REAL,DIMENSION(NCOLS, NROWS),INTENT(OUT) :: GAMBD

        DO I = 1,NCOLS
        DO J = 1, NROWS
         IF(LAI(I,J) < 2) THEN
        GAMBD(I,J) =  0.5 * LAI(I,J)
        ELSEIF (LAI(I,J) .LE. 6 .AND. LAI(I,J) .GE. 2) THEN
        GAMBD(I,J) = 1 - 0.0625 * (LAI(I,J) - 2)
        ELSE
        GAMBD(I,J) = 0.75
        ENDIF

        ENDDO
        ENDDO

        RETURN
      END SUBROUTINE GAMMA_LAIbidir
!----------------------------------------------------------------


!----------------------------------------------------------------
!
!   SUBROUTINE GAMLA
!
!     EA leaf age response
!----------------------------------------------------------------
!
!       GAMLA = Fnew*Anew + Fgro*Agro + Fmat*Amat + Fold*Aold
!       where Fnew = new foliage fraction
!             Fgro = growing foliage fraction
!             Fmat = mature foliage fraction
!             Fold = old foliage fraction
!             Anew = emission activity for new foliage
!             Agro = emission activity for growing foliage
!             Amat = emission activity for mature foliage
!             Aold = emission activity for old foliage
!           Age class fractions are determined from LAI changes
!             LAIc = current LAI
!             LAIp = past LAI
!             t  = length of the time step (days)
!             ti = days between budbreak and emission induction
!             tm = days between budbreak and peak emission
!             Tt = average above canopy temperature (K)
!
!----------------------------------------------------------------

      SUBROUTINE GAMMA_A( NCOLS, NROWS, S,
     &                    LAIp, LAIc, D_TEMP, GAMLA )

      IMPLICIT NONE
      INCLUDE 'MEGVEA.EXT'
! input
      INTEGER,INTENT(IN) :: NCOLS,NROWS, S
      REAL,DIMENSION(NCOLS,NROWS),INTENT(IN) :: D_TEMP, LAIp, LAIc
! output
      REAL,DIMENSION(NCOLS,NROWS),INTENT(OUT) :: GAMLA

! Local parameters
!      CHARACTER*16  :: FUNCNAME = 'GAMLA'

      INTEGER :: t                                ! time step
!      CHARACTER*256 :: MESG                       ! message buffer

      REAL,DIMENSION(ncols,nrows) :: Fnew, Fgro, Fmat, Fold
      REAL,DIMENSION(ncols,nrows) :: ti,tm
      REAL,DIMENSION(ncols,nrows) :: Tt

!---------------------------------------------------
! local parameter arrays
      t = TSTLEN
      Tt   = D_TEMP

!... Calculate foliage fraction

      WHERE (LAIp .LT. LAIc)

!        Calculate ti and tm
        WHERE (Tt .LE. 303.0)
          ti = 5.0 + 0.7*(300-Tt)
        ELSEWHERE (Tt .GT. 303.0)
          ti = 2.9
        ENDWHERE
        tm = 2.3*ti

!       Calculate Fnew and Fmat, then Fgro and Fold
!       Fnew
        WHERE (ti .GE. t)
          Fnew = 1.0 - (LAIp/LAIc)
        ELSEWHERE (ti .LT. t)
          Fnew = (ti/t) * ( 1-(LAIp/LAIc) )
        ENDWHERE

!       Fmat
        WHERE (tm .GE. t)
          Fmat = LAIp/LAIc
        ELSEWHERE (tm .LT. t)
          Fmat = (LAIp/LAIc) + ( (t-tm)/t ) * ( 1-(LAIp/LAIc) )
        ENDWHERE

        Fgro = 1.0 - Fnew - Fmat
        Fold = 0.0

      ELSEWHERE (LAIp .EQ. LAIc)

        Fnew = 0.0
        Fgro = 0.1
        Fmat = 0.8
        Fold = 0.1

      ELSEWHERE (LAIp .GT. LAIc)

        Fnew = 0.0
        Fgro = 0.0
        Fold = ( LAIp-LAIc ) / LAIp
        Fmat = 1-Fold

      ENDWHERE

!...  Calculate GAMLA
      GAMLA = Fnew*Anew(S) + Fgro*Agro(S) +
     &         Fmat*Amat(S) + Fold*Aold(S)

      RETURN
      END SUBROUTINE GAMMA_A

      
      SUBROUTINE RUN_MEGVEA_C(SDATE, STIME, MXREC, NCOLS, NROWS, TSTEP,
     &                NEMIS, Layers, N_MaxT, N_MinT, N_MaxWS, 
     &                GAMBD_YN, GAMAQ_YN, GAMCO2_YN, GAMHW_YN, 
     &                GAMHT_YN, GAMLT_YN, GAMSM_YN, GAMSM, AQI, 
     &                LDFMAP, LAIp, LAIc, SunT, ShaT, 
     &                SunP, ShaP, SunF, Max_temp, Max_wind, 
     &                Min_temp, D_TEMP, D_PPFD, 
     &                ISOP, MBO, MT_PINE, MT_ACYC, MT_CAMP, MT_SABI, 
     &                MT_AROM, MT_OXY, SQT_HR, SQT_LR, MEOH, ACTO, 
     &                ETOH, ACID, LVOC, OXPROD, STRESS, OTHER, CO, NO) 
     &                BIND(C,name='run_megvea_c')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_FLOAT, C_BOOL
      USE MEGVEA
      
      ! input parameters
      integer(c_int), intent(in)  :: SDATE, STIME, MXREC, NCOLS, NROWS, TSTEP,
     &                               NEMIS, Layers, N_MaxT, N_MinT, N_MaxWS
      logical(c_bool), intent(in)  :: GAMBD_YN, GAMAQ_YN, GAMCO2_YN, GAMHW_YN, 
     &                                GAMHT_YN, GAMLT_YN, GAMSM_YN
      
      ! input arrays
      real(c_float), intent(in)  :: GAMSM( MXREC, NCOLS, NROWS )      ! emission activity response to soil moisture (input)
      real(c_float), intent(in)  :: AQI( NCOLS, NROWS )        ! air quality index (i.e.W126) (input)
      real(c_float), intent(in)  :: LDFMAP( NEMIS, NCOLS, NROWS )     ! light depenedent fraction map (input)
      real(c_float), intent(in)  :: LAIp( MXREC, NCOLS, NROWS )    ! Previous time step LAI (input)
      real(c_float), intent(in)  :: LAIc( MXREC, NCOLS, NROWS )    ! Current time step LAI (input)
      real(c_float), intent(in)  :: SunT ( NCOLS, NROWS, Layers, MXREC )   ! Sun leaf temperature (K) (input)
      real(c_float), intent(in)  :: ShaT ( NCOLS, NROWS, Layers, MXREC )   ! Shade leaf temperature (K) (input)
      real(c_float), intent(in)  :: SunP ( NCOLS, NROWS, Layers, MXREC )   ! Sun leaf PPFD (umol/m2.s) (input)
      real(c_float), intent(in)  :: ShaP ( NCOLS, NROWS, Layers, MXREC )   ! Shade leaf PPFD (umol/m2.s) (input)
      real(c_float), intent(in)  :: SunF ( NCOLS, NROWS, Layers, MXREC )   ! Sun leaf fraction (input)
      real(c_float), intent(in)  :: Max_temp( N_MaxT, NCOLS, NROWS ) ! input
      real(c_float), intent(in)  :: Max_wind( N_MaxWS, NCOLS, NROWS ) ! input
      real(c_float), intent(in)  :: Min_temp( N_MinT, NCOLS, NROWS ) ! input
      real(c_float), intent(in)  :: D_TEMP( NCOLS, NROWS )    ! Daily averagye temperature (K) (input)
      real(c_float), intent(in)  :: D_PPFD( NCOLS, NROWS )    ! Daily averagye PPFD (umol/m2.s) (input)
      !real(c_float), intent(in)  :: MaxT( NCOLS, NROWS )    ! Maximum temperature of previous n days (K) (input)
      !real(c_float), intent(in)  :: MinT( NCOLS, NROWS )    ! Minimum temperature of previous n days (K) (input)
      !real(c_float), intent(in)  :: MaxWS( NCOLS, NROWS )   ! Maximum wind speed of previous n days (m/s) (input)

      ! output arrays
      real(c_float), intent(out)  :: ISOP(MXREC, NCOLS, NROWS)   
      real(c_float), intent(out)  :: MBO(MXREC, NCOLS, NROWS)     
      real(c_float), intent(out)  :: MT_PINE(MXREC, NCOLS, NROWS) 
      real(c_float), intent(out)  :: MT_ACYC(MXREC, NCOLS, NROWS) 
      real(c_float), intent(out)  :: MT_CAMP(MXREC, NCOLS, NROWS) 
      real(c_float), intent(out)  :: MT_SABI(MXREC, NCOLS, NROWS) 
      real(c_float), intent(out)  :: MT_AROM(MXREC, NCOLS, NROWS) 
      real(c_float), intent(out)  :: MT_OXY(MXREC, NCOLS, NROWS)  
      real(c_float), intent(out)  :: SQT_HR(MXREC, NCOLS, NROWS)  
      real(c_float), intent(out)  :: SQT_LR(MXREC, NCOLS, NROWS)  
      real(c_float), intent(out)  :: MEOH(MXREC, NCOLS, NROWS)    
      real(c_float), intent(out)  :: ACTO(MXREC, NCOLS, NROWS)    
      real(c_float), intent(out)  :: ETOH(MXREC, NCOLS, NROWS)    
      real(c_float), intent(out)  :: ACID(MXREC, NCOLS, NROWS)    
      real(c_float), intent(out)  :: LVOC(MXREC, NCOLS, NROWS)    
      real(c_float), intent(out)  :: OXPROD(MXREC, NCOLS, NROWS)  
      real(c_float), intent(out)  :: STRESS(MXREC, NCOLS, NROWS)  
      real(c_float), intent(out)  :: OTHER(MXREC, NCOLS, NROWS)   
      real(c_float), intent(out)  :: CO(MXREC, NCOLS, NROWS)      
      real(c_float), intent(out)  :: NO(MXREC, NCOLS, NROWS) 

      ! temporary arrays
      real(c_float), allocatable  :: NON_DIMGARMA(:, :, :, :)
      
      ALLOCATE(NON_DIMGARMA(MXREC, NEMIS, NCOLS, NROWS))

      !print*,'SDATE=',SDATE
      
      CALL RUN_MEGVEA(SDATE, STIME, MXREC, NCOLS, NROWS, TSTEP,
     &                NEMIS, Layers, N_MaxT, N_MinT, N_MaxWS, 
     &                logical(GAMBD_YN, 4), logical(GAMAQ_YN, 4), 
     &                logical(GAMCO2_YN, 4), logical(GAMHW_YN, 4), 
     &                logical(GAMHT_YN, 4), logical(GAMLT_YN, 4), 
     &                logical(GAMSM_YN, 4), GAMSM, AQI, 
     &                LDFMAP, LAIp, LAIc, SunT, ShaT, 
     &                SunP, ShaP, SunF, Max_temp, Max_wind, 
     &                Min_temp, D_TEMP, D_PPFD, NON_DIMGARMA)
     
      ISOP = NON_DIMGARMA(:, 1, :, :)   
      MBO = NON_DIMGARMA(:, 2, :, :)       
      MT_PINE = NON_DIMGARMA(:, 3, :, :)   
      MT_ACYC = NON_DIMGARMA(:, 4, :, :)   
      MT_CAMP = NON_DIMGARMA(:, 5, :, :)   
      MT_SABI = NON_DIMGARMA(:, 6, :, :)   
      MT_AROM = NON_DIMGARMA(:, 7, :, :)   
      MT_OXY = NON_DIMGARMA(:, 8, :, :)   
      SQT_HR = NON_DIMGARMA(:, 9, :, :)    
      SQT_LR = NON_DIMGARMA(:, 10, :, :)    
      MEOH = NON_DIMGARMA(:, 11, :, :)      
      ACTO = NON_DIMGARMA(:, 12, :, :)      
      ETOH = NON_DIMGARMA(:, 13, :, :)      
      ACID = NON_DIMGARMA(:, 14, :, :)      
      LVOC = NON_DIMGARMA(:, 15, :, :)      
      OXPROD = NON_DIMGARMA(:, 16, :, :)    
      STRESS = NON_DIMGARMA(:, 17, :, :)    
      OTHER = NON_DIMGARMA(:, 18, :, :)     
      CO = NON_DIMGARMA(:, 19, :, :)        
      NO = NON_DIMGARMA(:, 20, :, :)    
      
      DEALLOCATE(NON_DIMGARMA)
      
      END SUBROUTINE RUN_MEGVEA_C
      
   