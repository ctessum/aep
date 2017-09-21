      MODULE MGN2MECH

!***********************************************************************
!   This program does chemical speciation and MECHANISM conversion.
!   The output from megan.f is converted from 20 to 201 species which
!   are then lumped according to the MECHANISM assigned in the run script.  
!   The program loops through all timesteps of the input file.
!
!   Procedure
!   1) File set up and assign I/O parameters
!   2) Conversion from MGN 20 to speciated 201
!   3) Conversion from speciated species to MECHANISM species
!   4) Convert to tonne/hour if needed
!
!   The input file gives variables in units of g-species/sec.
!   All outputs are in mole/sec or tonne/hr depending on assignment.
!
!
!   INPUT:
!           1) MEGVEA output (netCDF-ioapi)
!           2) MEGSEA output (netCDF-ioapi)
!
!   OUTPUT:
!           1) MEGAN speciation or MECHANISM species (netCDF-ioapi)
!
!   Requirement:
!      Requires libnetcdf.a and libioapi.a to compile
!
!      setenv MGERFILE    <DEFANGED_input MEGAN output for emission activity factors>  
!      setenv OUTPFILE    <output speciated emission>
!
!   CALLS:  CHECKMEM
!
!   Originally created by Jack Chen 11/04 for MEGAN v.0
!   For MEGAN v2.0 created by Tan 12/01/06
!   For MEGAN v2.1 created by Xuemei Wang 11/04/07
!   For MEGAN v2.1 to use 150 species created by Xuemei Wang 09/30/09
!   For MEGAN v3 to use 201 species created by Alex Guenther 03/19/17
!
!  History:
!  08/14/07 Tan    - Move to MEGANv2.02 with no update
!  08/29/07 modified by A. Guenther to correct error in assigning   
!           emission factor. This version is called MEGANv2.03
!  10/29/07 modified by A. Guenther to correct omission of diurnal variation    
!           factor. This version is called MEGANv2.04
!  11/04/07 modified by Xuemei Wang to give two options for MAP or lookup table for
!           the emission factors. Also gives options for different chemical MECHANISMs
!           in the code: user modifies the external script to assign MECHANISM.
!           This version is called MEGANv2.1.0
!  06/04/08 modified by J. Lee-Taylor to accept vegetation-dependent speciation factors
!           in table format (RESHAPE tables) rather than from DATA statements.
!  09/30/08  modified by Xuemei Wang to give options for input file and test different mechanisms
!  09/27/11  Tan&Xuemei MEGANv2.10 includes soil NOx adjustment and a lot of updates
!  03/19/17  Ling Huang MEGANv3 (a) updates vegetation emission factors and
!            add additional compounds; (b) emission factor unit from
!            ug/m2/hr to nanomoles/m2/s; (c) fix NO emission bug
!  06/08/17  Ling Huang updated/added new mechanism for MEGAN3
!***********************************************************************

      IMPLICIT NONE
      
      CONTAINS
      
      SUBROUTINE RUN_MGN2MECH(SDATE, STIME, MXREC, NCOLS, NROWS, TSTEP,
     &                      NVAR, CONVERSION, TONPHR, MECHANISM, garea,
     &                      inper, EF, GAMNO, outer) ! N_MGN_SPC, 

!...  INCLUDES:
      !INCLUDE 'PARMS3.EXT'        !  I/O API parameters
      !INCLUDE 'IODECL3.EXT'       !  I/O API function declarations
      !INCLUDE 'FDESC3.EXT'        !  I/O API file description data structures.

      INCLUDE 'SPC_NOCONVER.EXT'
      INCLUDE 'SPC_CB05.EXT'
      INCLUDE 'SPC_CB6.EXT'
      INCLUDE 'SPC_CBMZ.EXT'
      INCLUDE 'SPC_SAPRC99.EXT'
      INCLUDE 'SPC_SAPRC99Q.EXT'
      INCLUDE 'SPC_SAPRC99X.EXT'
      INCLUDE 'SPC_SAPRCII.EXT'
      INCLUDE 'SPC_SOAX.EXT'
      INCLUDE 'SPC_RADM2.EXT'
      INCLUDE 'SPC_RACM.EXT'
      INCLUDE 'SPC_CB6X.EXT'         ! new in MEGAN3
      INCLUDE 'SPC_CRIv2.EXT'        ! new in MEGAN3
      INCLUDE 'SPC_RACM2.EXT'        ! new in MEGAN3
      INCLUDE 'SPC_SAPRC07.EXT'      ! new in MEGAN3
      INCLUDE 'MAP_CV2CB05.EXT'
      INCLUDE 'MAP_CV2CB6.EXT'      
      INCLUDE 'MAP_CV2CBMZ.EXT'
      INCLUDE 'MAP_CV2SAPRC99.EXT'
      INCLUDE 'MAP_CV2SAPRC99Q.EXT'
      INCLUDE 'MAP_CV2SAPRC99X.EXT'
      INCLUDE 'MAP_CV2SAPRCII.EXT'
      INCLUDE 'MAP_CV2SOAX.EXT'
      INCLUDE 'MAP_CV2RADM2.EXT'
      INCLUDE 'MAP_CV2RACM.EXT'
      INCLUDE 'MAP_CV2CB6X.EXT'
      INCLUDE 'MAP_CV2CRIv2.EXT'
      INCLUDE 'MAP_CV2RACM2.EXT'
      INCLUDE 'MAP_CV2SAPRC07.EXT'

C...........   EXTERNAL FUNCTIONS and their descriptions:
      INTEGER      PROMPTFFILE, STR2INT
      REAL         STR2REAL
      EXTERNAL     PROMPTFFILE,STR2INT
      EXTERNAL     STR2REAL
      LOGICAL      ENVYN
      EXTERNAL     ENVYN
      LOGICAL      DSCGRID
      EXTERNAL     DSCGRID


!...  Program I/O files
! Program name
      CHARACTER*16 :: PROGNAME = 'MGN2MECH'
! Input MEGAN emission activity factor files
      CHARACTER*16 :: MGNERS   = 'MGNERS'    ! Input MEGAN emission activity factor 
      CHARACTER*16 :: MGNSEA   = 'MGNSEA'    ! Input MEGAN soil NO emission activity factor
! Netcdf file
      CHARACTER*16 :: EFMAPS   = 'EFMAPS'    ! EFMAP input file  name
! Output file
      CHARACTER*16 :: MGNOUT   = 'MGNOUT'    ! Output file logical name
! Parameters for file units
      INTEGER :: logdev                      ! Logfile unit number

!...  Program I/O parameters
!...  External parameters

! from run script
      LOGICAL :: TONPHR      ! Output in tons/hr flag
      LOGICAL :: CONVERSION   ! Mechanism conversion flag

      CHARACTER*16 :: MECHANISM              ! Mechanism name

! I/O API file parameters
      INTEGER       jdate          ! Looping date YYYYDDD
      INTEGER       jtime          ! Looping time HHMMSS
      INTEGER       ncols          ! Number of columns
      INTEGER       nrows          ! Number of rows
      INTEGER       mxrec          ! Total number of time steps
      INTEGER       sdate          ! Start date YYYYDDD
      INTEGER       stime          ! Start time HHMMSS
      INTEGER       tstep          ! Time step


!...  Internal parameters
! internal paramters (status and buffer)
      INTEGER   ios                          ! i/o status
      CHARACTER*256 :: MESG                  ! Message buffer

! local variables and their descriptions:
      CHARACTER*16  :: GDNAM
      CHARACTER*16  :: CNAME        ! Coord name

      INTEGER :: t, s, I, N                   ! Counters
      INTEGER :: nmpmg, nmpsp, nmpmc          ! Counters
      REAL :: inper(MXREC, N_MGN_SPC, ncols, nrows)       ! Input emission buffer (input)
      REAL, ALLOCATABLE :: tmper(:,:,:)       ! Temp emission buffer (temporary)
      REAL :: outer(MXREC, NVAR, ncols, nrows)       ! Output emission buffer (output)
      REAL :: EF(N_MGN_SPC, ncols, nrows)          ! Emission factor (input)
      REAL :: GAMNO(MXREC, ncols, nrows)         ! NO emission factor (input)
      INTEGER :: n_scon_spc 
      INTEGER :: NVAR,INO
      REAL :: garea 

      INTEGER, ALLOCATABLE ::  spmh_map(:),mech_map(:)   ! speciated species name (temporary)
      CHARACTER*16, ALLOCATABLE :: mech_spc(:) ! temporary
      REAL, ALLOCATABLE :: conv_fac(:) ! temporary        
      REAL,ALLOCATABLE :: mech_mwt(:) ! temporary
      
      REAL, PARAMETER :: ug2g = 1E-6          ! convert microgram to metric gram
      REAL, PARAMETER :: g2tonne = 1E-6       ! convert microgram to metric ton
      REAL, PARAMETER :: hr2sec = 3600        ! convert hr to second
      REAL, PARAMETER :: n2no   = 2.142857    
      REAL, PARAMETER :: nmol2mol   = 1E-9    ! convert nanomoles to moles
      
      
      ! ADDED
      INTEGER :: NVARS3D
      

C***********************************************************************
      
      !print*,'SDATE=',SDATE
      !print*,'STIME=',STIME
      !print*,'MXREC=',MXREC
      !print*,'NCOLS=',NCOLS
      !print*,'NROWS=',NROWS
      !print*,'TSTEP=',TSTEP
      !print*,'NVAR=',NVAR
      !print*,'CONVERSION=',CONVERSION 
      !print*,'MECHANISM=',MECHANISM 
      !print*,'TONPHR=',TONPHR
      !print*,'garea=',garea       
      !print*,'inper=',inper
      !print*,'EF=',EF
      !print*,'GAMNO=',GAMNO

!=======================================================================
!...  Begin program
!=======================================================================
  
! Set attribute and variables for output
      IF ( CONVERSION ) THEN
        !MESG = 'Mechanism Name'
        !CALL ENVSTR( 'MECHANISM', MESG, 'SAPRCII', MECHANISM, ios )

        SELECT CASE ( TRIM(MECHANISM) )
          CASE ('SAPRCII')
            n_scon_spc = n_saprcii
            NVARS3D = n_saprcii_spc
          CASE ('SAPRC99')
            n_scon_spc = n_saprc99
            NVARS3D = n_saprc99_spc
          CASE ('RADM2')
            n_scon_spc = n_radm2
            NVARS3D = n_radm2_spc
          CASE ('RACM')
            n_scon_spc = n_racm
            NVARS3D = n_racm_spc
          CASE ('CBMZ')
            n_scon_spc = n_cbmz
            NVARS3D = n_cbmz_spc
          CASE ('SAPRC99X')
            n_scon_spc = n_saprc99_x
            NVARS3D = n_saprc99_x_spc
          CASE ('SAPRC99Q')
            n_scon_spc = n_saprc99_q
            NVARS3D = n_saprc99_q_spc
          CASE ('CB05')
            n_scon_spc = n_cb05
            NVARS3D = n_cb05_spc
          CASE ('CB6')
            n_scon_spc = n_cb6
            NVARS3D = n_cb6_spc
          CASE ('SOAX')
            n_scon_spc = n_soax
            NVARS3D = n_soax_spc
          CASE ('CB6X')
            n_scon_spc = n_cb6x
            NVARS3D = n_cb6x_spc
          CASE ('RACM2')
            n_scon_spc = n_racm2
            NVARS3D = n_racm2_spc
          CASE ('CRIv2')
            n_scon_spc = n_criv2
            NVARS3D = n_criv2_spc
          CASE ('SAPRC07')
            n_scon_spc = n_saprc07
            NVARS3D = n_saprc07_spc
          CASE DEFAULT
            MESG = 'Error: Mechanism conversion, invalid MECHANISM: '
     &            //TRIM(MECHANISM)
!            CALL M3EXIT(PROGNAME, 0, 0,MESG,2)
        ENDSELECT
 
        ALLOCATE ( spmh_map(n_scon_spc), STAT = ios )
!          CALL CHECKMEM ( ios, 'spmh_map', PROGNAME )
        ALLOCATE ( mech_map(n_scon_spc), STAT = ios )
!          CALL CHECKMEM ( ios, 'mech_map', PROGNAME )
        ALLOCATE ( conv_fac(n_scon_spc), STAT = ios )
!          CALL CHECKMEM ( ios, 'conv_fac', PROGNAME )
        ALLOCATE ( mech_spc(NVARS3D ), STAT = ios )
!          CALL CHECKMEM ( ios, 'mech_spc', PROGNAME )
        ALLOCATE ( mech_mwt(NVARS3D ), STAT = ios )
!          CALL CHECKMEM ( ios, 'mech_mwt', PROGNAME )

        SELECT CASE ( TRIM(MECHANISM) )

          CASE ('SAPRCII')
            spmh_map(1:n_scon_spc) = spmh_map_saprcii(1:n_scon_spc)
            mech_map(1:n_scon_spc) = mech_map_saprcii(1:n_scon_spc)
            conv_fac(1:n_scon_spc) = conv_fac_saprcii(1:n_scon_spc)
            mech_spc(1:NVARS3D)    = mech_spc_saprcii(1:NVARS3D)
            mech_mwt(1:NVARS3D)    = mech_mwt_saprcii(1:NVARS3D)
            !VNAME3D(1:NVARS3D)     = mech_spc(1:NVARS3D)
          CASE ('SAPRC99')
            spmh_map(1:n_scon_spc) = spmh_map_saprc99(1:n_scon_spc)
            mech_map(1:n_scon_spc) = mech_map_saprc99(1:n_scon_spc)
            conv_fac(1:n_scon_spc) = conv_fac_saprc99(1:n_scon_spc)
            mech_spc(1:NVARS3D)    = mech_spc_saprc99(1:NVARS3D)
            mech_mwt(1:NVARS3D)    = mech_mwt_saprc99(1:NVARS3D)
            !VNAME3D(1:NVARS3D)     = mech_spc(1:NVARS3D)
          CASE ('RADM2')
            spmh_map(1:n_scon_spc) = spmh_map_radm2(1:n_scon_spc)
            mech_map(1:n_scon_spc) = mech_map_radm2(1:n_scon_spc)
            conv_fac(1:n_scon_spc) = conv_fac_radm2(1:n_scon_spc)
            mech_spc(1:NVARS3D)    = mech_spc_radm2(1:NVARS3D)
            mech_mwt(1:NVARS3D)    = mech_mwt_radm2(1:NVARS3D)
            !VNAME3D(1:NVARS3D)     = mech_spc(1:NVARS3D)
          CASE ('RACM')
            spmh_map(1:n_scon_spc) = spmh_map_racm(1:n_scon_spc)
            mech_map(1:n_scon_spc) = mech_map_racm(1:n_scon_spc)
            conv_fac(1:n_scon_spc) = conv_fac_racm(1:n_scon_spc)
            mech_spc(1:NVARS3D)    = mech_spc_racm(1:NVARS3D)
            mech_mwt(1:NVARS3D)    = mech_mwt_racm(1:NVARS3D)
            !VNAME3D(1:NVARS3D)     = mech_spc(1:NVARS3D)
          CASE ('CBMZ')
            spmh_map(1:n_scon_spc) = spmh_map_cbmz(1:n_scon_spc)
            mech_map(1:n_scon_spc) = mech_map_cbmz(1:n_scon_spc)
            conv_fac(1:n_scon_spc) = conv_fac_cbmz(1:n_scon_spc)
            mech_spc(1:NVARS3D)    = mech_spc_cbmz(1:NVARS3D)
            mech_mwt(1:NVARS3D)    = mech_mwt_cbmz(1:NVARS3D)
            !VNAME3D(1:NVARS3D)     = mech_spc(1:NVARS3D)
          CASE ('SAPRC99X')
            spmh_map(1:n_scon_spc) = spmh_map_saprc99_X(1:n_scon_spc)
            mech_map(1:n_scon_spc) = mech_map_saprc99_X(1:n_scon_spc)
            conv_fac(1:n_scon_spc) = conv_fac_saprc99_X(1:n_scon_spc)
            mech_spc(1:NVARS3D)    = mech_spc_saprc99_X(1:NVARS3D)
            mech_mwt(1:NVARS3D)    = mech_mwt_saprc99_X(1:NVARS3D)
            !VNAME3D(1:NVARS3D)     = mech_spc(1:NVARS3D)
          CASE ('SAPRC99Q')
            spmh_map(1:n_scon_spc) = spmh_map_saprc99_Q(1:n_scon_spc)
            mech_map(1:n_scon_spc) = mech_map_saprc99_Q(1:n_scon_spc)
            conv_fac(1:n_scon_spc) = conv_fac_saprc99_Q(1:n_scon_spc)
            mech_spc(1:NVARS3D)    = mech_spc_saprc99_Q(1:NVARS3D)
            mech_mwt(1:NVARS3D)    = mech_mwt_saprc99_Q(1:NVARS3D)
            !VNAME3D(1:NVARS3D)     = mech_spc(1:NVARS3D)
          CASE ('CB05')
            spmh_map(1:n_scon_spc) = spmh_map_cb05(1:n_scon_spc)
            mech_map(1:n_scon_spc) = mech_map_cb05(1:n_scon_spc)
            conv_fac(1:n_scon_spc) = conv_fac_cb05(1:n_scon_spc)
            mech_spc(1:NVARS3D)    = mech_spc_cb05(1:NVARS3D)
            mech_mwt(1:NVARS3D)    = mech_mwt_cb05(1:NVARS3D)
            !VNAME3D(1:NVARS3D)     = mech_spc(1:NVARS3D)
          CASE ('CB6')
            spmh_map(1:n_scon_spc) = spmh_map_cb6(1:n_scon_spc)
            mech_map(1:n_scon_spc) = mech_map_cb6(1:n_scon_spc)
            conv_fac(1:n_scon_spc) = conv_fac_cb6(1:n_scon_spc)
            mech_spc(1:NVARS3D)    = mech_spc_cb6(1:NVARS3D)
            mech_mwt(1:NVARS3D)    = mech_mwt_cb6(1:NVARS3D)
            !VNAME3D(1:NVARS3D)     = mech_spc(1:NVARS3D)
          CASE ('SOAX')
            spmh_map(1:n_scon_spc) = spmh_map_soax(1:n_scon_spc)
            mech_map(1:n_scon_spc) = mech_map_soax(1:n_scon_spc)
            conv_fac(1:n_scon_spc) = conv_fac_soax(1:n_scon_spc)
            mech_spc(1:NVARS3D)    = mech_spc_soax(1:NVARS3D)
            mech_mwt(1:NVARS3D)    = mech_mwt_soax(1:NVARS3D)
            !VNAME3D(1:NVARS3D)     = mech_spc(1:NVARS3D)
          CASE ('CB6X')
            spmh_map(1:n_scon_spc) = spmh_map_cb6x(1:n_scon_spc)
            mech_map(1:n_scon_spc) = mech_map_cb6x(1:n_scon_spc)
            conv_fac(1:n_scon_spc) = conv_fac_cb6x(1:n_scon_spc)
            mech_spc(1:NVARS3D)    = mech_spc_cb6x(1:NVARS3D)
            mech_mwt(1:NVARS3D)    = mech_mwt_cb6x(1:NVARS3D)
            !VNAME3D(1:NVARS3D)     = mech_spc(1:NVARS3D)
          CASE ('RACM2')
            spmh_map(1:n_scon_spc) = spmh_map_racm2(1:n_scon_spc)
            mech_map(1:n_scon_spc) = mech_map_racm2(1:n_scon_spc)
            conv_fac(1:n_scon_spc) = conv_fac_racm2(1:n_scon_spc)
            mech_spc(1:NVARS3D)    = mech_spc_racm2(1:NVARS3D)
            mech_mwt(1:NVARS3D)    = mech_mwt_racm2(1:NVARS3D)
            !VNAME3D(1:NVARS3D)     = mech_spc(1:NVARS3D)
          CASE ('CRIv2')
            spmh_map(1:n_scon_spc) = spmh_map_criv2(1:n_scon_spc)
            mech_map(1:n_scon_spc) = mech_map_criv2(1:n_scon_spc)
            conv_fac(1:n_scon_spc) = conv_fac_criv2(1:n_scon_spc)
            mech_spc(1:NVARS3D)    = mech_spc_criv2(1:NVARS3D)
            mech_mwt(1:NVARS3D)    = mech_mwt_criv2(1:NVARS3D)
            !VNAME3D(1:NVARS3D)     = mech_spc(1:NVARS3D)
          CASE ('SAPRC07')
            spmh_map(1:n_scon_spc) = spmh_map_saprc07(1:n_scon_spc)
            mech_map(1:n_scon_spc) = mech_map_saprc07(1:n_scon_spc)
            conv_fac(1:n_scon_spc) = conv_fac_saprc07(1:n_scon_spc)
            mech_spc(1:NVARS3D)    = mech_spc_saprc07(1:NVARS3D)
            mech_mwt(1:NVARS3D)    = mech_mwt_saprc07(1:NVARS3D)
            !VNAME3D(1:NVARS3D)     = mech_spc(1:NVARS3D)
        ENDSELECT
      ELSEIF ( .NOT. CONVERSION ) THEN
        print*,'No conversion'
        NVARS3D = n_spca_spc
        !VNAME3D(1:NVARS3D) = spca_spc(1:NVARS3D)
      ELSE
         MESG = 'Error: Conversion flag is not assigned.'
!         CALL M3EXIT(PROGNAME, 0, 0,MESG,2)
      ENDIF
      
      NVAR = NVARS3D

      ALLOCATE ( tmper( n_spca_spc, ncols, nrows ), STAT = ios ) 
      INO = FindInList('NO',N_MGN_SPC,MGN_SPC)      
      
!...  Loop through time
      jdate = sdate
      jtime = stime
      DO t = 1, mxrec
        tmper = 0.
        DO s = 1, N_SMAP_SPC
            
          nmpmg = mg20_map(s)
          nmpsp = spca_map(s)
          IF (t .EQ. 1) THEN
            MESG='Convert '//MGN_SPC(NMPMG)//' to '//SPCA_SPC(NMPSP)
!            CALL M3MESG( MESG )
          ENDIF

         IF ( nmpmg .NE. INO ) then
           !...  Not NO
           MESG = 'Use EFMAPS for '//MGN_SPC(NMPMG)
!           CALL M3MESG( MESG )
           tmper(nmpsp,:,:) = inper(t,nmpmg,:,:) * EF(nmpmg,:,:)
     &                          * effs_all(nmpsp)

         ELSEIF ( nmpmg .EQ. INO ) then
!!-----------------NO Stuff-----------------------
!     GAMNO is emission activity factor
           tmper(nmpsp,:,:) = GAMNO(t,:,:) * EF(INO,:,:)
     &                           * effs_all(nmpsp)

!-----------------end of NO----------------------
         ENDIF     !IF ( nmpmg .NE. INO ) then

        ENDDO ! End species loop

!-----------------------------------------------------------------------
!.....3) Conversion from speciated species to MECHANISM species
!-----------------------------------------------------------------------
        ! convert from nanomol/m^2/s to mol/m^2/hr
        DO s = 1, n_spca_spc
           tmper(s,:,:) = tmper(s,:,:) * nmol2mol * hr2sec
        ENDDO

        !if (t .eq. 1) then
        !print*,'tmper3=',tmper
        !print*,'CONVERSION=',CONVERSION
        !print*,'n_scon_spc=',n_scon_spc
        !print*,'conv_fac=',conv_fac
        !print*,'garea=',garea
        !print*,'hr2sec=',hr2sec
        !print*,'TONPHR=',TONPHR
        !print*,'NVAR=',NVAR
        !print*,'mech_mwt=',mech_mwt
        !print*,'g2tonne=',g2tonne
        !print*,'spca_mwt=',spca_mwt
        !endif

        IF ( CONVERSION ) THEN
          ! lumping to MECHANISM species

          DO s = 1, n_scon_spc
            nmpsp = spmh_map(s)         ! Mapping value for SPCA
            nmpmc = mech_map(s)         ! Mapping value for MECHANISM
           IF ( nmpmc .NE. 999 ) THEN
            IF (t .EQ. 1) THEN
              MESG='Convert '//SPCA_SPC(NMPSP)//' to '//MECH_SPC(NMPMC)
!              CALL M3MESG( MESG )
            ENDIF
            outer(t,nmpmc,:,:) = outer(t,nmpmc,:,:) +
     &                        (tmper(nmpsp,:,:) * conv_fac(s)) *
     &                         garea/hr2sec
            ! units of these species are in mole/s
           ENDIF
          ENDDO ! End species loop
          !WRITE(MESG,*) 'conv_fac,outer',conv_fac(1),outer(1,101,101)
          !CALL M3MESG(MESG)

        ELSE
          ! get all 201 species into the output array
          outer(t,:,:,:) = tmper(:,:,:)*garea/hr2sec
          ! units of these species are in mole/s/grid

        ENDIF

!-----------------------------------------------------------------------
!.....4) Convert to tonne/hour if needed
!-----------------------------------------------------------------------
        IF ( TONPHR ) THEN
          IF ( CONVERSION ) THEN
            ! convert from mol/s to ton/hr
            DO s = 1, NVAR
              outer(t,s,:,:) = outer(t,s,:,:)*mech_mwt(s)*
     &                       g2tonne*hr2sec
            ENDDO
          ELSEIF ( .NOT. CONVERSION ) THEN
            ! convert from mol/s to ton/hr
            DO s = 1, NVAR
              outer(t,s,:,:) = outer(t,s,:,:)*spca_mwt(s)*
     &                       g2tonne*hr2sec
            ENDDO
          ENDIF
        ENDIF
        CALL IncrementDateTime(jdate,jtime,tstep)
      ENDDO ! End time loop 

!... Exit and close file
!      CALL M3EXIT(PROGNAME,0,0,' ',0)

      DEALLOCATE ( spmh_map )
      DEALLOCATE ( mech_map )
      DEALLOCATE ( conv_fac )
      DEALLOCATE ( mech_spc )
      DEALLOCATE ( mech_mwt )
!      DEALLOCATE (EF)
!      DEALLOCATE (GAMNO)
!      DEALLOCATE (outer)
!      DEALLOCATE (inper)
      DEALLOCATE (tmper)

!=======================================================================
!...  FORMAT
!=======================================================================        

      !print*,'outer3=',outer
!=======================================================================
!...  End program
!=======================================================================
      END SUBROUTINE RUN_MGN2MECH
      
      INTEGER FUNCTION FindInList(x, length, list)
      
         IMPLICIT NONE

        CHARACTER*(*), INTENT(IN) :: x        ! value to search in list
        INTEGER, INTENT(IN) :: length         ! length of list
        CHARACTER*(*), INTENT(IN) :: list(*)  !  list to search

        INTEGER       I   !  loop counter
        FindInList = 0
        DO I = 1, length
            IF ( x .EQ. list( I ) ) THEN
                FindInList = I
                RETURN
            ENDIF
        END DO
        
        RETURN

        END FUNCTION FindInList

      END MODULE MGN2MECH
      
      
      
      SUBROUTINE RUN_MGN2MECH_C(SDATE, STIME, MXREC, NCOLS, NROWS, TSTEP,
     &                  NVAR, CONVERSION, TONPHR, C_MECHANISM, MECHANISM_len,
     &                  garea, inper, EF, GAMNO, ISOP, TERP, PAR, XYL, OLE, 
     &                  NR, MEOH, CH4, NH3, NO, ALD2, ETOH, FORM, ALDX, TOL, 
     &                  IOLE, CO, ETHA, ETH, ETHY, PRPA, BENZ, ACET, KET, 
     &                  AACD, FACD, HCN, ISPD, N2O, SESQ, TRS, CH3BR, CH3CL, 
     &                  CH3I, ISP, TRP, XYLA, SQT, TOLA) 
     &                  BIND(C,name='run_mgn2mech_c')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_FLOAT, C_BOOL, C_CHAR
      USE MGN2MECH
           
      INTEGER,PARAMETER :: N_MGN_SPC  = 20 ! from 'SPC_NOCONVER.EXT'
      
      ! input parameters
      integer(c_int), intent(in)  :: SDATE, STIME, MXREC, NCOLS, NROWS, TSTEP, NVAR    
      real(c_float), intent(in)  :: garea
      logical(c_bool), intent(in)  :: CONVERSION, TONPHR
      integer(c_int), intent(in)  :: MECHANISM_len
      character(kind=C_CHAR), dimension(MECHANISM_len) , intent(in) :: C_MECHANISM
      CHARACTER*16 :: MECHANISM 

      ! input arrays
      real(c_float), intent(in)  :: inper(MXREC, N_MGN_SPC, ncols, nrows)
      real(c_float), intent(in)  :: EF(N_MGN_SPC, ncols, nrows)
      real(c_float), intent(in)  :: GAMNO(MXREC, ncols, nrows)

      ! output arrays 
      real(c_float), intent(out)  :: ISOP (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: TERP (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: PAR  (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: XYL  (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: OLE  (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: NR   (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: MEOH (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: CH4  (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: NH3  (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: NO   (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: ALD2 (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: ETOH (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: FORM (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: ALDX (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: TOL  (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: IOLE (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: CO   (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: ETHA (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: ETH  (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: ETHY (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: PRPA (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: BENZ (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: ACET (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: KET  (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: AACD (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: FACD (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: HCN  (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: ISPD (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: N2O  (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: SESQ (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: TRS  (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: CH3BR(MXREC, ncols, nrows)
      real(c_float), intent(out)  :: CH3CL(MXREC, ncols, nrows)
      real(c_float), intent(out)  :: CH3I (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: ISP  (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: TRP  (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: XYLA (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: SQT  (MXREC, ncols, nrows)
      real(c_float), intent(out)  :: TOLA (MXREC, ncols, nrows)

      ! temporary arrays
      real(c_float), allocatable  :: outer(:, :, :, :)
      
      ALLOCATE(outer(MXREC, NVAR, ncols, nrows))
      
      ! copy C string into Fortran string
      DO i = 1,SIZE(C_MECHANISM)
         MECHANISM(i:i) = C_MECHANISM(i)
      END DO
      MECHANISM(SIZE(C_MECHANISM)+1:) = CHAR(32) ! pad string with spaces     
      
      !print*,'SDATE=',SDATE
      !print*,'C_MECHANISM=',C_MECHANISM
      !print*,'MECHANISM=',MECHANISM
      
      CALL RUN_MGN2MECH(SDATE, STIME, MXREC, NCOLS, NROWS, TSTEP,
     &               NVAR, logical(CONVERSION, 4), logical(TONPHR, 4), 
     &               MECHANISM, garea, inper, EF, GAMNO, outer)
     
      ISOP = outer(:, 1, :, :) 
      TERP = outer(:, 2, :, :) 
      PAR  = outer(:, 3, :, :) 
      XYL  = outer(:, 4, :, :) 
      OLE  = outer(:, 5, :, :) 
      NR   = outer(:, 6, :, :) 
      MEOH = outer(:, 7, :, :) 
      CH4  = outer(:, 8, :, :) 
      NH3  = outer(:, 9, :, :) 
      NO   = outer(:, 10, :, :) 
      ALD2 = outer(:, 11, :, :) 
      ETOH = outer(:, 12, :, :) 
      FORM = outer(:, 13, :, :) 
      ALDX = outer(:, 14, :, :) 
      TOL  = outer(:, 15, :, :) 
      IOLE = outer(:, 16, :, :) 
      CO   = outer(:, 17, :, :) 
      ETHA = outer(:, 18, :, :) 
      ETH  = outer(:, 19, :, :) 
      ETHY = outer(:, 20, :, :) 
      PRPA = outer(:, 21, :, :) 
      BENZ = outer(:, 22, :, :) 
      ACET = outer(:, 23, :, :) 
      KET  = outer(:, 24, :, :) 
      AACD = outer(:, 25, :, :) 
      FACD = outer(:, 26, :, :) 
      HCN  = outer(:, 27, :, :) 
      ISPD = outer(:, 28, :, :) 
      N2O  = outer(:, 29, :, :) 
      SESQ = outer(:, 30, :, :) 
      TRS  = outer(:, 31, :, :) 
      CH3BR= outer(:, 32, :, :) 
      CH3CL= outer(:, 33, :, :) 
      CH3I = outer(:, 34, :, :) 
      ISP  = outer(:, 35, :, :) 
      TRP  = outer(:, 36, :, :) 
      XYLA = outer(:, 37, :, :) 
      SQT  = outer(:, 38, :, :) 
      TOLA = outer(:, 39, :, :) 
     
      DEALLOCATE(outer)
      
      END SUBROUTINE RUN_MGN2MECH_C
