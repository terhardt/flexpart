MODULE fpmetbinary_mod

  !*****************************************************************************
  !                                                                            *
  !     Contains data and routines for dumping and loading processed met       *
  !     fields.                                                                *
  !     Authors Don Morton (Don.Morton@borealscicomp.com)                      *
  !             Delia Arnold (deliona.arnold@gmail.com)                        *
  !                                                                            *
  !     07 Oct 2016                                                            *
  !                                                                            *
  !     Most of the data structures from com_mod.f90 that are dumped and       *
  !     loaded have a final dimension of size two, so that they may hold data  *
  !     from two met files.  When we dump the contents into a .fp file, we     *
  !     need to specify which of the two to dump.  Likewise, when we load      *
  !     from a .fp file, we need to specify which of the two possible indices  *
  !     to load into.                                                          *
  !                                                                            *
  !     Note that these routines need more robustness.  For example, what      *
  !     what happens if the filename can't be read or written.  Or, what       *
  !     happens if a read or write fails in any way.  Right now, it's crash    *
  !     city.                                                                  *
  !                                                                            *
  !     Recent enhancements (07 Oct 2016) DJM:                                 *
  !                                                                            *
  !     - file format changed so that compiled dimensions are output, and      *
  !       during input these same dimensions are compared with the dimensions  *
  !       compiled into the flexpart that is reading it.  A discrepancy        *
  !       causes abort, so that time isn't wasted reading an incompatible      *
  !       file.                                                                *
  !                                                                            *
  !     - file format changed so that first item is an 8-character string      *
  !       depicting the version of the preprocessed file format.               *
  !       An inconsistency between a detected and expected string results      *
  !       in program abort.                                                    *
  !                                                                            *
  !       *** IMPORTANT *** - when the format of the preprocessed output is    *
  !       modified in any way, be sure to change the version string below,     *
  !       PREPROC_FORMAT_VERSION_STR, so that attempts to read the output      *
  !       with a different format version will cause an abort.                 *
  !                                                                            *
  !*****************************************************************************

    USE com_mod
    USE conv_mod
    USE par_mod, ONLY : nxmax, nymax, nzmax, nuvzmax, nwzmax, numclass, maxspec, &
&                       maxnests, nxmaxn, nymaxn

    USE netcdf

    IMPLICIT NONE

    ! Users may want to change these IO Unit values if they conflict with other parts
    ! of code
    INTEGER, PARAMETER :: IOUNIT_DUMP = 33, IOUNIT_LOAD = 34, &
                          IOUNIT_TEXTOUT = 35

    ! When a change is made to the format of the preprocessed file, such that
    ! this routine will not be able to read a previous version, this version
    ! string should be modified
    CHARACTER(LEN=12), PARAMETER :: PREPROC_FORMAT_VERSION_STR = 'FP_p-9.3.1'//char(0)

    PRIVATE IOUNIT_DUMP, IOUNIT_LOAD, IOUNIT_TEXTOUT, fpio,    &
&           PREPROC_FORMAT_VERSION_STR


CONTAINS

  !*****************************************************************************
  !                                                                            *
  !    Subroutines fpmetbinary_dump() and fpmetbinary_load() provide the       *
  !    public interface to                                                     *
  !    this module functionality.  I created the PRIVATE fpio() because I      *
  !    wanted all interactions with variables to be in one place.  The read    *
  !    and write operations need to be done in exactly the same sequence, so   *
  !    I felt like keeping them in the same routine would at least allow for   *
  !    coders to more easily compare the two sequences than if they were       *
  !    separate.                                                               *
  !                                                                            *
  !    As mentioned above, the dumps and loads will, for most variables,       *
  !    need to refer to one of two index values for the last dimension of      *
  !    the array.                                                              *
  !                                                                            *
  !*****************************************************************************


    SUBROUTINE fpmetbinary_dump(filename, cm_index)
        CHARACTER(LEN=*), INTENT(IN) :: filename  ! Full path for file
        INTEGER, INTENT(IN) :: cm_index           ! Index of last dimension in
                                                  ! most com_mod variables.
                                                  ! Should be 1 or 2 

        INTEGER millisecs_start, millisecs_stop, count_rate, count_max

        INTEGER :: ncretval, ncid          ! NetCDF func return value, file id

        CALL SYSTEM_CLOCK(millisecs_start, count_rate, count_max)

        ! Create and open NC4 file for writing
        PRINT *, 'Opening NC4 file...'
        ncretval = nf90_create(filename // ".nc4", &
&                              OR(NF90_CLOBBER, NF90_HDF5), &
&                              ncid)

        OPEN(IOUNIT_DUMP, file=filename, action='WRITE', status='REPLACE', form="unformatted", access="stream")






        CALL fpio(IOUNIT_DUMP, ncid, 'DUMP', cm_index)
        CLOSE(IOUNIT_DUMP)

        PRINT *, 'Closing NC4 file...'
        ncretval = nf90_close(ncid)

        CALL SYSTEM_CLOCK(millisecs_stop, count_rate, count_max)

        !PRINT *, 'Dump walltime secs: ', (millisecs_stop-millisecs_start)/1000.0
    END SUBROUTINE fpmetbinary_dump

    SUBROUTINE fpmetbinary_load(filename, cm_index)
        CHARACTER(LEN=*), INTENT(IN) :: filename  ! Full path for file
        INTEGER, INTENT(IN) :: cm_index           ! Index of last dimension in
                                                  ! most com_mod variables.
                                                  ! Should be 1 or 2 

        INTEGER :: ncretval, ncid          ! NetCDF func return value, file id

        INTEGER millisecs_start, millisecs_stop, count_rate, count_max

        CALL SYSTEM_CLOCK(millisecs_start, count_rate, count_max)

        print *, "Opening nc file for reading"
        ncretval = nf90_open(filename // ".nc4", NF90_NOWRITE, ncid)



        OPEN(IOUNIT_LOAD, file=filename, action='READ', status='OLD', form="unformatted", access="stream")
        CALL fpio(IOUNIT_LOAD, ncid, 'LOAD', cm_index)
        CLOSE(IOUNIT_LOAD)
        CALL SYSTEM_CLOCK(millisecs_stop, count_rate, count_max)
        !PRINT *, 'Load walltime secs: ', (millisecs_stop-millisecs_start)/1000.0
    END SUBROUTINE fpmetbinary_load

    SUBROUTINE fpmetbinary_zero(cm_index)
        INTEGER, INTENT(IN) :: cm_index           ! Index of last dimension in
                                                  ! most com_mod variables.
                                                  ! Should be 1 or 2 


        ! Zeroes out, in local datastructures, the values dumped/loaded
        ! This was written primarily as a testing mechanism.
        ! DJM -- 17 February 2017 -- I don't think this routine has been used
        ! for anything in recent past.  Might want to consider 86'ing it.
        ! The lines here correspond to READ and WRITE in the dump/load routines

        ! Scalar values
        nx=0.0; ny=0.0; nxmin1=0.0; nymin1=0.0; nxfield=0.0
        nuvz=0.0; nwz=0.0; nz=0.0; nmixz=0.0; nlev_ec=0.0
        dx=0.0; dy=0.0; xlon0=0.0; ylat0=0.0; dxconst=0.0; dyconst=0.0

        ! Fixed fields, static in time
        oro=0.0; excessoro=0.0; lsm=0.0; xlanduse=0.0; height=0.0

        ! 3d fields
        uu(:,:,:,cm_index) = 0.0
        vv(:,:,:,cm_index) = 0.0
        uupol(:,:,:,cm_index) = 0.0
        vvpol(:,:,:,cm_index) = 0.0
        ww(:,:,:,cm_index) = 0.0
        tt(:,:,:,cm_index) = 0.0
        qv(:,:,:,cm_index) = 0.0
        pv(:,:,:,cm_index) = 0.0
        rho(:,:,:,cm_index) = 0.0
        drhodz(:,:,:,cm_index) = 0.0
        tth(:,:,:,cm_index) = 0.0
        qvh(:,:,:,cm_index) = 0.0
        pplev(:,:,:,cm_index) = 0.0
        clouds(:,:,:,cm_index) = 0.0
        cloudsh(:,:,cm_index) = 0.0

        ! 2d fields
        ps(:,:,:,cm_index) = 0.0
        sd(:,:,:,cm_index) = 0.0
        msl(:,:,:,cm_index) = 0.0
        tcc(:,:,:,cm_index) = 0.0
        u10(:,:,:,cm_index) = 0.0
        v10(:,:,:,cm_index) = 0.0
        tt2(:,:,:,cm_index) = 0.0
        td2(:,:,:,cm_index) = 0.0
        lsprec(:,:,:,cm_index) = 0.0
        convprec(:,:,:,cm_index) = 0.0
        sshf(:,:,:,cm_index) = 0.0
        ssr(:,:,:,cm_index) = 0.0
        surfstr(:,:,:,cm_index) = 0.0
        ustar(:,:,:,cm_index) = 0.0
        wstar(:,:,:,cm_index) = 0.0
        hmix(:,:,:,cm_index) = 0.0
        tropopause(:,:,:,cm_index) = 0.0
        oli(:,:,:,cm_index) = 0.0
        diffk(:,:,:,cm_index) = 0.0
        vdep(:,:,:,cm_index) = 0.0

        ! 1d fields
        z0(:) = 0.0
        akm(:) = 0.0
        bkm(:) = 0.0
        akz(:) = 0.0
        bkz(:) = 0.0
        aknew(:) = 0.0
        bknew(:) = 0.0

        ! Nested, scalar values (for each nest)
        nxn(:) = 0.0
        nyn(:) = 0.0
        dxn(:) = 0.0
        dyn(:) = 0.0
        xlon0n(:) = 0.0
        ylat0n(:) = 0.0

        ! Nested fields, static in time
        oron=0.0; excessoron=0.0; lsmn=0.0; xlandusen=0.0

        ! 3d nested fields
        uun(:,:,:,cm_index,:) = 0.0
        wwn(:,:,:,cm_index,:) = 0.0
        ttn(:,:,:,cm_index,:) = 0.0
        qvn(:,:,:,cm_index,:) = 0.0
        pvn(:,:,:,cm_index,:) = 0.0
        cloudsn(:,:,:,cm_index,:) = 0.0
        cloudsnh(:,:,cm_index,:) = 0.0
        rhon(:,:,:,cm_index,:) = 0.0
        drhodzn(:,:,:,cm_index,:) = 0.0
        tthn(:,:,:,cm_index,:) = 0.0
        qvhn(:,:,:,cm_index,:) = 0.0

        ! 2d nested fields
        psn(:,:,:,cm_index,:) = 0.0
        sdn(:,:,:,cm_index,:) = 0.0
        msln(:,:,:,cm_index,:) = 0.0
        tccn(:,:,:,cm_index,:) = 0.0
        u10n(:,:,:,cm_index,:) = 0.0
        v10n(:,:,:,cm_index,:) = 0.0
        tt2n(:,:,:,cm_index,:) = 0.0
        td2n(:,:,:,cm_index,:) = 0.0
        lsprecn(:,:,:,cm_index,:) = 0.0
        convprecn(:,:,:,cm_index,:) = 0.0
        sshfn(:,:,:,cm_index,:) = 0.0
        ssrn(:,:,:,cm_index,:) = 0.0
        surfstrn(:,:,:,cm_index,:) = 0.0
        ustarn(:,:,:,cm_index,:) = 0.0
        wstarn(:,:,:,cm_index,:) = 0.0
        hmixn(:,:,:,cm_index,:) = 0.0
        tropopausen(:,:,:,cm_index,:) = 0.0
        olin(:,:,:,cm_index,:) = 0.0
        diffkn(:,:,:,cm_index,:) = 0.0
        vdepn(:,:,:,cm_index,:) = 0.0

        ! Auxiliary variables for nests
        xresoln(:) = 0.0
        yresoln(:) = 0.0
        xln(:) = 0.0
        yln(:) = 0.0
        xrn(:) = 0.0
        yrn(:) = 0.0

        ! Variables for polar stereographic projection
        xglobal=.FALSE.; sglobal=.FALSE.; nglobal=.FALSE.
        switchnorthg=0.0; switchsouthg=0.0
        southpolemap(:) = 0.0
        northpolemap(:) = 0.0

        ! Variables declared in conv_mod (convection)
        pconv(:) = 0.0
        phconv(:) = 0.0
        dpr(:) = 0.0
        pconv_hpa(:) = 0.0
        phconv_hpa(:) = 0.0
        ft(:) = 0.0
        fq(:) = 0.0
        fmass(:,:) = 0.0
        sub(:) = 0.0
        fmassfrac(:,:) = 0.0
        cbaseflux(:,:) = 0.0
        cbasefluxn(:,:,:) = 0.0
        tconv(:) = 0.0
        qconv(:) = 0.0
        qsconv(:) = 0.0
        psconv=0.0; tt2conv=0.0; td2conv=0.0
        nconvlev=0.0; nconvtop=0.0

    END SUBROUTINE fpmetbinary_zero

    SUBROUTINE fpio(iounit, ncid, op, cm_index)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: ncid               ! NetCDF file id
        INTEGER, INTENT(IN) :: iounit
        CHARACTER(LEN=4), INTENT(IN) :: op        ! Operation - DUMP or LOAD
        INTEGER, INTENT(IN) :: cm_index           ! Index of last dimension in
                                                  ! most com_mod variables.
                                                  ! Should be 1 or 2 


        INTEGER :: ncret          ! Return value from NetCDF calls
        INTEGER :: ncvarid          ! NetCDF variable ID

        INTEGER :: nxmax_dimid, nymax_dimid, nzmax_dimid, nuvzmax_dimid, nwzmax_dimid, &
&                  maxspec_dimid, numclass_dimid, maxnests_dimid, nxmaxn_dimid, nymaxn_dimid, &
&                  zero_to_nzmax_dimid


        INTEGER, DIMENSION(1) :: dim1dids    ! Dimension IDs for 1D arrays
        INTEGER, DIMENSION(2) :: dim2dids    ! Dimension IDs for 2D arrays
        INTEGER, DIMENSION(3) :: dim3dids    ! Dimension IDs for 3D arrays
        INTEGER, DIMENSION(4) :: dim4dids    ! Dimension IDs for 4D arrays
        INTEGER, DIMENSION(5) :: dim5dids    ! Dimension IDs for 5D arrays




        ! These are used when loading in dimensions from NC file
        CHARACTER(LEN=NF90_MAX_NAME) :: nxmax_dimname, nymax_dimname, nzmax_dimname, &
&                                       nuvzmax_dimname, nwzmax_dimname,&
&                                       maxspec_dimname, numclass_dimname,&
&                                       maxnests_dimname, nxmaxn_dimname, nymaxn_dimname, &
&                                       zero_to_nzmax_dimname

        ! These are temporary variables, used in the LOAD option, for 
        ! comparing against the current values in FLEXPART of nxmax, nymax, ...
        INTEGER :: temp_nxmax, temp_nymax, temp_nzmax, &
&                  temp_nuvzmax, temp_nwzmax, &
&                  temp_maxspec, temp_numclass,&
&                  temp_maxnests, temp_nxmaxn, temp_nymaxn

        CHARACTER(LEN=12) :: temp_preproc_format_version_str

        CHARACTER(LEN=128) :: errmesg

        INTEGER, PARAMETER :: DEF_LEVEL = 3

        if (op == 'DUMP') THEN


            ! Write the preprocessing format version string
            WRITE (iounit) PREPROC_FORMAT_VERSION_STR

            ! Write the compiled max dimensions from par_mod - these are
            ! not meant to be reassigned during a LOAD, but used as "header"
            ! information to provide the structure of arrays
            WRITE (iounit) nxmax, nymax, nzmax, nuvzmax, nwzmax

            ncret = nf90_def_dim(ncid, 'nxmax', nxmax, nxmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'nymax', nymax, nymax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'nzmax', nzmax, nzmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'nuvzmax', nuvzmax, nuvzmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'nwzmax', nwzmax, nwzmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'maxspec', maxspec, maxspec_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'numclass', numclass, numclass_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'zero_to_nzmax', nzmax+1, zero_to_nzmax_dimid)
            call handle_nf90_err(ncret)


            ! Scalar values
            WRITE(iounit) nx, ny, nxmin1, nymin1, nxfield
            WRITE(iounit) nuvz, nwz, nz, nmixz, nlev_ec
            WRITE(iounit) dx, dy, xlon0, ylat0, dxconst, dyconst

            ncret = nf90_def_var(ncid, 'nx', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nx)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ny', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, ny)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nxmin1', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nxmin1)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nymin1', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nymin1)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nxfield', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nxfield)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nuvz', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nuvz)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nwz', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nwz)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nz', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nz)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nmixz', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nmixz)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nlev_ec', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nlev_ec)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'dx', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, dx)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'dy', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, dy)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'xlon0', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, xlon0)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ylat0', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, ylat0)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'dxconst', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, dxconst)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'dyconst', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, dyconst)
            call handle_nf90_err(ncret)



            ! Fixed fields, static in time
            WRITE(iounit) oro, excessoro, lsm, xlanduse, height

            dim2dids = (/nxmax_dimid, nymax_dimid/)

            ncret = nf90_def_var(ncid, 'oro', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                oro(0:nxmax-1, 0:nymax-1))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'excessoro', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                excessoro(0:nxmax-1, 0:nymax-1))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'lsm', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                lsm(0:nxmax-1, 0:nymax-1))
            call handle_nf90_err(ncret)

            dim3dids = (/nxmax_dimid, nymax_dimid, numclass_dimid/)
            ! numclass comes from par_mod - number of land use classes
            ncret = nf90_def_var(ncid, 'xlanduse', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                xlanduse(0:nxmax-1, 0:nymax-1, 1:numclass))
            call handle_nf90_err(ncret)

            dim1dids = (/nzmax_dimid/)
            ncret = nf90_def_var(ncid, 'height', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                height(1:nzmax))
            call handle_nf90_err(ncret)




            ! 3d fields
            WRITE(iounit) uu(:,:,:,cm_index)
            WRITE(iounit) vv(:,:,:,cm_index)
            WRITE(iounit) uupol(:,:,:,cm_index)
            WRITE(iounit) vvpol(:,:,:,cm_index)
            WRITE(iounit) ww(:,:,:,cm_index)
            WRITE(iounit) tt(:,:,:,cm_index)
            WRITE(iounit) qv(:,:,:,cm_index)
            WRITE(iounit) pv(:,:,:,cm_index)
            WRITE(iounit) rho(:,:,:,cm_index)
            WRITE(iounit) drhodz(:,:,:,cm_index)
            WRITE(iounit) tth(:,:,:,cm_index)
            WRITE(iounit) qvh(:,:,:,cm_index)
            WRITE(iounit) pplev(:,:,:,cm_index)
            WRITE(iounit) clouds(:,:,:,cm_index)
            WRITE(iounit) cloudsh(:,:,cm_index)

            dim3dids = (/nxmax_dimid, nymax_dimid, nzmax_dimid/)

            ncret = nf90_def_var(ncid, 'uu', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                uu(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'vv', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                vv(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'uupol', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                uupol(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'vvpol', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                vvpol(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ww', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                ww(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'tt', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tt(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'qv', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                qv(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'pv', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                pv(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'rho', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                rho(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'drhodz', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                drhodz(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'clouds', NF90_BYTE, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                clouds(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)



            ! Note the change in z dimension for the following
            dim3dids = (/nxmax_dimid, nymax_dimid, nuvzmax_dimid/)

            ncret = nf90_def_var(ncid, 'tth', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tth(0:nxmax-1, 0:nymax-1, 1:nuvzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'qvh', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                qvh(0:nxmax-1, 0:nymax-1, 1:nuvzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'pplev', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                pplev(0:nxmax-1, 0:nymax-1, 1:nuvzmax, cm_index))
            call handle_nf90_err(ncret)


            dim2dids = (/nxmax_dimid, nymax_dimid/)
            ncret = nf90_def_var(ncid, 'cloudsh', NF90_INT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                cloudsh(0:nxmax-1, 0:nymax-1, cm_index))
            call handle_nf90_err(ncret)



            PRINT *, 'SUM(tt(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index)): ', &
&                                        SUM(tt(0:nxmax-1,0:nymax-1,1:nzmax, cm_index))

            PRINT *, 'SUM(clouds(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index)): ', &
&                                        SUM(clouds(0:nxmax-1,0:nymax-1,1:nzmax, cm_index))



            ! 2d fields
            WRITE(iounit) ps(:,:,:,cm_index)
            WRITE(iounit) sd(:,:,:,cm_index)
            WRITE(iounit) msl(:,:,:,cm_index)
            WRITE(iounit) tcc(:,:,:,cm_index)
            WRITE(iounit) u10(:,:,:,cm_index)
            WRITE(iounit) v10(:,:,:,cm_index)
            WRITE(iounit) tt2(:,:,:,cm_index)
            WRITE(iounit) td2(:,:,:,cm_index)
            WRITE(iounit) lsprec(:,:,:,cm_index)
            WRITE(iounit) convprec(:,:,:,cm_index)
            WRITE(iounit) sshf(:,:,:,cm_index)
            WRITE(iounit) ssr(:,:,:,cm_index)
            WRITE(iounit) surfstr(:,:,:,cm_index)
            WRITE(iounit) ustar(:,:,:,cm_index)
            WRITE(iounit) wstar(:,:,:,cm_index)
            WRITE(iounit) hmix(:,:,:,cm_index)
            WRITE(iounit) tropopause(:,:,:,cm_index)
            WRITE(iounit) oli(:,:,:,cm_index)
            WRITE(iounit) diffk(:,:,:,cm_index)
            WRITE(iounit) vdep(:,:,:,cm_index)

            dim2dids = (/nxmax_dimid, nymax_dimid/)

            ncret = nf90_def_var(ncid, 'ps', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                ps(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'sd', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                sd(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'msl', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                msl(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'tcc', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tcc(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'u10', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                u10(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'v10', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                v10(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'tt2', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tt2(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'td2', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                td2(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'lsprec', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                lsprec(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'convprec', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                convprec(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'sshf', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                sshf(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ssr', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                ssr(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'surfstr', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                surfstr(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ustar', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                ustar(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'wstar', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                wstar(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'hmix', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                hmix(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'tropopause', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tropopause(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'oli', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                oli(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'diffk', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                diffk(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)



            PRINT *, 'SUM(ps(0:nxmax-1, 0:nymax-1, 1, cm_index)): ', &
&                                        SUM(ps(0:nxmax-1,0:nymax-1,1, cm_index))

            PRINT *, 'SUM(wstar(0:nxmax-1, 0:nymax-1, 1, cm_index)): ', &
&                                        SUM(wstar(0:nxmax-1,0:nymax-1,1, cm_index))


            dim3dids = (/nxmax_dimid, nymax_dimid, maxspec_dimid/)

            ncret = nf90_def_var(ncid, 'vdep', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                vdep(0:nxmax-1, 0:nymax-1, 1:maxspec, cm_index))
            call handle_nf90_err(ncret)



            ! 1d fields
            WRITE(iounit) z0(:)
            WRITE(iounit) akm(:)
            WRITE(iounit) bkm(:)
            WRITE(iounit) akz(:)
            WRITE(iounit) bkz(:)
            WRITE(iounit) aknew(:)
            WRITE(iounit) bknew(:)

            dim1dids = (/numclass_dimid/)

            ncret = nf90_def_var(ncid, 'z0', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                z0(1:numclass))


            dim1dids = (/nwzmax_dimid/)

            ncret = nf90_def_var(ncid, 'akm', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                akm(1:nwzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'bkm', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                bkm(1:nwzmax))
            call handle_nf90_err(ncret)


            dim1dids = (/nuvzmax_dimid/)

            ncret = nf90_def_var(ncid, 'akz', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                akz(1:nuvzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'bkz', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                bkz(1:nuvzmax))
            call handle_nf90_err(ncret)


            dim1dids = (/nzmax_dimid/)

            ncret = nf90_def_var(ncid, 'aknew', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                aknew(1:nzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'bknew', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                bknew(1:nzmax))
            call handle_nf90_err(ncret)


            PRINT *, 'SUM(bknew(1:nzmax)): ', &
&                                        SUM(bknew(1:nzmax))



            ! Getting ready to add in nested code

            ! These are compiled max dimensions from par_mod - these are
            ! not meant to be reassigned during a LOAD, but used as "header"
            ! information to provide the structure of arrays
            ncret = nf90_def_dim(ncid, 'maxnests', maxnests, maxnests_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'nxmaxn', nxmaxn, nxmaxn_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'nymaxn', nymaxn, nymaxn_dimid)
            call handle_nf90_err(ncret)

            WRITE(iounit) nxn(:)
            WRITE(iounit) nyn(:)
            WRITE(iounit) dxn(:)
            WRITE(iounit) dyn(:)
            WRITE(iounit) xlon0n(:)
            WRITE(iounit) ylat0n(:)

            ! Nested, scalar values (for each nest)

            dim1dids = (/maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'nxn', NF90_INT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                nxn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nyn', NF90_INT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                nyn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'dxn', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                dxn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'dyn', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                dyn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'xlon0n', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                xlon0n(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ylat0n', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                ylat0n(1:maxnests))
            call handle_nf90_err(ncret)




            ! Nested fields, static over time
            WRITE(iounit) oron, excessoron, lsmn, xlandusen 

            dim3dids = (/nxmaxn_dimid, nymaxn_dimid, maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'oron', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                oron(0:nxmaxn-1, 0:nymaxn-1, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'excessoron', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                excessoron(0:nxmaxn-1, 0:nymaxn-1, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'lsmn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                lsmn(0:nxmaxn-1, 0:nymaxn-1, 1:maxnests))
            call handle_nf90_err(ncret)

            dim4dids = (/nxmaxn_dimid, nymaxn_dimid, numclass_dimid, maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'xlandusen', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                xlandusen(0:nxmaxn-1, 0:nymaxn-1, 1:numclass, 1:maxnests))
            call handle_nf90_err(ncret)

            PRINT *, 'SUM(oron): ', SUM(oron)



            ! 3d nested fields
            WRITE(iounit) uun(:,:,:,cm_index,:)
            WRITE(iounit) vvn(:,:,:,cm_index,:)
            WRITE(iounit) wwn(:,:,:,cm_index,:)
            WRITE(iounit) ttn(:,:,:,cm_index,:)
            WRITE(iounit) qvn(:,:,:,cm_index,:)
            WRITE(iounit) pvn(:,:,:,cm_index,:)
            WRITE(iounit) cloudsn(:,:,:,cm_index,:)
            WRITE(iounit) cloudsnh(:,:,cm_index,:)
            WRITE(iounit) rhon(:,:,:,cm_index,:)
            WRITE(iounit) drhodzn(:,:,:,cm_index,:)
            WRITE(iounit) tthn(:,:,:,cm_index,:)
            WRITE(iounit) qvhn(:,:,:,cm_index,:)


            dim4dids = (/nxmaxn_dimid, nymaxn_dimid, nzmax_dimid, maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'uun', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                uun(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'vvn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                vvn(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'wwn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                wwn(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ttn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                ttn(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'qvn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                qvn(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'pvn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                pvn(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'rhon', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                rhon(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'drhodzn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                drhodzn(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)


            ! Note the new dimensions
            dim4dids = (/nxmaxn_dimid, nymaxn_dimid, nuvzmax_dimid, maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'tthn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tthn(0:nxmaxn-1, 0:nymaxn-1, 1:nuvzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'qvhn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                qvhn(0:nxmaxn-1, 0:nymaxn-1, 1:nuvzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ! Note the new dimensions
            dim4dids = (/nxmaxn_dimid, nymaxn_dimid, zero_to_nzmax_dimid, maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'cloudsn', NF90_INT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                cloudsn(0:nxmaxn-1, 0:nymaxn-1, 0:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ! Note the new dimensions
            dim3dids = (/nxmaxn_dimid, nymaxn_dimid, maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'cloudsnh', NF90_INT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                cloudsnh(0:nxmaxn-1, 0:nymaxn-1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)









            PRINT *, 'SUM(uun): ', SUM(uun(:,:,:,cm_index,:))
            PRINT *, 'SUM(qvhn): ', SUM(qvhn(:,:,:,cm_index,:))
            PRINT *, 'SUM(cloudsn): ', SUM(cloudsn(:,:,:,cm_index,:))



            ! 2d nested fields
            WRITE(iounit) psn(:,:,:,cm_index,:)
            WRITE(iounit) sdn(:,:,:,cm_index,:)
            WRITE(iounit) msln(:,:,:,cm_index,:)
            WRITE(iounit) tccn(:,:,:,cm_index,:)
            WRITE(iounit) u10n(:,:,:,cm_index,:)
            WRITE(iounit) v10n(:,:,:,cm_index,:)
            WRITE(iounit) tt2n(:,:,:,cm_index,:)
            WRITE(iounit) td2n(:,:,:,cm_index,:)
            WRITE(iounit) lsprecn(:,:,:,cm_index,:)
            WRITE(iounit) convprecn(:,:,:,cm_index,:)
            WRITE(iounit) sshfn(:,:,:,cm_index,:)
            WRITE(iounit) ssrn(:,:,:,cm_index,:)
            WRITE(iounit) surfstrn(:,:,:,cm_index,:)
            WRITE(iounit) ustarn(:,:,:,cm_index,:)
            WRITE(iounit) wstarn(:,:,:,cm_index,:)
            WRITE(iounit) hmixn(:,:,:,cm_index,:)
            WRITE(iounit) tropopausen(:,:,:,cm_index,:)
            WRITE(iounit) olin(:,:,:,cm_index,:)
            WRITE(iounit) diffkn(:,:,:,cm_index,:)
            WRITE(iounit) vdepn(:,:,:,cm_index,:)

            dim3dids = (/nxmax_dimid, nymax_dimid, maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'psn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                psn(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)






            PRINT *, 'SUM(psn): ', SUM(psn(:,:,:,cm_index,:))







            ! Auxiliary variables for nests
            WRITE(iounit) xresoln(:)
            WRITE(iounit) yresoln(:)
            WRITE(iounit) xln(:)
            WRITE(iounit) yln(:)
            WRITE(iounit) xrn(:)
            WRITE(iounit) yrn(:)

            ! Variables for polar stereographic projection
            WRITE(iounit) xglobal, sglobal, nglobal
            WRITE(iounit) switchnorthg, switchsouthg
            WRITE(iounit) southpolemap(:)
            WRITE(iounit) northpolemap(:)

            ! Variables declared in conv_mod (convection)
            WRITE(iounit) pconv(:)
            WRITE(iounit) phconv(:)
            WRITE(iounit) dpr(:)
            WRITE(iounit) pconv_hpa(:)
            WRITE(iounit) phconv_hpa(:)
            WRITE(iounit) ft(:)
            WRITE(iounit) fq(:)
            WRITE(iounit) fmass(:,:)
            WRITE(iounit) sub(:)
            WRITE(iounit) fmassfrac(:,:)
            WRITE(iounit) cbaseflux(:,:)
            WRITE(iounit) cbasefluxn(:,:,:)
            WRITE(iounit) tconv(:)
            WRITE(iounit) qconv(:)
            WRITE(iounit) qsconv(:)
            WRITE(iounit) psconv, tt2conv, td2conv
            WRITE(iounit) nconvlev, nconvtop

        ELSE IF (op == 'LOAD') THEN 

            ! Read the preprocessed format version string and insure it
            ! matches this version
            READ (iounit) temp_preproc_format_version_str
            PRINT *, 'Reading preprocessed file format version: ', &
&                    temp_preproc_format_version_str

            IF (TRIM(temp_preproc_format_version_str) == &
&                                        TRIM(PREPROC_FORMAT_VERSION_STR)) THEN
                CONTINUE
            ELSE
                ! PRINT *, ''  GK: causes relocation truncated to fit: R_X86_64_32
                PRINT *, 'Inconsistent preprocessing format version'
                PRINT *, 'Expected Version: ', PREPROC_FORMAT_VERSION_STR
                PRINT *, 'Detected Version: ', temp_preproc_format_version_str
                ! PRINT *, ''
                STOP
            END IF

            ! Read the compiled max dimensions that were dumped from par_mod 
            ! when creating the fp file, so that we can compare against
            ! current FLEXPART dimensions - they need to be the same, or else
            ! we abort.
            READ (iounit) temp_nxmax, temp_nymax, temp_nzmax, &
&                         temp_nuvzmax, temp_nwzmax

            ! Get dimensions
            ncret = nf90_inq_dimid(ncid, 'nxmax', nxmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, nxmax_dimid, nxmax_dimname, &
&                                                temp_nxmax)
            call handle_nf90_err(ncret)
            PRINT *, 'temp_nxmax: ', temp_nxmax

            ncret = nf90_inq_dimid(ncid, 'nymax', nymax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, nymax_dimid, nymax_dimname, &
&                                                temp_nymax)
            call handle_nf90_err(ncret)
            PRINT *, 'temp_nymax: ', temp_nymax

            ncret = nf90_inq_dimid(ncid, 'nzmax', nzmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, nzmax_dimid, nzmax_dimname, &
&                                                temp_nzmax)
            call handle_nf90_err(ncret)
            PRINT *, 'temp_nzmax: ', temp_nzmax

            ncret = nf90_inq_dimid(ncid, 'nuvzmax', nuvzmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, nuvzmax_dimid, nuvzmax_dimname, &
&                                                temp_nuvzmax)
            call handle_nf90_err(ncret)
            PRINT *, 'temp_nuvzmax: ', temp_nuvzmax

            ncret = nf90_inq_dimid(ncid, 'nwzmax', nwzmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, nwzmax_dimid, nwzmax_dimname, &
&                                                temp_nwzmax)
            call handle_nf90_err(ncret)
            PRINT *, 'temp_nwzmax: ', temp_nwzmax

            ncret = nf90_inq_dimid(ncid, 'numclass', numclass_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, numclass_dimid, numclass_dimname, &
&                                                temp_numclass)
            call handle_nf90_err(ncret)
            PRINT *, 'temp_numclass: ', temp_numclass

            ncret = nf90_inq_dimid(ncid, 'maxspec', maxspec_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, maxspec_dimid, maxspec_dimname, &
&                                                temp_maxspec)
            call handle_nf90_err(ncret)
            PRINT *, 'temp_maxspec: ', temp_maxspec



            IF ( (temp_nxmax == nxmax) .AND. (temp_nymax == nymax) .AND. &
&                   (temp_nzmax == nzmax) .AND. &
&                   (temp_nuvzmax == nuvzmax) .AND. &
&                   (temp_nwzmax == nwzmax) .AND. &
&                   (temp_numclass == numclass) .AND. &
&                   (temp_maxspec == maxspec) ) THEN
                CONTINUE
            ELSE
                PRINT *, 'Incompatible dimensions between fp file and current FLEXPART!'
                ! PRINT *, ''
                PRINT *, '                  FP file     Compiled FP'
                PRINT *, 'nxmax:     ', temp_nxmax, '    ', nxmax 
                PRINT *, 'nymax:     ', temp_nymax, '    ', nymax 
                PRINT *, 'nzmax:     ', temp_nzmax, '    ', nzmax 
                PRINT *, 'nuvzmax:     ', temp_nuvzmax, '    ', nuvzmax 
                PRINT *, 'nwzmax:     ', temp_nwzmax, '    ', nwzmax
                PRINT *, 'numclass:     ', temp_numclass, '    ', numclass
                PRINT *, 'maxspec:     ', temp_maxspec, '    ', maxspec
                ! PRINT *, ''
                STOP
            END IF




            ! Scalar values
            READ(iounit) nx, ny, nxmin1, nymin1, nxfield
            READ(iounit) nuvz, nwz, nz, nmixz, nlev_ec
            READ(iounit) dx, dy, xlon0, ylat0, dxconst, dyconst



            ! Get the varid , then read into scalar variable
            ncret = nf90_inq_varid(ncid, 'nx', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nx)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ny', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ny)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nxmin1', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nxmin1)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nymin1', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nymin1)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nxfield', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nxfield)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nuvz', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nuvz)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nwz', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nwz)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nz', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nz)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nmixz', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nmixz)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nlev_ec', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nlev_ec)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'dx', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, dx)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'dy', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, dy)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'xlon0', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, xlon0)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ylat0', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ylat0)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'dxconst', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, dxconst)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'dyconst', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, dyconst)
            call handle_nf90_err(ncret)






            ! Fixed fields, static in time
            READ(iounit) oro, excessoro, lsm, xlanduse, height

            ncret = nf90_inq_varid(ncid, 'oro', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, oro(0:nxmax-1,0:nymax-1))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'excessoro', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, excessoro(0:nxmax-1,0:nymax-1))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'lsm', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, lsm(0:nxmax-1,0:nymax-1))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'xlanduse', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, xlanduse(0:nxmax-1,0:nymax-1, 1:numclass))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'height', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, height(1:nzmax))
            call handle_nf90_err(ncret)




            ! 3d fields
            READ(iounit) uu(:,:,:,cm_index)
            READ(iounit) vv(:,:,:,cm_index)
            READ(iounit) uupol(:,:,:,cm_index)
            READ(iounit) vvpol(:,:,:,cm_index)
            READ(iounit) ww(:,:,:,cm_index)
            READ(iounit) tt(:,:,:,cm_index)
            READ(iounit) qv(:,:,:,cm_index)
            READ(iounit) pv(:,:,:,cm_index)
            READ(iounit) rho(:,:,:,cm_index)
            READ(iounit) drhodz(:,:,:,cm_index)
            READ(iounit) tth(:,:,:,cm_index)
            READ(iounit) qvh(:,:,:,cm_index)
            READ(iounit) pplev(:,:,:,cm_index)
            READ(iounit) clouds(:,:,:,cm_index)
            READ(iounit) cloudsh(:,:,cm_index)




            ! Get the varid and read the variable into the array
            ncret = nf90_inq_varid(ncid, 'uu', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, uu(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'vv', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, vv(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'uupol', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, uupol(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'vvpol', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, vvpol(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ww', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ww(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tt', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tt(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'qv', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, qv(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'pv', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, pv(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'rho', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, rho(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'drhodz', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, drhodz(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'clouds', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, clouds(0:nxmax-1,0:nzmax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tth', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tth(0:nxmax-1,0:nymax-1,1:nuvzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'qvh', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, qvh(0:nxmax-1,0:nymax-1,1:nuvzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'pplev', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, pplev(0:nxmax-1,0:nymax-1,1:nuvzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'cloudsh', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, cloudsh(0:nxmax-1,0:nymax-1,cm_index))
            call handle_nf90_err(ncret)




            PRINT *, 'SUM(tt(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index)): ', &
&                                        SUM(tt(0:nxmax-1,0:nymax-1,1:nzmax, cm_index))


            PRINT *, 'SUM(clouds(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index)): ', &
&                                        SUM(clouds(0:nxmax-1,0:nymax-1,1:nzmax, cm_index))



            ! 2d fields
            READ(iounit) ps(:,:,:,cm_index)
            READ(iounit) sd(:,:,:,cm_index)
            READ(iounit) msl(:,:,:,cm_index)
            READ(iounit) tcc(:,:,:,cm_index)
            READ(iounit) u10(:,:,:,cm_index)
            READ(iounit) v10(:,:,:,cm_index)
            READ(iounit) tt2(:,:,:,cm_index)
            READ(iounit) td2(:,:,:,cm_index)
            READ(iounit) lsprec(:,:,:,cm_index)
            READ(iounit) convprec(:,:,:,cm_index)
            READ(iounit) sshf(:,:,:,cm_index)
            READ(iounit) ssr(:,:,:,cm_index)
            READ(iounit) surfstr(:,:,:,cm_index)
            READ(iounit) ustar(:,:,:,cm_index)
            READ(iounit) wstar(:,:,:,cm_index)
            READ(iounit) hmix(:,:,:,cm_index)
            READ(iounit) tropopause(:,:,:,cm_index)
            READ(iounit) oli(:,:,:,cm_index)
            READ(iounit) diffk(:,:,:,cm_index)
            READ(iounit) vdep(:,:,:,cm_index)

            ! Get the varid and read the variable into the array
            ncret = nf90_inq_varid(ncid, 'ps', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ps(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'sd', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, sd(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'msl', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, msl(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tcc', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tcc(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'u10', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, u10(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'v10', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, v10(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tt2', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tt2(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'td2', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, td2(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'lsprec', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, lsprec(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'convprec', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, convprec(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'sshf', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, sshf(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ssr', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ssr(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'surfstr', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, surfstr(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ustar', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ustar(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'wstar', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, wstar(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'hmix', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, hmix(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tropopause', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tropopause(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'oli', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, oli(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'diffk', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, diffk(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)




            PRINT *, 'SUM(ps(0:nxmax-1, 0:nymax-1, 1, cm_index)): ', &
&                                        SUM(ps(0:nxmax-1,0:nymax-1,1, cm_index))

            PRINT *, 'SUM(wstar(0:nxmax-1, 0:nymax-1, 1, cm_index)): ', &
&                                        SUM(wstar(0:nxmax-1,0:nymax-1,1, cm_index))


            ncret = nf90_inq_varid(ncid, 'vdep', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, vdep(0:nxmax-1,0:nymax-1,1:maxspec, cm_index))
            call handle_nf90_err(ncret)





            ! 1d fields
            READ(iounit) z0(:)
            READ(iounit) akm(:)
            READ(iounit) bkm(:)
            READ(iounit) akz(:)
            READ(iounit) bkz(:)
            READ(iounit) aknew(:)
            READ(iounit) bknew(:)


            ncret = nf90_inq_varid(ncid, 'z0', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, z0(1:numclass))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'akm', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, akm(1:nwzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'bkm', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, bkm(1:nwzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'akz', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, akz(1:nuvzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'bkz', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, bkz(1:nuvzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'aknew', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, aknew(1:nzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'bknew', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, bknew(1:nzmax))
            call handle_nf90_err(ncret)



            PRINT *, 'SUM(bknew(1:nzmax)): ', &
&                                        SUM(bknew(1:nzmax))




            ! Now the nested input grid variables
            ! Get the compiled values that were written into the FP file, and
            ! make sure they are equal to the current compiled values, to make
            ! sure we are working with consistent arrays
            ncret = nf90_inq_dimid(ncid, 'maxnests', maxnests_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, maxnests_dimid, maxnests_dimname, &
&                                                temp_maxnests)
            call handle_nf90_err(ncret)
            PRINT *, 'temp_maxnests: ', temp_maxnests

            ncret = nf90_inq_dimid(ncid, 'nxmaxn', nxmaxn_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, nxmaxn_dimid, nxmaxn_dimname, &
&                                                temp_nxmaxn)
            call handle_nf90_err(ncret)
            PRINT *, 'temp_nxmaxn: ', temp_nxmaxn

            ncret = nf90_inq_dimid(ncid, 'nymaxn', nymaxn_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, nymaxn_dimid, nymaxn_dimname, &
&                                                temp_nymaxn)
            call handle_nf90_err(ncret)
            PRINT *, 'temp_nymaxn: ', temp_nymaxn

            ! Note that maxspec_dimid and numclass_dimid were checked above




            IF ( (temp_nxmaxn == nxmaxn) .AND. (temp_nymaxn == nymaxn) .AND. &
&                   (temp_maxnests == maxnests) ) THEN
                CONTINUE
            ELSE
                PRINT *, 'Incompatible dimensions between fp file and current FLEXPART!'
                ! PRINT *, ''
                PRINT *, '                  FP file     Compiled FP'
                PRINT *, 'nxmaxn:     ', temp_nxmaxn, '    ', nxmaxn
                PRINT *, 'nymaxn:     ', temp_nymaxn, '    ', nymaxn
                PRINT *, 'maxnests:     ', temp_maxnests, '    ', maxnests
                STOP
            END IF



            ! Nested, scalar values (for each nest)
            READ(iounit) nxn(:)
            READ(iounit) nyn(:)
            READ(iounit) dxn(:)
            READ(iounit) dyn(:)
            READ(iounit) xlon0n(:)
            READ(iounit) ylat0n(:)


            ncret = nf90_inq_varid(ncid, 'nxn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nxn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nyn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nyn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'dxn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, dxn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'dyn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, dyn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'xlon0n', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, xlon0n(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ylat0n', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ylat0n(1:maxnests))
            call handle_nf90_err(ncret)



            ! Nested fields, static over time
            READ(iounit) oron, excessoron, lsmn, xlandusen 

            ncret = nf90_inq_varid(ncid, 'oron', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, oron(0:nxmaxn-1,0:nymaxn-1,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'excessoron', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, excessoron(0:nxmaxn-1,0:nymaxn-1,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'lsmn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, lsmn(0:nxmaxn-1,0:nymaxn-1,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'xlandusen', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, xlandusen(0:nxmaxn-1,0:nymaxn-1,1:numclass,1:maxnests))
            call handle_nf90_err(ncret)


            PRINT *, 'SUM(oron): ', SUM(oron)




            ! 3d nested fields
            READ(iounit) uun(:,:,:,cm_index,:)
            READ(iounit) vvn(:,:,:,cm_index,:)
            READ(iounit) wwn(:,:,:,cm_index,:)
            READ(iounit) ttn(:,:,:,cm_index,:)
            READ(iounit) qvn(:,:,:,cm_index,:)
            READ(iounit) pvn(:,:,:,cm_index,:)
            READ(iounit) cloudsn(:,:,:,cm_index,:)
            READ(iounit) cloudsnh(:,:,cm_index,:)
            READ(iounit) rhon(:,:,:,cm_index,:)
            READ(iounit) drhodzn(:,:,:,cm_index,:)
            READ(iounit) tthn(:,:,:,cm_index,:)
            READ(iounit) qvhn(:,:,:,cm_index,:)


            ncret = nf90_inq_varid(ncid, 'uun', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, uun(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'vvn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, vvn(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'wwn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, wwn(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ttn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ttn(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'qvn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, qvn(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'pvn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, pvn(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'rhon', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, rhon(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'drhodzn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, drhodzn(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tthn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tthn(0:nxmaxn-1,0:nymaxn-1,1:nuvzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'qvhn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, qvhn(0:nxmaxn-1,0:nymaxn-1,1:nuvzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'cloudsn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, cloudsn(0:nxmaxn-1,0:nymaxn-1,0:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'cloudsnh', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, cloudsnh(0:nxmaxn-1,0:nymaxn-1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)




            PRINT *, 'SUM(uun): ', SUM(uun(:,:,:,cm_index,:))
            PRINT *, 'SUM(qvhn): ', SUM(qvhn(:,:,:,cm_index,:))
            PRINT *, 'SUM(cloudsn): ', SUM(cloudsn(:,:,:,cm_index,:))




            ! 2d nested fields
            READ(iounit) psn(:,:,:,cm_index,:)
            READ(iounit) sdn(:,:,:,cm_index,:)
            READ(iounit) msln(:,:,:,cm_index,:)
            READ(iounit) tccn(:,:,:,cm_index,:)
            READ(iounit) u10n(:,:,:,cm_index,:)
            READ(iounit) v10n(:,:,:,cm_index,:)
            READ(iounit) tt2n(:,:,:,cm_index,:)
            READ(iounit) td2n(:,:,:,cm_index,:)
            READ(iounit) lsprecn(:,:,:,cm_index,:)
            READ(iounit) convprecn(:,:,:,cm_index,:)
            READ(iounit) sshfn(:,:,:,cm_index,:)
            READ(iounit) ssrn(:,:,:,cm_index,:)
            READ(iounit) surfstrn(:,:,:,cm_index,:)
            READ(iounit) ustarn(:,:,:,cm_index,:)
            READ(iounit) wstarn(:,:,:,cm_index,:)
            READ(iounit) hmixn(:,:,:,cm_index,:)
            READ(iounit) tropopausen(:,:,:,cm_index,:)
            READ(iounit) olin(:,:,:,cm_index,:)
            READ(iounit) diffkn(:,:,:,cm_index,:)
            READ(iounit) vdepn(:,:,:,cm_index,:)

            ncret = nf90_inq_varid(ncid, 'psn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, psn(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)



            PRINT *, 'SUM(psn): ', SUM(psn(:,:,:,cm_index,:))



            ! Auxiliary variables for nests
            READ(iounit) xresoln(:)
            READ(iounit) yresoln(:)
            READ(iounit) xln(:)
            READ(iounit) yln(:)
            READ(iounit) xrn(:)
            READ(iounit) yrn(:)

            ! Variables for polar stereographic projection
            READ(iounit) xglobal, sglobal, nglobal
            READ(iounit) switchnorthg, switchsouthg
            READ(iounit) southpolemap(:)
            READ(iounit) northpolemap(:)

            ! Variables declared in conv_mod (convection)
            READ(iounit) pconv(:)
            READ(iounit) phconv(:)
            READ(iounit) dpr(:)
            READ(iounit) pconv_hpa(:)
            READ(iounit) phconv_hpa(:)
            READ(iounit) ft(:)
            READ(iounit) fq(:)
            READ(iounit) fmass(:,:)
            READ(iounit) sub(:)
            READ(iounit) fmassfrac(:,:)
            READ(iounit) cbaseflux(:,:)
            READ(iounit) cbasefluxn(:,:,:)
            READ(iounit) tconv(:)
            READ(iounit) qconv(:)
            READ(iounit) qsconv(:)
            READ(iounit) psconv, tt2conv, td2conv
            READ(iounit) nconvlev, nconvtop

        ELSE
            STOP 'fpio(): Illegal operation' 
            
        ENDIF
    END SUBROUTINE fpio

    SUBROUTINE fpmetbinary_filetext(filename, cm_index)

        ! This is a utility subroutine meant to be used for testing purposes.
        ! It facilitates the text output of variables read in from the 
        ! specified .fp file.  This routine will easily cause the program
        ! to crash due memory allocation issues, particularly when you are
        ! trying to text print 3d arrays in a single formatted statetment.
        
        CHARACTER(LEN=*), INTENT(IN) :: filename
        INTEGER, INTENT(IN) :: cm_index           ! Index of last dimension in
                                                  ! most com_mod variables.
                                                  ! Should be 1 or 2 

        !OPEN(IOUNIT_TEXTOUT, file=filename, action='WRITE', status='REPLACE', &
        !    form="formatted", access="stream")
        OPEN(IOUNIT_TEXTOUT, file=filename, action='WRITE', &
             form="formatted", access="APPEND")

        WRITE(IOUNIT_TEXTOUT, *) 'oro: ', oro
        WRITE(IOUNIT_TEXTOUT, *) 'excessoro: ', excessoro
        WRITE(IOUNIT_TEXTOUT, *) 'lsm: ', lsm
        WRITE(IOUNIT_TEXTOUT, *) 'xlanduse: ', xlanduse
        WRITE(IOUNIT_TEXTOUT, *) 'height: ', height

        WRITE(IOUNIT_TEXTOUT, *) 'uu: ', uu(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'vv: ', vv(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'uupol: ', uupol(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'vvpol: ', vvpol(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'ww: ', ww(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'tt: ', tt(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'qv: ', qv(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'pv: ', pv(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'rho: ', rho(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'drhodz: ', drhodz(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'tth: ', tth(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'qvh: ', qvh(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'pplev: ', pplev(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'clouds: ', clouds(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'cloudsh: ', cloudsh(:,:,cm_index)




        CLOSE(IOUNIT_TEXTOUT)
    END SUBROUTINE fpmetbinary_filetext

    subroutine handle_nf90_err(status)

        ! Custom routine for checking NF90 error status
        ! and aborting if necessary
        use netcdf
        implicit none
        integer, intent (in) :: status

        if (status /= nf90_noerr) then
            print *, trim(nf90_strerror(status))
            stop "Stopped..."
        endif
    end subroutine handle_nf90_err




END MODULE fpmetbinary_mod
