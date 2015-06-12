program da_update_bc_bin

   !-----------------------------------------------------------------------
   ! Purpose: update BC file from GSI or wrfvar output.
   ! This version reads only wrf-binary file format
   !
   ! Ming Hu, 02/14/2011:
   !    Add Binary IO
   !
   ! Y.-R. Guo, 03/18/2008:
   !   1) Fixed the bug for low_bdy_only;
   !   2) Introducing another namelist variable: update_lsm
   !      update_lsm = .true. --- The LSM predicted variables: 
   !                         TSLB, SMOIS, SNOW, SH2O, RHOSN, CANWAT, SNOWH
   !                              will be updated based on wrf_input file
   !                 = .false. -- no updated, default.
   !
   !-----------------------------------------------------------------------

!   use da_module_couple_uv, only : da_couple_uv

   use mpi
   use kinds, only: r_single,i_llong,i_kind,i_byte
   use da_updateBC_interface, only: da_get_bdyfrq, stderr, stdout
   use da_module_couple_uv, only : da_couple_uv

   implicit none

   integer :: npe, mype, mypeLocal,ierror
!  for wrfinput_d01
   integer(i_kind),allocatable:: start_block(:),end_block(:)
   integer(i_kind),allocatable:: start_byte(:),end_byte(:)
   integer(i_llong),allocatable:: file_offset(:)
   integer(i_llong) n_position
   character(132),allocatable:: datestr_all(:),varname_all(:),memoryorder_all(:)
   integer(i_kind),allocatable:: domainend_all(:,:)
   integer(i_kind) nrecs
   integer(i_kind) iunit_wrfinput
   integer(i_kind) status_hdr
   integer(i_kind) hdrbuf(512)
!  for gsi_output 
   integer(i_kind),allocatable:: start_block_gsi(:),end_block_gsi(:)
   integer(i_kind),allocatable:: start_byte_gsi(:),end_byte_gsi(:)
   integer(i_llong),allocatable:: file_offset_gsi(:)
   integer(i_llong) n_position_gsi
   character(132),allocatable:: datestr_all_gsi(:),varname_all_gsi(:),memoryorder_all_gsi(:)
   integer(i_kind),allocatable:: domainend_all_gsi(:,:)
   integer(i_kind) nrecs_gsi
   integer(i_kind) iunit_gsi
!  for wrfbdy_d01
   integer(i_kind),allocatable:: start_block_bdy(:),end_block_bdy(:)
   integer(i_kind),allocatable:: start_byte_bdy(:),end_byte_bdy(:)
   integer(i_llong),allocatable:: file_offset_bdy(:)
   integer(i_llong) n_position_bdy
   character(132),allocatable:: datestr_all_bdy(:),varname_all_bdy(:),memoryorder_all_bdy(:)
   integer(i_kind),allocatable:: domainend_all_bdy(:,:)
   integer(i_kind) nrecs_bdy
   integer(i_kind) iunit_bdy
   character(132):: datestr_bdy(4)

   real(r_single),allocatable:: field2(:,:)
   real(r_single),allocatable:: field3(:,:,:)

!
   CHARACTER (LEN=19)              :: VarName
   integer :: index_gsi,index_bdy,index_wrfinput
   integer :: hor_size,ierr

   integer, parameter :: max_3d_variables = 20, &
                         max_2d_variables = 25
 
   character(len=10)   :: da_file,      &
                         wrf_bdy_file, &
                         wrf_input
 
   character(len=20) :: var_pref, var_name, vbt_name

   character(len=20) :: var3d(max_3d_variables), &
                        varsf(max_2d_variables)

   character(len=10), dimension(4) :: bdyname, tenname

   integer           :: ids, ide, jds, jde, kds, kde
   integer           :: num3d, num2d, ndims
   integer           :: time_level
   integer           :: i,j,k,l,m,n

   integer, dimension(4) :: dims
 
   real(8), allocatable, dimension(:,:,:) :: tend3d, scnd3d, frst3d, full3d

   real, allocatable, dimension(:,:,:) :: u, v

   real, allocatable, dimension(:,  :) :: mu, mub, msfu, msfv, msfm
   real(8), allocatable, dimension(:,  :) :: tend2d, scnd2d, frst2d, full2d

   real, allocatable, dimension(:,  :) :: tsk, tsk_wrfvar
   real, allocatable, dimension(:,:)   :: snow, snowc, snowh

   integer, allocatable, dimension(:,:) :: ivgtyp, full2dint

   character(len=80), allocatable, dimension(:) :: times, &
                                                   thisbdytime, nextbdytime &
                                                  ,inittime
 
   integer :: east_end, north_end, io_status, cdfid, varid, domain_id, iswater

   logical :: debug, update_lateral_bdy, update_low_bdy, update_lsm, keep_tsk_wrf

   real :: bdyfrq, bdyfrqini

   character(len=512) :: wrfvar_output_file    ! obsolete. Kept for backward compatibility
   logical            :: cycling, low_bdy_only ! obsolete. Kept for backward compatibility

   integer, parameter :: namelist_unit = 7, &
                         ori_unit = 11, &
                         new_unit = 12

   namelist /control_param/ da_file,      &
                            wrf_bdy_file, &
                            wrf_input, domain_id, &
                            debug, update_lateral_bdy, update_low_bdy, update_lsm, &
                            keep_tsk_wrf, iswater, &
                            wrfvar_output_file, cycling, low_bdy_only

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

   da_file            = 'wrfvar_out'
   wrf_bdy_file       = 'wrfbdy_d01'
   wrf_input          = 'wrfinputd1'
   domain_id          = 1

   debug              = .false. 
   update_lateral_bdy = .true.
   update_low_bdy     = .true.
   update_lsm         = .false.
   keep_tsk_wrf       = .true.
   iswater            = 16      ! USGS water index: 16, MODIS water index: 17

   wrfvar_output_file = 'OBSOLETE'
   cycling            = .false.
   low_bdy_only       = .false.

   !---------------------------------------------------------------------
   ! Read namelist
   !---------------------------------------------------------------------
   io_status = 0

   iunit_wrfinput=33
   iunit_gsi=34
   iunit_bdy=35
   open(unit = namelist_unit, file = 'parame.in', &
          status = 'old' , access = 'sequential', &
          form   = 'formatted', action = 'read', &
          iostat = io_status)

   if (io_status /= 0) then
      write(unit=stdout,fmt=*) 'Error to open namelist file: parame.in.'
      write(unit=stdout,fmt=*) 'Will work for updating lateral boundary only.'
   else
      read(unit=namelist_unit, nml = control_param , iostat = io_status)

      if (io_status /= 0) then
         write(unit=stdout,fmt=*) 'Error to read control_param. Stopped.'
         stop
      end if

      ! deal with the old namelist
      if ( index(wrfvar_output_file, 'OBSOLETE') <= 0 ) then
         ! wrfvar_output_file is set in the user's parame.in
         ! reset the settings
         da_file = wrfvar_output_file
         if ( domain_id > 1 ) then
            low_bdy_only = .true.
         end if
         if ( cycling .and. domain_id == 1 ) then
            update_lateral_bdy = .true.
            update_low_bdy     = .true.
         else
            if ( low_bdy_only ) then
               update_lateral_bdy = .false.
               update_low_bdy     = .true.
            else
               update_lateral_bdy = .true.
               update_low_bdy     = .false.
            end if
         end if
      end if

      WRITE(unit=stdout, fmt='(2a)') &
           'da_file       = ', trim(da_file), &
           'wrf_bdy_file  = ', trim(wrf_bdy_file), &
           'wrf_input     = ', trim(wrf_input)

      WRITE(unit=stdout, fmt='(2(a, L10))')             &
           'update_lateral_bdy = ', update_lateral_bdy, &
           'update_low_bdy     = ', update_low_bdy

      close(unit=namelist_unit)
   end if

   ! 3D need update
   num3d=6
   var3d(1)='U'
   var3d(2)='V'
   var3d(3)='W'
   var3d(4)='T'
   var3d(5)='PH'
   var3d(6)='QVAPOR'

   ! 2D need update
   num2d=16
   varsf(1)='MUB'
   varsf(2)='MU'
   varsf(3)='MAPFAC_U'
   varsf(4)='MAPFAC_V'
   varsf(5)='MAPFAC_M'
   varsf(6)='TMN'
   varsf(7)='SST'
   varsf(8)='TSK'
   varsf(9)='VEGFRA'
   varsf(10)='ALBBCK'
   varsf(11)='TSLB'
   varsf(12)='SMOIS'
   varsf(13)='SNOW'
   varsf(14)='SEAICE'
   varsf(15)='SH2O'
   varsf(16)='CANWAT'

   if ( domain_id > 1 ) then
      write(unit=stdout, fmt='(a,i2)') 'Nested domain ID=',domain_id
      write(unit=stdout, fmt='(a)') &
        'No wrfbdy file needed, only low boundary need to be updated.'
      if ( update_lateral_bdy ) then
         write(unit=stdout, fmt='(a)') &
            'Re-setting update_lateral_bdy to be false for nested domain.'
         update_lateral_bdy = .false.
      end if
      update_low_bdy     = .true.
   end if

!
!  open binary files
!
!  for wrfinput_d01
   open(iunit_wrfinput,file=trim(wrf_input),form='unformatted')
   read(iunit_wrfinput,iostat=status_hdr)hdrbuf
   if(status_hdr /= 0) then
     write(6,*)'update BC:  problem with wrfges = ',&
          trim(wrf_input),', Status = ',status_hdr
     call stop2(74)
   endif
   close(iunit_wrfinput)

!   write(*,*) 'hdrbuf=',hdrbuf(1:20)

   call count_recs_wrf_binary_file(iunit_wrfinput, trim(wrf_input), nrecs)
!        write(*,*) 'nrecs: ', nrecs

   allocate(datestr_all(nrecs),varname_all(nrecs),domainend_all(3,nrecs))
   allocate(memoryorder_all(nrecs))
   allocate(start_block(nrecs),end_block(nrecs))
   allocate(start_byte(nrecs),end_byte(nrecs),file_offset(nrecs))

   call inventory_wrf_binary_file(iunit_wrfinput, trim(wrf_input), nrecs,  &
                      datestr_all,varname_all,memoryorder_all,domainend_all,   &
                      start_block,end_block,start_byte,end_byte,file_offset)

   close(iunit_wrfinput)
!   do N=1,NRECS
!      write(*,'(i4,2x,a30,a5,3i5)') N, trim(varname_all(N)),trim(memoryorder_all(n)) &
!                ,domainend_all(:,n)
!   enddo

!  for gsi
!   write(*,*) 'da_file=',trim(da_file)
   open(iunit_gsi,file=trim(da_file),form='unformatted')
   read(iunit_gsi,iostat=status_hdr)hdrbuf
   if(status_hdr /= 0) then
     write(6,*)'update BC:  problem with wrfges = ',&
          trim(da_file),', Status = ',status_hdr
     call stop2(74)
   endif
   close(iunit_gsi)

!   write(*,*) 'hdrbuf=',hdrbuf(1:20)

   call count_recs_wrf_binary_file(iunit_gsi, trim(da_file), nrecs_gsi)
!        write(*,*) 'nrecs: ', nrecs_gsi

   allocate(datestr_all_gsi(nrecs_gsi),varname_all_gsi(nrecs_gsi),domainend_all_gsi(3,nrecs_gsi))
   allocate(memoryorder_all_gsi(nrecs_gsi))
   allocate(start_block_gsi(nrecs_gsi),end_block_gsi(nrecs_gsi))
   allocate(start_byte_gsi(nrecs_gsi),end_byte_gsi(nrecs_gsi),file_offset_gsi(nrecs_gsi))

   call inventory_wrf_binary_file(iunit_gsi, trim(da_file), nrecs_gsi,  &
                      datestr_all_gsi,varname_all_gsi,memoryorder_all_gsi,domainend_all_gsi,   &
                      start_block_gsi,end_block_gsi,start_byte_gsi,end_byte_gsi,file_offset_gsi)
   close(iunit_gsi)

!   do N=1,NRECS_gsi
!      write(*,'(i4,2x,a30,a5,3i5)') N, trim(varname_all_gsi(N)),trim(memoryorder_all_gsi(n)) &
!                ,domainend_all_gsi(:,n)
!   enddo

!  for boundary file
!   write(*,*) 'wrf_bdy_file=',trim(wrf_bdy_file)
   open(iunit_bdy,file=trim(wrf_bdy_file),form='unformatted')
   read(iunit_bdy,iostat=status_hdr)hdrbuf
   if(status_hdr /= 0) then
     write(6,*)'update BC:  problem with wrfges = ',&
          trim(wrf_bdy_file),', Status = ',status_hdr
     call stop2(74)
   endif
   close(iunit_bdy)

!   write(*,*) 'hdrbuf=',hdrbuf(1:20)

   call count_recs_wrf_binary_file(iunit_bdy, trim(wrf_bdy_file), nrecs_bdy)
!        write(*,*) 'nrecs: ', nrecs_bdy

   allocate(datestr_all_bdy(nrecs_bdy),varname_all_bdy(nrecs_bdy),domainend_all_bdy(3,nrecs_bdy))
   allocate(memoryorder_all_bdy(nrecs_bdy))
   allocate(start_block_bdy(nrecs_bdy),end_block_bdy(nrecs_bdy))
   allocate(start_byte_bdy(nrecs_bdy),end_byte_bdy(nrecs_bdy),file_offset_bdy(nrecs_bdy))

   call inventory_wrf_binary_file(iunit_bdy, trim(wrf_bdy_file), nrecs_bdy,  &
                      datestr_all_bdy,varname_all_bdy,memoryorder_all_bdy,domainend_all_bdy,   &
                      start_block_bdy,end_block_bdy,start_byte_bdy,end_byte_bdy,file_offset_bdy)
   close(iunit_gsi)

!   do N=1,NRECS_bdy
!      write(*,'(i4,2x,a30,a5,3i5)') N, trim(varname_all_bdy(N)),trim(memoryorder_all_bdy(n)) &
!                ,domainend_all_bdy(:,n)
!   enddo
! find first 4 time levels for boundary file   
   ndims=0
   do N=1,NRECS_bdy
      if((trim(varname_all_bdy(N)) == 'T_BXS' ) &
         .and. (ndims < 4) ) then
         ndims=ndims+1
         datestr_all(ndims)=datestr_all_bdy(n)
      endif
   enddo
!
   if ( update_lateral_bdy ) then
   ! First, the boundary times
     do N=1,ndims
       write(*,*) n,trim(datestr_all(n))
     enddo 
   endif

   time_level = 1

   if (time_level < 1) then
      write(unit=stdout, fmt='(a,i2/a)') &
           'time_level = ', time_level, &
           'We need at least one time-level BDY.'
      stop 'Wrong BDY file.'
   end if

   allocate(times(2))
   allocate(thisbdytime(1))
   allocate(nextbdytime(1))
!tgs
   allocate(inittime(1))
   
   times(1)=datestr_all(1)
   times(2)=datestr_all(2)
   thisbdytime(1)=datestr_all(1)
   nextbdytime(1)=datestr_all(2)

   varName='T'
   call retrieve_index(index_gsi,VarName,varname_all_gsi,nrecs_gsi)
   if(index_gsi < 0) then
      print*,VarName," not found in file"
   else
     inittime(1)=datestr_all_gsi(index_gsi)
   end if

   call da_get_bdyfrq(thisbdytime(1), nextbdytime(1), bdyfrq, debug)
   call da_get_bdyfrq(inittime(1), nextbdytime(1), bdyfrqini, debug)
   if(mype==0) print *,'bdyfrq=',bdyfrq
   if(mype==0) print *,'bdyfrqini=',bdyfrqini

   if (debug) then
      do n=1, 1
         write(unit=stdout, fmt='(4(a, i2, 2a,2x))') &
           '       times(', n, ')=', trim(times(n)), &
           'thisbdytime (', n, ')=', trim(thisbdytime(n)), &
           'nextbdytime (', n, ')=', trim(nextbdytime(n)), &
!tgs
           'inittime    (', n, ')=', trim(inittime(n))
      end do
   end if

   east_end=0
   north_end=0
!
!   MPI file open
!         
  call mpi_file_open(mpi_comm_world, trim(wrf_input),     &
                    mpi_mode_rdonly,mpi_info_null, iunit_wrfinput, ierr)
  call mpi_file_open(mpi_comm_world, trim(da_file),     &
                    mpi_mode_rdonly,mpi_info_null, iunit_gsi, ierr)
  call mpi_file_open(mpi_comm_world, trim(wrf_bdy_file),     &
                    mpi_mode_rdwr ,mpi_info_null, iunit_bdy, ierr)

   ! For 2D variables
   ! Get mu, mub, msfu, and msfv
                                   
   do n=1,num2d

     varName=trim(varsf(n))
     call retrieve_index(index_gsi,VarName,varname_all_gsi,nrecs_gsi)
     if(index_gsi < 0) then
         print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, index_gsi, trim(varsf(n)), " not existed"
         cycle
     endif
         
     dims(1:3)=domainend_all_gsi(:,index_gsi)
     dims(4)=1

     select case(trim(varsf(n)))
     case ('MU') ;                
        if ( .not. update_lateral_bdy ) cycle
         
        allocate(field2(dims(1), dims(2)),mu(dims(1), dims(2)))
        hor_size=real(dims(1)*dims(2))

        CALL mpi_file_read_at(iunit_gsi,file_offset_gsi(index_gsi+1),     &
                          field2,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
         if (ierr /= 0) then
            print*,"Error reading ", VarName," using MPIIO"
         else
            if(mype==0) print*, "MU, max,min ", maxval(field2),minval(field2)
            mu=field2
         endif
         deallocate(field2)
         east_end=dims(1)+1
         north_end=dims(2)+1
     case ('MUB') ;
        if ( .not. update_lateral_bdy ) cycle

        allocate(field2(dims(1), dims(2)),mub(dims(1), dims(2)))
        hor_size=real(dims(1)*dims(2))

        CALL mpi_file_read_at(iunit_gsi,file_offset_gsi(index_gsi+1),     &
                          field2,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
         if (ierr /= 0) then
            print*,"Error reading ", VarName," using MPIIO"
         else
            if(mype==0) print*, "MUB, max,min ", maxval(field2),minval(field2)
            mub=field2
         endif
         deallocate(field2)
     case ('MAPFAC_U') ;
        if ( .not. update_lateral_bdy ) cycle
     
        allocate(field2(dims(1), dims(2)),msfu(dims(1), dims(2)))
        hor_size=real(dims(1)*dims(2))
   
        CALL mpi_file_read_at(iunit_gsi,file_offset_gsi(index_gsi+1),     &
                          field2,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
         if (ierr /= 0) then
            print*,"Error reading ", VarName," using MPIIO"
         else
           if(mype==0)  print*, "MAPFAC_U, max,min ", maxval(field2),minval(field2)
           msfu =field2
         endif
         deallocate(field2)
     case ('MAPFAC_V') ;
        if ( .not. update_lateral_bdy ) cycle
     
        allocate(field2(dims(1), dims(2)),msfv(dims(1), dims(2)))
        hor_size=real(dims(1)*dims(2))
   
        CALL mpi_file_read_at(iunit_gsi,file_offset_gsi(index_gsi+1),     &
                          field2,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
         if (ierr /= 0) then
            print*,"Error reading ", VarName," using MPIIO"
         else
            if(mype==0) print*, "MAPFAC_V, max,min ", maxval(field2),minval(field2)
            msfv=field2
         endif
         deallocate(field2)
     case ('MAPFAC_M') ;
        if ( .not. update_lateral_bdy ) cycle
     
        allocate(field2(dims(1), dims(2)),msfm(dims(1), dims(2)))
        hor_size=real(dims(1)*dims(2))
   
        CALL mpi_file_read_at(iunit_gsi,file_offset_gsi(index_gsi+1),     &
                          field2,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
         if (ierr /= 0) then
            print*,"Error reading ", VarName," using MPIIO"
         else
            if(mype==0) print*, "MAPFAC_M, max,min ", maxval(field2),minval(field2)
            msfm=field2
         endif
         deallocate(field2)
         
     case default ;               
       write(unit=stdout,fmt=*) 'skip varsf(n)=', trim(varsf(n))
     end select 
   end do                             
            

  if ( update_lateral_bdy ) then

    if (east_end < 1 .or. north_end < 1) then
        write(unit=stdout, fmt='(a)') 'Wrong data for Boundary.'
        stop
    end if

    write(unit=stdout,fmt='(/a/)') 'Processing the lateral boundary condition:'

   ! boundary variables
    bdyname(1)='_BXS'
    bdyname(2)='_BXE'
    bdyname(3)='_BYS'
    bdyname(4)='_BYE'

   ! boundary tendancy variables
    tenname(1)='_BTXS'
    tenname(2)='_BTXE'
    tenname(3)='_BTYS'
    tenname(4)='_BTYE'

   do m=1,4
     var_name='MU' // trim(bdyname(m))
     vbt_name='MU' // trim(tenname(m))

     call retrieve_index_bdy(index_bdy,var_name,varname_all_bdy,   &  
                            thisbdytime(1),datestr_all_bdy,nrecs_bdy)
     if(mype==0)  write(*,*) '2d=',trim(var_name),index_bdy
     if(index_bdy < 0) then
         print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, index_bdy, trim(var_name), " not existed"
         cycle
     endif

     dims(1:3)=domainend_all_bdy(:,index_bdy)
     dims(4)=1

     allocate(field2(dims(1), dims(2)))
     allocate(frst2d(dims(1), dims(2)))
     allocate(scnd2d(dims(1), dims(2)))
     allocate(tend2d(dims(1), dims(2)))
     hor_size=real(dims(1)*dims(2))

      ! Get variable at second time level
     if (time_level > 1) then
        call retrieve_index_bdy(index_bdy,var_name,varname_all_bdy,   &  
                            nextbdytime(1),datestr_all_bdy,nrecs_bdy)
        if(index_bdy < 0) then
         print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, index_bdy, trim(var_name), " not existed"
           stop 
        endif
        CALL mpi_file_read_at(iunit_bdy,file_offset_bdy(index_bdy+1),     &
                          field2,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
         if (ierr /= 0) then
            print*,"Error reading ", var_name," using MPIIO"
         else
            if(mype==0) print*, trim(var_name),",2, max,min ", maxval(field2),minval(field2)
            scnd2d=field2
         endif
      else
        call retrieve_index_bdy(index_bdy,var_name,varname_all_bdy,   &
                            thisbdytime(1),datestr_all_bdy,nrecs_bdy)
        if(index_bdy < 0) then
         print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, index_bdy, trim(var_name), " not existed"
           stop
        endif
        CALL mpi_file_read_at(iunit_bdy,file_offset_bdy(index_bdy+1),     &
                          field2,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
         if (ierr /= 0) then
            print*,"Error reading ", var_name," using MPIIO"
         else
            if(mype==0) print*, trim(var_name)," max,min ", maxval(field2),minval(field2)
            frst2d=field2
         endif

        call retrieve_index_bdy(index_bdy,vbt_name,varname_all_bdy,   &
                            thisbdytime(1),datestr_all_bdy,nrecs_bdy)
        if(index_bdy < 0) then     
         print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, index_bdy, trim(vbt_name), " not existed"
           stop
        endif
        CALL mpi_file_read_at(iunit_bdy,file_offset_bdy(index_bdy+1),     &
                          field2,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
         if (ierr /= 0) then
            print*,"Error reading ", vbt_name," using MPIIO"
         else 
            if(mype==0) print*, trim(vbt_name)," max,min ", maxval(field2),minval(field2)
            tend2d=field2
         endif
      end if

     if (debug) then
         write(unit=ori_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
              'No.', m, 'Variable: ', trim(vbt_name), &
              'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

        call retrieve_index_bdy(index_bdy,vbt_name,varname_all_bdy,   &
                            thisbdytime(1),datestr_all_bdy,nrecs_bdy)
        if(index_bdy < 0) then
         print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, index_bdy, trim(vbt_name), " not existed"
           stop
        endif
        CALL mpi_file_read_at(iunit_bdy,file_offset_bdy(index_bdy+1),     &
                          field2,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
         if (ierr /= 0) then
            print*,"Error reading ", vbt_name," using MPIIO"
         else
            if(mype==0) print*, trim(vbt_name)," max,min ", maxval(field2),minval(field2)
            tend2d=field2
         endif

         write(unit=ori_unit, fmt='(a, 10i12)') &
              ' old ', (i, i=1,dims(2))
         do j=1,dims(1)
            write(unit=ori_unit, fmt='(i4, 1x, 10e20.7)') &
                  j, (tend2d(j,i), i=1,dims(2))
         end do
      end if

      ! calculate variable at first time level
      select case(m)
      case (1) ;             ! West boundary
         do l=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2) &
               scnd2d(j,l)=frst2d(j,l)+tend2d(j,l)*bdyfrq
               frst2d(j,l)=mu(l,j)
            end do
         end do
      case (2) ;             ! East boundary
         do l=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2) &
                  scnd2d(j,l)=frst2d(j,l)+tend2d(j,l)*bdyfrq
               frst2d(j,l)=mu(east_end-l,j)
            end do
         end do
      case (3) ;             ! South boundary
         do l=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2) &
                  scnd2d(i,l)=frst2d(i,l)+tend2d(i,l)*bdyfrq
               frst2d(i,l)=mu(i,l)
            end do
         end do
      case (4) ;             ! North boundary
         do l=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2) &
                  scnd2d(i,l)=frst2d(i,l)+tend2d(i,l)*bdyfrq
               frst2d(i,l)=mu(i,north_end-l)
            end do
         end do
      case default ;
         write(unit=stdout,fmt=*) 'It is impossible here. mu, m=', m
      end select

      ! calculate new tendancy 
      do l=1,dims(2)
         do i=1,dims(1)
            tend2d(i,l)=(scnd2d(i,l)-frst2d(i,l))/bdyfrqini
         end do
      end do

      if (debug) then
         write(unit=new_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
              'No.', m, 'Variable: ', trim(vbt_name), &
              'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

         write(unit=new_unit, fmt='(a, 10i12)') &
              ' new ', (i, i=1,dims(2))

         do j=1,dims(1)
            write(unit=new_unit, fmt='(i4, 1x, 10e20.7)') &
                  j, (tend2d(j,i), i=1,dims(2))
         end do
      end if

      ! output new variable at first time level
      call retrieve_index_bdy(index_bdy,var_name,varname_all_bdy,   &
                            thisbdytime(1),datestr_all_bdy,nrecs_bdy)
      if(index_bdy < 0) then
         print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, index_bdy, trim(var_name), " not existed"
           stop
      endif
      field2=frst2d
      CALL mpi_file_write_at(iunit_bdy,file_offset_bdy(index_bdy+1),     &
                          field2,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error write ", var_name," using MPIIO"
      else
          if(mype==0) print*, "write out ",trim(var_name)," max,min ", maxval(field2),minval(field2)
      endif

      ! output new tendancy 
      call retrieve_index_bdy(index_bdy,vbt_name,varname_all_bdy,   &
                            thisbdytime(1),datestr_all_bdy,nrecs_bdy)
      if(index_bdy < 0) then
         print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, index_bdy, trim(vbt_name), " not existed"
           stop
      endif
      field2=tend2d
      CALL mpi_file_write_at(iunit_bdy,file_offset_bdy(index_bdy+1),     &
                          field2,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error write ", vbt_name," using MPIIO"
      else
          if(mype==0) print*, "write out ",trim(vbt_name)," max,min ", maxval(field2),minval(field2)
      endif

      deallocate(field2)
      deallocate(frst2d)
      deallocate(scnd2d)
      deallocate(tend2d)
   end do

   !---------------------------------------------------------------------
   ! For 3D variables
   varName='U'
   call retrieve_index(index_gsi,VarName,varname_all_gsi,nrecs_gsi)
   if(index_gsi < 0) then
      print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                  n, index_gsi, trim(varName), " not existed"
      stop 123
   endif

   dims(1:3)=domainend_all_gsi(:,index_gsi)
   dims(4)=1

   allocate(field3(dims(1), dims(2), dims(3)))  ! XZY
   allocate(u(dims(1), dims(3), dims(2)))       ! XYZ
      
   ids=1 
   ide=dims(1)-1
   jds=1      
   jde=dims(3)
   kds=1 
   kde=dims(2)

   hor_size=real(dims(1)*dims(3))

   CALL mpi_file_read_at(iunit_gsi,file_offset_gsi(index_gsi+1),   &
                          field3,hor_size*dims(2),mpi_real4,       &
                          mpi_status_ignore, ierr)
   if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
   else
     DO k=1,dims(2)
       if(mype==0) print*, trim(VarName)," max,min ", k,maxval(field3(:,k,:)),minval(field3(:,k,:))
       u(:,:,k)=field3(:,k,:)
     ENDDO
   endif
   deallocate(field3)

   varName='V'
   call retrieve_index(index_gsi,VarName,varname_all_gsi,nrecs_gsi)
   if(index_gsi < 0) then
      print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                  n, index_gsi, trim(varName), " not existed"
      stop 123 
   endif

   dims(1:3)=domainend_all_gsi(:,index_gsi)
   dims(4)=1
                            
   allocate(field3(dims(1), dims(2), dims(3)))  ! XZY
   allocate(v(dims(1), dims(3), dims(2)))       ! XYZ                   
   hor_size=real(dims(1)*dims(3))
   CALL mpi_file_read_at(iunit_gsi,file_offset_gsi(index_gsi+1),   &
                          field3,hor_size*dims(2),mpi_real4,       &
                          mpi_status_ignore, ierr)
   if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
   else
     DO k=1,dims(2)
       if(mype==0) print*, trim(VarName)," max,min ", k,maxval(field3(:,k,:)),minval(field3(:,k,:))
       v(:,:,k)=field3(:,k,:)
       if(mype==0) print*, trim(VarName)," max,min ", k,maxval(v(:,:,k)),minval(v(:,:,k))
     ENDDO
   endif
   deallocate(field3)

   if (debug) then
      write(unit=stdout, fmt='(a,e20.12,4x)') &
           'Before couple Sample u=', u(dims(1)/2,dims(3)/2,dims(2)/2), &
           'Before couple Sample v=', v(dims(1)/2,dims(3)/2,dims(2)/2)
   end if

   !---------------------------------------------------------------------
   ! Couple u, v.
   call da_couple_uv ( u, v, mu, mub, msfu, msfv, ids, ide, jds, jde, kds, kde)

   if (debug) then
      write(unit=stdout, fmt='(a,e20.12,4x)') &
           'After  couple Sample u=', u(dims(1)/2,dims(3)/2,dims(2)/2), &
           'After  couple Sample v=', v(dims(1)/2,dims(3)/2,dims(2)/2)
   end if

   !---------------------------------------------------------------------
   !For 3D variables

   do n=1,num3d
      write(unit=stdout, fmt='(a, i3, 2a)') 'Processing: var3d(', n, ')=', trim(var3d(n))

      varName=trim(var3d(n))
      call retrieve_index(index_gsi,VarName,varname_all_gsi,nrecs_gsi)
      if(index_gsi < 0) then 
         print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                  n, index_gsi, trim(varName), " not existed"
         stop 123 
      endif   
           
      dims(1:3)=domainend_all_gsi(:,index_gsi)
      dims(4)=1

      allocate(full3d(dims(1), dims(3), dims(2)))
      allocate(field3(dims(1), dims(2), dims(3)))

      east_end=dims(1)+1
      north_end=dims(3)+1

      select case(trim(var3d(n)))
      case ('U') ;           ! U
         ! var_pref='R' // trim(var3d(n))
         var_pref=trim(var3d(n))
         full3d(:,:,:)=u(:,:,:)
      case ('V') ;           ! V 
         ! var_pref='R' // trim(var3d(n))
         var_pref=trim(var3d(n))
         full3d(:,:,:)=v(:,:,:)
      case ('W') ;
         ! var_pref = 'R' // trim(var3d(n))
         var_pref = trim(var3d(n))

         hor_size=real(dims(1)*dims(3))
         CALL mpi_file_read_at(iunit_gsi,file_offset_gsi(index_gsi+1),   &
                           field3,hor_size*dims(2),mpi_real4,       &
                          mpi_status_ignore, ierr)
         if (ierr /= 0) then
           print*,"Error reading ", VarName," using MPIIO"
         else
           DO k=1,dims(2)
             if(mype==0) print*, trim(VarName)," max,min ", k, &
                     maxval(field3(:,k,:)),minval(field3(:,k,:))   
             full3d(:,:,k)=field3(:,k,:)
           ENDDO
         endif

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(3)/2,dims(2)/2)
         end if
         
         do k=1,dims(2)
            do j=1,dims(3)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*(mu(i,j)+mub(i,j))/msfm(i,j)
               end do
            end do
         end do

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'After  couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(3)/2,dims(2)/2)
         end if
      case ('T', 'PH') ;
         var_pref=trim(var3d(n))

         hor_size=real(dims(1)*dims(3))
         CALL mpi_file_read_at(iunit_gsi,file_offset_gsi(index_gsi+1),   &       
                    field3,hor_size*dims(2),mpi_real4,       &
                          mpi_status_ignore, ierr)
         if (ierr /= 0) then
           print*,"Error reading ", VarName," using MPIIO"
         else
           DO k=1,dims(2)
             if(mype==0) print*, trim(VarName)," max,min ", k, &
                     maxval(field3(:,k,:)),minval(field3(:,k,:))
             full3d(:,:,k)=field3(:,k,:)
           ENDDO
         endif

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(3)/2,dims(2)/2)
         end if

         do k=1,dims(2)
            do j=1,dims(3)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*(mu(i,j)+mub(i,j))
               end do
            end do
         end do

            if (debug) then
               write(unit=stdout, fmt='(3a,e20.12,4x)') &
                    'After  couple Sample ', trim(var3d(n)), &
                    '=', full3d(dims(1)/2,dims(3)/2,dims(2)/2)
            end if
      case ('QVAPOR', 'QCLOUD', 'QRAIN', 'QICE', 'QSNOW', 'QGRAUP') ;
         ! var_pref='R' // var3d(n)(1:2)
         ! var_pref=var3d(n)(1:2)
         var_pref=var3d(n)

         hor_size=real(dims(1)*dims(3))
         CALL mpi_file_read_at(iunit_gsi,file_offset_gsi(index_gsi+1),   &
                    field3,hor_size*dims(2),mpi_real4,       &
                          mpi_status_ignore, ierr)
         if (ierr /= 0) then
           print*,"Error reading ", VarName," using MPIIO"
         else
           DO k=1,dims(2)
             if(mype==0) print*, trim(VarName)," max,min ", k, &
                     maxval(field3(:,k,:)),minval(field3(:,k,:))
             full3d(:,:,k)=field3(:,k,:)
           ENDDO
         endif

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(3)/2,dims(2)/2)
         end if

         do k=1,dims(2)
            do j=1,dims(3)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*(mu(i,j)+mub(i,j))
               end do
            end do
         end do

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'After  couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(3)/2,dims(2)/2)
         end if
      case default ;
         write(unit=stdout,fmt=*) 'It is impossible here. var3d(', n, ')=', trim(var3d(n))
      end select

      deallocate(field3)

      do m=1,4
         var_name=trim(var_pref) // trim(bdyname(m))
         vbt_name=trim(var_pref) // trim(tenname(m))

         write(unit=stdout, fmt='(a, i3, 2a)') &
            'Processing: bdyname(', m, ')=', trim(var_name)

         call retrieve_index_bdy(index_bdy,var_name,varname_all_bdy,   &
                            thisbdytime(1),datestr_all_bdy,nrecs_bdy)
         if(mype==0) write(*,*) '3d=',trim(var_name),index_bdy
         if(index_bdy < 0) then 
             print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, index_bdy, trim(var_name), " not existed"
             stop  1234    
         endif 
        
         dims(1:3)=domainend_all_bdy(:,index_bdy)
         dims(4)=1            

         allocate(field3(dims(1), dims(2), dims(3)))
         allocate(frst3d(dims(1), dims(2), dims(3)))
         allocate(scnd3d(dims(1), dims(2), dims(3)))
         allocate(tend3d(dims(1), dims(2), dims(3)))
         hor_size=dims(1)*dims(3)

         ! Get variable at second time level
         if (time_level > 1) then
           call retrieve_index_bdy(index_bdy,var_name,varname_all_bdy,   &
                            nextbdytime(1),datestr_all_bdy,nrecs_bdy)
           if(index_bdy < 0) then
              print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, index_bdy, trim(var_name), " not existed"
              stop
           endif
           CALL mpi_file_read_at(iunit_bdy,file_offset_bdy(index_bdy+1),     &
                          field3,hor_size*dims(2),mpi_real4,       &
                          mpi_status_ignore, ierr)
           if (ierr /= 0) then
              print*,"Error reading ", var_name," using MPIIO"
           else
              scnd3d=field3
              DO k=1,dims(2)
                 if(mype==0) print*, trim(VarName)," max,min ", k,   &
                    maxval(field3(:,k,:)),minval(field3(:,k,:))
              enddo
           endif

         else
           call retrieve_index_bdy(index_bdy,var_name,varname_all_bdy,   &
                         thisbdytime(1),datestr_all_bdy,nrecs_bdy)
           if(index_bdy < 0) then 
              print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, index_bdy, trim(var_name), " not existed"
              stop
           endif
           CALL mpi_file_read_at(iunit_bdy,file_offset_bdy(index_bdy+1),     &
                          field3,hor_size*dims(2),mpi_real4,       &
                          mpi_status_ignore, ierr)
           if (ierr /= 0) then
              print*,"Error reading ", var_name," using MPIIO"
           else
              frst3d=field3
              DO k=1,dims(2)
                 if(mype==0) print*, trim(var_name)," max,min ", k,   &
                    maxval(field3(:,k,:)),minval(field3(:,k,:))
              enddo 
           endif
!            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(var_name), frst3d, &
!                                      dims(1), dims(2), dims(3), 1, debug)
           call retrieve_index_bdy(index_bdy,vbt_name,varname_all_bdy,   &       
                         thisbdytime(1),datestr_all_bdy,nrecs_bdy)
           if(index_bdy < 0) then 
              print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, index_bdy, trim(vbt_name), " not existed"
              stop
           endif
           CALL mpi_file_read_at(iunit_bdy,file_offset_bdy(index_bdy+1),     &
                          field3,hor_size*dims(2),mpi_real4,       &
                          mpi_status_ignore, ierr)
           if (ierr /= 0) then
              print*,"Error reading ", var_name," using MPIIO"
           else
              tend3d=field3
              DO k=1,dims(2)
                 if(mype==0) print*, trim(vbt_name)," max,min ", k,   &
                    maxval(field3(:,k,:)),minval(field3(:,k,:))
              enddo
           endif
!            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(vbt_name), tend3d, &
!                                      dims(1), dims(2), dims(3), 1, debug)
         end if

         if (debug) then
            write(unit=ori_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
                 'No.', m, 'Variable: ', trim(vbt_name), &
                 'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

           call retrieve_index_bdy(index_bdy,vbt_name,varname_all_bdy,   &
                         thisbdytime(1),datestr_all_bdy,nrecs_bdy)
           if(index_bdy < 0) then
              print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, index_bdy, trim(vbt_name), " not existed"
              stop
           endif
           CALL mpi_file_read_at(iunit_bdy,file_offset_bdy(index_bdy+1),     &
                          field3,hor_size*dims(2),mpi_real4,       &
                          mpi_status_ignore, ierr)
           if (ierr /= 0) then
              print*,"Error reading ", var_name," using MPIIO"
           else
              tend3d=field3
              DO k=1,dims(2)
                 if(mype==0) print*, trim(vbt_name)," max,min ", k,   &
                    maxval(field3(:,k,:)),minval(field3(:,k,:))
              enddo
           endif

            write(unit=ori_unit, fmt='(a, 10i12)') &
                 ' old ', (i, i=1,dims(3))
            do j=1,dims(1)
               write(unit=ori_unit, fmt='(i4, 1x, 10e20.7)') &
                     j, (tend3d(j,dims(2)/2,i), i=1,dims(3))
            end do
         end if

         select case(trim(bdyname(m)))
         case ('_BXS') ;             ! West boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2) &
               scnd3d(j,k,l)=frst3d(j,k,l)+tend3d(j,k,l)*bdyfrq
               frst3d(j,k,l)=full3d(l,j,k)
            end do
            end do
            end do

         case ('_BXE') ;             ! East boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2) &
               scnd3d(j,k,l)=frst3d(j,k,l)+tend3d(j,k,l)*bdyfrq
               frst3d(j,k,l)=full3d(east_end-l,j,k)
            end do
            end do
            end do
         case ('_BYS') ;             ! South boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2) &
               scnd3d(i,k,l)=frst3d(i,k,l)+tend3d(i,k,l)*bdyfrq
               frst3d(i,k,l)=full3d(i,l,k)
            end do
            end do
            end do
         case ('_BYE') ;             ! North boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2) &
               scnd3d(i,k,l)=frst3d(i,k,l)+tend3d(i,k,l)*bdyfrq
               frst3d(i,k,l)=full3d(i,north_end-l,k)
            end do
            end do
            end do
         case default ;
            write(unit=stdout,fmt=*) 'It is impossible here.'
            write(unit=stdout,fmt=*) 'bdyname(', m, ')=', trim(bdyname(m))
            stop
         end select


         write(unit=stdout, fmt='(a, i3, 2a)') &
            'cal. tend: bdyname(', m, ')=', trim(vbt_name)

         ! calculate new tendancy 
         do l=1,dims(3)
            do k=1,dims(2)
               do i=1,dims(1)
!tgs bdyfrqini - time interval between analysis time and second time in wrfbdy_d01
                  tend3d(i,k,l)=(scnd3d(i,k,l)-frst3d(i,k,l))/bdyfrqini
!tgs                  tend3d(i,k,l)=(scnd3d(i,k,l)-frst3d(i,k,l))/bdyfrq
               end do
            end do
         end do

         if (debug) then
            write(unit=new_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
                 'No.', m, 'Variable: ', trim(vbt_name), &
                 'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

            write(unit=new_unit, fmt='(a, 10i12)') &
                 ' new ', (i, i=1,dims(3))

            do j=1,dims(1)
               write(unit=new_unit, fmt='(i4, 1x, 10e20.7)') &
                     j, (tend3d(j,dims(2)/2,i), i=1,dims(3))
            end do
         end if

         ! output new variable at first time level
         call retrieve_index_bdy(index_bdy,var_name,varname_all_bdy,   &
                            thisbdytime(1),datestr_all_bdy,nrecs_bdy)
         if(index_bdy < 0) then
            print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, index_bdy, trim(var_name), " not existed"
           stop
         endif
         field3=frst3d
         CALL mpi_file_write_at(iunit_bdy,file_offset_bdy(index_bdy+1),     &
                          field3,hor_size*dims(2),mpi_real4,       &
                          mpi_status_ignore, ierr)
         if (ierr /= 0) then
             print*,"Error write ", var_name," using MPIIO"
         else
            if(mype==0) print*,"write out ", var_name," using MPIIO"
            DO k=1,dims(2)
               if(mype==0) print*, " max,min ", k,   &
                  maxval(field3(:,k,:)),minval(field3(:,k,:))
            enddo
         endif
!         call da_put_var_3d_real_cdf( wrf_bdy_file, trim(var_name), frst3d, &
!                                dims(1), dims(2), dims(3), 1, debug)
         call retrieve_index_bdy(index_bdy,vbt_name,varname_all_bdy,   &
                            thisbdytime(1),datestr_all_bdy,nrecs_bdy)
         if(index_bdy < 0) then
            print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, index_bdy, trim(vbt_name), " not existed"
           stop
         endif
         field3=tend3d
         CALL mpi_file_write_at(iunit_bdy,file_offset_bdy(index_bdy+1),     &
                          field3,hor_size*dims(2),mpi_real4,       &
                          mpi_status_ignore, ierr)
         if (ierr /= 0) then
             print*,"Error write ", vbt_name," using MPIIO"
         else
            if(mype==0) print*,"write out ", vbt_name," using MPIIO"
            DO k=1,dims(2)
               if(mype==0) print*, " max,min ", k,   &
                  maxval(field3(:,k,:)),minval(field3(:,k,:))
            enddo
         endif
!         call da_put_var_3d_real_cdf( wrf_bdy_file, trim(vbt_name), tend3d, &
!                                   dims(1), dims(2), dims(3), 1, debug)

         deallocate(field3)
         deallocate(frst3d)
         deallocate(scnd3d)
         deallocate(tend3d)
      end do

      deallocate(full3d)
   end do

   deallocate(mu)
   deallocate(mub)
   deallocate(msfm)
   deallocate(msfu)
   deallocate(msfv)
   deallocate(u)
   deallocate(v)

  end if



  call mpi_file_close(iunit_gsi,ierror)
  call mpi_file_close(iunit_bdy,ierror)
  call mpi_file_close(iunit_wrfinput,ierror)

  call MPI_FINALIZE(ierror)

 write(unit=stdout,fmt=*) &
    '=================================================================='
 if ( update_lateral_bdy ) then
    write(unit=stdout,fmt=*) 'Lateral boundary tendency updated.'
 end if
 if ( update_low_bdy ) then
    write(unit=stdout,fmt=*) 'Low boundary updated with wrf_input fields.'
 end if
 if ( update_lsm ) then
    write(unit=stdout,fmt=*) 'LSM variables updated with wrf_input fields.'
 end if

   if (io_status == 0) &
      write (unit=stdout,fmt=*) "*** Update_bc completed successfully ***"

end program da_update_bc_bin

