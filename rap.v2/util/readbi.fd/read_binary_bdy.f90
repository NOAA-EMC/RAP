program read_binary_mass
!$$$  documentation block
!                .      .    .                                       .
!
! program history log:
!


  use mpi
  use kinds, only: r_single,i_llong,i_kind

  IMPLICIT NONE

!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

! for background file
  integer(i_kind),allocatable:: start_block(:),end_block(:)
  integer(i_kind),allocatable:: start_byte(:),end_byte(:)
  integer(i_llong),allocatable:: file_offset(:)
  character(132),allocatable:: datestr_all(:),varname_all(:),memoryorder_all(:)
  integer(i_kind),allocatable:: domainend_all(:,:)
  integer(i_kind) nrecs
  CHARACTER(9)  :: filename
  integer(i_kind) :: iunit

! for background file
  integer(i_kind),allocatable:: start_block_sfc(:),end_block_sfc(:)
  integer(i_kind),allocatable:: start_byte_sfc(:),end_byte_sfc(:)
  integer(i_llong),allocatable:: file_offset_sfc(:)
  character(132),allocatable:: datestr_all_sfc(:),varname_all_sfc(:),memoryorder_all_sfc(:)
  integer(i_kind),allocatable:: domainend_all_sfc(:,:)
  integer(i_kind) nrecs_sfc
  CHARACTER(9)  :: filename_sfc
  integer(i_kind) :: iunit_sfc
!
  integer(i_kind) status_hdr
  integer(i_kind) hdrbuf(512)

  CHARACTER (LEN=19)  :: VarName
!
!
  INTEGER :: istatus,iret,ierr
  INTEGER :: n
  INTEGER :: iyear,imn,iday,ihrst,imin
  integer(i_kind) :: iw3jdn,JDATE(8),IDATE(8)
  real(r_single) :: rinc(5), timediff
  logical :: ifswap

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)
  
  ifswap=.true.

!!! MPI IO

! open and check background file
  iunit=33
  fileName='wrfbdy_d1'
  open(iunit,file=trim(fileName),form='unformatted')
! Check for valid input file
  read(iunit,iostat=status_hdr)hdrbuf
  if(status_hdr /= 0) then
     write(6,*)'full_cycle_surface_binary_mass:  problem with = ',&
          trim(fileName),', Status = ',status_hdr
     call stop2(74)
  endif
  close(iunit)

!
!  inventory both files
!
! first background file
  call count_recs_wrf_binary_file(iunit, ifswap,trim(fileName), nrecs)
  write(*,*) 'number of records in ',trim(fileName), '=', nrecs

  allocate(datestr_all(nrecs),varname_all(nrecs),domainend_all(3,nrecs))
  allocate(memoryorder_all(nrecs))
  allocate(start_block(nrecs),end_block(nrecs))
  allocate(start_byte(nrecs),end_byte(nrecs),file_offset(nrecs))

  call inventory_wrf_binary_file(iunit, ifswap,trim(filename), nrecs,  &
                      datestr_all,varname_all,memoryorder_all,domainend_all,   &
                      start_block,end_block,start_byte,end_byte,file_offset)

!  do N=1,NRECS
!     write(*,'(i4,2x,a30,a5,3i5)') N, trim(varname_all(N)),trim(memoryorder_all(n)),domainend_all(:,n)  
!  enddo

  read(datestr_all(100),15)iyear,imn,iday,ihrst,imin
  write(*,*) 'Current date and time for background = ',iyear,imn,iday,ihrst,imin
15   format(i4,1x,i2,1x,i2,1x,i2,1x,i2)

  IDATE=0
  IDATE(1)=iyear
  IDATE(2)=imn
  IDATE(3)=iday
  IDATE(5)=ihrst
  IDATE(6)=imin

  close(iunit)

!
!   cycle the surface fields
!
  call mpi_file_open(mpi_comm_world, trim(filename),     &
                     mpi_mode_rdwr,mpi_info_null, iunit, ierr)
  if (ierr /= 0) then
      call wrf_error_fatal("Error opening file with mpi io")
  end if

!  ------ read SMOIS
!  VarName='SMOIS'
!  call mpi_barrier(mpi_comm_world,ierror)
!  call read_varaible_binary(VarName,ifswap,iunit,nrecs, &
!           varname_all,memoryorder_all,domainend_all,file_offset)

!  ------ read 
  VarName='QCLOUD'
  call mpi_barrier(mpi_comm_world,ierror)
  call read_varaible_binary(VarName,ifswap,iunit,nrecs, &
           varname_all,memoryorder_all,domainend_all,file_offset)

  call mpi_file_close(iunit,ierror)

  call MPI_FINALIZE(ierror)

end program read_binary_mass

