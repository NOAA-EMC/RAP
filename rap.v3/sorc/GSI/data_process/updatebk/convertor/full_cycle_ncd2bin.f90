subroutine full_cycle_ncd2bin(VarName,dh,DateStr,iunit,nrecs, &
           varname_all,memoryorder_all,domainend_all,file_offset)

  use mpi
  use kinds, only: r_single,i_llong,i_kind

  IMPLICIT NONE
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

! for binary file
  integer(i_kind),intent(in) :: iunit
  integer(i_kind),intent(in) :: nrecs
  integer(i_llong),intent(in):: file_offset(nrecs)
  character(132), intent(in) :: varname_all(nrecs),memoryorder_all(nrecs)
  integer(i_kind),intent(in) :: domainend_all(3,nrecs)
  integer :: index
!
  CHARACTER (LEN=19),intent(in) :: VarName
!
  integer(i_kind), intent(in)   :: dh
  character(len=19),intent(in)  :: DateStr

! rmse stuff
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index
  character (len= 4) :: staggering
  character (len= 3) :: ordering

  character (len=80), dimension(3)  ::  dimnames
  integer(i_kind) wrf_real
!
!
  integer(i_kind)  :: nlon_regional,nlat_regional,nsig_regional

  integer :: i,j,k,n,hor_size
  INTEGER :: istatus,iret,ierr
  real(r_single),allocatable:: field3(:,:,:)
  real(r_single),allocatable:: field3bin(:,:,:)

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
!
!   cycle one field
!
  write(6,*)
  write(6,*) ' ================== '
  write(6,*) ' Update variable ', trim(VarName)
  write(6,*) ' ================== '

  wrf_real=104_i_kind
!-------------  get grid info
  end_index=0
  call ext_ncd_get_var_info (dh,trim(VarName),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' <<<<<<<<<<<<<<   Read in data from dh1  = ',dh
  write(6,*)' rmse_var=',trim(VarName)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  write(6,*)'ierr  = ',ierr   !DEDE
  nlon_regional=end_index(1)
  nlat_regional=end_index(2)
  nsig_regional=end_index(3)
  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional

  if( ndim1 == 2 ) then
      allocate(field3(nlon_regional,nlat_regional,1))
  elseif( ndim1 == 3 ) then
      allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  else
      write(6,*) 'update_netcdf_mass: Wrong dimension '
      stop 123
  endif

  call ext_ncd_read_field(dh,DateStr,TRIM(VarName),         &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )

  if(ndim1 == 2) end_index(3)=1
  DO k=1,end_index(3)
    write(6,*)' max,min =',maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
!
!
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif

  if(trim(memoryorder_all(index))=='XZY') then
     if( (nlon_regional==domainend_all(1,index)) .and.  &
         (nlat_regional==domainend_all(3,index)) .and.  &
         (nsig_regional==domainend_all(2,index)) ) then
           allocate(field3bin(nlon_regional,nsig_regional,nlat_regional))
           DO k=1,nsig_regional
              field3bin(:,k,:)=field3(:,:,k)
           ENDDO
     else
          write(*,*) 'Dimension mismatch'
          stop 1234
     endif
  else if(trim(memoryorder_all(index))=='XYZ') then 
     if( (nlon_regional==domainend_all(1,index)) .and.  &
         (nlat_regional==domainend_all(2,index)) .and.  &
         (nsig_regional==domainend_all(3,index)) ) then
           allocate(field3bin(nlon_regional,nlat_regional,nsig_regional))
           field3bin=field3
     else
          write(*,*) 'Dimension mismatch'
          stop 1234
     endif
  else if(trim(memoryorder_all(index))=='XY') then
     if( (nlon_regional==domainend_all(1,index)) .and.  &
         (nlat_regional==domainend_all(2,index))) then 
           allocate(field3bin(nlon_regional,nlat_regional,1))
           field3bin=field3
           nsig_regional=1
     else
          write(*,*) 'Dimension mismatch'
          stop 1234
     endif
  else
     write(6,*) ' No such memory order ',trim(memoryorder_all(index))
     stop 123
  end if

  hor_size=nlon_regional*nlat_regional

  call mpi_file_write_at(iunit,file_offset(index+1),field3bin,  &
            hor_size*nsig_regional,mpi_real4,mpi_status_ignore, ierr)
  if (ierr /= 0) then
     print*,"Error writing ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: write out ',VarName
  end if

  deallocate(field3)
  deallocate(field3bin)

end subroutine full_cycle_ncd2bin

