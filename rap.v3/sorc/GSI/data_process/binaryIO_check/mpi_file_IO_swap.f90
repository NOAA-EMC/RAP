!
subroutine mpi_file_read_at_swap4(FH, OFFSET, BUF, &
              NCOUNT,DATATYPE,status,IERROR)
!
!
  use mpimod, only: mpi_byte,mpi_integer1,mpi_real4,mpi_comm_world,npe, &
       mpi_offset_kind,mpi_info_null,mpi_mode_rdwr,mpi_status_size
!  use mpi
  use kinds, only: r_single,i_llong,i_kind,i_byte
!
  implicit none

  INTEGER, intent(in) ::  FH
  INTEGER(i_llong), intent(in) ::  OFFSET
  INTEGER, intent(in) ::  NCOUNT
  INTEGER(i_byte),  intent(out) :: BUF(4*NCOUNT)
  INTEGER, intent(in) ::  DATATYPE
  INTEGER, intent(out) :: STATUS
  INTEGER, intent(out) :: IERROR

  INTEGER(i_byte) :: BUF1(NCOUNT*4)

  INTEGER(i_kind) :: i,n 

  CALL mpi_file_read_at(FH,OFFSET,buf1,4*ncount,mpi_integer1, & 
                        STATUS,IERROR)
  if (IERROR /= 0) then
     return
  else
     do i=1,ncount
       do n=1,4
          buf((i-1)*4 + n)=buf1((i-1)*4+4-n+1)
       enddo
     enddo
  endif

end subroutine mpi_file_read_at_swap4
!
subroutine mpi_file_write_at_swap4(FH, OFFSET, BUF, &
              NCOUNT,DATATYPE,status,IERROR)
!
!
  use mpimod, only: mpi_byte,mpi_integer1,mpi_real4,mpi_comm_world,npe, &
       mpi_offset_kind,mpi_info_null,mpi_mode_rdwr,mpi_status_size
  use kinds, only: r_single,i_llong,i_kind,i_byte
!
  implicit none

  INTEGER, intent(in) ::  FH
  INTEGER(i_llong), intent(in) ::  OFFSET
  INTEGER, intent(in) ::  NCOUNT
  INTEGER(i_byte),  intent(in) :: BUF(4*NCOUNT)
  INTEGER, intent(in) ::  DATATYPE
  INTEGER, intent(out) :: STATUS
  INTEGER, intent(out) :: IERROR

  INTEGER(i_byte) :: BUF1(NCOUNT*4)

  INTEGER(i_kind) :: i,n 

  do i=1,ncount
    do n=1,4
        buf1((i-1)*4+4-n+1)=buf((i-1)*4 + n)
    enddo
  enddo

  CALL mpi_file_write_at(FH,OFFSET,buf1,4*ncount,mpi_integer1, & 
                        STATUS,IERROR)

end subroutine mpi_file_write_at_swap4
