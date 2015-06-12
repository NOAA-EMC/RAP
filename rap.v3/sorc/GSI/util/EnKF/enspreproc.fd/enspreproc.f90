!<------------------------------------------------------------------>
!< --- next few lines under version control, D O  N O T  E D I T --->
! $Date: 2015-03-30 17:55:07 +0000 (Mon, 30 Mar 2015) $
! $Revision: 53690 $
! $Author: rahul.mahajan@noaa.gov $
! $Id: enspreproc.f90 53690 2015-03-30 17:55:07Z rahul.mahajan@noaa.gov $
!<------------------------------------------------------------------>

!$$$  main program documentation block
!
! program:  enspreproc - preprocess the spectral ensemble into gridded ensemble 
!                        so it is faster to read within GSI
!
! prgmmr: mahajan          org: emc/ncep               date: 2015-02-20
!
! abstract:  preprocess the spectral ensemble members into gridded ensemble
!            so they are faster to read within GSI.
!
!$$$
  
program enspreproc

   use mpi, only: mpi_status_ignore
   use kinds, only: r_kind
   use mpimod, only: mpi_comm_world,npe,mype,ierror
   use mpimod, only: mpi_rtype,mpi_info_null,mpi_offset_kind,mpi_mode_create,mpi_mode_wronly
   use gridmod, only: nsig,regional
   use constants, only: init_constants,init_constants_derived 
   use sigio_module, only: sigio_srohdc,sigio_head,sigio_dbta,sigio_axdbta
   use general_specmod, only: spec_cut,general_init_spec_vars
   use general_sub2grid_mod, only: general_sub2grid_create_info,general_sub2grid
   use hybrid_ensemble_parameters, only: n_ens,jcap_ens,nlat_ens,nlon_ens,grd_ens,sp_ens,uv_hyb_ens
   use sigio_module, only: sigio_srhead

   implicit none

   type(sigio_head) :: sighead
   type(sigio_dbta) :: sigdata
   integer, parameter :: iunit=10
   integer :: inner_vars,num_fields
   integer :: lunit,count
   integer(mpi_offset_kind) :: disp
   integer :: i_ens,im,jm,km,i,j,k
   real(r_kind), allocatable, dimension(:,:)     :: ps,z
   real(r_kind), allocatable, dimension(:,:,:)   :: u,v,vor,div,tv,q,cwmr,oz
   real(r_kind), allocatable, dimension(:,:,:,:) :: work_sub,work_grd
   character(len=500) :: filenamein,filenameout
   character(len=10)  :: datestring
   character(len=3)   :: charfhr
   character(len=7)   :: charmem
   logical, dimension(:), allocatable :: vector
   logical,parameter :: vordivflag=.false.,zflag=.true.
   integer :: lunges

   ! mype is process number, npe is total number of processes.
   call mpi_init(ierror)
   call mpi_comm_rank(mpi_comm_world,mype,ierror)
   call mpi_comm_size(mpi_comm_world,npe,ierror)

   ! get datestring,n_ens,charfhr from command line on every task.
   call getarg(1,datestring)
   call getarg(2,charfhr)
   call getarg(3,charmem)
   read(charmem,'(i4)') n_ens

   if ( mype == 0 ) then
      write(6,'(a)'  ) 'Command line input:'
      write(6,'(a,a)') ' datestring    = ',trim(adjustl(datestring))
      write(6,'(a,a)') ' forecast hour = ',trim(adjustl(charfhr))
      write(6,'(a,a)') ' n_ens         = ',trim(adjustl(charmem))
   endif
      
   ! Get header and other information on all tasks from ensemble member 1
   i_ens = 1
   lunges=15
   write(charmem,'("_mem",i3.3)') i_ens
   filenamein = "sfg_" // trim(adjustl(datestring)) // "_fhr" // trim(adjustl(charfhr)) // trim(adjustl(charmem))
   write(*,*) trim(filenamein)
!mhu   call sigio_srohdc(iunit,trim(adjustl(filenamein)),sighead,sigdata,ierror)
   open(lunges,file=trim(filenamein),form='unformatted')
   call sigio_srhead(lunges,sighead,ierror)

   if (ierror /= 0) then
      write(6,*) '***ERROR***  Reading ',trim(adjustl(filenamein)),' ierror=',ierror,' aborting!'
      go to 999
   endif

   jcap_ens = sighead%jcap
   nlon_ens = sighead%lonf
   nlat_ens = sighead%latf + 2
   nsig     = sighead%levs

   if (mype == 0) then
      write(6,'(a,a)' ) 'Read header information from ',trim(adjustl(filenamein))
      write(6,'(a,i4)') ' jcap_ens = ',jcap_ens
      write(6,'(a,i4)') ' nlon_ens = ',nlon_ens
      write(6,'(a,i4)') ' nlat_ens = ',nlat_ens
      write(6,'(a,i4)') ' nsig     = ',nsig
   endif


   ! Create a grd_ens sub2grid_info structure
   inner_vars = 1
   num_fields = 6*nsig+2 ! 6(2) 3(2)-d variables
   regional   = .false.
   allocate(vector(num_fields))
   vector           = .false.
   vector(1:nsig*2) = .true.
   grd_ens%npe  = npe
   grd_ens%mype = mype
   call general_sub2grid_create_info(grd_ens,inner_vars,nlat_ens,nlon_ens,nsig,num_fields,regional,vector)
   deallocate(vector)

   ! print some info about the grd_ens data structure
!   if ( mype <= 10 .or. mype >= npe-10 ) &
!   print*,mype,grd_ens%kbegin_loc,grd_ens%kend_alloc,grd_ens%nlevs_loc,grd_ens%nlevs_alloc

   km = grd_ens%nsig
   jm = grd_ens%lon2
   im = grd_ens%lat2

   ! Initialize spectral variables
   spec_cut = jcap_ens + 1
   call init_constants(regional)
   call init_constants_derived()
   call general_init_spec_vars(sp_ens,jcap_ens,jcap_ens,grd_ens%nlat,grd_ens%nlon)

   uv_hyb_ens = .true.

   allocate(  ps(im,jm   ))
   allocate(   z(im,jm   ))
   allocate( vor(im,jm,km))
   allocate( div(im,jm,km))
   allocate(   u(im,jm,km))
   allocate(   v(im,jm,km))
   allocate(  tv(im,jm,km))
   allocate(   q(im,jm,km))
   allocate(  oz(im,jm,km))
   allocate(cwmr(im,jm,km))

   allocate(work_sub(grd_ens%inner_vars,im,jm,grd_ens%num_fields))
   allocate(work_grd(grd_ens%inner_vars,grd_ens%nlat,grd_ens%nlon,grd_ens%kbegin_loc:grd_ens%kend_alloc))
   
   do i_ens = 1,n_ens

      write(charmem,'("_mem",i3.3)') i_ens 

      filenamein = "sfg_" // trim(adjustl(datestring)) // "_fhr" // trim(adjustl(charfhr)) // trim(adjustl(charmem))

      call general_read_gfsatm(grd_ens,sp_ens,sp_ens,filenamein,mype,uv_hyb_ens,vordivflag,zflag, &
           z,ps,vor,div,u,v,tv,q,cwmr,oz,ierror)
      if ( ierror /= 0 ) then
         if ( mype == 0 ) &
            write(6,'(a,a,a,i5,a)') '***ERROR***  Reading ',trim(adjustl(filenamein)),' ierror = ',ierror,' aborting!'
         goto 999
      endif

      do k = 1,km ; do j = 1,jm ; do i = 1,im
         work_sub(1,i,j,k+0*km) =    u(i,j,k)
         work_sub(1,i,j,k+1*km) =    v(i,j,k)
         work_sub(1,i,j,k+2*km) =   tv(i,j,k)
         work_sub(1,i,j,k+3*km) =    q(i,j,k)
         work_sub(1,i,j,k+4*km) =   oz(i,j,k)
         work_sub(1,i,j,k+5*km) = cwmr(i,j,k)
      enddo ; enddo ; enddo

      do j = 1,jm ; do i = 1,im
         work_sub(1,i,j,grd_ens%num_fields-1) = ps(i,j)
         work_sub(1,i,j,grd_ens%num_fields)   =  z(i,j)
      enddo ; enddo

      call general_sub2grid(grd_ens,work_sub,work_grd)

      filenameout="enspreproc_f" // trim(adjustl(charfhr)) // trim(adjustl(charmem))

      call mpi_file_open(mpi_comm_world,trim(adjustl(filenameout)), &
                         mpi_mode_wronly+mpi_mode_create, &
                         mpi_info_null,lunit,ierror)
      if ( ierror /= 0 ) then
         write(6,'(a,i5,a,i5,a)') '***ERROR***  MPI_FILE_OPEN failed on task = ', mype ,' ierror = ',ierror,' aborting!'
         goto 999
      endif

      disp = grd_ens%nlat * grd_ens%nlon * (grd_ens%kbegin_loc-1) * r_kind

      call mpi_file_set_view(lunit,disp,mpi_rtype,mpi_rtype,'native',mpi_info_null,ierror)
      if ( ierror /= 0 ) then
         write(6,'(a,i5,a,i5,a)') '***ERROR***  MPI_FILE_SET_VIEW failed on task = ', mype ,' ierror = ',ierror,' aborting!'
         goto 999
      endif

      count = grd_ens%nlat * grd_ens%nlon * grd_ens%nlevs_alloc

      call mpi_file_write(lunit,work_grd,count,mpi_rtype,mpi_status_ignore,ierror)
      if ( ierror /= 0 ) then
         write(6,'(a,i5,a,i5,a)') '***ERROR***  MPI_FILE_WRITE failed on task = ', mype ,' ierror = ',ierror,' aborting!'
         goto 999
      endif

      call mpi_file_close(lunit,ierror)
      if ( ierror /= 0 ) then
         write(6,'(a,i5,a,i5,a)') '***ERROR***  MPI_FILE_CLOSE failed on task = ', mype ,' ierror = ',ierror,' aborting!'
         goto 999
      endif

   enddo ! do i_ens = 1,n_ens

   deallocate(ps,z,vor,div,u,v,tv,q,oz,cwmr)
   deallocate(work_sub,work_grd)

999 continue

   call mpi_barrier(mpi_comm_world,ierror)
   call mpi_finalize(ierror)

   stop

end program enspreproc
