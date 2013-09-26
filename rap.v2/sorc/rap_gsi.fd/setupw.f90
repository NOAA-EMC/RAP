!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  setupw --- Compute rhs of oi for wind component obs
!
! !INTERFACE:
!

subroutine setupw(lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)

! !USES:

  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,r_double,i_kind
  use obsmod, only: wtail,whead,rmiss_single,perturb_obs,oberror_tune,&
       i_w_ob_type,obsdiags,obsptr,lobsdiagsave,nobskeep,lobsdiag_allocated,&
       time_offset,ext_sonde,bmiss
  use obsmod, only: w_ob_type
  use obsmod, only: obs_diag
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use qcmod, only: npres_print,ptop,pbot,dfact,dfact1
  use oneobmod, only: oneobtest,oneob_type,magoberr,maginnov 
  use gridmod, only: get_ijk,nsig,twodvar_regional,regional,rotate_wind_xy2ll
  use guess_grids, only: nfldsig,hrdifsig,geop_hgtl,sfcmod_gfs
  use guess_grids, only: ges_u,ges_v,tropprs,ges_ps,ges_z,sfcmod_mm5
  use guess_grids, only: ges_tv,ges_lnprsl,comp_fact10,pt_ll,pbl_height
  use constants, only: zero,half,one,tiny_r_kind,two,cg_term, &
           three,rd,grav,four,five,huge_single,r1000,wgtlim,r10,r400
  use constants, only: grav_ratio,flattening,deg2rad, &
       grav_equator,somigliana,semi_major_axis,eccentricity
  use jfunc, only: jiter,last,jiterstart,miter
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use converr, only: ptabl
  use rapidrefresh_cldsurf_mod, only: l_PBL_pseudo_SurfobsUV, pblH_ration,pps_press_incr

  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  implicit none
  
! !INPUT PARAMETERS:

   integer(i_kind)                                  ,intent(in   ) :: lunin ! unit from which to read observations
   integer(i_kind)                                  ,intent(in   ) :: mype  ! mpi task id
   integer(i_kind)                                  ,intent(in   ) :: nele  ! number of data elements per observation
   integer(i_kind)                                  ,intent(in   ) :: nobs  ! number of observations
   integer(i_kind)                                  ,intent(in   ) :: is    ! ndat index
   logical                                          ,intent(in   ) :: conv_diagsave ! logical to save innovation dignostics
   
! !INPUT/OUTPUT PARAMETERS:

   real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork ! obs-ges stats
   real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork ! data counts and gross checks

!
! !DESCRIPTION:  For wind component observations, this routine
!  \begin{enumerate}
!       \item reads obs assigned to given mpi task (geographic region),
!       \item simulates obs from guess,
!       \item apply some quality control to obs,
!       \item load weight and innovation arrays used in minimization
!       \item collects statistics for runtime diagnostic output
!       \item writes additional diagnostic information to output file
!  \end{enumerate}
!
! !REVISION HISTORY:
!
!   1990-10-06  parrish
!   1998-04-10  weiyu yang
!   1999-03-01  wu - ozone processing moved into setuprhs from setupoz
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-17  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue; added intent/only's
!   2004-10-06  parrish - increase size of vwork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-05-27  derber - level output change
!   2005-07-22  jung - add modis winds
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-10-21  su  - modified variational qc and diagnose output
!   2005-11-03  treadon - correct error in ilone,ilate data array indices
!   2005-11-22  wu - add option to perturb conventional obs
!   2005-11-29 derber - remove psfcg and use ges_lnps instead
!   2006-01-13 treadon - correct bugs in modis wind qc
!   2006-01-31  todling - storing wgt/wgtlim in diag file instead of wgt only
!   2006-02-02  treadon - rename lnprsl as ges_lnprsl
!   2006-02-08  treadon - correct vertical dimension (nsig) in call tintrp2a(ges_tv...)
!   2006-02-15  treadon - use height when processing type 223, 224, 229 winds
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-03-21  treadon - modify optional perturbation to observation
!   2006-04-03  derber  - fix bugs and move all surface data to height calculation
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - modify handling of multiple data at same location
!   2006-07-31  kleist - use ges_ps instead of ln(ps)
!   2006-08-28      su - fix a bug in variational qc
!   2006-11-30  jung/sienkiewicz - add type 259 for modis winds
!   2006-10-28      su - turn off rawinsonde Vqc at south hemisphere
!   2007-03-09  su     - modify observation pertabation for adjusting obs error
!   2007-03-19  tremolet - binning of observations
!   2007-03-27  li.bi - add qc for type 289 windsat winds
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-08-28  su     - modify observation gross check error 
!   2008-03-24  wu     - oberror tuning and perturb obs
!   2008-03-31  li.bi - add qc for type 290 ascat winds
!   2008-05-20  safford - rm unused vars
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2008-12-03  todling - changed handle of tail%time
!   2009-08-19  guo     - changed for multi-pass setup with dtime_check().
!   2009-02-06  pondeca - for each observation site, add the following to the
!                         diagnostic file: local terrain height, dominate surface
!                         type, station provider name, and station subprovider name
!   2010-11-25  su      - data items to hold quality mark for satellite wind 
!   2011-03-08  parrish - for regional=.true., convert wind components in rdiagbuf from grid relative
!                           to earth relative, using subroutine rotate_wind_xy2ll.
!   2011-05-05  su      - ome quality control for satellite satellite winds 
!   2012-01-10  hu      - add additional quality control for PBL profiler 223, 224, 227 
!   2011-12-14  wu      - add code for rawinsonde level enhancement ( ext_sonde )
!   2011-10-14  Hu      - add code for producing pseudo-obs in PBL 
!                                       layer based on surface obs UV
!
! REMARKS:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
! !AUTHOR: parrish          org: np22                date: 1990-10-06
!
!EOP
!-------------------------------------------------------------------------

! Declare local parameters
  real(r_kind),parameter:: r0_7=0.7_r_kind
  real(r_kind),parameter:: r6=6.0_r_kind
  real(r_kind),parameter:: r7=7.0_r_kind
  real(r_kind),parameter:: r15=15.0_r_kind
  real(r_kind),parameter:: r20=20.0_r_kind
  real(r_kind),parameter:: r50=50.0_r_kind
  real(r_kind),parameter:: r200=200.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: r0_1_bmiss=0.1_r_kind*bmiss

  character(len=*),parameter:: myname='setupw'

! Declare external calls for code analysis
  external:: tintrp2a
  external:: tintrp3
  external:: grdcrd
  external:: stop2

! Declare local variables

  real(r_double) rstation_id
  real(r_kind) qcu,qcv,qc_spd,qc_prs,trop5,tfact,fact
  real(r_kind) scale,ratio,obserror,obserrlm
  real(r_kind) residual,ressw,ress,val,val2,valqc2,dudiff,dvdiff
  real(r_kind) valqc,valu,valv,dx10,rlow,rhgh,drpx,prsfc
  real(r_kind) cg_w,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2,qcgross
  real(r_kind) presw,factw,dpres,ugesin,vgesin,rwgt,dpressave
  real(r_kind) sfcchk,prsln2,error,dtime,dlon,dlat,r0_001,rsig,thirty,rsigp
  real(r_kind) ratio_errors,goverrd,spdges,spdob,ten,psges,zsges
  real(r_kind) slat,sin2,termg,termr,termrg,pobl,uob,vob
  real(r_kind) uob_reg,vob_reg,uob_e,vob_e,dlon_e,uges_e,vges_e,dudiff_e,dvdiff_e
  real(r_kind) dz,zob,z1,z2,p1,p2,dz21,dlnp21,spdb,dstn
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final,skint,sfcr
  real(r_kind) dudiff_opp, dvdiff_opp, vecdiff, vecdiff_opp
  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nsig)::prsltmp,tges,zges
  real(r_kind) wdirob,wdirgesin,wdirdiffmax
  real(r_single),allocatable,dimension(:,:)::rdiagbuf

  real(r_kind) dpreso,dpk,uint,ugint,vint,vgint

  integer(i_kind) i,nchar,nreal,k,j,l,ii,itype
  integer(i_kind) jsig,mm1,iptrbu,iptrbv,jj,kk,iptrbu_sat,iptrbv_sat,icat
  integer(i_kind) k1,k2,ikxx,nn,isli,ibin,ioff
  integer(i_kind) ier,ilon,ilat,ipres,iuob,ivob,id,itime,ikx,ielev,iqc
  integer(i_kind) ihgt,ier2,iuse,ilate,ilone,istat,isatqc,iuob1,ivob1,isatang
  integer(i_kind) izz,iprvd,isprvd
  integer(i_kind) idomsfc,isfcr,iskint,iff10

  integer(i_kind) ku,kl,im
  integer(i_kind) cat,cato

  character(8) station_id,station_ido
  character(8),allocatable,dimension(:):: cdiagbuf
  character(8),allocatable,dimension(:):: cprvstg,csprvstg
  character(8) c_prvstg,c_sprvstg
  real(r_double) r_prvstg,r_sprvstg

  logical z_height,sfc_data
  logical,dimension(nobs):: luse,muse
  logical lowlevelsat

  logical:: in_curbin, in_anybin
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  type(w_ob_type),pointer :: my_head
  type(obs_diag),pointer :: my_diag
  real(r_kind) :: thisPBL_height,ratio_PBL_height,prest,prestsfc,dudiffsfc,dvdiffsfc

  equivalence(rstation_id,station_id)
  equivalence(r_prvstg,c_prvstg)
  equivalence(r_sprvstg,c_sprvstg)

  n_alloc(:)=0
  m_alloc(:)=0
  cato=0
  station_ido=''
!******************************************************************************
! Read and reformat observations in work arrays.
  spdb=zero

  read(lunin)data,luse

!    index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  ihgt=5      ! index of height
  iuob=6      ! index of u observation
  ivob=7      ! index of v observation
  id=8        ! index of station id
  itime=9     ! index of observation time in data array
  ikxx=10     ! index of ndex ob type in convinfo file
  ielev=11    ! index of station elevation
  iqc=12      ! index of quality mark
  ier2=13     ! index of original-original obs error ratio
  iuse=14     ! index of use parameter
  idomsfc=15  ! index of dominant surface type
  iskint=16   ! index of surface skin temperature
  iff10=17    ! index of 10 meter wind factor
  isfcr=18    ! index of surface roughness
  ilone=19    ! index of longitude (degrees)
  ilate=20    ! index of latitude (degrees)
  izz=21      ! index of surface height
  iprvd=22    ! index of observation provider
  isprvd=23   ! index of observation subprovider
  icat=24     ! index of data level category
  iptrbu=25   ! index of u perturbation
  iptrbv=26   ! index of v perturbation

  mm1=mype+1
  scale=one
  rsig=nsig
  thirty = 30.0_r_kind
  ten = 10.0_r_kind
  r0_001=0.001_r_kind
  rsigp=rsig+one
  goverrd=grav/rd

! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=0
     nchar=1
     nreal=23
     if (lobsdiagsave) nreal=nreal+7*miter+2
     if (twodvar_regional) then; nreal=nreal+2; allocate(cprvstg(nobs),csprvstg(nobs)); endif
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
  end if

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do

  dup=one
  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and.  &
           data(ipres,k)== data(ipres,l) .and. &
           data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
           muse(l) .and. muse(k))then

           tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
           dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
           dup(l)=dup(l)+one-tfact*tfact*(one-dfact)

        end if
     end do
  end do

  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        dlat=data(ilat,i)
        dlon=data(ilon,i)
        rstation_id     = data(id,i)
        error=data(ier2,i)
        ikx=nint(data(ikxx,i))
        isli = data(idomsfc,i)
     endif

!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

!    Link obs to diagnostics structure
     do jj=1,2
        if (.not.lobsdiag_allocated) then
           if (.not.associated(obsdiags(i_w_ob_type,ibin)%head)) then
              allocate(obsdiags(i_w_ob_type,ibin)%head,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupw: failure to allocate obsdiags',istat
                 call stop2(304)
              end if
              obsdiags(i_w_ob_type,ibin)%tail => obsdiags(i_w_ob_type,ibin)%head
           else
              allocate(obsdiags(i_w_ob_type,ibin)%tail%next,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupw: failure to allocate obsdiags',istat
                 call stop2(305)
              end if
              obsdiags(i_w_ob_type,ibin)%tail => obsdiags(i_w_ob_type,ibin)%tail%next
           end if
           allocate(obsdiags(i_w_ob_type,ibin)%tail%muse(miter+1))
           allocate(obsdiags(i_w_ob_type,ibin)%tail%nldepart(miter+1))
           allocate(obsdiags(i_w_ob_type,ibin)%tail%tldepart(miter))
           allocate(obsdiags(i_w_ob_type,ibin)%tail%obssen(miter))
           obsdiags(i_w_ob_type,ibin)%tail%indxglb=i
           obsdiags(i_w_ob_type,ibin)%tail%nchnperobs=-99999
           obsdiags(i_w_ob_type,ibin)%tail%luse=.false.
           obsdiags(i_w_ob_type,ibin)%tail%muse(:)=.false.
           obsdiags(i_w_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
           obsdiags(i_w_ob_type,ibin)%tail%tldepart(:)=zero
           obsdiags(i_w_ob_type,ibin)%tail%wgtjo=-huge(zero)
           obsdiags(i_w_ob_type,ibin)%tail%obssen(:)=zero

           n_alloc(ibin) = n_alloc(ibin) +1
           my_diag => obsdiags(i_w_ob_type,ibin)%tail
           my_diag%idv = is
           my_diag%iob = i
           my_diag%ich = jj

        else
           if (.not.associated(obsdiags(i_w_ob_type,ibin)%tail)) then
              obsdiags(i_w_ob_type,ibin)%tail => obsdiags(i_w_ob_type,ibin)%head
           else
              obsdiags(i_w_ob_type,ibin)%tail => obsdiags(i_w_ob_type,ibin)%tail%next
           end if
           if (obsdiags(i_w_ob_type,ibin)%tail%indxglb/=i) then
              write(6,*)'setupw: index error'
              call stop2(306)
           end if
        endif
        if (jj==1) obsptr => obsdiags(i_w_ob_type,ibin)%tail
     enddo

     if(.not.in_curbin) cycle

!    Load observation error and values into local variables
     obserror = max(cermin(ikx),min(cermax(ikx),data(ier,i)))
     uob = data(iuob,i)
     vob = data(ivob,i)
     spdob=sqrt(uob*uob+vob*vob)
     call tintrp2a(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          1,1,mype,nfldsig)
     call tintrp2a(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          1,nsig,mype,nfldsig)

     itype=ictype(ikx)

!    Type 221=pibal winds contain a mixture of wind observations reported
!    by pressure and others by height.  Those levels only reported by 
!    pressure have a missing value (ie, large) value for the reported 
!    height.  The logic below determines whether to process type 221 
!    wind observations using height or pressure as the vertical coordinate.
!    If height is not bad (less than r0_1_bmiss), we use height in the
!    forward model.  Otherwise, use reported pressure.

     z_height = .false.
     if ((itype>=221 .and. itype <= 229) .and. (data(ihgt,i)<r0_1_bmiss)) z_height = .true.

!    Process observations reported with height differently than those
!    reported with pressure.  Type 223=profiler and 224=vadwnd are 
!    encoded in NCEP prepbufr files with geometric height above 
!    sea level.  Type 229=pibal profiler is reported using 
!    geopotenital height.  Some type 221=pibal wind observations are
!    also repoted using geopotential height.

     sfc_data = (itype >=280 .and. itype < 300) .and. (.not.twodvar_regional)
     if (z_height .or. sfc_data) then

        drpx = zero
        dpres = data(ihgt,i)
        dstn = data(ielev,i)
        call tintrp2a(ges_z,zsges,dlat,dlon,dtime,hrdifsig,&
             1,1,mype,nfldsig)
!       Subtract off combination of surface station elevation and
!       model elevation depending on how close to surface
        fact = zero
        if(dpres-dstn > 10._r_kind)then
           if(dpres-dstn > r1000)then
              fact = one
           else
              fact=(dpres-dstn)/990._r_kind
           end if
        end if
        dpres=dpres-(dstn+fact*(zsges-dstn))

!       Get guess surface elevation and geopotential height profile 
!       at observation location.
        call tintrp2a(geop_hgtl,zges,dlat,dlon,dtime,hrdifsig,&
             1,nsig,mype,nfldsig)

!       For observation reported with geometric height above sea level,
!       convert geopotential to geometric height.

        if ((itype>=223 .and. itype<=228) .or. sfc_data) then
!          Convert geopotential height at layer midpoints to geometric 
!          height using equations (17, 20, 23) in MJ Mahoney's note 
!          "A discussion of various measures of altitude" (2001).  
!          Available on the web at
!          http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
!          termg  = equation 17
!          termr  = equation 21
!          termrg = first term in the denominator of equation 23
!          zges  = equation 23

           slat = data(ilate,i)*deg2rad
           sin2  = sin(slat)*sin(slat)
           termg = grav_equator * &
                ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
           termr = semi_major_axis /(one + flattening + grav_ratio -  &
                two*flattening*sin2)
           termrg = (termg/grav)*termr
           do k=1,nsig
              zges(k) = (termr*zges(k)) / (termrg-zges(k))  ! eq (23)
           end do
 
        endif

!       Given observation height, (1) adjust 10 meter wind factor if
!       necessary, (2) convert height to grid relative units, (3) compute
!       compute observation pressure (for diagnostic purposes only), and
!       (4) compute location of midpoint of first model layer above surface
!       in grid relative units

!       Adjust 10m wind factor if necessary.  Rarely do we have a
!       profiler/vad obs within 10 meters of the surface.  Almost always,
!       the code below resets the 10m wind factor to 1.0 (i.e., no
!       reduction in wind speed due to surface friction).

!       Convert observation height (in dpres) from meters to grid relative
!       units.  Save the observation height in zob for later use.
        zob = dpres
        call grdcrd(dpres,1,zges,nsig,1)

!       Interpolate guess u and v to observation location and time.
 
        call tintrp3(ges_u,ugesin,dlat,dlon,dpres,dtime, &
           hrdifsig,1,mype,nfldsig)
        call tintrp3(ges_v,vgesin,dlat,dlon,dpres,dtime, &
           hrdifsig,1,mype,nfldsig)

        if (zob > zges(1)) then
           factw=one
        else
           factw = data(iff10,i)
           if(sfcmod_gfs .or. sfcmod_mm5) then
              sfcr = data(isfcr,i)
              skint = data(iskint,i)
              call comp_fact10(dlat,dlon,dtime,skint,sfcr,isli,mype,factw)
           end if

           if (zob <= ten) then
              if(zob < ten)then
                 term = max(zob,zero)/ten
                 factw = term*factw
              end if
           else
              term = (zges(1)-zob)/(zges(1)-ten)
              factw = one-term+factw*term
           end if
 
           ugesin=factw*ugesin
           vgesin=factw*vgesin

        endif

        if(sfc_data .or. dpres < one) then
           drpx=0.005_r_kind*abs(dstn-zsges)*(one-fact)
        end if
 
!       Compute observation pressure (only used for diagnostics)

!       Set indices of model levels below (k1) and above (k2) observation.
        if (dpres<one) then
           z1=zero;    p1=log(psges)
           z2=zges(1); p2=prsltmp(1)
        elseif (dpres>nsig) then
           z1=zges(nsig-1); p1=prsltmp(nsig-1)
           z2=zges(nsig);   p2=prsltmp(nsig)
           drpx = 1.e6_r_kind
        else
           k=dpres
           k1=min(max(1,k),nsig)
           k2=max(1,min(k+1,nsig))
           z1=zges(k1); p1=prsltmp(k1)
           z2=zges(k2); p2=prsltmp(k2)
        endif
       
        dz21     = z2-z1
        dlnp21   = p2-p1
        dz       = zob-z1
        pobl     = p1 + (dlnp21/dz21)*dz
        presw    = ten*exp(pobl)

!       Determine location in terms of grid units for midpoint of
!       first layer above surface
        sfcchk=zero
!       call grdcrd(sfcchk,1,zges,nsig,1)


!    Process observations with reported pressure
     else
        dpres = data(ipres,i)
        presw = ten*exp(dpres)
        dpres = dpres-log(psges)
        drpx=zero

        prsfc=psges
        prsln2=log(exp(prsltmp(1))/prsfc)
        dpressave=dpres

!       Put obs pressure in correct units to get grid coord. number
        dpres=log(exp(dpres)*prsfc)
        call grdcrd(dpres,1,prsltmp(1),nsig,-1)
 
!       Interpolate guess u and v to observation location and time.
 
        call tintrp3(ges_u,ugesin,dlat,dlon,dpres,dtime, &
           hrdifsig,1,mype,nfldsig)
        call tintrp3(ges_v,vgesin,dlat,dlon,dpres,dtime, &
           hrdifsig,1,mype,nfldsig)
        if(dpressave <= prsln2)then
           factw=one
        else
           factw = data(iff10,i)
           if(sfcmod_gfs .or. sfcmod_mm5) then
              sfcr = data(isfcr,i)
              skint = data(iskint,i)
              call comp_fact10(dlat,dlon,dtime,skint,sfcr,isli,mype,factw)
           end if
 
           call tintrp2a(ges_tv,tges,dlat,dlon,dtime,hrdifsig,&
              1,nsig,mype,nfldsig)
!          Apply 10-meter wind reduction factor to guess winds
           dx10=-goverrd*ten/tges(1)
           if (dpressave < dx10)then
              term=(prsln2-dpressave)/(prsln2-dx10)
              factw=one-term+factw*term
           end if
           ugesin=factw*ugesin   
           vgesin=factw*vgesin
 
        end if
       
!       Get approx k value of sfc by using surface pressure
        sfcchk=log(psges)
        call grdcrd(sfcchk,1,prsltmp(1),nsig,-1)
 
     endif


!    Checks based on observation location relative to model surface and top
     rlow=max(sfcchk-dpres,zero)
     rhgh=max(dpres-r0_001-rsigp,zero)
     if(luse(i))then
        awork(1) = awork(1) + one
        if(rlow/=zero) awork(2) = awork(2) + one
        if(rhgh/=zero) awork(3) = awork(3) + one
     end if
     ratio_errors=error/(data(ier,i)+drpx+1.0e6_r_kind*rhgh+four*rlow)

!    Invert observation error
     error=one/error

!    Check to see if observation below model surface or above model top.
!    If so, don't use observation
     if (dpres > rsig )then
        if( regional .and. presw > pt_ll )then
           dpres=rsig
        else
           ratio_errors=zero
        endif
     endif

     if ( (itype>=221 .and. itype<=229).and. (dpres<zero) ) ratio_errors=zero
 

!    Compute innovations
     lowlevelsat=itype==242.or.itype==243.or.itype==245.or.itype==246.or. &
                 itype==247.or.itype==250.or.itype==251.or.itype==252.or. &
                 itype==253.or.itype==254.or.itype==257.or.itype==258.or. &
                 itype==259
     if (lowlevelsat .and. twodvar_regional) then
         call windfactor(presw,factw)
         data(iuob,i)=factw*data(iuob,i)
         data(ivob,i)=factw*data(ivob,i)
         uob = data(iuob,i)
         vob = data(ivob,i)
     endif
     dudiff=uob-ugesin
     dvdiff=vob-vgesin
     spdb=sqrt(uob**2+vob**2)-sqrt(ugesin**2+vgesin**2)

! QC PBL profiler  227 and 223, 224
     if(itype==227 .or. itype==223 .or. itype==224) then
        if(abs(uob) < 1.0_r_kind .and. abs(vob) <1.0_r_kind )  then
           muse(i)=.false.
           data(iqc,i)=10.0_r_kind
           data(iuse,i)=101.0_r_kind
        endif
     endif

!    VADWND and aircraft winds quality control
     if( itype ==224 .and. presw < 226.0_r_kind) then
        error=zero
     endif
     if(itype >=230 .and. itype <=239 .and.  presw <126.0_r_kind ) then
        error=zero
     endif

!    Quality control for satellite winds

     if (itype >240 .and. itype <260) then
        call intrp2a(tropprs,trop5,dlat,dlon,1,1,mype)
        if(presw < trop5-r50) error=zero            ! tropopose check for all satellite winds 
     endif  

     if(itype >=242 .and. itype <=256) then
        if( presw >950.0_r_kind) error =zero    !  screen data beloww 950mb
     endif
     if(itype ==242 .or. itype ==243 ) then  !  visible winds from JMA and EUMETSAT
        if(presw <700.0_r_kind) error=zero    !  no visible winds above 700mb
     endif
     if(itype ==245 ) then
        if( presw >399.0_r_kind .and. presw <801.0_r_kind) then  !GOES IR  winds
           error=zero                          !  no data between 400-800mb
        endif
     endif
     if(itype == 252 .and. presw >499.0_r_kind .and. presw <801.0_r_kind) then  ! JMA IR winds
        error=zero
     endif
     if(itype == 253 )  then
        if(presw >401.0_r_kind .and. presw <801.0_r_kind) then  ! EUMET IR winds
           error=zero
        endif
     endif
     if( itype == 246 .or. itype == 250 .or. itype == 254 )   then     ! water vapor cloud top
        if(presw >399.0_r_kind) error=zero
     endif
     if(itype ==257 .and. presw <249.0_r_kind) error=zero
     if(itype ==258 .and. presw >600.0_r_kind) error=zero
     if(itype ==259 .and. presw >600.0_r_kind) error=zero
     if(itype ==259 .and. presw <249.0_r_kind) error=zero
   
!    QC MODIS winds
     if (itype==257 .or. itype==258 .or. itype==259) then
!       Get guess values of tropopause pressure and sea/land/ice
!       mask at observation location
        prsfc = r10*prsfc       ! surface pressure in hPa

!       Compute observed and guess wind speeds (m/s).  
        spdges = sqrt(ugesin* ugesin +vgesin* vgesin )
 
!       Set and computes modis specific qc parameters
        qcu = r7
        qcv = r7
        qc_spd = (spdges+r15)/three
        qc_prs=zero
        if (itype==257) qc_prs = prsfc - r200
        if (itype==258 .or. itype==259) qc_prs = r400
        if ( presw > qc_prs .and. qc_spd < qcu ) then
           qcu = (spdob + r15)/three
           qcv = (qcv*qcu)/r7
        endif

!       if (presw < trop5-r50 .or. &                      !  tropopause check
        if(abs(dudiff) > qcu .or. &                      !  u component check
            abs(dvdiff) > qcv .or. &                      !  v component check
            (presw > prsfc-r200 .and. isli /= 0))then ! near surface check
           error = zero
        endif
     endif                                                  ! end if all satellite winds
     

!    QC WindSAT winds
     if (itype==289) then
        qcu = r6
        qcv = r6
        if ( spdob > r20 .or. &          ! high wind speed check
             abs(dudiff) > qcu  .or. &   ! u component check
             abs(dvdiff) > qcv ) then    ! v component check
           error = zero
        endif
     endif

!    QC ASCAT winds
     if (itype==290) then
        qcu = five
        qcv = five
!       Compute innovations for opposite vectors
        dudiff_opp = -uob - ugesin
        dvdiff_opp = -vob - vgesin
        vecdiff = sqrt(dudiff**2 + dvdiff**2)
        vecdiff_opp = sqrt(dudiff_opp**2 + dvdiff_opp**2)
        if ( abs(dudiff) > qcu  .or. &       ! u component check
             abs(dvdiff) > qcv  .or. &       ! v component check
             vecdiff > vecdiff_opp ) then    ! ambiguity check
 
           error = zero

        endif
     endif

!    If requested, setup for single obs test.
     if (oneobtest) then
        if (oneob_type=='u') then
           dudiff=maginnov
           dvdiff=zero
        elseif (oneob_type=='v') then
           dudiff=zero
           dvdiff=maginnov
        endif
        error=one/magoberr
        ratio_errors=one
     end if
 
!    Gross error checks
     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = sqrt(dudiff**2+dvdiff**2)
     ratio    = residual/obserrlm
!!   modify cgross depending on the quality mark, qcmark=3, cgross=0.7*cgross
!!   apply asymetric gross check for satellite winds
     qcgross=cgross(ikx)
     if(data(iqc,i) == three  ) then
        qcgross=r0_7*cgross(ikx)
     endif

     if(spdb <0 )then
        if( itype == 245 .or. itype ==246) then
           if(presw <400.0_r_kind .and. presw >300.0_r_kind ) qcgross=r0_7*cgross(ikx)
        endif
        if(itype == 253 ) then
           if( presw <400.0_r_kind .and. presw >200.0_r_kind) qcgross=r0_7*cgross(ikx)
        endif
        if(itype >=257 .and. itype <=259 ) then
          qcgross=r0_7*cgross(ikx)
        endif
     endif

     if (ratio>qcgross .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(4) = awork(4)+one
        error = zero
        ratio_errors = zero
     else
        ratio_errors =ratio_errors/sqrt(dup(i)) 
     end if

     if (lowlevelsat .and. twodvar_regional) then
        if (abs(ten*psges-presw) > 200._r_kind) then
           error = zero
           ratio_errors = zero
        endif
     endif

     if (twodvar_regional) then
        if (lowlevelsat .or. itype==289 .or. itype==290) then
            wdirdiffmax=45._r_kind
          else
           wdirdiffmax=100000._r_kind
        endif
        if (spdob > zero .and. (spdob-spdb) > zero) then
           call getwdir(uob,vob,wdirob)
           call getwdir(ugesin,vgesin,wdirgesin)
           if ( min(abs(wdirob-wdirgesin),abs(wdirob-wdirgesin+r360), &
                          abs(wdirob-wdirgesin-r360)) > wdirdiffmax ) then
               error = zero
               ratio_errors = zero
           endif
        endif
     endif

     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.

     if (nobskeep>0) muse(i)=obsdiags(i_w_ob_type,ibin)%tail%muse(nobskeep)

!    Oberror Tuning and Perturb Obs
     if(muse(i)) then
        if(oberror_tune )then
           if( jiter > jiterstart ) then
              dudiff=dudiff+data(iptrbu,i)/error/ratio_errors
              dvdiff=dvdiff+data(iptrbv,i)/error/ratio_errors
           endif
        else if(perturb_obs )then
           dudiff=dudiff+data(iptrbu,i)/error/ratio_errors
           dvdiff=dvdiff+data(iptrbv,i)/error/ratio_errors
        endif
     endif
 
     valu     = error*dudiff
     valv     = error*dvdiff

!    Compute penalty terms (linear & nonlinear qc).
     if(luse(i))then
        val      = valu*valu+valv*valv
        exp_arg  = -half*val
        rat_err2 = ratio_errors**2
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_w=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_w*wnotgross)
           term =log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
        else
           term = exp_arg
           wgt  = wgtlim
           rwgt = wgt/wgtlim
        endif
        valqc = -two*rat_err2*term


!       Accumulate statistics for obs belonging to this task
        if (muse(i)) then
           if(rwgt < one) awork(21) = awork(21)+one
           jsig = dpres
           jsig=max(1,min(jsig,nsig))
           awork(4*nsig+jsig+100)=awork(4*nsig+jsig+100)+valu*valu*rat_err2
           awork(5*nsig+jsig+100)=awork(5*nsig+jsig+100)+valv*valv*rat_err2
           awork(6*nsig+jsig+100)=awork(6*nsig+jsig+100)+one
           awork(3*nsig+jsig+100)=awork(3*nsig+jsig+100)+valqc
        end if

!       Loop over pressure level groupings and obs to accumulate statistics
!       as a function of observation type.
        ress  = scale*sqrt(dudiff**2+dvdiff**2)
        ressw = ress*ress
        val2    = half*(valu*valu+valv*valv)
        valqc2  = half*valqc
        nn=1
        if (.not. muse(i)) then
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        do k = 1,npres_print
           if(presw >ptop(k) .and. presw<=pbot(k))then
              bwork(k,ikx,1,nn) = bwork(k,ikx,1,nn)+one            ! count
              bwork(k,ikx,2,nn) = bwork(k,ikx,2,nn)+spdb           ! speed bias
              bwork(k,ikx,3,nn) = bwork(k,ikx,3,nn)+ressw          ! (o-g)**2
              bwork(k,ikx,4,nn) = bwork(k,ikx,4,nn)+val2*rat_err2  ! penalty
              bwork(k,ikx,5,nn) = bwork(k,ikx,5,nn)+valqc2         ! nonlin qc penalty
 
           end if
        end do
     end if


!    Fill obs to diagnostics structure
!    U
     obsptr%luse=luse(i)
     obsptr%muse(jiter)=muse(i)
     obsptr%nldepart(jiter)=dudiff
     obsptr%wgtjo= (error*ratio_errors)**2
!    V
     obsdiags(i_w_ob_type,ibin)%tail%luse=luse(i)
     obsdiags(i_w_ob_type,ibin)%tail%muse(jiter)=muse(i)
     obsdiags(i_w_ob_type,ibin)%tail%nldepart(jiter)=dvdiff
     obsdiags(i_w_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
 
     if (.not. last .and. muse(i)) then

        if(.not. associated(whead(ibin)%head))then
           allocate(whead(ibin)%head,stat=istat)
           if(istat /= 0)write(6,*)' failure to write whead '
           wtail(ibin)%head => whead(ibin)%head
        else
           allocate(wtail(ibin)%head%llpoint,stat=istat)
           if(istat /= 0)write(6,*)' failure to write wtail%llpoint '
           wtail(ibin)%head => wtail(ibin)%head%llpoint
        end if
 
        m_alloc(ibin) = m_alloc(ibin) +1
        my_head => wtail(ibin)%head
        my_head%idv = is
        my_head%iob = i

        call get_ijk(mm1,dlat,dlon,dpres,wtail(ibin)%head%ij(1),wtail(ibin)%head%wij(1))

        do j=1,8
           wtail(ibin)%head%wij(j)=factw*wtail(ibin)%head%wij(j)
        end do
 
        wtail(ibin)%head%ures=dudiff
        wtail(ibin)%head%vres=dvdiff
        wtail(ibin)%head%err2=error**2
        wtail(ibin)%head%raterr2=ratio_errors **2  
        wtail(ibin)%head%time = dtime
        wtail(ibin)%head%b=cvar_b(ikx)
        wtail(ibin)%head%pg=cvar_pg(ikx)
        wtail(ibin)%head%luse=luse(i)
        wtail(ibin)%head%diagu => obsptr

        my_head => wtail(ibin)%head
        my_diag => wtail(ibin)%head%diagu
        if(my_head%idv/=my_diag%idv .or. &
           my_head%iob/=my_diag%iob .or. &
                     1/=my_diag%ich ) then
           call perr(myname,'mismatched %[head,diag], (idv,iob,ich,ibin) =',&
                 (/is,i,1,ibin/))
           call perr(myname,'head%(idv,iob,ich) =',(/my_head%idv,my_head%iob,1/))
           call perr(myname,'diag%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
           call die(myname)
        endif

        if(oberror_tune) then
           wtail(ibin)%head%upertb=data(iptrbu,i)/error/ratio_errors
           wtail(ibin)%head%vpertb=data(iptrbv,i)/error/ratio_errors
           wtail(ibin)%head%kx=ikx
           if(presw > ptabl(2))then
              wtail(ibin)%head%k1=1
           else if( presw <= ptabl(33)) then
              wtail(ibin)%head%k1=33
           else
              k_loop: do k=2,32
                 if(presw > ptabl(k+1) .and. presw <= ptabl(k)) then
                    wtail(ibin)%head%k1=k
                    exit k_loop
                 endif
              enddo k_loop
           endif
        endif

        wtail(ibin)%head%diagv => obsdiags(i_w_ob_type,ibin)%tail

        my_head => wtail(ibin)%head
        my_diag => wtail(ibin)%head%diagv
        if(my_head%idv/=my_diag%idv .or. &
           my_head%iob/=my_diag%iob .or. &
                     2/=my_diag%ich ) then
           call perr(myname,'mismatched %[head,diag], (idv,iob,ich,ibin) =',&
                 (/is,i,2,ibin/))
           call perr(myname,'head%(idv,iob,ich) =',(/my_head%idv,my_head%iob,2/))
           call perr(myname,'diag%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
           call die(myname)
        endif
 
     end if

!    Save select output for diagnostic file
     if (conv_diagsave .and. luse(i)) then
        ii=ii+1
        rstation_id     = data(id,i)
        cdiagbuf(ii)    = station_id         ! station id
 
        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype

        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(ielev,i)      ! station elevation (meters) 
        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        err_input = data(ier2,i)
        err_adjst = data(ier,i)
        if (ratio_errors*error>tiny_r_kind) then
           err_final = one/(ratio_errors*error)
        else
           err_final = huge_single
        endif
 
        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
        if (err_input>tiny_r_kind) errinv_input = one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst = one/err_adjst
        if (err_final>tiny_r_kind) errinv_final = one/err_final
 
        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (m/s)**-1
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (m/s)**-1
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (m/s)**-1

        rdiagbuf(17,ii) = data(iuob,i)       ! u wind component observation (m/s)
        rdiagbuf(18,ii) = dudiff             ! u obs-ges used in analysis (m/s)
        rdiagbuf(19,ii) = uob-ugesin         ! u obs-ges w/o bias correction (m/s) (future slot)

        rdiagbuf(20,ii) = data(ivob,i)       ! v wind component observation (m/s)
        rdiagbuf(21,ii) = dvdiff             ! v obs-ges used in analysis (m/s)
        rdiagbuf(22,ii) = vob-vgesin         ! v obs-ges w/o bias correction (m/s) (future slot)

        if(regional) then

!           replace positions 17-22 with earth relative wind component information

           uob_reg=data(iuob,i)
           vob_reg=data(ivob,i)
           dlon_e=data(ilone,i)*deg2rad
           call rotate_wind_xy2ll(uob_reg,vob_reg,uob_e,vob_e,dlon_e,dlon,dlat)
           call rotate_wind_xy2ll(ugesin,vgesin,uges_e,vges_e,dlon_e,dlon,dlat)
           call rotate_wind_xy2ll(dudiff,dvdiff,dudiff_e,dvdiff_e,dlon_e,dlon,dlat)
           rdiagbuf(17,ii) = uob_e         ! earth relative u wind component observation (m/s)
           rdiagbuf(18,ii) = dudiff_e      ! earth relative u obs-ges used in analysis (m/s)
           rdiagbuf(19,ii) = uob_e-uges_e  ! earth relative u obs-ges w/o bias correction (m/s) (future slot)

           rdiagbuf(20,ii) = vob_e         ! earth relative v wind component observation (m/s)
           rdiagbuf(21,ii) = dvdiff_e      ! earth relative v obs-ges used in analysis (m/s)
           rdiagbuf(22,ii) = vob_e-vges_e  ! earth relative v obs-ges w/o bias correction (m/s) (future slot)
        end if

        rdiagbuf(23,ii) = factw              ! 10m wind reduction factor

        ioff=23
        if (lobsdiagsave) then
           do jj=1,miter
              ioff=ioff+1
              if (obsdiags(i_w_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsptr%nldepart(jj)
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_w_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsptr%tldepart(jj)
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_w_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsptr%obssen(jj)
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_w_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif

        if (twodvar_regional) then
           rdiagbuf(ioff+1,ii) = data(idomsfc,i) ! dominate surface type
           rdiagbuf(ioff+2,ii) = data(izz,i)     ! model terrain at ob location
           r_prvstg            = data(iprvd,i)
           cprvstg(ii)         = c_prvstg        ! provider name
           r_sprvstg           = data(isprvd,i)
           csprvstg(ii)        = c_sprvstg       ! subprovider name
        endif

     endif
!!!!!!!!!!!!!!!!!!  sonde ext  !!!!!!!!!!!!!!!!!!!!!!!
     cat = nint(data(icat,i))
     im=max(i-1,1)
     if(   .not. last .and. ext_sonde .and. itype==220 .and. &
        muse(i) .and.  muse(im) .and. &
       (cat==3 .or. cato==3 .or. cat==5 .or. cato==5) .and. &
        dpres > 0._r_kind .and. station_id== station_ido  )  then

        ku=dpres-1
        ku=min(nsig,ku)
        kl=dpreso+2
        kl=max(2,kl)
        do k = kl,ku
           allocate(wtail(ibin)%head%llpoint,stat=istat)
           if(istat /= 0)write(6,*)' failure to write wtail%llpoint '
           wtail(ibin)%head => wtail(ibin)%head%llpoint

!!! find wob (wint)
           uint=(data(iuob,im)*(prsltmp(k)-data(ipres,i)) &
                +data(iuob,i)*(data(ipres,im)-prsltmp(k))) /(data(ipres,im)-data(ipres,i))
           vint=(data(ivob,im)*(prsltmp(k)-data(ipres,i))  &
                +data(ivob,i)*(data(ipres,im)-prsltmp(k))) /(data(ipres,im)-data(ipres,i))
!!! find wges (wgint)
           dpk=k
           call tintrp3(ges_u,ugesin,dlat,dlon,dpk,dtime, &
              hrdifsig,1,mype,nfldsig)
           call tintrp3(ges_v,vgesin,dlat,dlon,dpk,dtime, &
              hrdifsig,1,mype,nfldsig)

!!! Set (i,j,k) indices of guess gridpoint that bound obs location
           call get_ijk(mm1,dlat,dlon,dpk,wtail(ibin)%head%ij(1),wtail(ibin)%head%wij(1))

!!! find ddiff
           dudiff=uint-ugesin
           dvdiff=vint-vgesin

!!! set oberror of pseudo_obs
           error=one/max(data(ier2,i),data(ier2,im))

           wtail(ibin)%head%ures=dudiff
           wtail(ibin)%head%vres=dvdiff
           wtail(ibin)%head%err2=error**2
           wtail(ibin)%head%raterr2=ratio_errors **2
           wtail(ibin)%head%time = dtime
           wtail(ibin)%head%b=cvar_b(ikx)
           wtail(ibin)%head%pg=cvar_pg(ikx)
           wtail(ibin)%head%luse=luse(i)
           wtail(ibin)%head%diagu => obsptr
           wtail(ibin)%head%diagv => obsdiags(i_w_ob_type,ibin)%tail

        enddo

     endif    !   itype=120

     station_ido=station_id
     dpreso=dpres
     cato=cat

!!!!!!!!!!!!!!!!!!  sonde ext  !!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!  PBL pseudo surface obs  !!!!!!!!!!!!!!!!
     if( .not. last .and. l_PBL_pseudo_SurfobsUV .and.        &
         ( itype==281 .or. itype==283 .or.itype==287 )  .and. &
           muse(i) .and. dpres > -1.0_r_kind ) then
        prest=presw     ! in mb
        prestsfc=prest
        dudiffsfc=dudiff
        dvdiffsfc=dvdiff
        call tintrp2a(pbl_height,thisPBL_height,dlat,dlon,dtime,hrdifsig,&
             1,1,mype,nfldsig)
        ratio_PBL_height = (prest - thisPBL_height) * pblH_ration
        if(ratio_PBL_height > zero) thisPBL_height = prest - ratio_PBL_height
        prest = prest - pps_press_incr
        DO while (prest > thisPBL_height)
           ratio_PBL_height=1.0_r_kind-(prestsfc-prest)/(prestsfc-thisPBL_height)

           allocate(wtail(ibin)%head%llpoint,stat=istat)
           if(istat /= 0)write(6,*)' failure to write wtail%llpoint '
           wtail(ibin)%head => wtail(ibin)%head%llpoint

!!! find uob and vob 
           uob = data(iuob,i)
           vob = data(ivob,i)


!    Put obs pressure in correct units to get grid coord. number
           dpres=log(prest/r10)
           call grdcrd(dpres,1,prsltmp(1),nsig,-1)

!    Interpolate guess u and v to observation location and time.

           call tintrp3(ges_u,ugesin,dlat,dlon,dpres,dtime, &
              hrdifsig,1,mype,nfldsig)
           call tintrp3(ges_v,vgesin,dlat,dlon,dpres,dtime, &
              hrdifsig,1,mype,nfldsig)

!!! Set (i,j,k) indices of guess gridpoint that bound obs location
           call get_ijk(mm1,dlat,dlon,dpres,wtail(ibin)%head%ij(1),wtail(ibin)%head%wij(1))
!!! find ddiff

           dudiff = dudiffsfc*(0.5_r_kind + 0.5_r_kind*ratio_PBL_height)
           dvdiff = dvdiffsfc*(0.5_r_kind + 0.5_r_kind*ratio_PBL_height)

           error=one/data(ier2,i)

           wtail(ibin)%head%ures=dudiff
           wtail(ibin)%head%vres=dvdiff
           wtail(ibin)%head%err2=error**2
           wtail(ibin)%head%raterr2=ratio_errors **2
           wtail(ibin)%head%time = dtime
           wtail(ibin)%head%b=cvar_b(ikx)
           wtail(ibin)%head%pg=cvar_pg(ikx)
           wtail(ibin)%head%luse=luse(i)
           wtail(ibin)%head%diagu => obsptr
           wtail(ibin)%head%diagv => obsdiags(i_w_ob_type,ibin)%tail

           prest = prest - pps_press_incr

        ENDDO

     endif  ! 281,283,287
!!!!!!!!!!!!!!!!!!  PBL pseudo surface obs  !!!!!!!!!!!!!!!!!!!!!!!

  end do
! End of loop over observations


! Write information to diagnostic file
  if(conv_diagsave .and. ii>0)then
     call dtime_show(myname,'diagsave:w',i_w_ob_type)
     write(7)' uv',nchar,nreal,ii,mype
     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
     deallocate(cdiagbuf,rdiagbuf)

     if (twodvar_regional) then
        write(7)cprvstg(1:ii),csprvstg(1:ii)
        deallocate(cprvstg,csprvstg)
     endif
  end if


! End of routine
end subroutine setupw