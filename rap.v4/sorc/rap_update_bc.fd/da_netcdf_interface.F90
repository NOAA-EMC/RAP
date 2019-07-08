

module da_netcdf_interface

integer, parameter :: stderr = 0
integer, parameter :: stdout = 6

contains
subroutine da_get_times_cdf(file, times, n_times, max_times, debug)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none
! NetCDF-3.
!
! netcdf version 3 fortran interface:
!
!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
!
! default fill values:
!
      integer nf_fill_byte
      integer nf_fill_int1
      integer nf_fill_char
      integer nf_fill_short
      integer nf_fill_int2
      integer nf_fill_int
      real nf_fill_float
      real nf_fill_real
      doubleprecision nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)
!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)
!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc

      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer nf_fatal
      integer nf_verbose

      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)

!
! miscellaneous routines:
!
      character*80 nf_inq_libvers
      external nf_inq_libvers

      character*80 nf_strerror
! (integer ncerr)
      external nf_strerror

      logical nf_issyserr
! (integer ncerr)
      external nf_issyserr

!
! control routines:
!
      integer nf_inq_base_pe
! (integer ncid,
! integer pe)
      external nf_inq_base_pe

      integer nf_set_base_pe
! (integer ncid,
! integer pe)
      external nf_set_base_pe

      integer nf_create
! (character*(*) path,
! integer cmode,
! integer ncid)
      external nf_create

      integer nf__create
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer chunksizehint,
! integer ncid)
      external nf__create

      integer nf__create_mp
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__create_mp

      integer nf_open
! (character*(*) path,
! integer mode,
! integer ncid)
      external nf_open

      integer nf__open
! (character*(*) path,
! integer mode,
! integer chunksizehint,
! integer ncid)
      external nf__open

      integer nf__open_mp
! (character*(*) path,
! integer mode,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__open_mp

      integer nf_set_fill
! (integer ncid,
! integer fillmode,
! integer old_mode)
      external nf_set_fill

      integer nf_set_default_format
! (integer format,
! integer old_format)
      external nf_set_default_format

      integer nf_redef
! (integer ncid)
      external nf_redef

      integer nf_enddef
! (integer ncid)
      external nf_enddef

      integer nf__enddef
! (integer ncid,
! integer h_minfree,
! integer v_align,
! integer v_minfree,
! integer r_align)
      external nf__enddef

      integer nf_sync
! (integer ncid)
      external nf_sync

      integer nf_abort
! (integer ncid)
      external nf_abort

      integer nf_close
! (integer ncid)
      external nf_close

      integer nf_delete
! (character*(*) ncid)
      external nf_delete

!
! general inquiry routines:
!

      integer nf_inq
! (integer ncid,
! integer ndims,
! integer nvars,
! integer ngatts,
! integer unlimdimid)
      external nf_inq

      integer nf_inq_ndims
! (integer ncid,
! integer ndims)
      external nf_inq_ndims

      integer nf_inq_nvars
! (integer ncid,
! integer nvars)
      external nf_inq_nvars

      integer nf_inq_natts
! (integer ncid,
! integer ngatts)
      external nf_inq_natts

      integer nf_inq_unlimdim
! (integer ncid,
! integer unlimdimid)
      external nf_inq_unlimdim

      integer nf_inq_format
! (integer ncid,
! integer format)
      external nf_inq_format

!
! dimension routines:
!

      integer nf_def_dim
! (integer ncid,
! character(*) name,
! integer len,
! integer dimid)
      external nf_def_dim

      integer nf_inq_dimid
! (integer ncid,
! character(*) name,
! integer dimid)
      external nf_inq_dimid

      integer nf_inq_dim
! (integer ncid,
! integer dimid,
! character(*) name,
! integer len)
      external nf_inq_dim

      integer nf_inq_dimname
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_inq_dimname

      integer nf_inq_dimlen
! (integer ncid,
! integer dimid,
! integer len)
      external nf_inq_dimlen

      integer nf_rename_dim
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_rename_dim

!
! general attribute routines:
!

      integer nf_inq_att
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len)
      external nf_inq_att

      integer nf_inq_attid
! (integer ncid,
! integer varid,
! character(*) name,
! integer attnum)
      external nf_inq_attid

      integer nf_inq_atttype
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype)
      external nf_inq_atttype

      integer nf_inq_attlen
! (integer ncid,
! integer varid,
! character(*) name,
! integer len)
      external nf_inq_attlen

      integer nf_inq_attname
! (integer ncid,
! integer varid,
! integer attnum,
! character(*) name)
      external nf_inq_attname

      integer nf_copy_att
! (integer ncid_in,
! integer varid_in,
! character(*) name,
! integer ncid_out,
! integer varid_out)
      external nf_copy_att

      integer nf_rename_att
! (integer ncid,
! integer varid,
! character(*) curname,
! character(*) newname)
      external nf_rename_att

      integer nf_del_att
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_del_att

!
! attribute put/get routines:
!

      integer nf_put_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! integer len,
! character(*) text)
      external nf_put_att_text

      integer nf_get_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! character(*) text)
      external nf_get_att_text

      integer nf_put_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int1_t i1vals(1))
      external nf_put_att_int1

      integer nf_get_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int1_t i1vals(1))
      external nf_get_att_int1

      integer nf_put_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int2_t i2vals(1))
      external nf_put_att_int2

      integer nf_get_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int2_t i2vals(1))
      external nf_get_att_int2

      integer nf_put_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! integer ivals(1))
      external nf_put_att_int

      integer nf_get_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer ivals(1))
      external nf_get_att_int

      integer nf_put_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! real rvals(1))
      external nf_put_att_real

      integer nf_get_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! real rvals(1))
      external nf_get_att_real

      integer nf_put_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! double dvals(1))
      external nf_put_att_double

      integer nf_get_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! double dvals(1))
      external nf_get_att_double

!
! general variable routines:
!

      integer nf_def_var
! (integer ncid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer varid)
      external nf_def_var

      integer nf_inq_var
! (integer ncid,
! integer varid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer natts)
      external nf_inq_var

      integer nf_inq_varid
! (integer ncid,
! character(*) name,
! integer varid)
      external nf_inq_varid

      integer nf_inq_varname
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_inq_varname

      integer nf_inq_vartype
! (integer ncid,
! integer varid,
! integer xtype)
      external nf_inq_vartype

      integer nf_inq_varndims
! (integer ncid,
! integer varid,
! integer ndims)
      external nf_inq_varndims

      integer nf_inq_vardimid
! (integer ncid,
! integer varid,
! integer dimids(1))
      external nf_inq_vardimid

      integer nf_inq_varnatts
! (integer ncid,
! integer varid,
! integer natts)
      external nf_inq_varnatts

      integer nf_rename_var
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_rename_var

      integer nf_copy_var
! (integer ncid_in,
! integer varid,
! integer ncid_out)
      external nf_copy_var

!
! entire variable put/get routines:
!

      integer nf_put_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_put_var_text

      integer nf_get_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_get_var_text

      integer nf_put_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_put_var_int1

      integer nf_get_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_get_var_int1

      integer nf_put_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_put_var_int2

      integer nf_get_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_get_var_int2

      integer nf_put_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_put_var_int

      integer nf_get_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_get_var_int

      integer nf_put_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_put_var_real

      integer nf_get_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_get_var_real

      integer nf_put_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_put_var_double

      integer nf_get_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_get_var_double

!
! single variable put/get routines:
!

      integer nf_put_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_put_var1_text

      integer nf_get_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_get_var1_text

      integer nf_put_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_put_var1_int1

      integer nf_get_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_get_var1_int1

      integer nf_put_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_put_var1_int2

      integer nf_get_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_get_var1_int2

      integer nf_put_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_put_var1_int

      integer nf_get_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_get_var1_int

      integer nf_put_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_put_var1_real

      integer nf_get_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_get_var1_real

      integer nf_put_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_put_var1_double

      integer nf_get_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_get_var1_double

!
! variable array put/get routines:
!

      integer nf_put_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_put_vara_text

      integer nf_get_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_get_vara_text

      integer nf_put_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_put_vara_int1

      integer nf_get_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_get_vara_int1

      integer nf_put_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_put_vara_int2

      integer nf_get_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_get_vara_int2

      integer nf_put_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_put_vara_int

      integer nf_get_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_get_vara_int

      integer nf_put_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_put_vara_real

      integer nf_get_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_get_vara_real

      integer nf_put_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_put_vara_double

      integer nf_get_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_get_vara_double

!
! strided variable put/get routines:
!

      integer nf_put_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_put_vars_text

      integer nf_get_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_get_vars_text

      integer nf_put_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_put_vars_int1

      integer nf_get_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_get_vars_int1

      integer nf_put_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_put_vars_int2

      integer nf_get_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_get_vars_int2

      integer nf_put_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_put_vars_int

      integer nf_get_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_get_vars_int

      integer nf_put_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_put_vars_real

      integer nf_get_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_get_vars_real

      integer nf_put_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_put_vars_double

      integer nf_get_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_get_vars_double

!
! mapped variable put/get routines:
!

      integer nf_put_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_put_varm_text

      integer nf_get_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_get_varm_text

      integer nf_put_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_put_varm_int1

      integer nf_get_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_get_varm_int1

      integer nf_put_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_put_varm_int2

      integer nf_get_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_get_varm_int2

      integer nf_put_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_put_varm_int

      integer nf_get_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_get_varm_int

      integer nf_put_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_put_varm_real

      integer nf_get_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_get_varm_real

      integer nf_put_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_put_varm_double

      integer nf_get_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_get_varm_double


! NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!

!
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil

      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil


      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool


!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble

      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)

!
! masks for the struct nc flag field; passed in as 'mode' arg to
! nccreate and ncopen.
!

! read/write, 0 => readonly
      parameter(ncrdwr = 1)
! in create phase, cleared by ncendef
      parameter(nccreat = 2)
! on create destroy existing file
      parameter(ncexcl = 4)
! in define mode, cleared by ncendef
      parameter(ncindef = 8)
! synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
! synchronise whole header on change (x'20')
      parameter(nchsync = 32)
! numrecs has changed (x'40')
      parameter(ncndirty = 64)
! header info has changed (x'80')
      parameter(nchdirty = 128)
! prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
! do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
! isa link (x'8000')
      parameter(nclink = 32768)

!
! 'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)

!
! 'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)

!
! attribute id to put/get a global attribute
!
      parameter(ncglobal = 0)

!
! advisory maximums:
!
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
! not enforced
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)

!
! global netcdf error status variable
! initialized in error.c
!

! no error
      parameter(ncnoerr = nf_noerr)
! not a netcdf id
      parameter(ncebadid = nf_ebadid)
! too many netcdfs open
      parameter(ncenfile = -31) ! nc_syserr
! netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
! invalid argument
      parameter(nceinval = nf_einval)
! write to read only
      parameter(nceperm = nf_eperm)
! operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine )
! operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
! coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
! maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
! string match to name in use
      parameter(ncename = nf_enameinuse)
! attribute not found
      parameter(ncenoatt = nf_enotatt)
! maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
! not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
! invalid dimension id
      parameter(ncebadd = nf_ebaddim)
! ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
! maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
! variable not found
      parameter(ncenotvr = nf_enotvar)
! action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
! not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)

!
! global options variable. used to determine behavior of error handler.
! initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)

!
! default fill values. these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub

      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   integer, intent(in) :: max_times
   integer, intent(out) :: n_times
   character (len=*), intent(in) :: file
   character (len=80), intent(out) :: times(max_times)
   logical, intent(in) :: debug
   integer :: cdfid, rcode, id_time
   character (len=80) :: varnam, time1
   integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
   integer :: i, ivtype
   ! if (trace_use) call da_trace_entry("da_get_times_cdf")
   cdfid = ncopn(file, NCNOWRIT, rcode)
   if (rcode == 0) then
      if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
      write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
      stop
   end if
   id_time = ncvid(cdfid, 'Times', rcode)
   rcode = nf_inq_var(cdfid, id_time, varnam, ivtype, ndims, dimids, natts)
   if (debug) then
      write(unit=stdout,fmt=*) ' number of dims for Time ',ndims
   end if
   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), idims(i))
      if (debug) write(unit=stdout,fmt=*) ' dimension ',i,idims(i)
   end do
   ! get the times
   n_times = idims(2)
   do i=1,idims(2)
      istart(1) = 1
      iend(1) = idims(1)
      istart(2) = i
      iend(2) = 1
      rcode = NF_GET_VARA_TEXT (cdfid, id_time, &
                                 istart, iend, &
                                 times(i) )
      time1 = times(i)
      if (debug) write(unit=stdout,fmt=*) trim(file), time1(1:19)
      times(i) = time1(1:19)
   end do
   write(unit=stdout,fmt=*) ' exiting get_times_cdf '
   call ncclos(cdfid,rcode)
   ! if (trace_use) call da_trace_exit("da_get_times_cdf")
end subroutine da_get_times_cdf
subroutine da_get_dims_cdf(file, var, dims, ndims, debug)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none
! NetCDF-3.
!
! netcdf version 3 fortran interface:
!
!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
!
! default fill values:
!
      integer nf_fill_byte
      integer nf_fill_int1
      integer nf_fill_char
      integer nf_fill_short
      integer nf_fill_int2
      integer nf_fill_int
      real nf_fill_float
      real nf_fill_real
      doubleprecision nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)
!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)
!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc
      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer nf_fatal
      integer nf_verbose
      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)
!
! miscellaneous routines:
!
      character*80 nf_inq_libvers
      external nf_inq_libvers
      character*80 nf_strerror
! (integer ncerr)
      external nf_strerror
      logical nf_issyserr
! (integer ncerr)
      external nf_issyserr
!
! control routines:
!
      integer nf_inq_base_pe
! (integer ncid,
! integer pe)
      external nf_inq_base_pe
      integer nf_set_base_pe
! (integer ncid,
! integer pe)
      external nf_set_base_pe
      integer nf_create
! (character*(*) path,
! integer cmode,
! integer ncid)
      external nf_create
      integer nf__create
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer chunksizehint,
! integer ncid)
      external nf__create
      integer nf__create_mp
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__create_mp
      integer nf_open
! (character*(*) path,
! integer mode,
! integer ncid)
      external nf_open
      integer nf__open
! (character*(*) path,
! integer mode,
! integer chunksizehint,
! integer ncid)
      external nf__open
      integer nf__open_mp
! (character*(*) path,
! integer mode,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__open_mp
      integer nf_set_fill
! (integer ncid,
! integer fillmode,
! integer old_mode)
      external nf_set_fill
      integer nf_set_default_format
! (integer format,
! integer old_format)
      external nf_set_default_format
      integer nf_redef
! (integer ncid)
      external nf_redef
      integer nf_enddef
! (integer ncid)
      external nf_enddef
      integer nf__enddef
! (integer ncid,
! integer h_minfree,
! integer v_align,
! integer v_minfree,
! integer r_align)
      external nf__enddef
      integer nf_sync
! (integer ncid)
      external nf_sync
      integer nf_abort
! (integer ncid)
      external nf_abort
      integer nf_close
! (integer ncid)
      external nf_close
      integer nf_delete
! (character*(*) ncid)
      external nf_delete
!
! general inquiry routines:
!
      integer nf_inq
! (integer ncid,
! integer ndims,
! integer nvars,
! integer ngatts,
! integer unlimdimid)
      external nf_inq
      integer nf_inq_ndims
! (integer ncid,
! integer ndims)
      external nf_inq_ndims
      integer nf_inq_nvars
! (integer ncid,
! integer nvars)
      external nf_inq_nvars
      integer nf_inq_natts
! (integer ncid,
! integer ngatts)
      external nf_inq_natts
      integer nf_inq_unlimdim
! (integer ncid,
! integer unlimdimid)
      external nf_inq_unlimdim
      integer nf_inq_format
! (integer ncid,
! integer format)
      external nf_inq_format
!
! dimension routines:
!
      integer nf_def_dim
! (integer ncid,
! character(*) name,
! integer len,
! integer dimid)
      external nf_def_dim
      integer nf_inq_dimid
! (integer ncid,
! character(*) name,
! integer dimid)
      external nf_inq_dimid
      integer nf_inq_dim
! (integer ncid,
! integer dimid,
! character(*) name,
! integer len)
      external nf_inq_dim
      integer nf_inq_dimname
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_inq_dimname
      integer nf_inq_dimlen
! (integer ncid,
! integer dimid,
! integer len)
      external nf_inq_dimlen
      integer nf_rename_dim
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_rename_dim
!
! general attribute routines:
!
      integer nf_inq_att
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len)
      external nf_inq_att
      integer nf_inq_attid
! (integer ncid,
! integer varid,
! character(*) name,
! integer attnum)
      external nf_inq_attid
      integer nf_inq_atttype
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype)
      external nf_inq_atttype
      integer nf_inq_attlen
! (integer ncid,
! integer varid,
! character(*) name,
! integer len)
      external nf_inq_attlen
      integer nf_inq_attname
! (integer ncid,
! integer varid,
! integer attnum,
! character(*) name)
      external nf_inq_attname
      integer nf_copy_att
! (integer ncid_in,
! integer varid_in,
! character(*) name,
! integer ncid_out,
! integer varid_out)
      external nf_copy_att
      integer nf_rename_att
! (integer ncid,
! integer varid,
! character(*) curname,
! character(*) newname)
      external nf_rename_att
      integer nf_del_att
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_del_att
!
! attribute put/get routines:
!
      integer nf_put_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! integer len,
! character(*) text)
      external nf_put_att_text
      integer nf_get_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! character(*) text)
      external nf_get_att_text
      integer nf_put_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int1_t i1vals(1))
      external nf_put_att_int1
      integer nf_get_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int1_t i1vals(1))
      external nf_get_att_int1
      integer nf_put_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int2_t i2vals(1))
      external nf_put_att_int2
      integer nf_get_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int2_t i2vals(1))
      external nf_get_att_int2
      integer nf_put_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! integer ivals(1))
      external nf_put_att_int
      integer nf_get_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer ivals(1))
      external nf_get_att_int
      integer nf_put_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! real rvals(1))
      external nf_put_att_real
      integer nf_get_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! real rvals(1))
      external nf_get_att_real
      integer nf_put_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! double dvals(1))
      external nf_put_att_double
      integer nf_get_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! double dvals(1))
      external nf_get_att_double
!
! general variable routines:
!
      integer nf_def_var
! (integer ncid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer varid)
      external nf_def_var
      integer nf_inq_var
! (integer ncid,
! integer varid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer natts)
      external nf_inq_var
      integer nf_inq_varid
! (integer ncid,
! character(*) name,
! integer varid)
      external nf_inq_varid
      integer nf_inq_varname
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_inq_varname
      integer nf_inq_vartype
! (integer ncid,
! integer varid,
! integer xtype)
      external nf_inq_vartype
      integer nf_inq_varndims
! (integer ncid,
! integer varid,
! integer ndims)
      external nf_inq_varndims
      integer nf_inq_vardimid
! (integer ncid,
! integer varid,
! integer dimids(1))
      external nf_inq_vardimid
      integer nf_inq_varnatts
! (integer ncid,
! integer varid,
! integer natts)
      external nf_inq_varnatts
      integer nf_rename_var
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_rename_var
      integer nf_copy_var
! (integer ncid_in,
! integer varid,
! integer ncid_out)
      external nf_copy_var
!
! entire variable put/get routines:
!
      integer nf_put_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_put_var_text
      integer nf_get_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_get_var_text
      integer nf_put_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_put_var_int1
      integer nf_get_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_get_var_int1
      integer nf_put_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_put_var_int2
      integer nf_get_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_get_var_int2
      integer nf_put_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_put_var_int
      integer nf_get_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_get_var_int
      integer nf_put_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_put_var_real
      integer nf_get_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_get_var_real
      integer nf_put_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_put_var_double
      integer nf_get_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_get_var_double
!
! single variable put/get routines:
!
      integer nf_put_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_put_var1_text
      integer nf_get_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_get_var1_text
      integer nf_put_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_put_var1_int1
      integer nf_get_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_get_var1_int1
      integer nf_put_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_put_var1_int2
      integer nf_get_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_get_var1_int2
      integer nf_put_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_put_var1_int
      integer nf_get_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_get_var1_int
      integer nf_put_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_put_var1_real
      integer nf_get_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_get_var1_real
      integer nf_put_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_put_var1_double
      integer nf_get_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_get_var1_double
!
! variable array put/get routines:
!
      integer nf_put_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_put_vara_text
      integer nf_get_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_get_vara_text
      integer nf_put_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_put_vara_int1
      integer nf_get_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_get_vara_int1
      integer nf_put_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_put_vara_int2
      integer nf_get_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_get_vara_int2
      integer nf_put_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_put_vara_int
      integer nf_get_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_get_vara_int
      integer nf_put_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_put_vara_real
      integer nf_get_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_get_vara_real
      integer nf_put_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_put_vara_double
      integer nf_get_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_get_vara_double
!
! strided variable put/get routines:
!
      integer nf_put_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_put_vars_text
      integer nf_get_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_get_vars_text
      integer nf_put_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_put_vars_int1
      integer nf_get_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_get_vars_int1
      integer nf_put_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_put_vars_int2
      integer nf_get_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_get_vars_int2
      integer nf_put_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_put_vars_int
      integer nf_get_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_get_vars_int
      integer nf_put_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_put_vars_real
      integer nf_get_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_get_vars_real
      integer nf_put_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_put_vars_double
      integer nf_get_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_get_vars_double
!
! mapped variable put/get routines:
!
      integer nf_put_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_put_varm_text
      integer nf_get_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_get_varm_text
      integer nf_put_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_put_varm_int1
      integer nf_get_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_get_varm_int1
      integer nf_put_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_put_varm_int2
      integer nf_get_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_get_varm_int2
      integer nf_put_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_put_varm_int
      integer nf_get_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_get_varm_int
      integer nf_put_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_put_varm_real
      integer nf_get_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_get_varm_real
      integer nf_put_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_put_varm_double
      integer nf_get_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_get_varm_double
! NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!
!
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil
      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil
      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool
!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble
      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)
!
! masks for the struct nc flag field; passed in as 'mode' arg to
! nccreate and ncopen.
!
! read/write, 0 => readonly
      parameter(ncrdwr = 1)
! in create phase, cleared by ncendef
      parameter(nccreat = 2)
! on create destroy existing file
      parameter(ncexcl = 4)
! in define mode, cleared by ncendef
      parameter(ncindef = 8)
! synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
! synchronise whole header on change (x'20')
      parameter(nchsync = 32)
! numrecs has changed (x'40')
      parameter(ncndirty = 64)
! header info has changed (x'80')
      parameter(nchdirty = 128)
! prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
! do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
! isa link (x'8000')
      parameter(nclink = 32768)
!
! 'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)
!
! 'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)
!
! attribute id to put/get a global attribute
!
      parameter(ncglobal = 0)
!
! advisory maximums:
!
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
! not enforced
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)
!
! global netcdf error status variable
! initialized in error.c
!
! no error
      parameter(ncnoerr = nf_noerr)
! not a netcdf id
      parameter(ncebadid = nf_ebadid)
! too many netcdfs open
      parameter(ncenfile = -31) ! nc_syserr
! netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
! invalid argument
      parameter(nceinval = nf_einval)
! write to read only
      parameter(nceperm = nf_eperm)
! operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine )
! operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
! coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
! maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
! string match to name in use
      parameter(ncename = nf_enameinuse)
! attribute not found
      parameter(ncenoatt = nf_enotatt)
! maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
! not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
! invalid dimension id
      parameter(ncebadd = nf_ebaddim)
! ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
! maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
! variable not found
      parameter(ncenotvr = nf_enotvar)
! action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
! not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)
!
! global options variable. used to determine behavior of error handler.
! initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)
!
! default fill values. these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub
      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   character (len=*), intent(in) :: file
   character (len=*), intent(in) :: var
   logical, intent(in) :: debug
   integer, intent(out) :: dims(4)
   integer, intent(out) :: ndims
   integer :: cdfid, rcode, id_time
   character (len=80) :: varnam
   integer :: natts, dimids(10)
   integer :: i, ivtype
   ! if (trace_use) call da_trace_entry("da_get_dims_cdf")
   cdfid = ncopn(file, NCNOWRIT, rcode)
   if (rcode == 0) then
      if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
      write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
      stop
   end if
   id_time = ncvid(cdfid, var, rcode)
   rcode = nf_inq_var(cdfid, id_time, varnam, ivtype, ndims, dimids, natts)
   if (debug) then
      write(unit=stdout,fmt=*) ' number of dims for ',var,' ',ndims
   end if
   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), dims(i))
      if (debug) write(unit=stdout,fmt=*) ' dimension ',i,dims(i)
   end do
   call ncclos(cdfid,rcode)
   ! if (trace_use) call da_trace_exit("da_get_dims_cdf")
end subroutine da_get_dims_cdf
subroutine da_get_gl_att_int_cdf(file, att_name, value, debug)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none
! NetCDF-3.
!
! netcdf version 3 fortran interface:
!
!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
!
! default fill values:
!
      integer nf_fill_byte
      integer nf_fill_int1
      integer nf_fill_char
      integer nf_fill_short
      integer nf_fill_int2
      integer nf_fill_int
      real nf_fill_float
      real nf_fill_real
      doubleprecision nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)
!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)
!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc
      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer nf_fatal
      integer nf_verbose
      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)
!
! miscellaneous routines:
!
      character*80 nf_inq_libvers
      external nf_inq_libvers
      character*80 nf_strerror
! (integer ncerr)
      external nf_strerror
      logical nf_issyserr
! (integer ncerr)
      external nf_issyserr
!
! control routines:
!
      integer nf_inq_base_pe
! (integer ncid,
! integer pe)
      external nf_inq_base_pe
      integer nf_set_base_pe
! (integer ncid,
! integer pe)
      external nf_set_base_pe
      integer nf_create
! (character*(*) path,
! integer cmode,
! integer ncid)
      external nf_create
      integer nf__create
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer chunksizehint,
! integer ncid)
      external nf__create
      integer nf__create_mp
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__create_mp
      integer nf_open
! (character*(*) path,
! integer mode,
! integer ncid)
      external nf_open
      integer nf__open
! (character*(*) path,
! integer mode,
! integer chunksizehint,
! integer ncid)
      external nf__open
      integer nf__open_mp
! (character*(*) path,
! integer mode,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__open_mp
      integer nf_set_fill
! (integer ncid,
! integer fillmode,
! integer old_mode)
      external nf_set_fill
      integer nf_set_default_format
! (integer format,
! integer old_format)
      external nf_set_default_format
      integer nf_redef
! (integer ncid)
      external nf_redef
      integer nf_enddef
! (integer ncid)
      external nf_enddef
      integer nf__enddef
! (integer ncid,
! integer h_minfree,
! integer v_align,
! integer v_minfree,
! integer r_align)
      external nf__enddef
      integer nf_sync
! (integer ncid)
      external nf_sync
      integer nf_abort
! (integer ncid)
      external nf_abort
      integer nf_close
! (integer ncid)
      external nf_close
      integer nf_delete
! (character*(*) ncid)
      external nf_delete
!
! general inquiry routines:
!
      integer nf_inq
! (integer ncid,
! integer ndims,
! integer nvars,
! integer ngatts,
! integer unlimdimid)
      external nf_inq
      integer nf_inq_ndims
! (integer ncid,
! integer ndims)
      external nf_inq_ndims
      integer nf_inq_nvars
! (integer ncid,
! integer nvars)
      external nf_inq_nvars
      integer nf_inq_natts
! (integer ncid,
! integer ngatts)
      external nf_inq_natts
      integer nf_inq_unlimdim
! (integer ncid,
! integer unlimdimid)
      external nf_inq_unlimdim
      integer nf_inq_format
! (integer ncid,
! integer format)
      external nf_inq_format
!
! dimension routines:
!
      integer nf_def_dim
! (integer ncid,
! character(*) name,
! integer len,
! integer dimid)
      external nf_def_dim
      integer nf_inq_dimid
! (integer ncid,
! character(*) name,
! integer dimid)
      external nf_inq_dimid
      integer nf_inq_dim
! (integer ncid,
! integer dimid,
! character(*) name,
! integer len)
      external nf_inq_dim
      integer nf_inq_dimname
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_inq_dimname
      integer nf_inq_dimlen
! (integer ncid,
! integer dimid,
! integer len)
      external nf_inq_dimlen
      integer nf_rename_dim
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_rename_dim
!
! general attribute routines:
!
      integer nf_inq_att
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len)
      external nf_inq_att
      integer nf_inq_attid
! (integer ncid,
! integer varid,
! character(*) name,
! integer attnum)
      external nf_inq_attid
      integer nf_inq_atttype
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype)
      external nf_inq_atttype
      integer nf_inq_attlen
! (integer ncid,
! integer varid,
! character(*) name,
! integer len)
      external nf_inq_attlen
      integer nf_inq_attname
! (integer ncid,
! integer varid,
! integer attnum,
! character(*) name)
      external nf_inq_attname
      integer nf_copy_att
! (integer ncid_in,
! integer varid_in,
! character(*) name,
! integer ncid_out,
! integer varid_out)
      external nf_copy_att
      integer nf_rename_att
! (integer ncid,
! integer varid,
! character(*) curname,
! character(*) newname)
      external nf_rename_att
      integer nf_del_att
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_del_att
!
! attribute put/get routines:
!
      integer nf_put_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! integer len,
! character(*) text)
      external nf_put_att_text
      integer nf_get_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! character(*) text)
      external nf_get_att_text
      integer nf_put_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int1_t i1vals(1))
      external nf_put_att_int1
      integer nf_get_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int1_t i1vals(1))
      external nf_get_att_int1
      integer nf_put_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int2_t i2vals(1))
      external nf_put_att_int2
      integer nf_get_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int2_t i2vals(1))
      external nf_get_att_int2
      integer nf_put_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! integer ivals(1))
      external nf_put_att_int
      integer nf_get_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer ivals(1))
      external nf_get_att_int
      integer nf_put_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! real rvals(1))
      external nf_put_att_real
      integer nf_get_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! real rvals(1))
      external nf_get_att_real
      integer nf_put_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! double dvals(1))
      external nf_put_att_double
      integer nf_get_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! double dvals(1))
      external nf_get_att_double
!
! general variable routines:
!
      integer nf_def_var
! (integer ncid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer varid)
      external nf_def_var
      integer nf_inq_var
! (integer ncid,
! integer varid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer natts)
      external nf_inq_var
      integer nf_inq_varid
! (integer ncid,
! character(*) name,
! integer varid)
      external nf_inq_varid
      integer nf_inq_varname
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_inq_varname
      integer nf_inq_vartype
! (integer ncid,
! integer varid,
! integer xtype)
      external nf_inq_vartype
      integer nf_inq_varndims
! (integer ncid,
! integer varid,
! integer ndims)
      external nf_inq_varndims
      integer nf_inq_vardimid
! (integer ncid,
! integer varid,
! integer dimids(1))
      external nf_inq_vardimid
      integer nf_inq_varnatts
! (integer ncid,
! integer varid,
! integer natts)
      external nf_inq_varnatts
      integer nf_rename_var
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_rename_var
      integer nf_copy_var
! (integer ncid_in,
! integer varid,
! integer ncid_out)
      external nf_copy_var
!
! entire variable put/get routines:
!
      integer nf_put_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_put_var_text
      integer nf_get_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_get_var_text
      integer nf_put_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_put_var_int1
      integer nf_get_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_get_var_int1
      integer nf_put_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_put_var_int2
      integer nf_get_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_get_var_int2
      integer nf_put_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_put_var_int
      integer nf_get_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_get_var_int
      integer nf_put_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_put_var_real
      integer nf_get_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_get_var_real
      integer nf_put_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_put_var_double
      integer nf_get_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_get_var_double
!
! single variable put/get routines:
!
      integer nf_put_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_put_var1_text
      integer nf_get_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_get_var1_text
      integer nf_put_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_put_var1_int1
      integer nf_get_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_get_var1_int1
      integer nf_put_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_put_var1_int2
      integer nf_get_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_get_var1_int2
      integer nf_put_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_put_var1_int
      integer nf_get_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_get_var1_int
      integer nf_put_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_put_var1_real
      integer nf_get_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_get_var1_real
      integer nf_put_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_put_var1_double
      integer nf_get_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_get_var1_double
!
! variable array put/get routines:
!
      integer nf_put_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_put_vara_text
      integer nf_get_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_get_vara_text
      integer nf_put_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_put_vara_int1
      integer nf_get_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_get_vara_int1
      integer nf_put_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_put_vara_int2
      integer nf_get_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_get_vara_int2
      integer nf_put_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_put_vara_int
      integer nf_get_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_get_vara_int
      integer nf_put_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_put_vara_real
      integer nf_get_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_get_vara_real
      integer nf_put_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_put_vara_double
      integer nf_get_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_get_vara_double
!
! strided variable put/get routines:
!
      integer nf_put_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_put_vars_text
      integer nf_get_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_get_vars_text
      integer nf_put_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_put_vars_int1
      integer nf_get_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_get_vars_int1
      integer nf_put_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_put_vars_int2
      integer nf_get_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_get_vars_int2
      integer nf_put_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_put_vars_int
      integer nf_get_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_get_vars_int
      integer nf_put_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_put_vars_real
      integer nf_get_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_get_vars_real
      integer nf_put_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_put_vars_double
      integer nf_get_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_get_vars_double
!
! mapped variable put/get routines:
!
      integer nf_put_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_put_varm_text
      integer nf_get_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_get_varm_text
      integer nf_put_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_put_varm_int1
      integer nf_get_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_get_varm_int1
      integer nf_put_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_put_varm_int2
      integer nf_get_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_get_varm_int2
      integer nf_put_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_put_varm_int
      integer nf_get_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_get_varm_int
      integer nf_put_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_put_varm_real
      integer nf_get_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_get_varm_real
      integer nf_put_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_put_varm_double
      integer nf_get_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_get_varm_double
! NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!
!
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil
      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil
      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool
!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble
      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)
!
! masks for the struct nc flag field; passed in as 'mode' arg to
! nccreate and ncopen.
!
! read/write, 0 => readonly
      parameter(ncrdwr = 1)
! in create phase, cleared by ncendef
      parameter(nccreat = 2)
! on create destroy existing file
      parameter(ncexcl = 4)
! in define mode, cleared by ncendef
      parameter(ncindef = 8)
! synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
! synchronise whole header on change (x'20')
      parameter(nchsync = 32)
! numrecs has changed (x'40')
      parameter(ncndirty = 64)
! header info has changed (x'80')
      parameter(nchdirty = 128)
! prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
! do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
! isa link (x'8000')
      parameter(nclink = 32768)
!
! 'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)
!
! 'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)
!
! attribute id to put/get a global attribute
!
      parameter(ncglobal = 0)
!
! advisory maximums:
!
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
! not enforced
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)
!
! global netcdf error status variable
! initialized in error.c
!
! no error
      parameter(ncnoerr = nf_noerr)
! not a netcdf id
      parameter(ncebadid = nf_ebadid)
! too many netcdfs open
      parameter(ncenfile = -31) ! nc_syserr
! netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
! invalid argument
      parameter(nceinval = nf_einval)
! write to read only
      parameter(nceperm = nf_eperm)
! operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine )
! operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
! coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
! maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
! string match to name in use
      parameter(ncename = nf_enameinuse)
! attribute not found
      parameter(ncenoatt = nf_enotatt)
! maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
! not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
! invalid dimension id
      parameter(ncebadd = nf_ebaddim)
! ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
! maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
! variable not found
      parameter(ncenotvr = nf_enotvar)
! action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
! not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)
!
! global options variable. used to determine behavior of error handler.
! initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)
!
! default fill values. these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub
      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   character (len=*), intent(in) :: file
   character (len=*), intent(in) :: att_name
   logical, intent(in) :: debug
   integer, intent(out) :: value
   integer :: cdfid, rcode
   ! if (trace_use_dull) call da_trace_entry("da_get_gl_att_int_cdf")
   cdfid = ncopn(file, NCNOWRIT, rcode)
   if (rcode == 0) then
     if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
     write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
     stop
   end if
   rcode = NF_GET_ATT_inT(cdfid, nf_global, att_name, value)
   call ncclos(cdfid,rcode)
   if (debug) write(unit=stdout,fmt=*) ' global attribute ',att_name,' is ',value
   ! if (trace_use_dull) call da_trace_exit("da_get_gl_att_int_cdf")
end subroutine da_get_gl_att_int_cdf
subroutine da_get_gl_att_real_cdf(file, att_name, value, debug)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none
! NetCDF-3.
!
! netcdf version 3 fortran interface:
!
!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
!
! default fill values:
!
      integer nf_fill_byte
      integer nf_fill_int1
      integer nf_fill_char
      integer nf_fill_short
      integer nf_fill_int2
      integer nf_fill_int
      real nf_fill_float
      real nf_fill_real
      doubleprecision nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)
!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)
!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc
      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer nf_fatal
      integer nf_verbose
      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)
!
! miscellaneous routines:
!
      character*80 nf_inq_libvers
      external nf_inq_libvers
      character*80 nf_strerror
! (integer ncerr)
      external nf_strerror
      logical nf_issyserr
! (integer ncerr)
      external nf_issyserr
!
! control routines:
!
      integer nf_inq_base_pe
! (integer ncid,
! integer pe)
      external nf_inq_base_pe
      integer nf_set_base_pe
! (integer ncid,
! integer pe)
      external nf_set_base_pe
      integer nf_create
! (character*(*) path,
! integer cmode,
! integer ncid)
      external nf_create
      integer nf__create
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer chunksizehint,
! integer ncid)
      external nf__create
      integer nf__create_mp
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__create_mp
      integer nf_open
! (character*(*) path,
! integer mode,
! integer ncid)
      external nf_open
      integer nf__open
! (character*(*) path,
! integer mode,
! integer chunksizehint,
! integer ncid)
      external nf__open
      integer nf__open_mp
! (character*(*) path,
! integer mode,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__open_mp
      integer nf_set_fill
! (integer ncid,
! integer fillmode,
! integer old_mode)
      external nf_set_fill
      integer nf_set_default_format
! (integer format,
! integer old_format)
      external nf_set_default_format
      integer nf_redef
! (integer ncid)
      external nf_redef
      integer nf_enddef
! (integer ncid)
      external nf_enddef
      integer nf__enddef
! (integer ncid,
! integer h_minfree,
! integer v_align,
! integer v_minfree,
! integer r_align)
      external nf__enddef
      integer nf_sync
! (integer ncid)
      external nf_sync
      integer nf_abort
! (integer ncid)
      external nf_abort
      integer nf_close
! (integer ncid)
      external nf_close
      integer nf_delete
! (character*(*) ncid)
      external nf_delete
!
! general inquiry routines:
!
      integer nf_inq
! (integer ncid,
! integer ndims,
! integer nvars,
! integer ngatts,
! integer unlimdimid)
      external nf_inq
      integer nf_inq_ndims
! (integer ncid,
! integer ndims)
      external nf_inq_ndims
      integer nf_inq_nvars
! (integer ncid,
! integer nvars)
      external nf_inq_nvars
      integer nf_inq_natts
! (integer ncid,
! integer ngatts)
      external nf_inq_natts
      integer nf_inq_unlimdim
! (integer ncid,
! integer unlimdimid)
      external nf_inq_unlimdim
      integer nf_inq_format
! (integer ncid,
! integer format)
      external nf_inq_format
!
! dimension routines:
!
      integer nf_def_dim
! (integer ncid,
! character(*) name,
! integer len,
! integer dimid)
      external nf_def_dim
      integer nf_inq_dimid
! (integer ncid,
! character(*) name,
! integer dimid)
      external nf_inq_dimid
      integer nf_inq_dim
! (integer ncid,
! integer dimid,
! character(*) name,
! integer len)
      external nf_inq_dim
      integer nf_inq_dimname
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_inq_dimname
      integer nf_inq_dimlen
! (integer ncid,
! integer dimid,
! integer len)
      external nf_inq_dimlen
      integer nf_rename_dim
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_rename_dim
!
! general attribute routines:
!
      integer nf_inq_att
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len)
      external nf_inq_att
      integer nf_inq_attid
! (integer ncid,
! integer varid,
! character(*) name,
! integer attnum)
      external nf_inq_attid
      integer nf_inq_atttype
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype)
      external nf_inq_atttype
      integer nf_inq_attlen
! (integer ncid,
! integer varid,
! character(*) name,
! integer len)
      external nf_inq_attlen
      integer nf_inq_attname
! (integer ncid,
! integer varid,
! integer attnum,
! character(*) name)
      external nf_inq_attname
      integer nf_copy_att
! (integer ncid_in,
! integer varid_in,
! character(*) name,
! integer ncid_out,
! integer varid_out)
      external nf_copy_att
      integer nf_rename_att
! (integer ncid,
! integer varid,
! character(*) curname,
! character(*) newname)
      external nf_rename_att
      integer nf_del_att
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_del_att
!
! attribute put/get routines:
!
      integer nf_put_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! integer len,
! character(*) text)
      external nf_put_att_text
      integer nf_get_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! character(*) text)
      external nf_get_att_text
      integer nf_put_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int1_t i1vals(1))
      external nf_put_att_int1
      integer nf_get_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int1_t i1vals(1))
      external nf_get_att_int1
      integer nf_put_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int2_t i2vals(1))
      external nf_put_att_int2
      integer nf_get_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int2_t i2vals(1))
      external nf_get_att_int2
      integer nf_put_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! integer ivals(1))
      external nf_put_att_int
      integer nf_get_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer ivals(1))
      external nf_get_att_int
      integer nf_put_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! real rvals(1))
      external nf_put_att_real
      integer nf_get_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! real rvals(1))
      external nf_get_att_real
      integer nf_put_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! double dvals(1))
      external nf_put_att_double
      integer nf_get_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! double dvals(1))
      external nf_get_att_double
!
! general variable routines:
!
      integer nf_def_var
! (integer ncid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer varid)
      external nf_def_var
      integer nf_inq_var
! (integer ncid,
! integer varid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer natts)
      external nf_inq_var
      integer nf_inq_varid
! (integer ncid,
! character(*) name,
! integer varid)
      external nf_inq_varid
      integer nf_inq_varname
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_inq_varname
      integer nf_inq_vartype
! (integer ncid,
! integer varid,
! integer xtype)
      external nf_inq_vartype
      integer nf_inq_varndims
! (integer ncid,
! integer varid,
! integer ndims)
      external nf_inq_varndims
      integer nf_inq_vardimid
! (integer ncid,
! integer varid,
! integer dimids(1))
      external nf_inq_vardimid
      integer nf_inq_varnatts
! (integer ncid,
! integer varid,
! integer natts)
      external nf_inq_varnatts
      integer nf_rename_var
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_rename_var
      integer nf_copy_var
! (integer ncid_in,
! integer varid,
! integer ncid_out)
      external nf_copy_var
!
! entire variable put/get routines:
!
      integer nf_put_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_put_var_text
      integer nf_get_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_get_var_text
      integer nf_put_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_put_var_int1
      integer nf_get_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_get_var_int1
      integer nf_put_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_put_var_int2
      integer nf_get_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_get_var_int2
      integer nf_put_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_put_var_int
      integer nf_get_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_get_var_int
      integer nf_put_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_put_var_real
      integer nf_get_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_get_var_real
      integer nf_put_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_put_var_double
      integer nf_get_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_get_var_double
!
! single variable put/get routines:
!
      integer nf_put_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_put_var1_text
      integer nf_get_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_get_var1_text
      integer nf_put_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_put_var1_int1
      integer nf_get_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_get_var1_int1
      integer nf_put_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_put_var1_int2
      integer nf_get_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_get_var1_int2
      integer nf_put_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_put_var1_int
      integer nf_get_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_get_var1_int
      integer nf_put_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_put_var1_real
      integer nf_get_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_get_var1_real
      integer nf_put_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_put_var1_double
      integer nf_get_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_get_var1_double
!
! variable array put/get routines:
!
      integer nf_put_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_put_vara_text
      integer nf_get_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_get_vara_text
      integer nf_put_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_put_vara_int1
      integer nf_get_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_get_vara_int1
      integer nf_put_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_put_vara_int2
      integer nf_get_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_get_vara_int2
      integer nf_put_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_put_vara_int
      integer nf_get_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_get_vara_int
      integer nf_put_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_put_vara_real
      integer nf_get_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_get_vara_real
      integer nf_put_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_put_vara_double
      integer nf_get_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_get_vara_double
!
! strided variable put/get routines:
!
      integer nf_put_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_put_vars_text
      integer nf_get_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_get_vars_text
      integer nf_put_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_put_vars_int1
      integer nf_get_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_get_vars_int1
      integer nf_put_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_put_vars_int2
      integer nf_get_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_get_vars_int2
      integer nf_put_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_put_vars_int
      integer nf_get_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_get_vars_int
      integer nf_put_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_put_vars_real
      integer nf_get_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_get_vars_real
      integer nf_put_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_put_vars_double
      integer nf_get_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_get_vars_double
!
! mapped variable put/get routines:
!
      integer nf_put_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_put_varm_text
      integer nf_get_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_get_varm_text
      integer nf_put_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_put_varm_int1
      integer nf_get_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_get_varm_int1
      integer nf_put_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_put_varm_int2
      integer nf_get_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_get_varm_int2
      integer nf_put_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_put_varm_int
      integer nf_get_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_get_varm_int
      integer nf_put_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_put_varm_real
      integer nf_get_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_get_varm_real
      integer nf_put_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_put_varm_double
      integer nf_get_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_get_varm_double
! NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!
!
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil
      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil
      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool
!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble
      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)
!
! masks for the struct nc flag field; passed in as 'mode' arg to
! nccreate and ncopen.
!
! read/write, 0 => readonly
      parameter(ncrdwr = 1)
! in create phase, cleared by ncendef
      parameter(nccreat = 2)
! on create destroy existing file
      parameter(ncexcl = 4)
! in define mode, cleared by ncendef
      parameter(ncindef = 8)
! synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
! synchronise whole header on change (x'20')
      parameter(nchsync = 32)
! numrecs has changed (x'40')
      parameter(ncndirty = 64)
! header info has changed (x'80')
      parameter(nchdirty = 128)
! prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
! do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
! isa link (x'8000')
      parameter(nclink = 32768)
!
! 'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)
!
! 'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)
!
! attribute id to put/get a global attribute
!
      parameter(ncglobal = 0)
!
! advisory maximums:
!
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
! not enforced
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)
!
! global netcdf error status variable
! initialized in error.c
!
! no error
      parameter(ncnoerr = nf_noerr)
! not a netcdf id
      parameter(ncebadid = nf_ebadid)
! too many netcdfs open
      parameter(ncenfile = -31) ! nc_syserr
! netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
! invalid argument
      parameter(nceinval = nf_einval)
! write to read only
      parameter(nceperm = nf_eperm)
! operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine )
! operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
! coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
! maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
! string match to name in use
      parameter(ncename = nf_enameinuse)
! attribute not found
      parameter(ncenoatt = nf_enotatt)
! maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
! not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
! invalid dimension id
      parameter(ncebadd = nf_ebaddim)
! ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
! maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
! variable not found
      parameter(ncenotvr = nf_enotvar)
! action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
! not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)
!
! global options variable. used to determine behavior of error handler.
! initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)
!
! default fill values. these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub
      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   character (len=*), intent(in) :: file
   character (len=*), intent(in) :: att_name
   logical, intent(in) :: debug
   real, intent(out) :: value
   real(kind=8) :: tmp
   real(kind=4) :: tmp4
   integer :: cdfid, rcode, ivtype
   ! if (trace_use_dull) call da_trace_entry("da_get_gl_att_real_cdf")
   cdfid = ncopn(file, NCNOWRIT, rcode)
   if (rcode == 0) then
     if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
     write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
     stop
   end if
   rcode = NF_inQ_ATTtype(cdfid, nf_global, att_name, ivtype)
   write(unit=stdout, fmt='(a, i6)') &
        'ivtype:', ivtype, &
        'NF_real=', NF_real, &
        'NF_DOUBLE=', NF_DOUBLE, &
        'kind(value)=', kind(value)
   if ((ivtype == NF_real) .and. (kind(value) == 4)) then
      rcode = NF_GET_ATT_real(cdfid, nf_global, att_name, value)
   else if ((ivtype == NF_DOUBLE) .and. (kind(value) == 4)) then
      rcode = NF_GET_ATT_real(cdfid, nf_global, att_name, tmp)
      value = tmp
   else if ((ivtype == NF_DOUBLE) .and. (kind(value) == 8)) then
      rcode = NF_GET_ATT_real(cdfid, nf_global, att_name, value)
   else if ((ivtype == NF_REAL) .and. (kind(value) == 8)) then
      rcode = NF_GET_ATT_real(cdfid, nf_global, att_name, tmp4)
      value = tmp4
   else
      write(unit=stdout, fmt='(a, i6)') &
         'Unrecognizable ivtype:', ivtype
      stop
   end if
   call ncclos(cdfid,rcode)
   if (debug) write(unit=stdout,fmt=*) ' global attribute ',att_name,' is ',value
   ! if (trace_use_dull) call da_trace_exit("da_get_gl_att_real_cdf")
end subroutine da_get_gl_att_real_cdf
subroutine da_get_var_3d_real_cdf(file, var, data, i1, i2, i3, time, debug)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none
! NetCDF-3.
!
! netcdf version 3 fortran interface:
!
!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
!
! default fill values:
!
      integer nf_fill_byte
      integer nf_fill_int1
      integer nf_fill_char
      integer nf_fill_short
      integer nf_fill_int2
      integer nf_fill_int
      real nf_fill_float
      real nf_fill_real
      doubleprecision nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)
!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)
!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc
      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer nf_fatal
      integer nf_verbose
      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)
!
! miscellaneous routines:
!
      character*80 nf_inq_libvers
      external nf_inq_libvers
      character*80 nf_strerror
! (integer ncerr)
      external nf_strerror
      logical nf_issyserr
! (integer ncerr)
      external nf_issyserr
!
! control routines:
!
      integer nf_inq_base_pe
! (integer ncid,
! integer pe)
      external nf_inq_base_pe
      integer nf_set_base_pe
! (integer ncid,
! integer pe)
      external nf_set_base_pe
      integer nf_create
! (character*(*) path,
! integer cmode,
! integer ncid)
      external nf_create
      integer nf__create
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer chunksizehint,
! integer ncid)
      external nf__create
      integer nf__create_mp
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__create_mp
      integer nf_open
! (character*(*) path,
! integer mode,
! integer ncid)
      external nf_open
      integer nf__open
! (character*(*) path,
! integer mode,
! integer chunksizehint,
! integer ncid)
      external nf__open
      integer nf__open_mp
! (character*(*) path,
! integer mode,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__open_mp
      integer nf_set_fill
! (integer ncid,
! integer fillmode,
! integer old_mode)
      external nf_set_fill
      integer nf_set_default_format
! (integer format,
! integer old_format)
      external nf_set_default_format
      integer nf_redef
! (integer ncid)
      external nf_redef
      integer nf_enddef
! (integer ncid)
      external nf_enddef
      integer nf__enddef
! (integer ncid,
! integer h_minfree,
! integer v_align,
! integer v_minfree,
! integer r_align)
      external nf__enddef
      integer nf_sync
! (integer ncid)
      external nf_sync
      integer nf_abort
! (integer ncid)
      external nf_abort
      integer nf_close
! (integer ncid)
      external nf_close
      integer nf_delete
! (character*(*) ncid)
      external nf_delete
!
! general inquiry routines:
!
      integer nf_inq
! (integer ncid,
! integer ndims,
! integer nvars,
! integer ngatts,
! integer unlimdimid)
      external nf_inq
      integer nf_inq_ndims
! (integer ncid,
! integer ndims)
      external nf_inq_ndims
      integer nf_inq_nvars
! (integer ncid,
! integer nvars)
      external nf_inq_nvars
      integer nf_inq_natts
! (integer ncid,
! integer ngatts)
      external nf_inq_natts
      integer nf_inq_unlimdim
! (integer ncid,
! integer unlimdimid)
      external nf_inq_unlimdim
      integer nf_inq_format
! (integer ncid,
! integer format)
      external nf_inq_format
!
! dimension routines:
!
      integer nf_def_dim
! (integer ncid,
! character(*) name,
! integer len,
! integer dimid)
      external nf_def_dim
      integer nf_inq_dimid
! (integer ncid,
! character(*) name,
! integer dimid)
      external nf_inq_dimid
      integer nf_inq_dim
! (integer ncid,
! integer dimid,
! character(*) name,
! integer len)
      external nf_inq_dim
      integer nf_inq_dimname
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_inq_dimname
      integer nf_inq_dimlen
! (integer ncid,
! integer dimid,
! integer len)
      external nf_inq_dimlen
      integer nf_rename_dim
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_rename_dim
!
! general attribute routines:
!
      integer nf_inq_att
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len)
      external nf_inq_att
      integer nf_inq_attid
! (integer ncid,
! integer varid,
! character(*) name,
! integer attnum)
      external nf_inq_attid
      integer nf_inq_atttype
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype)
      external nf_inq_atttype
      integer nf_inq_attlen
! (integer ncid,
! integer varid,
! character(*) name,
! integer len)
      external nf_inq_attlen
      integer nf_inq_attname
! (integer ncid,
! integer varid,
! integer attnum,
! character(*) name)
      external nf_inq_attname
      integer nf_copy_att
! (integer ncid_in,
! integer varid_in,
! character(*) name,
! integer ncid_out,
! integer varid_out)
      external nf_copy_att
      integer nf_rename_att
! (integer ncid,
! integer varid,
! character(*) curname,
! character(*) newname)
      external nf_rename_att
      integer nf_del_att
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_del_att
!
! attribute put/get routines:
!
      integer nf_put_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! integer len,
! character(*) text)
      external nf_put_att_text
      integer nf_get_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! character(*) text)
      external nf_get_att_text
      integer nf_put_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int1_t i1vals(1))
      external nf_put_att_int1
      integer nf_get_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int1_t i1vals(1))
      external nf_get_att_int1
      integer nf_put_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int2_t i2vals(1))
      external nf_put_att_int2
      integer nf_get_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int2_t i2vals(1))
      external nf_get_att_int2
      integer nf_put_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! integer ivals(1))
      external nf_put_att_int
      integer nf_get_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer ivals(1))
      external nf_get_att_int
      integer nf_put_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! real rvals(1))
      external nf_put_att_real
      integer nf_get_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! real rvals(1))
      external nf_get_att_real
      integer nf_put_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! double dvals(1))
      external nf_put_att_double
      integer nf_get_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! double dvals(1))
      external nf_get_att_double
!
! general variable routines:
!
      integer nf_def_var
! (integer ncid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer varid)
      external nf_def_var
      integer nf_inq_var
! (integer ncid,
! integer varid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer natts)
      external nf_inq_var
      integer nf_inq_varid
! (integer ncid,
! character(*) name,
! integer varid)
      external nf_inq_varid
      integer nf_inq_varname
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_inq_varname
      integer nf_inq_vartype
! (integer ncid,
! integer varid,
! integer xtype)
      external nf_inq_vartype
      integer nf_inq_varndims
! (integer ncid,
! integer varid,
! integer ndims)
      external nf_inq_varndims
      integer nf_inq_vardimid
! (integer ncid,
! integer varid,
! integer dimids(1))
      external nf_inq_vardimid
      integer nf_inq_varnatts
! (integer ncid,
! integer varid,
! integer natts)
      external nf_inq_varnatts
      integer nf_rename_var
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_rename_var
      integer nf_copy_var
! (integer ncid_in,
! integer varid,
! integer ncid_out)
      external nf_copy_var
!
! entire variable put/get routines:
!
      integer nf_put_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_put_var_text
      integer nf_get_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_get_var_text
      integer nf_put_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_put_var_int1
      integer nf_get_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_get_var_int1
      integer nf_put_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_put_var_int2
      integer nf_get_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_get_var_int2
      integer nf_put_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_put_var_int
      integer nf_get_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_get_var_int
      integer nf_put_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_put_var_real
      integer nf_get_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_get_var_real
      integer nf_put_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_put_var_double
      integer nf_get_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_get_var_double
!
! single variable put/get routines:
!
      integer nf_put_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_put_var1_text
      integer nf_get_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_get_var1_text
      integer nf_put_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_put_var1_int1
      integer nf_get_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_get_var1_int1
      integer nf_put_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_put_var1_int2
      integer nf_get_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_get_var1_int2
      integer nf_put_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_put_var1_int
      integer nf_get_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_get_var1_int
      integer nf_put_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_put_var1_real
      integer nf_get_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_get_var1_real
      integer nf_put_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_put_var1_double
      integer nf_get_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_get_var1_double
!
! variable array put/get routines:
!
      integer nf_put_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_put_vara_text
      integer nf_get_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_get_vara_text
      integer nf_put_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_put_vara_int1
      integer nf_get_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_get_vara_int1
      integer nf_put_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_put_vara_int2
      integer nf_get_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_get_vara_int2
      integer nf_put_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_put_vara_int
      integer nf_get_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_get_vara_int
      integer nf_put_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_put_vara_real
      integer nf_get_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_get_vara_real
      integer nf_put_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_put_vara_double
      integer nf_get_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_get_vara_double
!
! strided variable put/get routines:
!
      integer nf_put_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_put_vars_text
      integer nf_get_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_get_vars_text
      integer nf_put_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_put_vars_int1
      integer nf_get_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_get_vars_int1
      integer nf_put_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_put_vars_int2
      integer nf_get_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_get_vars_int2
      integer nf_put_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_put_vars_int
      integer nf_get_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_get_vars_int
      integer nf_put_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_put_vars_real
      integer nf_get_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_get_vars_real
      integer nf_put_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_put_vars_double
      integer nf_get_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_get_vars_double
!
! mapped variable put/get routines:
!
      integer nf_put_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_put_varm_text
      integer nf_get_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_get_varm_text
      integer nf_put_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_put_varm_int1
      integer nf_get_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_get_varm_int1
      integer nf_put_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_put_varm_int2
      integer nf_get_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_get_varm_int2
      integer nf_put_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_put_varm_int
      integer nf_get_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_get_varm_int
      integer nf_put_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_put_varm_real
      integer nf_get_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_get_varm_real
      integer nf_put_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_put_varm_double
      integer nf_get_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_get_varm_double
! NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!
!
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil
      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil
      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool
!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble
      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)
!
! masks for the struct nc flag field; passed in as 'mode' arg to
! nccreate and ncopen.
!
! read/write, 0 => readonly
      parameter(ncrdwr = 1)
! in create phase, cleared by ncendef
      parameter(nccreat = 2)
! on create destroy existing file
      parameter(ncexcl = 4)
! in define mode, cleared by ncendef
      parameter(ncindef = 8)
! synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
! synchronise whole header on change (x'20')
      parameter(nchsync = 32)
! numrecs has changed (x'40')
      parameter(ncndirty = 64)
! header info has changed (x'80')
      parameter(nchdirty = 128)
! prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
! do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
! isa link (x'8000')
      parameter(nclink = 32768)
!
! 'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)
!
! 'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)
!
! attribute id to put/get a global attribute
!
      parameter(ncglobal = 0)
!
! advisory maximums:
!
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
! not enforced
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)
!
! global netcdf error status variable
! initialized in error.c
!
! no error
      parameter(ncnoerr = nf_noerr)
! not a netcdf id
      parameter(ncebadid = nf_ebadid)
! too many netcdfs open
      parameter(ncenfile = -31) ! nc_syserr
! netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
! invalid argument
      parameter(nceinval = nf_einval)
! write to read only
      parameter(nceperm = nf_eperm)
! operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine )
! operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
! coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
! maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
! string match to name in use
      parameter(ncename = nf_enameinuse)
! attribute not found
      parameter(ncenoatt = nf_enotatt)
! maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
! not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
! invalid dimension id
      parameter(ncebadd = nf_ebaddim)
! ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
! maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
! variable not found
      parameter(ncenotvr = nf_enotvar)
! action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
! not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)
!
! global options variable. used to determine behavior of error handler.
! initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)
!
! default fill values. these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub
      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   integer, intent(in) :: i1, i2, i3, time
   character (len=*), intent(in) :: file
   logical, intent(in) :: debug
   character (len=*), intent(in) :: var
   real, intent(out) :: data(i1,i2,i3)
   real(kind=8) :: tmp(i1,i2,i3)
   real(kind=4) :: tmp4(i1,i2,i3)
   character (len=80) :: varnam
   integer :: cdfid, rcode, id_data
   integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
   integer :: i, ivtype
   ! if (trace_use) call da_trace_entry("da_get_var_3d_real_cdf")
   cdfid = ncopn(file, NCNOWRIT, rcode)
   if (rcode /= 0) then
      write(unit=stdout,fmt=*) ' error opening netcdf file ', trim(file)
      stop
   end if
   id_data = ncvid(cdfid, var, rcode)
   rcode = nf_inq_var(cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
   if (debug) then
      write(unit=stdout, fmt='(3a,i6)') ' get_var_3d_real_cdf: dims for ',var,' ',ndims
      write(unit=stdout, fmt='(a,i6)') ' ivtype=', ivtype
      write(unit=stdout, fmt='(a, a)') ' varnam=', trim(varnam)
      write(unit=stdout, fmt='(a,i6)') ' kind(data)=', kind(data)
   end if
   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), idims(i))
      if (debug) write(unit=stdout, fmt='(a,2i6)') ' dimension ',i,idims(i)
   end do
   ! check the dimensions
   if ((i1 /= idims(1)) .or. &
       (i2 /= idims(2)) .or. &
       (i3 /= idims(3)) .or. &
       (time > idims(4)) ) then
      write(unit=stdout,fmt=*) ' error in 3d_var_real read, dimension problem '
      write(unit=stdout,fmt=*) i1, idims(1)
      write(unit=stdout,fmt=*) i2, idims(2)
      write(unit=stdout,fmt=*) i3, idims(3)
      write(unit=stdout,fmt=*) time, idims(4)
      write(unit=stdout,fmt=*) ' error stop '
      stop
   end if
   ! get the data
   istart(1) = 1
   iend(1) = i1
   istart(2) = 1
   iend(2) = i2
   istart(3) = 1
   iend(3) = i3
   istart(4) = time
   iend(4) = 1
   if ((ivtype == NF_real) .and. (kind(data) == 4)) then
      call ncvgt(cdfid,id_data,istart,iend,data,rcode)
   else if ((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
      call ncvgt(cdfid,id_data,istart,iend,tmp,rcode)
      data = tmp
   else if ((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
      call ncvgt(cdfid,id_data,istart,iend,data,rcode)
   else if ((ivtype == NF_REAL) .and. (kind(data) == 8)) then
      call ncvgt(cdfid,id_data,istart,iend,tmp4,rcode)
      data = tmp4
   else
      write(unit=stdout, fmt='(a, i6)') &
         'Unrecognizable ivtype:', ivtype
      stop
   end if
   if (debug) then
      write(unit=stdout, fmt='(a,e24.12)') ' Sample data=', data(1,1,1)
   end if
   call ncclos(cdfid,rcode)
   ! if (trace_use) call da_trace_exit("da_get_var_3d_real_cdf")
end subroutine da_get_var_3d_real_cdf
subroutine da_get_var_1d_real_cdf(file, var, data, i1, time, debug)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none
! NetCDF-3.
!
! netcdf version 3 fortran interface:
!
!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
!
! default fill values:
!
      integer nf_fill_byte
      integer nf_fill_int1
      integer nf_fill_char
      integer nf_fill_short
      integer nf_fill_int2
      integer nf_fill_int
      real nf_fill_float
      real nf_fill_real
      doubleprecision nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)
!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)
!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc
      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer nf_fatal
      integer nf_verbose
      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)
!
! miscellaneous routines:
!
      character*80 nf_inq_libvers
      external nf_inq_libvers
      character*80 nf_strerror
! (integer ncerr)
      external nf_strerror
      logical nf_issyserr
! (integer ncerr)
      external nf_issyserr
!
! control routines:
!
      integer nf_inq_base_pe
! (integer ncid,
! integer pe)
      external nf_inq_base_pe
      integer nf_set_base_pe
! (integer ncid,
! integer pe)
      external nf_set_base_pe
      integer nf_create
! (character*(*) path,
! integer cmode,
! integer ncid)
      external nf_create
      integer nf__create
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer chunksizehint,
! integer ncid)
      external nf__create
      integer nf__create_mp
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__create_mp
      integer nf_open
! (character*(*) path,
! integer mode,
! integer ncid)
      external nf_open
      integer nf__open
! (character*(*) path,
! integer mode,
! integer chunksizehint,
! integer ncid)
      external nf__open
      integer nf__open_mp
! (character*(*) path,
! integer mode,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__open_mp
      integer nf_set_fill
! (integer ncid,
! integer fillmode,
! integer old_mode)
      external nf_set_fill
      integer nf_set_default_format
! (integer format,
! integer old_format)
      external nf_set_default_format
      integer nf_redef
! (integer ncid)
      external nf_redef
      integer nf_enddef
! (integer ncid)
      external nf_enddef
      integer nf__enddef
! (integer ncid,
! integer h_minfree,
! integer v_align,
! integer v_minfree,
! integer r_align)
      external nf__enddef
      integer nf_sync
! (integer ncid)
      external nf_sync
      integer nf_abort
! (integer ncid)
      external nf_abort
      integer nf_close
! (integer ncid)
      external nf_close
      integer nf_delete
! (character*(*) ncid)
      external nf_delete
!
! general inquiry routines:
!
      integer nf_inq
! (integer ncid,
! integer ndims,
! integer nvars,
! integer ngatts,
! integer unlimdimid)
      external nf_inq
      integer nf_inq_ndims
! (integer ncid,
! integer ndims)
      external nf_inq_ndims
      integer nf_inq_nvars
! (integer ncid,
! integer nvars)
      external nf_inq_nvars
      integer nf_inq_natts
! (integer ncid,
! integer ngatts)
      external nf_inq_natts
      integer nf_inq_unlimdim
! (integer ncid,
! integer unlimdimid)
      external nf_inq_unlimdim
      integer nf_inq_format
! (integer ncid,
! integer format)
      external nf_inq_format
!
! dimension routines:
!
      integer nf_def_dim
! (integer ncid,
! character(*) name,
! integer len,
! integer dimid)
      external nf_def_dim
      integer nf_inq_dimid
! (integer ncid,
! character(*) name,
! integer dimid)
      external nf_inq_dimid
      integer nf_inq_dim
! (integer ncid,
! integer dimid,
! character(*) name,
! integer len)
      external nf_inq_dim
      integer nf_inq_dimname
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_inq_dimname
      integer nf_inq_dimlen
! (integer ncid,
! integer dimid,
! integer len)
      external nf_inq_dimlen
      integer nf_rename_dim
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_rename_dim
!
! general attribute routines:
!
      integer nf_inq_att
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len)
      external nf_inq_att
      integer nf_inq_attid
! (integer ncid,
! integer varid,
! character(*) name,
! integer attnum)
      external nf_inq_attid
      integer nf_inq_atttype
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype)
      external nf_inq_atttype
      integer nf_inq_attlen
! (integer ncid,
! integer varid,
! character(*) name,
! integer len)
      external nf_inq_attlen
      integer nf_inq_attname
! (integer ncid,
! integer varid,
! integer attnum,
! character(*) name)
      external nf_inq_attname
      integer nf_copy_att
! (integer ncid_in,
! integer varid_in,
! character(*) name,
! integer ncid_out,
! integer varid_out)
      external nf_copy_att
      integer nf_rename_att
! (integer ncid,
! integer varid,
! character(*) curname,
! character(*) newname)
      external nf_rename_att
      integer nf_del_att
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_del_att
!
! attribute put/get routines:
!
      integer nf_put_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! integer len,
! character(*) text)
      external nf_put_att_text
      integer nf_get_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! character(*) text)
      external nf_get_att_text
      integer nf_put_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int1_t i1vals(1))
      external nf_put_att_int1
      integer nf_get_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int1_t i1vals(1))
      external nf_get_att_int1
      integer nf_put_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int2_t i2vals(1))
      external nf_put_att_int2
      integer nf_get_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int2_t i2vals(1))
      external nf_get_att_int2
      integer nf_put_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! integer ivals(1))
      external nf_put_att_int
      integer nf_get_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer ivals(1))
      external nf_get_att_int
      integer nf_put_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! real rvals(1))
      external nf_put_att_real
      integer nf_get_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! real rvals(1))
      external nf_get_att_real
      integer nf_put_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! double dvals(1))
      external nf_put_att_double
      integer nf_get_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! double dvals(1))
      external nf_get_att_double
!
! general variable routines:
!
      integer nf_def_var
! (integer ncid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer varid)
      external nf_def_var
      integer nf_inq_var
! (integer ncid,
! integer varid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer natts)
      external nf_inq_var
      integer nf_inq_varid
! (integer ncid,
! character(*) name,
! integer varid)
      external nf_inq_varid
      integer nf_inq_varname
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_inq_varname
      integer nf_inq_vartype
! (integer ncid,
! integer varid,
! integer xtype)
      external nf_inq_vartype
      integer nf_inq_varndims
! (integer ncid,
! integer varid,
! integer ndims)
      external nf_inq_varndims
      integer nf_inq_vardimid
! (integer ncid,
! integer varid,
! integer dimids(1))
      external nf_inq_vardimid
      integer nf_inq_varnatts
! (integer ncid,
! integer varid,
! integer natts)
      external nf_inq_varnatts
      integer nf_rename_var
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_rename_var
      integer nf_copy_var
! (integer ncid_in,
! integer varid,
! integer ncid_out)
      external nf_copy_var
!
! entire variable put/get routines:
!
      integer nf_put_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_put_var_text
      integer nf_get_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_get_var_text
      integer nf_put_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_put_var_int1
      integer nf_get_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_get_var_int1
      integer nf_put_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_put_var_int2
      integer nf_get_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_get_var_int2
      integer nf_put_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_put_var_int
      integer nf_get_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_get_var_int
      integer nf_put_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_put_var_real
      integer nf_get_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_get_var_real
      integer nf_put_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_put_var_double
      integer nf_get_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_get_var_double
!
! single variable put/get routines:
!
      integer nf_put_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_put_var1_text
      integer nf_get_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_get_var1_text
      integer nf_put_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_put_var1_int1
      integer nf_get_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_get_var1_int1
      integer nf_put_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_put_var1_int2
      integer nf_get_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_get_var1_int2
      integer nf_put_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_put_var1_int
      integer nf_get_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_get_var1_int
      integer nf_put_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_put_var1_real
      integer nf_get_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_get_var1_real
      integer nf_put_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_put_var1_double
      integer nf_get_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_get_var1_double
!
! variable array put/get routines:
!
      integer nf_put_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_put_vara_text
      integer nf_get_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_get_vara_text
      integer nf_put_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_put_vara_int1
      integer nf_get_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_get_vara_int1
      integer nf_put_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_put_vara_int2
      integer nf_get_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_get_vara_int2
      integer nf_put_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_put_vara_int
      integer nf_get_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_get_vara_int
      integer nf_put_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_put_vara_real
      integer nf_get_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_get_vara_real
      integer nf_put_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_put_vara_double
      integer nf_get_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_get_vara_double
!
! strided variable put/get routines:
!
      integer nf_put_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_put_vars_text
      integer nf_get_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_get_vars_text
      integer nf_put_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_put_vars_int1
      integer nf_get_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_get_vars_int1
      integer nf_put_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_put_vars_int2
      integer nf_get_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_get_vars_int2
      integer nf_put_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_put_vars_int
      integer nf_get_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_get_vars_int
      integer nf_put_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_put_vars_real
      integer nf_get_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_get_vars_real
      integer nf_put_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_put_vars_double
      integer nf_get_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_get_vars_double
!
! mapped variable put/get routines:
!
      integer nf_put_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_put_varm_text
      integer nf_get_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_get_varm_text
      integer nf_put_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_put_varm_int1
      integer nf_get_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_get_varm_int1
      integer nf_put_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_put_varm_int2
      integer nf_get_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_get_varm_int2
      integer nf_put_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_put_varm_int
      integer nf_get_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_get_varm_int
      integer nf_put_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_put_varm_real
      integer nf_get_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_get_varm_real
      integer nf_put_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_put_varm_double
      integer nf_get_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_get_varm_double
! NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!
!
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil
      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil
      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool
!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble
      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)
!
! masks for the struct nc flag field; passed in as 'mode' arg to
! nccreate and ncopen.
!
! read/write, 0 => readonly
      parameter(ncrdwr = 1)
! in create phase, cleared by ncendef
      parameter(nccreat = 2)
! on create destroy existing file
      parameter(ncexcl = 4)
! in define mode, cleared by ncendef
      parameter(ncindef = 8)
! synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
! synchronise whole header on change (x'20')
      parameter(nchsync = 32)
! numrecs has changed (x'40')
      parameter(ncndirty = 64)
! header info has changed (x'80')
      parameter(nchdirty = 128)
! prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
! do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
! isa link (x'8000')
      parameter(nclink = 32768)
!
! 'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)
!
! 'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)
!
! attribute id to put/get a global attribute
!
      parameter(ncglobal = 0)
!
! advisory maximums:
!
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
! not enforced
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)
!
! global netcdf error status variable
! initialized in error.c
!
! no error
      parameter(ncnoerr = nf_noerr)
! not a netcdf id
      parameter(ncebadid = nf_ebadid)
! too many netcdfs open
      parameter(ncenfile = -31) ! nc_syserr
! netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
! invalid argument
      parameter(nceinval = nf_einval)
! write to read only
      parameter(nceperm = nf_eperm)
! operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine )
! operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
! coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
! maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
! string match to name in use
      parameter(ncename = nf_enameinuse)
! attribute not found
      parameter(ncenoatt = nf_enotatt)
! maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
! not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
! invalid dimension id
      parameter(ncebadd = nf_ebaddim)
! ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
! maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
! variable not found
      parameter(ncenotvr = nf_enotvar)
! action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
! not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)
!
! global options variable. used to determine behavior of error handler.
! initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)
!
! default fill values. these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub
      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   integer, intent(in) :: i1, time
   character (len=*), intent(in) :: file
   logical, intent(in) :: debug
   character (len=*), intent(in) :: var
   real, intent(out) :: data(i1)
   real(kind=8) :: tmp(i1)
   real(kind=4) :: tmp4(i1)
   integer :: cdfid, rcode, id_data
   character (len=80) :: varnam
   integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
   integer :: i, ivtype
   ! if (trace_use) call da_trace_entry("da_get_var_1d_real_cdf")
   cdfid = ncopn(file, NCNOWRIT, rcode)
   if (rcode /= 0) then
      write(unit=stdout, fmt='(2a)') ' error openiing netcdf file ', trim(file)
      stop
   end if
   id_data = ncvid(cdfid, var, rcode)
   rcode = nf_inq_var(cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
   if (debug) then
      write(unit=stdout, fmt='(3a,i6)') ' get_var_1d_real_cdf: dims for ',var,' ',ndims
   end if
   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), idims(i))
      if (debug) then
         write(unit=stdout, fmt='(a,2i6)') ' dimension ',i,idims(i)
         write(unit=stdout, fmt='(a,i6)') ' ivtype=', ivtype
         write(unit=stdout, fmt='(a, a)') ' varnam=', trim(varnam)
      end if
   end do
   ! check the dimensions
   if ((i1 /= idims(1)) .or. &
       (time > idims(2)) ) then
      write(unit=stdout,fmt=*) ' error in 1d_var_real read, dimension problem '
      write(unit=stdout,fmt=*) i1, idims(1)
      write(unit=stdout,fmt=*) time, idims(2)
      write(unit=stdout,fmt=*) ' error stop '
      stop
   end if
   ! get the data
   istart(1) = 1
   iend(1) = i1
   istart(2) = time
   iend(2) = 1
   if ((ivtype == NF_real) .and. (kind(data) == 4)) then
      call ncvgt(cdfid,id_data,istart,iend,data,rcode)
   else if ((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
      call ncvgt(cdfid,id_data,istart,iend,data,rcode)
   else if ((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
      call ncvgt(cdfid,id_data,istart,iend,tmp,rcode)
      data = tmp
   else if ((ivtype == NF_REAL) .and. (kind(data) == 8)) then
      call ncvgt(cdfid,id_data,istart,iend,tmp4,rcode)
      data = tmp4
   else
      write(unit=stdout, fmt='(a, i6)') &
         'Unrecognizable ivtype:', ivtype
      stop
   end if
   if (debug) then
      write(unit=stdout, fmt='(a,e24.12)') ' Sample data=', data(1)
   end if
   call ncclos(cdfid,rcode)
   ! if (trace_use) call da_trace_exit("da_get_var_1d_real_cdf")
end subroutine da_get_var_1d_real_cdf
subroutine da_get_var_2d_real_cdf(file, var, data, i1, i2, time, debug)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none
! NetCDF-3.
!
! netcdf version 3 fortran interface:
!
!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
!
! default fill values:
!
      integer nf_fill_byte
      integer nf_fill_int1
      integer nf_fill_char
      integer nf_fill_short
      integer nf_fill_int2
      integer nf_fill_int
      real nf_fill_float
      real nf_fill_real
      doubleprecision nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)
!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)
!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc
      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer nf_fatal
      integer nf_verbose
      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)
!
! miscellaneous routines:
!
      character*80 nf_inq_libvers
      external nf_inq_libvers
      character*80 nf_strerror
! (integer ncerr)
      external nf_strerror
      logical nf_issyserr
! (integer ncerr)
      external nf_issyserr
!
! control routines:
!
      integer nf_inq_base_pe
! (integer ncid,
! integer pe)
      external nf_inq_base_pe
      integer nf_set_base_pe
! (integer ncid,
! integer pe)
      external nf_set_base_pe
      integer nf_create
! (character*(*) path,
! integer cmode,
! integer ncid)
      external nf_create
      integer nf__create
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer chunksizehint,
! integer ncid)
      external nf__create
      integer nf__create_mp
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__create_mp
      integer nf_open
! (character*(*) path,
! integer mode,
! integer ncid)
      external nf_open
      integer nf__open
! (character*(*) path,
! integer mode,
! integer chunksizehint,
! integer ncid)
      external nf__open
      integer nf__open_mp
! (character*(*) path,
! integer mode,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__open_mp
      integer nf_set_fill
! (integer ncid,
! integer fillmode,
! integer old_mode)
      external nf_set_fill
      integer nf_set_default_format
! (integer format,
! integer old_format)
      external nf_set_default_format
      integer nf_redef
! (integer ncid)
      external nf_redef
      integer nf_enddef
! (integer ncid)
      external nf_enddef
      integer nf__enddef
! (integer ncid,
! integer h_minfree,
! integer v_align,
! integer v_minfree,
! integer r_align)
      external nf__enddef
      integer nf_sync
! (integer ncid)
      external nf_sync
      integer nf_abort
! (integer ncid)
      external nf_abort
      integer nf_close
! (integer ncid)
      external nf_close
      integer nf_delete
! (character*(*) ncid)
      external nf_delete
!
! general inquiry routines:
!
      integer nf_inq
! (integer ncid,
! integer ndims,
! integer nvars,
! integer ngatts,
! integer unlimdimid)
      external nf_inq
      integer nf_inq_ndims
! (integer ncid,
! integer ndims)
      external nf_inq_ndims
      integer nf_inq_nvars
! (integer ncid,
! integer nvars)
      external nf_inq_nvars
      integer nf_inq_natts
! (integer ncid,
! integer ngatts)
      external nf_inq_natts
      integer nf_inq_unlimdim
! (integer ncid,
! integer unlimdimid)
      external nf_inq_unlimdim
      integer nf_inq_format
! (integer ncid,
! integer format)
      external nf_inq_format
!
! dimension routines:
!
      integer nf_def_dim
! (integer ncid,
! character(*) name,
! integer len,
! integer dimid)
      external nf_def_dim
      integer nf_inq_dimid
! (integer ncid,
! character(*) name,
! integer dimid)
      external nf_inq_dimid
      integer nf_inq_dim
! (integer ncid,
! integer dimid,
! character(*) name,
! integer len)
      external nf_inq_dim
      integer nf_inq_dimname
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_inq_dimname
      integer nf_inq_dimlen
! (integer ncid,
! integer dimid,
! integer len)
      external nf_inq_dimlen
      integer nf_rename_dim
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_rename_dim
!
! general attribute routines:
!
      integer nf_inq_att
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len)
      external nf_inq_att
      integer nf_inq_attid
! (integer ncid,
! integer varid,
! character(*) name,
! integer attnum)
      external nf_inq_attid
      integer nf_inq_atttype
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype)
      external nf_inq_atttype
      integer nf_inq_attlen
! (integer ncid,
! integer varid,
! character(*) name,
! integer len)
      external nf_inq_attlen
      integer nf_inq_attname
! (integer ncid,
! integer varid,
! integer attnum,
! character(*) name)
      external nf_inq_attname
      integer nf_copy_att
! (integer ncid_in,
! integer varid_in,
! character(*) name,
! integer ncid_out,
! integer varid_out)
      external nf_copy_att
      integer nf_rename_att
! (integer ncid,
! integer varid,
! character(*) curname,
! character(*) newname)
      external nf_rename_att
      integer nf_del_att
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_del_att
!
! attribute put/get routines:
!
      integer nf_put_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! integer len,
! character(*) text)
      external nf_put_att_text
      integer nf_get_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! character(*) text)
      external nf_get_att_text
      integer nf_put_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int1_t i1vals(1))
      external nf_put_att_int1
      integer nf_get_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int1_t i1vals(1))
      external nf_get_att_int1
      integer nf_put_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int2_t i2vals(1))
      external nf_put_att_int2
      integer nf_get_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int2_t i2vals(1))
      external nf_get_att_int2
      integer nf_put_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! integer ivals(1))
      external nf_put_att_int
      integer nf_get_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer ivals(1))
      external nf_get_att_int
      integer nf_put_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! real rvals(1))
      external nf_put_att_real
      integer nf_get_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! real rvals(1))
      external nf_get_att_real
      integer nf_put_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! double dvals(1))
      external nf_put_att_double
      integer nf_get_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! double dvals(1))
      external nf_get_att_double
!
! general variable routines:
!
      integer nf_def_var
! (integer ncid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer varid)
      external nf_def_var
      integer nf_inq_var
! (integer ncid,
! integer varid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer natts)
      external nf_inq_var
      integer nf_inq_varid
! (integer ncid,
! character(*) name,
! integer varid)
      external nf_inq_varid
      integer nf_inq_varname
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_inq_varname
      integer nf_inq_vartype
! (integer ncid,
! integer varid,
! integer xtype)
      external nf_inq_vartype
      integer nf_inq_varndims
! (integer ncid,
! integer varid,
! integer ndims)
      external nf_inq_varndims
      integer nf_inq_vardimid
! (integer ncid,
! integer varid,
! integer dimids(1))
      external nf_inq_vardimid
      integer nf_inq_varnatts
! (integer ncid,
! integer varid,
! integer natts)
      external nf_inq_varnatts
      integer nf_rename_var
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_rename_var
      integer nf_copy_var
! (integer ncid_in,
! integer varid,
! integer ncid_out)
      external nf_copy_var
!
! entire variable put/get routines:
!
      integer nf_put_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_put_var_text
      integer nf_get_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_get_var_text
      integer nf_put_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_put_var_int1
      integer nf_get_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_get_var_int1
      integer nf_put_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_put_var_int2
      integer nf_get_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_get_var_int2
      integer nf_put_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_put_var_int
      integer nf_get_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_get_var_int
      integer nf_put_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_put_var_real
      integer nf_get_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_get_var_real
      integer nf_put_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_put_var_double
      integer nf_get_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_get_var_double
!
! single variable put/get routines:
!
      integer nf_put_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_put_var1_text
      integer nf_get_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_get_var1_text
      integer nf_put_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_put_var1_int1
      integer nf_get_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_get_var1_int1
      integer nf_put_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_put_var1_int2
      integer nf_get_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_get_var1_int2
      integer nf_put_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_put_var1_int
      integer nf_get_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_get_var1_int
      integer nf_put_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_put_var1_real
      integer nf_get_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_get_var1_real
      integer nf_put_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_put_var1_double
      integer nf_get_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_get_var1_double
!
! variable array put/get routines:
!
      integer nf_put_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_put_vara_text
      integer nf_get_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_get_vara_text
      integer nf_put_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_put_vara_int1
      integer nf_get_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_get_vara_int1
      integer nf_put_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_put_vara_int2
      integer nf_get_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_get_vara_int2
      integer nf_put_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_put_vara_int
      integer nf_get_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_get_vara_int
      integer nf_put_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_put_vara_real
      integer nf_get_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_get_vara_real
      integer nf_put_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_put_vara_double
      integer nf_get_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_get_vara_double
!
! strided variable put/get routines:
!
      integer nf_put_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_put_vars_text
      integer nf_get_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_get_vars_text
      integer nf_put_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_put_vars_int1
      integer nf_get_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_get_vars_int1
      integer nf_put_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_put_vars_int2
      integer nf_get_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_get_vars_int2
      integer nf_put_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_put_vars_int
      integer nf_get_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_get_vars_int
      integer nf_put_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_put_vars_real
      integer nf_get_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_get_vars_real
      integer nf_put_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_put_vars_double
      integer nf_get_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_get_vars_double
!
! mapped variable put/get routines:
!
      integer nf_put_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_put_varm_text
      integer nf_get_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_get_varm_text
      integer nf_put_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_put_varm_int1
      integer nf_get_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_get_varm_int1
      integer nf_put_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_put_varm_int2
      integer nf_get_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_get_varm_int2
      integer nf_put_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_put_varm_int
      integer nf_get_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_get_varm_int
      integer nf_put_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_put_varm_real
      integer nf_get_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_get_varm_real
      integer nf_put_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_put_varm_double
      integer nf_get_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_get_varm_double
! NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!
!
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil
      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil
      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool
!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble
      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)
!
! masks for the struct nc flag field; passed in as 'mode' arg to
! nccreate and ncopen.
!
! read/write, 0 => readonly
      parameter(ncrdwr = 1)
! in create phase, cleared by ncendef
      parameter(nccreat = 2)
! on create destroy existing file
      parameter(ncexcl = 4)
! in define mode, cleared by ncendef
      parameter(ncindef = 8)
! synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
! synchronise whole header on change (x'20')
      parameter(nchsync = 32)
! numrecs has changed (x'40')
      parameter(ncndirty = 64)
! header info has changed (x'80')
      parameter(nchdirty = 128)
! prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
! do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
! isa link (x'8000')
      parameter(nclink = 32768)
!
! 'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)
!
! 'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)
!
! attribute id to put/get a global attribute
!
      parameter(ncglobal = 0)
!
! advisory maximums:
!
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
! not enforced
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)
!
! global netcdf error status variable
! initialized in error.c
!
! no error
      parameter(ncnoerr = nf_noerr)
! not a netcdf id
      parameter(ncebadid = nf_ebadid)
! too many netcdfs open
      parameter(ncenfile = -31) ! nc_syserr
! netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
! invalid argument
      parameter(nceinval = nf_einval)
! write to read only
      parameter(nceperm = nf_eperm)
! operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine )
! operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
! coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
! maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
! string match to name in use
      parameter(ncename = nf_enameinuse)
! attribute not found
      parameter(ncenoatt = nf_enotatt)
! maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
! not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
! invalid dimension id
      parameter(ncebadd = nf_ebaddim)
! ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
! maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
! variable not found
      parameter(ncenotvr = nf_enotvar)
! action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
! not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)
!
! global options variable. used to determine behavior of error handler.
! initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)
!
! default fill values. these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub
      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   integer, intent(in) :: i1, i2, time
   character (len=*), intent(in) :: file
   logical, intent(in) :: debug
   character (len=*), intent(in) :: var
   real, intent(out) :: data(i1,i2)
   real(kind=8) :: tmp(i1,i2)
   real(kind=4) :: tmp4(i1,i2)
   integer :: cdfid, rcode, id_data
   character (len=80) :: varnam
   integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
   integer :: i, ivtype
   ! if (trace_use) call da_trace_entry("da_get_var_2d_real_cdf")
   cdfid = ncopn(file, NCNOWRIT, rcode)
   if (rcode /= 0) then
      write(unit=stdout, fmt='(2a)') ' error openiing netcdf file ', trim(file)
      stop
   end if
   id_data = ncvid(cdfid, var, rcode)
   rcode = nf_inq_var(cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
   if (debug) then
      write(unit=stdout, fmt='(3a,i6)') ' get_var_2d_real_cdf: dims for ',var,' ',ndims
   end if
   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), idims(i))
      if (debug) then
         write(unit=stdout, fmt='(a,2i6)') ' dimension ',i,idims(i)
         write(unit=stdout, fmt='(a,i6)') ' ivtype=', ivtype
         write(unit=stdout, fmt='(a, a)') ' varnam=', trim(varnam)
      end if
   end do
   ! check the dimensions
   if ((i1 /= idims(1)) .or. &
       (i2 /= idims(2)) .or. &
       (time > idims(3)) ) then
      write(unit=stdout,fmt=*) ' error in 2d_var_real read, dimension problem '
      write(unit=stdout,fmt=*) i1, idims(1)
      write(unit=stdout,fmt=*) i2, idims(2)
      write(unit=stdout,fmt=*) time, idims(3)
      write(unit=stdout,fmt=*) ' error stop '
      stop
   end if
   ! get the data
   istart(1) = 1
   iend(1) = i1
   istart(2) = 1
   iend(2) = i2
   istart(3) = time
   iend(3) = 1
   if ((ivtype == NF_real) .and. (kind(data) == 4)) then
      call ncvgt(cdfid,id_data,istart,iend,data,rcode)
   else if ((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
      call ncvgt(cdfid,id_data,istart,iend,data,rcode)
   else if ((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
      call ncvgt(cdfid,id_data,istart,iend,tmp,rcode)
      data = tmp
   else if ((ivtype == NF_REAL) .and. (kind(data) == 8)) then
      call ncvgt(cdfid,id_data,istart,iend,tmp4,rcode)
      data = tmp4
   else
      write(unit=stdout, fmt='(a, i6)') &
         'Unrecognizable ivtype:', ivtype
      stop
   end if
   if (debug) then
      write(unit=stdout, fmt='(a,e24.12)') ' Sample data=', data(1,1)
   end if
   call ncclos(cdfid,rcode)
   ! if (trace_use) call da_trace_exit("da_get_var_2d_real_cdf")
end subroutine da_get_var_2d_real_cdf
subroutine da_put_var_3d_real_cdf(file, var, data, i1, i2, i3, time, debug)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none
! NetCDF-3.
!
! netcdf version 3 fortran interface:
!
!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
!
! default fill values:
!
      integer nf_fill_byte
      integer nf_fill_int1
      integer nf_fill_char
      integer nf_fill_short
      integer nf_fill_int2
      integer nf_fill_int
      real nf_fill_float
      real nf_fill_real
      doubleprecision nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)
!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)
!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc
      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer nf_fatal
      integer nf_verbose
      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)
!
! miscellaneous routines:
!
      character*80 nf_inq_libvers
      external nf_inq_libvers
      character*80 nf_strerror
! (integer ncerr)
      external nf_strerror
      logical nf_issyserr
! (integer ncerr)
      external nf_issyserr
!
! control routines:
!
      integer nf_inq_base_pe
! (integer ncid,
! integer pe)
      external nf_inq_base_pe
      integer nf_set_base_pe
! (integer ncid,
! integer pe)
      external nf_set_base_pe
      integer nf_create
! (character*(*) path,
! integer cmode,
! integer ncid)
      external nf_create
      integer nf__create
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer chunksizehint,
! integer ncid)
      external nf__create
      integer nf__create_mp
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__create_mp
      integer nf_open
! (character*(*) path,
! integer mode,
! integer ncid)
      external nf_open
      integer nf__open
! (character*(*) path,
! integer mode,
! integer chunksizehint,
! integer ncid)
      external nf__open
      integer nf__open_mp
! (character*(*) path,
! integer mode,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__open_mp
      integer nf_set_fill
! (integer ncid,
! integer fillmode,
! integer old_mode)
      external nf_set_fill
      integer nf_set_default_format
! (integer format,
! integer old_format)
      external nf_set_default_format
      integer nf_redef
! (integer ncid)
      external nf_redef
      integer nf_enddef
! (integer ncid)
      external nf_enddef
      integer nf__enddef
! (integer ncid,
! integer h_minfree,
! integer v_align,
! integer v_minfree,
! integer r_align)
      external nf__enddef
      integer nf_sync
! (integer ncid)
      external nf_sync
      integer nf_abort
! (integer ncid)
      external nf_abort
      integer nf_close
! (integer ncid)
      external nf_close
      integer nf_delete
! (character*(*) ncid)
      external nf_delete
!
! general inquiry routines:
!
      integer nf_inq
! (integer ncid,
! integer ndims,
! integer nvars,
! integer ngatts,
! integer unlimdimid)
      external nf_inq
      integer nf_inq_ndims
! (integer ncid,
! integer ndims)
      external nf_inq_ndims
      integer nf_inq_nvars
! (integer ncid,
! integer nvars)
      external nf_inq_nvars
      integer nf_inq_natts
! (integer ncid,
! integer ngatts)
      external nf_inq_natts
      integer nf_inq_unlimdim
! (integer ncid,
! integer unlimdimid)
      external nf_inq_unlimdim
      integer nf_inq_format
! (integer ncid,
! integer format)
      external nf_inq_format
!
! dimension routines:
!
      integer nf_def_dim
! (integer ncid,
! character(*) name,
! integer len,
! integer dimid)
      external nf_def_dim
      integer nf_inq_dimid
! (integer ncid,
! character(*) name,
! integer dimid)
      external nf_inq_dimid
      integer nf_inq_dim
! (integer ncid,
! integer dimid,
! character(*) name,
! integer len)
      external nf_inq_dim
      integer nf_inq_dimname
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_inq_dimname
      integer nf_inq_dimlen
! (integer ncid,
! integer dimid,
! integer len)
      external nf_inq_dimlen
      integer nf_rename_dim
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_rename_dim
!
! general attribute routines:
!
      integer nf_inq_att
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len)
      external nf_inq_att
      integer nf_inq_attid
! (integer ncid,
! integer varid,
! character(*) name,
! integer attnum)
      external nf_inq_attid
      integer nf_inq_atttype
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype)
      external nf_inq_atttype
      integer nf_inq_attlen
! (integer ncid,
! integer varid,
! character(*) name,
! integer len)
      external nf_inq_attlen
      integer nf_inq_attname
! (integer ncid,
! integer varid,
! integer attnum,
! character(*) name)
      external nf_inq_attname
      integer nf_copy_att
! (integer ncid_in,
! integer varid_in,
! character(*) name,
! integer ncid_out,
! integer varid_out)
      external nf_copy_att
      integer nf_rename_att
! (integer ncid,
! integer varid,
! character(*) curname,
! character(*) newname)
      external nf_rename_att
      integer nf_del_att
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_del_att
!
! attribute put/get routines:
!
      integer nf_put_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! integer len,
! character(*) text)
      external nf_put_att_text
      integer nf_get_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! character(*) text)
      external nf_get_att_text
      integer nf_put_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int1_t i1vals(1))
      external nf_put_att_int1
      integer nf_get_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int1_t i1vals(1))
      external nf_get_att_int1
      integer nf_put_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int2_t i2vals(1))
      external nf_put_att_int2
      integer nf_get_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int2_t i2vals(1))
      external nf_get_att_int2
      integer nf_put_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! integer ivals(1))
      external nf_put_att_int
      integer nf_get_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer ivals(1))
      external nf_get_att_int
      integer nf_put_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! real rvals(1))
      external nf_put_att_real
      integer nf_get_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! real rvals(1))
      external nf_get_att_real
      integer nf_put_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! double dvals(1))
      external nf_put_att_double
      integer nf_get_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! double dvals(1))
      external nf_get_att_double
!
! general variable routines:
!
      integer nf_def_var
! (integer ncid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer varid)
      external nf_def_var
      integer nf_inq_var
! (integer ncid,
! integer varid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer natts)
      external nf_inq_var
      integer nf_inq_varid
! (integer ncid,
! character(*) name,
! integer varid)
      external nf_inq_varid
      integer nf_inq_varname
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_inq_varname
      integer nf_inq_vartype
! (integer ncid,
! integer varid,
! integer xtype)
      external nf_inq_vartype
      integer nf_inq_varndims
! (integer ncid,
! integer varid,
! integer ndims)
      external nf_inq_varndims
      integer nf_inq_vardimid
! (integer ncid,
! integer varid,
! integer dimids(1))
      external nf_inq_vardimid
      integer nf_inq_varnatts
! (integer ncid,
! integer varid,
! integer natts)
      external nf_inq_varnatts
      integer nf_rename_var
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_rename_var
      integer nf_copy_var
! (integer ncid_in,
! integer varid,
! integer ncid_out)
      external nf_copy_var
!
! entire variable put/get routines:
!
      integer nf_put_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_put_var_text
      integer nf_get_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_get_var_text
      integer nf_put_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_put_var_int1
      integer nf_get_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_get_var_int1
      integer nf_put_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_put_var_int2
      integer nf_get_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_get_var_int2
      integer nf_put_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_put_var_int
      integer nf_get_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_get_var_int
      integer nf_put_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_put_var_real
      integer nf_get_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_get_var_real
      integer nf_put_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_put_var_double
      integer nf_get_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_get_var_double
!
! single variable put/get routines:
!
      integer nf_put_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_put_var1_text
      integer nf_get_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_get_var1_text
      integer nf_put_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_put_var1_int1
      integer nf_get_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_get_var1_int1
      integer nf_put_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_put_var1_int2
      integer nf_get_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_get_var1_int2
      integer nf_put_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_put_var1_int
      integer nf_get_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_get_var1_int
      integer nf_put_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_put_var1_real
      integer nf_get_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_get_var1_real
      integer nf_put_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_put_var1_double
      integer nf_get_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_get_var1_double
!
! variable array put/get routines:
!
      integer nf_put_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_put_vara_text
      integer nf_get_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_get_vara_text
      integer nf_put_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_put_vara_int1
      integer nf_get_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_get_vara_int1
      integer nf_put_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_put_vara_int2
      integer nf_get_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_get_vara_int2
      integer nf_put_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_put_vara_int
      integer nf_get_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_get_vara_int
      integer nf_put_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_put_vara_real
      integer nf_get_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_get_vara_real
      integer nf_put_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_put_vara_double
      integer nf_get_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_get_vara_double
!
! strided variable put/get routines:
!
      integer nf_put_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_put_vars_text
      integer nf_get_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_get_vars_text
      integer nf_put_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_put_vars_int1
      integer nf_get_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_get_vars_int1
      integer nf_put_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_put_vars_int2
      integer nf_get_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_get_vars_int2
      integer nf_put_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_put_vars_int
      integer nf_get_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_get_vars_int
      integer nf_put_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_put_vars_real
      integer nf_get_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_get_vars_real
      integer nf_put_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_put_vars_double
      integer nf_get_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_get_vars_double
!
! mapped variable put/get routines:
!
      integer nf_put_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_put_varm_text
      integer nf_get_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_get_varm_text
      integer nf_put_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_put_varm_int1
      integer nf_get_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_get_varm_int1
      integer nf_put_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_put_varm_int2
      integer nf_get_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_get_varm_int2
      integer nf_put_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_put_varm_int
      integer nf_get_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_get_varm_int
      integer nf_put_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_put_varm_real
      integer nf_get_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_get_varm_real
      integer nf_put_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_put_varm_double
      integer nf_get_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_get_varm_double
! NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!
!
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil
      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil
      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool
!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble
      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)
!
! masks for the struct nc flag field; passed in as 'mode' arg to
! nccreate and ncopen.
!
! read/write, 0 => readonly
      parameter(ncrdwr = 1)
! in create phase, cleared by ncendef
      parameter(nccreat = 2)
! on create destroy existing file
      parameter(ncexcl = 4)
! in define mode, cleared by ncendef
      parameter(ncindef = 8)
! synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
! synchronise whole header on change (x'20')
      parameter(nchsync = 32)
! numrecs has changed (x'40')
      parameter(ncndirty = 64)
! header info has changed (x'80')
      parameter(nchdirty = 128)
! prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
! do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
! isa link (x'8000')
      parameter(nclink = 32768)
!
! 'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)
!
! 'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)
!
! attribute id to put/get a global attribute
!
      parameter(ncglobal = 0)
!
! advisory maximums:
!
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
! not enforced
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)
!
! global netcdf error status variable
! initialized in error.c
!
! no error
      parameter(ncnoerr = nf_noerr)
! not a netcdf id
      parameter(ncebadid = nf_ebadid)
! too many netcdfs open
      parameter(ncenfile = -31) ! nc_syserr
! netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
! invalid argument
      parameter(nceinval = nf_einval)
! write to read only
      parameter(nceperm = nf_eperm)
! operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine )
! operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
! coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
! maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
! string match to name in use
      parameter(ncename = nf_enameinuse)
! attribute not found
      parameter(ncenoatt = nf_enotatt)
! maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
! not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
! invalid dimension id
      parameter(ncebadd = nf_ebaddim)
! ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
! maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
! variable not found
      parameter(ncenotvr = nf_enotvar)
! action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
! not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)
!
! global options variable. used to determine behavior of error handler.
! initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)
!
! default fill values. these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub
      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   integer, intent(in) :: i1, i2, i3, time
   character (len=*), intent(in) :: file
   logical, intent(in) :: debug
   character (len=*), intent(in) :: var
   real, intent(in) :: data(i1,i2,i3)
   real(kind=8) :: tmp(i1,i2,i3)
   real(kind=4) :: tmp4(i1,i2,i3)
   integer :: cdfid, rcode, id_data
   character (len=80) :: varnam
   integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
   integer :: i, ivtype
   ! not for update_bc
   ! if (trace_use) call da_trace_entry("da_put_var_3d_real_cdf")
   cdfid = ncopn(file, NCWRITE, rcode)
   if (rcode /= 0) then
      write(unit=stdout, fmt='(2a)') ' error openiing netcdf file ', trim(file)
      stop
   end if
   id_data = ncvid(cdfid, var, rcode)
   rcode = nf_inq_var(cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
   if (debug) then
      write(unit=stdout, fmt='(3a,i6)') ' put_var_3d_real_cdf: dims for ',var,' ',ndims
   end if
   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), idims(i))
      if (debug) write(unit=stdout,fmt=*) ' dimension ',i,idims(i)
   end do
   ! check the dimensions
   if ((i1 /= idims(1)) .or. &
       (i2 /= idims(2)) .or. &
       (i3 /= idims(3)) .or. &
       (time > idims(4)) ) then
      write(unit=stdout,fmt=*) ' error in 3d_var_real read, dimension problem '
      write(unit=stdout,fmt=*) i1, idims(1)
      write(unit=stdout,fmt=*) i2, idims(2)
      write(unit=stdout,fmt=*) i3, idims(3)
      write(unit=stdout,fmt=*) time, idims(4)
      write(unit=stdout,fmt=*) ' error stop '
      stop
   end if
   ! get the data
   istart(1) = 1
   iend(1) = i1
   istart(2) = 1
   iend(2) = i2
   istart(3) = 1
   iend(3) = i3
   istart(4) = time
   iend(4) = 1
   if ((ivtype == NF_real) .and. (kind(data) == 4)) then
      call ncvpt(cdfid,id_data,istart,iend,data,rcode)
   else if ((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
      tmp = data
      call ncvpt(cdfid,id_data,istart,iend,tmp,rcode)
   else if ((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
      tmp = data
      call ncvpt(cdfid,id_data,istart,iend,tmp,rcode)
   else if ((ivtype == NF_REAL) .and. (kind(data) == 8)) then
      tmp4 = data
      call ncvpt(cdfid,id_data,istart,iend,tmp4,rcode)
   else
      write(unit=stdout, fmt='(a, 4i6)') &
         'Unrecognizable ivtype:', ivtype,nf_double,nf_real,kind(data)
      stop
   end if
   call ncclos(cdfid,rcode)
   ! if (trace_use) call da_trace_exit("da_put_var_3d_real_cdf")
end subroutine da_put_var_3d_real_cdf
subroutine da_put_var_2d_real_cdf(file, var, data, i1, i2, time, debug)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none
! NetCDF-3.
!
! netcdf version 3 fortran interface:
!
!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
!
! default fill values:
!
      integer nf_fill_byte
      integer nf_fill_int1
      integer nf_fill_char
      integer nf_fill_short
      integer nf_fill_int2
      integer nf_fill_int
      real nf_fill_float
      real nf_fill_real
      doubleprecision nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)
!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)
!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc
      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer nf_fatal
      integer nf_verbose
      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)
!
! miscellaneous routines:
!
      character*80 nf_inq_libvers
      external nf_inq_libvers
      character*80 nf_strerror
! (integer ncerr)
      external nf_strerror
      logical nf_issyserr
! (integer ncerr)
      external nf_issyserr
!
! control routines:
!
      integer nf_inq_base_pe
! (integer ncid,
! integer pe)
      external nf_inq_base_pe
      integer nf_set_base_pe
! (integer ncid,
! integer pe)
      external nf_set_base_pe
      integer nf_create
! (character*(*) path,
! integer cmode,
! integer ncid)
      external nf_create
      integer nf__create
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer chunksizehint,
! integer ncid)
      external nf__create
      integer nf__create_mp
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__create_mp
      integer nf_open
! (character*(*) path,
! integer mode,
! integer ncid)
      external nf_open
      integer nf__open
! (character*(*) path,
! integer mode,
! integer chunksizehint,
! integer ncid)
      external nf__open
      integer nf__open_mp
! (character*(*) path,
! integer mode,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__open_mp
      integer nf_set_fill
! (integer ncid,
! integer fillmode,
! integer old_mode)
      external nf_set_fill
      integer nf_set_default_format
! (integer format,
! integer old_format)
      external nf_set_default_format
      integer nf_redef
! (integer ncid)
      external nf_redef
      integer nf_enddef
! (integer ncid)
      external nf_enddef
      integer nf__enddef
! (integer ncid,
! integer h_minfree,
! integer v_align,
! integer v_minfree,
! integer r_align)
      external nf__enddef
      integer nf_sync
! (integer ncid)
      external nf_sync
      integer nf_abort
! (integer ncid)
      external nf_abort
      integer nf_close
! (integer ncid)
      external nf_close
      integer nf_delete
! (character*(*) ncid)
      external nf_delete
!
! general inquiry routines:
!
      integer nf_inq
! (integer ncid,
! integer ndims,
! integer nvars,
! integer ngatts,
! integer unlimdimid)
      external nf_inq
      integer nf_inq_ndims
! (integer ncid,
! integer ndims)
      external nf_inq_ndims
      integer nf_inq_nvars
! (integer ncid,
! integer nvars)
      external nf_inq_nvars
      integer nf_inq_natts
! (integer ncid,
! integer ngatts)
      external nf_inq_natts
      integer nf_inq_unlimdim
! (integer ncid,
! integer unlimdimid)
      external nf_inq_unlimdim
      integer nf_inq_format
! (integer ncid,
! integer format)
      external nf_inq_format
!
! dimension routines:
!
      integer nf_def_dim
! (integer ncid,
! character(*) name,
! integer len,
! integer dimid)
      external nf_def_dim
      integer nf_inq_dimid
! (integer ncid,
! character(*) name,
! integer dimid)
      external nf_inq_dimid
      integer nf_inq_dim
! (integer ncid,
! integer dimid,
! character(*) name,
! integer len)
      external nf_inq_dim
      integer nf_inq_dimname
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_inq_dimname
      integer nf_inq_dimlen
! (integer ncid,
! integer dimid,
! integer len)
      external nf_inq_dimlen
      integer nf_rename_dim
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_rename_dim
!
! general attribute routines:
!
      integer nf_inq_att
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len)
      external nf_inq_att
      integer nf_inq_attid
! (integer ncid,
! integer varid,
! character(*) name,
! integer attnum)
      external nf_inq_attid
      integer nf_inq_atttype
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype)
      external nf_inq_atttype
      integer nf_inq_attlen
! (integer ncid,
! integer varid,
! character(*) name,
! integer len)
      external nf_inq_attlen
      integer nf_inq_attname
! (integer ncid,
! integer varid,
! integer attnum,
! character(*) name)
      external nf_inq_attname
      integer nf_copy_att
! (integer ncid_in,
! integer varid_in,
! character(*) name,
! integer ncid_out,
! integer varid_out)
      external nf_copy_att
      integer nf_rename_att
! (integer ncid,
! integer varid,
! character(*) curname,
! character(*) newname)
      external nf_rename_att
      integer nf_del_att
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_del_att
!
! attribute put/get routines:
!
      integer nf_put_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! integer len,
! character(*) text)
      external nf_put_att_text
      integer nf_get_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! character(*) text)
      external nf_get_att_text
      integer nf_put_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int1_t i1vals(1))
      external nf_put_att_int1
      integer nf_get_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int1_t i1vals(1))
      external nf_get_att_int1
      integer nf_put_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int2_t i2vals(1))
      external nf_put_att_int2
      integer nf_get_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int2_t i2vals(1))
      external nf_get_att_int2
      integer nf_put_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! integer ivals(1))
      external nf_put_att_int
      integer nf_get_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer ivals(1))
      external nf_get_att_int
      integer nf_put_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! real rvals(1))
      external nf_put_att_real
      integer nf_get_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! real rvals(1))
      external nf_get_att_real
      integer nf_put_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! double dvals(1))
      external nf_put_att_double
      integer nf_get_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! double dvals(1))
      external nf_get_att_double
!
! general variable routines:
!
      integer nf_def_var
! (integer ncid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer varid)
      external nf_def_var
      integer nf_inq_var
! (integer ncid,
! integer varid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer natts)
      external nf_inq_var
      integer nf_inq_varid
! (integer ncid,
! character(*) name,
! integer varid)
      external nf_inq_varid
      integer nf_inq_varname
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_inq_varname
      integer nf_inq_vartype
! (integer ncid,
! integer varid,
! integer xtype)
      external nf_inq_vartype
      integer nf_inq_varndims
! (integer ncid,
! integer varid,
! integer ndims)
      external nf_inq_varndims
      integer nf_inq_vardimid
! (integer ncid,
! integer varid,
! integer dimids(1))
      external nf_inq_vardimid
      integer nf_inq_varnatts
! (integer ncid,
! integer varid,
! integer natts)
      external nf_inq_varnatts
      integer nf_rename_var
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_rename_var
      integer nf_copy_var
! (integer ncid_in,
! integer varid,
! integer ncid_out)
      external nf_copy_var
!
! entire variable put/get routines:
!
      integer nf_put_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_put_var_text
      integer nf_get_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_get_var_text
      integer nf_put_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_put_var_int1
      integer nf_get_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_get_var_int1
      integer nf_put_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_put_var_int2
      integer nf_get_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_get_var_int2
      integer nf_put_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_put_var_int
      integer nf_get_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_get_var_int
      integer nf_put_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_put_var_real
      integer nf_get_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_get_var_real
      integer nf_put_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_put_var_double
      integer nf_get_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_get_var_double
!
! single variable put/get routines:
!
      integer nf_put_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_put_var1_text
      integer nf_get_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_get_var1_text
      integer nf_put_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_put_var1_int1
      integer nf_get_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_get_var1_int1
      integer nf_put_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_put_var1_int2
      integer nf_get_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_get_var1_int2
      integer nf_put_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_put_var1_int
      integer nf_get_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_get_var1_int
      integer nf_put_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_put_var1_real
      integer nf_get_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_get_var1_real
      integer nf_put_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_put_var1_double
      integer nf_get_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_get_var1_double
!
! variable array put/get routines:
!
      integer nf_put_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_put_vara_text
      integer nf_get_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_get_vara_text
      integer nf_put_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_put_vara_int1
      integer nf_get_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_get_vara_int1
      integer nf_put_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_put_vara_int2
      integer nf_get_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_get_vara_int2
      integer nf_put_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_put_vara_int
      integer nf_get_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_get_vara_int
      integer nf_put_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_put_vara_real
      integer nf_get_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_get_vara_real
      integer nf_put_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_put_vara_double
      integer nf_get_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_get_vara_double
!
! strided variable put/get routines:
!
      integer nf_put_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_put_vars_text
      integer nf_get_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_get_vars_text
      integer nf_put_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_put_vars_int1
      integer nf_get_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_get_vars_int1
      integer nf_put_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_put_vars_int2
      integer nf_get_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_get_vars_int2
      integer nf_put_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_put_vars_int
      integer nf_get_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_get_vars_int
      integer nf_put_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_put_vars_real
      integer nf_get_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_get_vars_real
      integer nf_put_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_put_vars_double
      integer nf_get_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_get_vars_double
!
! mapped variable put/get routines:
!
      integer nf_put_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_put_varm_text
      integer nf_get_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_get_varm_text
      integer nf_put_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_put_varm_int1
      integer nf_get_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_get_varm_int1
      integer nf_put_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_put_varm_int2
      integer nf_get_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_get_varm_int2
      integer nf_put_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_put_varm_int
      integer nf_get_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_get_varm_int
      integer nf_put_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_put_varm_real
      integer nf_get_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_get_varm_real
      integer nf_put_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_put_varm_double
      integer nf_get_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_get_varm_double
! NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!
!
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil
      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil
      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool
!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble
      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)
!
! masks for the struct nc flag field; passed in as 'mode' arg to
! nccreate and ncopen.
!
! read/write, 0 => readonly
      parameter(ncrdwr = 1)
! in create phase, cleared by ncendef
      parameter(nccreat = 2)
! on create destroy existing file
      parameter(ncexcl = 4)
! in define mode, cleared by ncendef
      parameter(ncindef = 8)
! synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
! synchronise whole header on change (x'20')
      parameter(nchsync = 32)
! numrecs has changed (x'40')
      parameter(ncndirty = 64)
! header info has changed (x'80')
      parameter(nchdirty = 128)
! prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
! do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
! isa link (x'8000')
      parameter(nclink = 32768)
!
! 'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)
!
! 'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)
!
! attribute id to put/get a global attribute
!
      parameter(ncglobal = 0)
!
! advisory maximums:
!
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
! not enforced
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)
!
! global netcdf error status variable
! initialized in error.c
!
! no error
      parameter(ncnoerr = nf_noerr)
! not a netcdf id
      parameter(ncebadid = nf_ebadid)
! too many netcdfs open
      parameter(ncenfile = -31) ! nc_syserr
! netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
! invalid argument
      parameter(nceinval = nf_einval)
! write to read only
      parameter(nceperm = nf_eperm)
! operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine )
! operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
! coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
! maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
! string match to name in use
      parameter(ncename = nf_enameinuse)
! attribute not found
      parameter(ncenoatt = nf_enotatt)
! maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
! not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
! invalid dimension id
      parameter(ncebadd = nf_ebaddim)
! ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
! maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
! variable not found
      parameter(ncenotvr = nf_enotvar)
! action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
! not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)
!
! global options variable. used to determine behavior of error handler.
! initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)
!
! default fill values. these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub
      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   integer, intent(in) :: i1, i2, time
   character (len=*), intent(in) :: file
   logical, intent(in) :: debug
   character (len=*), intent(in) :: var
   real, intent(in) :: data(i1,i2)
   real(kind=8) :: tmp(i1,i2)
   real(kind=4) :: tmp4(i1,i2)
   integer :: cdfid, rcode, id_data
   character (len=80) :: varnam
   integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
   integer :: i, ivtype
   ! not for update_bc
   !if (trace_use) call da_trace_entry("da_put_var_2d_real_cdf")
   cdfid = ncopn(file, NCWRITE, rcode)
   if (rcode == 0) then
      if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
      write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
      stop
   end if
   id_data = ncvid(cdfid, var, rcode)
   rcode = nf_inq_var(cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
   if (debug) then
      write(unit=stdout,fmt=*) ' number of dims for ',var,' ',ndims
   end if
   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), idims(i))
      if (debug) write(unit=stdout,fmt=*) ' dimension ',i,idims(i)
   end do
   ! check the dimensions
   if ((i1 /= idims(1)) .or. &
      (i2 /= idims(2)) .or. &
      (time > idims(3)) ) then
      write(unit=stdout,fmt=*) ' error in 3d_var_real read, dimension problem '
      write(unit=stdout,fmt=*) i1, idims(1)
      write(unit=stdout,fmt=*) i2, idims(2)
      write(unit=stdout,fmt=*) time, idims(3)
      write(unit=stdout,fmt=*) ' error stop '
      stop
   end if
   ! get the data
   istart(1) = 1
   iend(1) = i1
   istart(2) = 1
   iend(2) = i2
   istart(3) = time
   iend(3) = 1
   if ((ivtype == NF_real) .and. (kind(data) == 4)) then
      call ncvpt(cdfid,id_data,istart,iend,data,rcode)
   else if ((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
      tmp = data
      call ncvpt(cdfid,id_data,istart,iend,tmp,rcode)
   else if ((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
      tmp = data
      call ncvpt(cdfid,id_data,istart,iend,tmp,rcode)
   else if ((ivtype == NF_REAL) .and. (kind(data) == 8)) then
      tmp4 = data
      call ncvpt(cdfid,id_data,istart,iend,tmp4,rcode)
   else
      write(unit=stdout, fmt='(a, 4i6)') &
         'Unrecognizable ivtype:', ivtype,nf_double,nf_real,kind(data)
      stop
   end if
   call ncclos(cdfid,rcode)
   !if (trace_use) call da_trace_exit("da_put_var_2d_real_cdf")
end subroutine da_put_var_2d_real_cdf
subroutine da_get_var_2d_int_cdf(file, var, data, i1, i2, time, debug)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none
! NetCDF-3.
!
! netcdf version 3 fortran interface:
!
!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
!
! default fill values:
!
      integer nf_fill_byte
      integer nf_fill_int1
      integer nf_fill_char
      integer nf_fill_short
      integer nf_fill_int2
      integer nf_fill_int
      real nf_fill_float
      real nf_fill_real
      doubleprecision nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)
!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)
!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc
      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer nf_fatal
      integer nf_verbose
      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)
!
! miscellaneous routines:
!
      character*80 nf_inq_libvers
      external nf_inq_libvers
      character*80 nf_strerror
! (integer ncerr)
      external nf_strerror
      logical nf_issyserr
! (integer ncerr)
      external nf_issyserr
!
! control routines:
!
      integer nf_inq_base_pe
! (integer ncid,
! integer pe)
      external nf_inq_base_pe
      integer nf_set_base_pe
! (integer ncid,
! integer pe)
      external nf_set_base_pe
      integer nf_create
! (character*(*) path,
! integer cmode,
! integer ncid)
      external nf_create
      integer nf__create
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer chunksizehint,
! integer ncid)
      external nf__create
      integer nf__create_mp
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__create_mp
      integer nf_open
! (character*(*) path,
! integer mode,
! integer ncid)
      external nf_open
      integer nf__open
! (character*(*) path,
! integer mode,
! integer chunksizehint,
! integer ncid)
      external nf__open
      integer nf__open_mp
! (character*(*) path,
! integer mode,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__open_mp
      integer nf_set_fill
! (integer ncid,
! integer fillmode,
! integer old_mode)
      external nf_set_fill
      integer nf_set_default_format
! (integer format,
! integer old_format)
      external nf_set_default_format
      integer nf_redef
! (integer ncid)
      external nf_redef
      integer nf_enddef
! (integer ncid)
      external nf_enddef
      integer nf__enddef
! (integer ncid,
! integer h_minfree,
! integer v_align,
! integer v_minfree,
! integer r_align)
      external nf__enddef
      integer nf_sync
! (integer ncid)
      external nf_sync
      integer nf_abort
! (integer ncid)
      external nf_abort
      integer nf_close
! (integer ncid)
      external nf_close
      integer nf_delete
! (character*(*) ncid)
      external nf_delete
!
! general inquiry routines:
!
      integer nf_inq
! (integer ncid,
! integer ndims,
! integer nvars,
! integer ngatts,
! integer unlimdimid)
      external nf_inq
      integer nf_inq_ndims
! (integer ncid,
! integer ndims)
      external nf_inq_ndims
      integer nf_inq_nvars
! (integer ncid,
! integer nvars)
      external nf_inq_nvars
      integer nf_inq_natts
! (integer ncid,
! integer ngatts)
      external nf_inq_natts
      integer nf_inq_unlimdim
! (integer ncid,
! integer unlimdimid)
      external nf_inq_unlimdim
      integer nf_inq_format
! (integer ncid,
! integer format)
      external nf_inq_format
!
! dimension routines:
!
      integer nf_def_dim
! (integer ncid,
! character(*) name,
! integer len,
! integer dimid)
      external nf_def_dim
      integer nf_inq_dimid
! (integer ncid,
! character(*) name,
! integer dimid)
      external nf_inq_dimid
      integer nf_inq_dim
! (integer ncid,
! integer dimid,
! character(*) name,
! integer len)
      external nf_inq_dim
      integer nf_inq_dimname
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_inq_dimname
      integer nf_inq_dimlen
! (integer ncid,
! integer dimid,
! integer len)
      external nf_inq_dimlen
      integer nf_rename_dim
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_rename_dim
!
! general attribute routines:
!
      integer nf_inq_att
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len)
      external nf_inq_att
      integer nf_inq_attid
! (integer ncid,
! integer varid,
! character(*) name,
! integer attnum)
      external nf_inq_attid
      integer nf_inq_atttype
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype)
      external nf_inq_atttype
      integer nf_inq_attlen
! (integer ncid,
! integer varid,
! character(*) name,
! integer len)
      external nf_inq_attlen
      integer nf_inq_attname
! (integer ncid,
! integer varid,
! integer attnum,
! character(*) name)
      external nf_inq_attname
      integer nf_copy_att
! (integer ncid_in,
! integer varid_in,
! character(*) name,
! integer ncid_out,
! integer varid_out)
      external nf_copy_att
      integer nf_rename_att
! (integer ncid,
! integer varid,
! character(*) curname,
! character(*) newname)
      external nf_rename_att
      integer nf_del_att
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_del_att
!
! attribute put/get routines:
!
      integer nf_put_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! integer len,
! character(*) text)
      external nf_put_att_text
      integer nf_get_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! character(*) text)
      external nf_get_att_text
      integer nf_put_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int1_t i1vals(1))
      external nf_put_att_int1
      integer nf_get_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int1_t i1vals(1))
      external nf_get_att_int1
      integer nf_put_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int2_t i2vals(1))
      external nf_put_att_int2
      integer nf_get_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int2_t i2vals(1))
      external nf_get_att_int2
      integer nf_put_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! integer ivals(1))
      external nf_put_att_int
      integer nf_get_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer ivals(1))
      external nf_get_att_int
      integer nf_put_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! real rvals(1))
      external nf_put_att_real
      integer nf_get_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! real rvals(1))
      external nf_get_att_real
      integer nf_put_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! double dvals(1))
      external nf_put_att_double
      integer nf_get_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! double dvals(1))
      external nf_get_att_double
!
! general variable routines:
!
      integer nf_def_var
! (integer ncid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer varid)
      external nf_def_var
      integer nf_inq_var
! (integer ncid,
! integer varid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer natts)
      external nf_inq_var
      integer nf_inq_varid
! (integer ncid,
! character(*) name,
! integer varid)
      external nf_inq_varid
      integer nf_inq_varname
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_inq_varname
      integer nf_inq_vartype
! (integer ncid,
! integer varid,
! integer xtype)
      external nf_inq_vartype
      integer nf_inq_varndims
! (integer ncid,
! integer varid,
! integer ndims)
      external nf_inq_varndims
      integer nf_inq_vardimid
! (integer ncid,
! integer varid,
! integer dimids(1))
      external nf_inq_vardimid
      integer nf_inq_varnatts
! (integer ncid,
! integer varid,
! integer natts)
      external nf_inq_varnatts
      integer nf_rename_var
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_rename_var
      integer nf_copy_var
! (integer ncid_in,
! integer varid,
! integer ncid_out)
      external nf_copy_var
!
! entire variable put/get routines:
!
      integer nf_put_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_put_var_text
      integer nf_get_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_get_var_text
      integer nf_put_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_put_var_int1
      integer nf_get_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_get_var_int1
      integer nf_put_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_put_var_int2
      integer nf_get_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_get_var_int2
      integer nf_put_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_put_var_int
      integer nf_get_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_get_var_int
      integer nf_put_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_put_var_real
      integer nf_get_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_get_var_real
      integer nf_put_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_put_var_double
      integer nf_get_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_get_var_double
!
! single variable put/get routines:
!
      integer nf_put_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_put_var1_text
      integer nf_get_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_get_var1_text
      integer nf_put_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_put_var1_int1
      integer nf_get_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_get_var1_int1
      integer nf_put_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_put_var1_int2
      integer nf_get_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_get_var1_int2
      integer nf_put_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_put_var1_int
      integer nf_get_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_get_var1_int
      integer nf_put_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_put_var1_real
      integer nf_get_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_get_var1_real
      integer nf_put_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_put_var1_double
      integer nf_get_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_get_var1_double
!
! variable array put/get routines:
!
      integer nf_put_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_put_vara_text
      integer nf_get_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_get_vara_text
      integer nf_put_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_put_vara_int1
      integer nf_get_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_get_vara_int1
      integer nf_put_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_put_vara_int2
      integer nf_get_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_get_vara_int2
      integer nf_put_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_put_vara_int
      integer nf_get_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_get_vara_int
      integer nf_put_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_put_vara_real
      integer nf_get_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_get_vara_real
      integer nf_put_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_put_vara_double
      integer nf_get_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_get_vara_double
!
! strided variable put/get routines:
!
      integer nf_put_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_put_vars_text
      integer nf_get_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_get_vars_text
      integer nf_put_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_put_vars_int1
      integer nf_get_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_get_vars_int1
      integer nf_put_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_put_vars_int2
      integer nf_get_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_get_vars_int2
      integer nf_put_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_put_vars_int
      integer nf_get_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_get_vars_int
      integer nf_put_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_put_vars_real
      integer nf_get_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_get_vars_real
      integer nf_put_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_put_vars_double
      integer nf_get_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_get_vars_double
!
! mapped variable put/get routines:
!
      integer nf_put_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_put_varm_text
      integer nf_get_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_get_varm_text
      integer nf_put_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_put_varm_int1
      integer nf_get_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_get_varm_int1
      integer nf_put_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_put_varm_int2
      integer nf_get_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_get_varm_int2
      integer nf_put_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_put_varm_int
      integer nf_get_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_get_varm_int
      integer nf_put_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_put_varm_real
      integer nf_get_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_get_varm_real
      integer nf_put_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_put_varm_double
      integer nf_get_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_get_varm_double
! NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!
!
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil
      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil
      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool
!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble
      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)
!
! masks for the struct nc flag field; passed in as 'mode' arg to
! nccreate and ncopen.
!
! read/write, 0 => readonly
      parameter(ncrdwr = 1)
! in create phase, cleared by ncendef
      parameter(nccreat = 2)
! on create destroy existing file
      parameter(ncexcl = 4)
! in define mode, cleared by ncendef
      parameter(ncindef = 8)
! synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
! synchronise whole header on change (x'20')
      parameter(nchsync = 32)
! numrecs has changed (x'40')
      parameter(ncndirty = 64)
! header info has changed (x'80')
      parameter(nchdirty = 128)
! prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
! do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
! isa link (x'8000')
      parameter(nclink = 32768)
!
! 'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)
!
! 'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)
!
! attribute id to put/get a global attribute
!
      parameter(ncglobal = 0)
!
! advisory maximums:
!
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
! not enforced
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)
!
! global netcdf error status variable
! initialized in error.c
!
! no error
      parameter(ncnoerr = nf_noerr)
! not a netcdf id
      parameter(ncebadid = nf_ebadid)
! too many netcdfs open
      parameter(ncenfile = -31) ! nc_syserr
! netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
! invalid argument
      parameter(nceinval = nf_einval)
! write to read only
      parameter(nceperm = nf_eperm)
! operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine )
! operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
! coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
! maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
! string match to name in use
      parameter(ncename = nf_enameinuse)
! attribute not found
      parameter(ncenoatt = nf_enotatt)
! maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
! not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
! invalid dimension id
      parameter(ncebadd = nf_ebaddim)
! ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
! maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
! variable not found
      parameter(ncenotvr = nf_enotvar)
! action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
! not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)
!
! global options variable. used to determine behavior of error handler.
! initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)
!
! default fill values. these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub
      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   integer, intent(in) :: i1, i2, time
   character (len=*), intent(in) :: file
   logical, intent(in) :: debug
   character (len=*), intent(in) :: var
   integer, intent(out) :: data(i1,i2)
   integer :: cdfid, rcode, id_data
   character (len=80) :: varnam
   integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
   integer :: i, ivtype
   ! if (trace_use) call da_trace_entry("da_get_var_2d_int_cdf")
   cdfid = ncopn(file, NCNOWRIT, rcode)
   if (rcode /= 0) then
      write(unit=stdout, fmt='(2a)') ' error opening netcdf file ', trim(file)
      stop
   end if
   id_data = ncvid(cdfid, var, rcode)
   rcode = nf_inq_var(cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
   if (debug) then
      write(unit=stdout, fmt='(3a,i6)') ' get_var_2d_real_cdf: dims for ',var,' ',ndims
   end if
   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), idims(i))
      if (debug) then
         write(unit=stdout, fmt='(a,2i6)') ' dimension ',i,idims(i)
         write(unit=stdout, fmt='(a,i6)') ' ivtype=', ivtype
         write(unit=stdout, fmt='(a, a)') ' varnam=', trim(varnam)
      end if
   end do
   ! check the dimensions
   if ((i1 /= idims(1)) .or. &
       (i2 /= idims(2)) .or. &
       (time > idims(3)) ) then
      write(unit=stdout,fmt=*) ' error in 2d_var_real read, dimension problem '
      write(unit=stdout,fmt=*) i1, idims(1)
      write(unit=stdout,fmt=*) i2, idims(2)
      write(unit=stdout,fmt=*) time, idims(4)
      write(unit=stdout,fmt=*) ' error stop '
      stop
   end if
   ! get the data
   istart(1) = 1
   iend(1) = i1
   istart(2) = 1
   iend(2) = i2
   istart(3) = time
   iend(3) = 1
   call ncvgt(cdfid,id_data,istart,iend,data,rcode)
   if (debug) then
      write(unit=stdout, fmt='(a, i8)') ' Sample data=', data(1,1)
   end if
   call ncclos(cdfid,rcode)
   ! if (trace_use) call da_trace_exit("da_get_var_2d_int_cdf")
end subroutine da_get_var_2d_int_cdf
subroutine da_put_var_2d_int_cdf(file, var, data, i1, i2, time, debug)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none
! NetCDF-3.
!
! netcdf version 3 fortran interface:
!
!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
!
! default fill values:
!
      integer nf_fill_byte
      integer nf_fill_int1
      integer nf_fill_char
      integer nf_fill_short
      integer nf_fill_int2
      integer nf_fill_int
      real nf_fill_float
      real nf_fill_real
      doubleprecision nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)
!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)
!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc
      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer nf_fatal
      integer nf_verbose
      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)
!
! miscellaneous routines:
!
      character*80 nf_inq_libvers
      external nf_inq_libvers
      character*80 nf_strerror
! (integer ncerr)
      external nf_strerror
      logical nf_issyserr
! (integer ncerr)
      external nf_issyserr
!
! control routines:
!
      integer nf_inq_base_pe
! (integer ncid,
! integer pe)
      external nf_inq_base_pe
      integer nf_set_base_pe
! (integer ncid,
! integer pe)
      external nf_set_base_pe
      integer nf_create
! (character*(*) path,
! integer cmode,
! integer ncid)
      external nf_create
      integer nf__create
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer chunksizehint,
! integer ncid)
      external nf__create
      integer nf__create_mp
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__create_mp
      integer nf_open
! (character*(*) path,
! integer mode,
! integer ncid)
      external nf_open
      integer nf__open
! (character*(*) path,
! integer mode,
! integer chunksizehint,
! integer ncid)
      external nf__open
      integer nf__open_mp
! (character*(*) path,
! integer mode,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__open_mp
      integer nf_set_fill
! (integer ncid,
! integer fillmode,
! integer old_mode)
      external nf_set_fill
      integer nf_set_default_format
! (integer format,
! integer old_format)
      external nf_set_default_format
      integer nf_redef
! (integer ncid)
      external nf_redef
      integer nf_enddef
! (integer ncid)
      external nf_enddef
      integer nf__enddef
! (integer ncid,
! integer h_minfree,
! integer v_align,
! integer v_minfree,
! integer r_align)
      external nf__enddef
      integer nf_sync
! (integer ncid)
      external nf_sync
      integer nf_abort
! (integer ncid)
      external nf_abort
      integer nf_close
! (integer ncid)
      external nf_close
      integer nf_delete
! (character*(*) ncid)
      external nf_delete
!
! general inquiry routines:
!
      integer nf_inq
! (integer ncid,
! integer ndims,
! integer nvars,
! integer ngatts,
! integer unlimdimid)
      external nf_inq
      integer nf_inq_ndims
! (integer ncid,
! integer ndims)
      external nf_inq_ndims
      integer nf_inq_nvars
! (integer ncid,
! integer nvars)
      external nf_inq_nvars
      integer nf_inq_natts
! (integer ncid,
! integer ngatts)
      external nf_inq_natts
      integer nf_inq_unlimdim
! (integer ncid,
! integer unlimdimid)
      external nf_inq_unlimdim
      integer nf_inq_format
! (integer ncid,
! integer format)
      external nf_inq_format
!
! dimension routines:
!
      integer nf_def_dim
! (integer ncid,
! character(*) name,
! integer len,
! integer dimid)
      external nf_def_dim
      integer nf_inq_dimid
! (integer ncid,
! character(*) name,
! integer dimid)
      external nf_inq_dimid
      integer nf_inq_dim
! (integer ncid,
! integer dimid,
! character(*) name,
! integer len)
      external nf_inq_dim
      integer nf_inq_dimname
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_inq_dimname
      integer nf_inq_dimlen
! (integer ncid,
! integer dimid,
! integer len)
      external nf_inq_dimlen
      integer nf_rename_dim
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_rename_dim
!
! general attribute routines:
!
      integer nf_inq_att
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len)
      external nf_inq_att
      integer nf_inq_attid
! (integer ncid,
! integer varid,
! character(*) name,
! integer attnum)
      external nf_inq_attid
      integer nf_inq_atttype
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype)
      external nf_inq_atttype
      integer nf_inq_attlen
! (integer ncid,
! integer varid,
! character(*) name,
! integer len)
      external nf_inq_attlen
      integer nf_inq_attname
! (integer ncid,
! integer varid,
! integer attnum,
! character(*) name)
      external nf_inq_attname
      integer nf_copy_att
! (integer ncid_in,
! integer varid_in,
! character(*) name,
! integer ncid_out,
! integer varid_out)
      external nf_copy_att
      integer nf_rename_att
! (integer ncid,
! integer varid,
! character(*) curname,
! character(*) newname)
      external nf_rename_att
      integer nf_del_att
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_del_att
!
! attribute put/get routines:
!
      integer nf_put_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! integer len,
! character(*) text)
      external nf_put_att_text
      integer nf_get_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! character(*) text)
      external nf_get_att_text
      integer nf_put_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int1_t i1vals(1))
      external nf_put_att_int1
      integer nf_get_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int1_t i1vals(1))
      external nf_get_att_int1
      integer nf_put_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int2_t i2vals(1))
      external nf_put_att_int2
      integer nf_get_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int2_t i2vals(1))
      external nf_get_att_int2
      integer nf_put_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! integer ivals(1))
      external nf_put_att_int
      integer nf_get_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer ivals(1))
      external nf_get_att_int
      integer nf_put_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! real rvals(1))
      external nf_put_att_real
      integer nf_get_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! real rvals(1))
      external nf_get_att_real
      integer nf_put_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! double dvals(1))
      external nf_put_att_double
      integer nf_get_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! double dvals(1))
      external nf_get_att_double
!
! general variable routines:
!
      integer nf_def_var
! (integer ncid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer varid)
      external nf_def_var
      integer nf_inq_var
! (integer ncid,
! integer varid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer natts)
      external nf_inq_var
      integer nf_inq_varid
! (integer ncid,
! character(*) name,
! integer varid)
      external nf_inq_varid
      integer nf_inq_varname
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_inq_varname
      integer nf_inq_vartype
! (integer ncid,
! integer varid,
! integer xtype)
      external nf_inq_vartype
      integer nf_inq_varndims
! (integer ncid,
! integer varid,
! integer ndims)
      external nf_inq_varndims
      integer nf_inq_vardimid
! (integer ncid,
! integer varid,
! integer dimids(1))
      external nf_inq_vardimid
      integer nf_inq_varnatts
! (integer ncid,
! integer varid,
! integer natts)
      external nf_inq_varnatts
      integer nf_rename_var
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_rename_var
      integer nf_copy_var
! (integer ncid_in,
! integer varid,
! integer ncid_out)
      external nf_copy_var
!
! entire variable put/get routines:
!
      integer nf_put_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_put_var_text
      integer nf_get_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_get_var_text
      integer nf_put_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_put_var_int1
      integer nf_get_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_get_var_int1
      integer nf_put_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_put_var_int2
      integer nf_get_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_get_var_int2
      integer nf_put_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_put_var_int
      integer nf_get_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_get_var_int
      integer nf_put_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_put_var_real
      integer nf_get_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_get_var_real
      integer nf_put_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_put_var_double
      integer nf_get_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_get_var_double
!
! single variable put/get routines:
!
      integer nf_put_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_put_var1_text
      integer nf_get_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_get_var1_text
      integer nf_put_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_put_var1_int1
      integer nf_get_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_get_var1_int1
      integer nf_put_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_put_var1_int2
      integer nf_get_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_get_var1_int2
      integer nf_put_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_put_var1_int
      integer nf_get_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_get_var1_int
      integer nf_put_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_put_var1_real
      integer nf_get_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_get_var1_real
      integer nf_put_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_put_var1_double
      integer nf_get_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_get_var1_double
!
! variable array put/get routines:
!
      integer nf_put_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_put_vara_text
      integer nf_get_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_get_vara_text
      integer nf_put_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_put_vara_int1
      integer nf_get_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_get_vara_int1
      integer nf_put_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_put_vara_int2
      integer nf_get_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_get_vara_int2
      integer nf_put_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_put_vara_int
      integer nf_get_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_get_vara_int
      integer nf_put_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_put_vara_real
      integer nf_get_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_get_vara_real
      integer nf_put_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_put_vara_double
      integer nf_get_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_get_vara_double
!
! strided variable put/get routines:
!
      integer nf_put_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_put_vars_text
      integer nf_get_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_get_vars_text
      integer nf_put_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_put_vars_int1
      integer nf_get_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_get_vars_int1
      integer nf_put_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_put_vars_int2
      integer nf_get_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_get_vars_int2
      integer nf_put_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_put_vars_int
      integer nf_get_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_get_vars_int
      integer nf_put_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_put_vars_real
      integer nf_get_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_get_vars_real
      integer nf_put_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_put_vars_double
      integer nf_get_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_get_vars_double
!
! mapped variable put/get routines:
!
      integer nf_put_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_put_varm_text
      integer nf_get_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_get_varm_text
      integer nf_put_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_put_varm_int1
      integer nf_get_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_get_varm_int1
      integer nf_put_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_put_varm_int2
      integer nf_get_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_get_varm_int2
      integer nf_put_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_put_varm_int
      integer nf_get_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_get_varm_int
      integer nf_put_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_put_varm_real
      integer nf_get_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_get_varm_real
      integer nf_put_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_put_varm_double
      integer nf_get_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_get_varm_double
! NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!
!
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil
      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil
      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool
!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble
      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)
!
! masks for the struct nc flag field; passed in as 'mode' arg to
! nccreate and ncopen.
!
! read/write, 0 => readonly
      parameter(ncrdwr = 1)
! in create phase, cleared by ncendef
      parameter(nccreat = 2)
! on create destroy existing file
      parameter(ncexcl = 4)
! in define mode, cleared by ncendef
      parameter(ncindef = 8)
! synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
! synchronise whole header on change (x'20')
      parameter(nchsync = 32)
! numrecs has changed (x'40')
      parameter(ncndirty = 64)
! header info has changed (x'80')
      parameter(nchdirty = 128)
! prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
! do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
! isa link (x'8000')
      parameter(nclink = 32768)
!
! 'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)
!
! 'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)
!
! attribute id to put/get a global attribute
!
      parameter(ncglobal = 0)
!
! advisory maximums:
!
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
! not enforced
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)
!
! global netcdf error status variable
! initialized in error.c
!
! no error
      parameter(ncnoerr = nf_noerr)
! not a netcdf id
      parameter(ncebadid = nf_ebadid)
! too many netcdfs open
      parameter(ncenfile = -31) ! nc_syserr
! netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
! invalid argument
      parameter(nceinval = nf_einval)
! write to read only
      parameter(nceperm = nf_eperm)
! operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine )
! operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
! coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
! maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
! string match to name in use
      parameter(ncename = nf_enameinuse)
! attribute not found
      parameter(ncenoatt = nf_enotatt)
! maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
! not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
! invalid dimension id
      parameter(ncebadd = nf_ebaddim)
! ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
! maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
! variable not found
      parameter(ncenotvr = nf_enotvar)
! action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
! not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)
!
! global options variable. used to determine behavior of error handler.
! initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)
!
! default fill values. these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub
      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   integer, intent(in) :: i1, i2, time
   character (len=*), intent(in) :: file
   logical, intent(in) :: debug
   character (len=*), intent(in) :: var
   integer, intent(out) :: data(i1,i2)
   integer :: cdfid, rcode, id_data
   character (len=80) :: varnam
   integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
   integer :: i, ivtype
   ! if (trace_use) call da_trace_entry("da_put_var_2d_int_cdf")
   cdfid = ncopn(file, NCWRITE, rcode)
   if ( rcode == 0 ) then
      if (debug) write(unit=stdout,fmt='(2a)') ' open netcdf file ', trim(file)
   else
      write(unit=stdout, fmt='(2a)') ' error opening netcdf file ', trim(file)
      stop
   end if
   id_data = ncvid(cdfid, var, rcode)
   rcode = nf_inq_var(cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
   if (debug) then
      write(unit=stdout, fmt='(3a,i6)') ' number of dims for ',var,' ',ndims
   end if
   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), idims(i))
      if (debug) then
         write(unit=stdout, fmt='(a,2i6)') ' dimension ',i,idims(i)
         write(unit=stdout, fmt='(a,i6)') ' ivtype=', ivtype
         write(unit=stdout, fmt='(a, a)') ' varnam=', trim(varnam)
      end if
   end do
   ! check the dimensions
   if ((i1 /= idims(1)) .or. &
       (i2 /= idims(2)) .or. &
       (time > idims(3)) ) then
      write(unit=stdout,fmt=*) ' error in 2d_var_int read, dimension problem '
      write(unit=stdout,fmt=*) i1, idims(1)
      write(unit=stdout,fmt=*) i2, idims(2)
      write(unit=stdout,fmt=*) time, idims(4)
      write(unit=stdout,fmt=*) ' error stop '
      stop
   end if
   ! get the data
   istart(1) = 1
   iend(1) = i1
   istart(2) = 1
   iend(2) = i2
   istart(3) = time
   iend(3) = 1
   call ncvpt(cdfid,id_data,istart,iend,data,rcode)
   call ncclos(cdfid,rcode)
   ! if (trace_use) call da_trace_exit("da_put_var_2d_int_cdf")
end subroutine da_put_var_2d_int_cdf
subroutine da_get_att_cdf(file, var, debug)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none
! NetCDF-3.
!
! netcdf version 3 fortran interface:
!
!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
!
! default fill values:
!
      integer nf_fill_byte
      integer nf_fill_int1
      integer nf_fill_char
      integer nf_fill_short
      integer nf_fill_int2
      integer nf_fill_int
      real nf_fill_float
      real nf_fill_real
      doubleprecision nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)
!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)
!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc
      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer nf_fatal
      integer nf_verbose
      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)
!
! miscellaneous routines:
!
      character*80 nf_inq_libvers
      external nf_inq_libvers
      character*80 nf_strerror
! (integer ncerr)
      external nf_strerror
      logical nf_issyserr
! (integer ncerr)
      external nf_issyserr
!
! control routines:
!
      integer nf_inq_base_pe
! (integer ncid,
! integer pe)
      external nf_inq_base_pe
      integer nf_set_base_pe
! (integer ncid,
! integer pe)
      external nf_set_base_pe
      integer nf_create
! (character*(*) path,
! integer cmode,
! integer ncid)
      external nf_create
      integer nf__create
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer chunksizehint,
! integer ncid)
      external nf__create
      integer nf__create_mp
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__create_mp
      integer nf_open
! (character*(*) path,
! integer mode,
! integer ncid)
      external nf_open
      integer nf__open
! (character*(*) path,
! integer mode,
! integer chunksizehint,
! integer ncid)
      external nf__open
      integer nf__open_mp
! (character*(*) path,
! integer mode,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__open_mp
      integer nf_set_fill
! (integer ncid,
! integer fillmode,
! integer old_mode)
      external nf_set_fill
      integer nf_set_default_format
! (integer format,
! integer old_format)
      external nf_set_default_format
      integer nf_redef
! (integer ncid)
      external nf_redef
      integer nf_enddef
! (integer ncid)
      external nf_enddef
      integer nf__enddef
! (integer ncid,
! integer h_minfree,
! integer v_align,
! integer v_minfree,
! integer r_align)
      external nf__enddef
      integer nf_sync
! (integer ncid)
      external nf_sync
      integer nf_abort
! (integer ncid)
      external nf_abort
      integer nf_close
! (integer ncid)
      external nf_close
      integer nf_delete
! (character*(*) ncid)
      external nf_delete
!
! general inquiry routines:
!
      integer nf_inq
! (integer ncid,
! integer ndims,
! integer nvars,
! integer ngatts,
! integer unlimdimid)
      external nf_inq
      integer nf_inq_ndims
! (integer ncid,
! integer ndims)
      external nf_inq_ndims
      integer nf_inq_nvars
! (integer ncid,
! integer nvars)
      external nf_inq_nvars
      integer nf_inq_natts
! (integer ncid,
! integer ngatts)
      external nf_inq_natts
      integer nf_inq_unlimdim
! (integer ncid,
! integer unlimdimid)
      external nf_inq_unlimdim
      integer nf_inq_format
! (integer ncid,
! integer format)
      external nf_inq_format
!
! dimension routines:
!
      integer nf_def_dim
! (integer ncid,
! character(*) name,
! integer len,
! integer dimid)
      external nf_def_dim
      integer nf_inq_dimid
! (integer ncid,
! character(*) name,
! integer dimid)
      external nf_inq_dimid
      integer nf_inq_dim
! (integer ncid,
! integer dimid,
! character(*) name,
! integer len)
      external nf_inq_dim
      integer nf_inq_dimname
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_inq_dimname
      integer nf_inq_dimlen
! (integer ncid,
! integer dimid,
! integer len)
      external nf_inq_dimlen
      integer nf_rename_dim
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_rename_dim
!
! general attribute routines:
!
      integer nf_inq_att
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len)
      external nf_inq_att
      integer nf_inq_attid
! (integer ncid,
! integer varid,
! character(*) name,
! integer attnum)
      external nf_inq_attid
      integer nf_inq_atttype
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype)
      external nf_inq_atttype
      integer nf_inq_attlen
! (integer ncid,
! integer varid,
! character(*) name,
! integer len)
      external nf_inq_attlen
      integer nf_inq_attname
! (integer ncid,
! integer varid,
! integer attnum,
! character(*) name)
      external nf_inq_attname
      integer nf_copy_att
! (integer ncid_in,
! integer varid_in,
! character(*) name,
! integer ncid_out,
! integer varid_out)
      external nf_copy_att
      integer nf_rename_att
! (integer ncid,
! integer varid,
! character(*) curname,
! character(*) newname)
      external nf_rename_att
      integer nf_del_att
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_del_att
!
! attribute put/get routines:
!
      integer nf_put_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! integer len,
! character(*) text)
      external nf_put_att_text
      integer nf_get_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! character(*) text)
      external nf_get_att_text
      integer nf_put_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int1_t i1vals(1))
      external nf_put_att_int1
      integer nf_get_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int1_t i1vals(1))
      external nf_get_att_int1
      integer nf_put_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int2_t i2vals(1))
      external nf_put_att_int2
      integer nf_get_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int2_t i2vals(1))
      external nf_get_att_int2
      integer nf_put_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! integer ivals(1))
      external nf_put_att_int
      integer nf_get_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer ivals(1))
      external nf_get_att_int
      integer nf_put_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! real rvals(1))
      external nf_put_att_real
      integer nf_get_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! real rvals(1))
      external nf_get_att_real
      integer nf_put_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! double dvals(1))
      external nf_put_att_double
      integer nf_get_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! double dvals(1))
      external nf_get_att_double
!
! general variable routines:
!
      integer nf_def_var
! (integer ncid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer varid)
      external nf_def_var
      integer nf_inq_var
! (integer ncid,
! integer varid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer natts)
      external nf_inq_var
      integer nf_inq_varid
! (integer ncid,
! character(*) name,
! integer varid)
      external nf_inq_varid
      integer nf_inq_varname
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_inq_varname
      integer nf_inq_vartype
! (integer ncid,
! integer varid,
! integer xtype)
      external nf_inq_vartype
      integer nf_inq_varndims
! (integer ncid,
! integer varid,
! integer ndims)
      external nf_inq_varndims
      integer nf_inq_vardimid
! (integer ncid,
! integer varid,
! integer dimids(1))
      external nf_inq_vardimid
      integer nf_inq_varnatts
! (integer ncid,
! integer varid,
! integer natts)
      external nf_inq_varnatts
      integer nf_rename_var
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_rename_var
      integer nf_copy_var
! (integer ncid_in,
! integer varid,
! integer ncid_out)
      external nf_copy_var
!
! entire variable put/get routines:
!
      integer nf_put_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_put_var_text
      integer nf_get_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_get_var_text
      integer nf_put_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_put_var_int1
      integer nf_get_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_get_var_int1
      integer nf_put_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_put_var_int2
      integer nf_get_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_get_var_int2
      integer nf_put_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_put_var_int
      integer nf_get_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_get_var_int
      integer nf_put_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_put_var_real
      integer nf_get_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_get_var_real
      integer nf_put_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_put_var_double
      integer nf_get_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_get_var_double
!
! single variable put/get routines:
!
      integer nf_put_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_put_var1_text
      integer nf_get_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_get_var1_text
      integer nf_put_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_put_var1_int1
      integer nf_get_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_get_var1_int1
      integer nf_put_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_put_var1_int2
      integer nf_get_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_get_var1_int2
      integer nf_put_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_put_var1_int
      integer nf_get_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_get_var1_int
      integer nf_put_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_put_var1_real
      integer nf_get_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_get_var1_real
      integer nf_put_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_put_var1_double
      integer nf_get_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_get_var1_double
!
! variable array put/get routines:
!
      integer nf_put_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_put_vara_text
      integer nf_get_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_get_vara_text
      integer nf_put_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_put_vara_int1
      integer nf_get_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_get_vara_int1
      integer nf_put_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_put_vara_int2
      integer nf_get_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_get_vara_int2
      integer nf_put_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_put_vara_int
      integer nf_get_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_get_vara_int
      integer nf_put_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_put_vara_real
      integer nf_get_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_get_vara_real
      integer nf_put_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_put_vara_double
      integer nf_get_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_get_vara_double
!
! strided variable put/get routines:
!
      integer nf_put_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_put_vars_text
      integer nf_get_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_get_vars_text
      integer nf_put_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_put_vars_int1
      integer nf_get_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_get_vars_int1
      integer nf_put_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_put_vars_int2
      integer nf_get_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_get_vars_int2
      integer nf_put_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_put_vars_int
      integer nf_get_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_get_vars_int
      integer nf_put_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_put_vars_real
      integer nf_get_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_get_vars_real
      integer nf_put_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_put_vars_double
      integer nf_get_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_get_vars_double
!
! mapped variable put/get routines:
!
      integer nf_put_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_put_varm_text
      integer nf_get_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_get_varm_text
      integer nf_put_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_put_varm_int1
      integer nf_get_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_get_varm_int1
      integer nf_put_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_put_varm_int2
      integer nf_get_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_get_varm_int2
      integer nf_put_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_put_varm_int
      integer nf_get_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_get_varm_int
      integer nf_put_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_put_varm_real
      integer nf_get_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_get_varm_real
      integer nf_put_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_put_varm_double
      integer nf_get_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_get_varm_double
! NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!
!
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil
      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil
      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool
!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble
      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)
!
! masks for the struct nc flag field; passed in as 'mode' arg to
! nccreate and ncopen.
!
! read/write, 0 => readonly
      parameter(ncrdwr = 1)
! in create phase, cleared by ncendef
      parameter(nccreat = 2)
! on create destroy existing file
      parameter(ncexcl = 4)
! in define mode, cleared by ncendef
      parameter(ncindef = 8)
! synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
! synchronise whole header on change (x'20')
      parameter(nchsync = 32)
! numrecs has changed (x'40')
      parameter(ncndirty = 64)
! header info has changed (x'80')
      parameter(nchdirty = 128)
! prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
! do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
! isa link (x'8000')
      parameter(nclink = 32768)
!
! 'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)
!
! 'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)
!
! attribute id to put/get a global attribute
!
      parameter(ncglobal = 0)
!
! advisory maximums:
!
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
! not enforced
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)
!
! global netcdf error status variable
! initialized in error.c
!
! no error
      parameter(ncnoerr = nf_noerr)
! not a netcdf id
      parameter(ncebadid = nf_ebadid)
! too many netcdfs open
      parameter(ncenfile = -31) ! nc_syserr
! netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
! invalid argument
      parameter(nceinval = nf_einval)
! write to read only
      parameter(nceperm = nf_eperm)
! operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine )
! operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
! coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
! maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
! string match to name in use
      parameter(ncename = nf_enameinuse)
! attribute not found
      parameter(ncenoatt = nf_enotatt)
! maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
! not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
! invalid dimension id
      parameter(ncebadd = nf_ebaddim)
! ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
! maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
! variable not found
      parameter(ncenotvr = nf_enotvar)
! action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
! not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)
!
! global options variable. used to determine behavior of error handler.
! initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)
!
! default fill values. these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub
      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   character (len=*), intent(in) :: file
   character (len=*), intent(in) :: var
   logical, intent(in) :: debug
   integer :: cdfid, status, varid, n, natts
   character (len=256) :: att_name
   ! if (trace_use) call da_trace_entry("da_get_att_cdf")
   status = NF_OPEN(file, NF_NOWRITE, cdfid)
   status = NF_inQ_VARID(cdfid, var, varid)
   if (status == 0) then
      if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
      write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
      stop
   end if
   status = NF_inQ_VARNATTS(cdfid, varid, natts)
   do n=1, natts
      status = NF_inQ_ATTNAME(cdfid, varid, n, att_name)
      write(unit=stdout, fmt='(a,i2,2a)') &
        'att_name(',n,')=', trim(att_name)
   end do
   status = NF_close(cdfid)
   ! if (trace_use) call da_trace_exit("da_get_att_cdf")
end subroutine da_get_att_cdf
subroutine da_put_att_cdf(file, var, att_name, text, debug)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none
! NetCDF-3.
!
! netcdf version 3 fortran interface:
!
!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
!
! default fill values:
!
      integer nf_fill_byte
      integer nf_fill_int1
      integer nf_fill_char
      integer nf_fill_short
      integer nf_fill_int2
      integer nf_fill_int
      real nf_fill_float
      real nf_fill_real
      doubleprecision nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)
!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)
!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc
      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer nf_fatal
      integer nf_verbose
      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)
!
! miscellaneous routines:
!
      character*80 nf_inq_libvers
      external nf_inq_libvers
      character*80 nf_strerror
! (integer ncerr)
      external nf_strerror
      logical nf_issyserr
! (integer ncerr)
      external nf_issyserr
!
! control routines:
!
      integer nf_inq_base_pe
! (integer ncid,
! integer pe)
      external nf_inq_base_pe
      integer nf_set_base_pe
! (integer ncid,
! integer pe)
      external nf_set_base_pe
      integer nf_create
! (character*(*) path,
! integer cmode,
! integer ncid)
      external nf_create
      integer nf__create
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer chunksizehint,
! integer ncid)
      external nf__create
      integer nf__create_mp
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__create_mp
      integer nf_open
! (character*(*) path,
! integer mode,
! integer ncid)
      external nf_open
      integer nf__open
! (character*(*) path,
! integer mode,
! integer chunksizehint,
! integer ncid)
      external nf__open
      integer nf__open_mp
! (character*(*) path,
! integer mode,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__open_mp
      integer nf_set_fill
! (integer ncid,
! integer fillmode,
! integer old_mode)
      external nf_set_fill
      integer nf_set_default_format
! (integer format,
! integer old_format)
      external nf_set_default_format
      integer nf_redef
! (integer ncid)
      external nf_redef
      integer nf_enddef
! (integer ncid)
      external nf_enddef
      integer nf__enddef
! (integer ncid,
! integer h_minfree,
! integer v_align,
! integer v_minfree,
! integer r_align)
      external nf__enddef
      integer nf_sync
! (integer ncid)
      external nf_sync
      integer nf_abort
! (integer ncid)
      external nf_abort
      integer nf_close
! (integer ncid)
      external nf_close
      integer nf_delete
! (character*(*) ncid)
      external nf_delete
!
! general inquiry routines:
!
      integer nf_inq
! (integer ncid,
! integer ndims,
! integer nvars,
! integer ngatts,
! integer unlimdimid)
      external nf_inq
      integer nf_inq_ndims
! (integer ncid,
! integer ndims)
      external nf_inq_ndims
      integer nf_inq_nvars
! (integer ncid,
! integer nvars)
      external nf_inq_nvars
      integer nf_inq_natts
! (integer ncid,
! integer ngatts)
      external nf_inq_natts
      integer nf_inq_unlimdim
! (integer ncid,
! integer unlimdimid)
      external nf_inq_unlimdim
      integer nf_inq_format
! (integer ncid,
! integer format)
      external nf_inq_format
!
! dimension routines:
!
      integer nf_def_dim
! (integer ncid,
! character(*) name,
! integer len,
! integer dimid)
      external nf_def_dim
      integer nf_inq_dimid
! (integer ncid,
! character(*) name,
! integer dimid)
      external nf_inq_dimid
      integer nf_inq_dim
! (integer ncid,
! integer dimid,
! character(*) name,
! integer len)
      external nf_inq_dim
      integer nf_inq_dimname
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_inq_dimname
      integer nf_inq_dimlen
! (integer ncid,
! integer dimid,
! integer len)
      external nf_inq_dimlen
      integer nf_rename_dim
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_rename_dim
!
! general attribute routines:
!
      integer nf_inq_att
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len)
      external nf_inq_att
      integer nf_inq_attid
! (integer ncid,
! integer varid,
! character(*) name,
! integer attnum)
      external nf_inq_attid
      integer nf_inq_atttype
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype)
      external nf_inq_atttype
      integer nf_inq_attlen
! (integer ncid,
! integer varid,
! character(*) name,
! integer len)
      external nf_inq_attlen
      integer nf_inq_attname
! (integer ncid,
! integer varid,
! integer attnum,
! character(*) name)
      external nf_inq_attname
      integer nf_copy_att
! (integer ncid_in,
! integer varid_in,
! character(*) name,
! integer ncid_out,
! integer varid_out)
      external nf_copy_att
      integer nf_rename_att
! (integer ncid,
! integer varid,
! character(*) curname,
! character(*) newname)
      external nf_rename_att
      integer nf_del_att
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_del_att
!
! attribute put/get routines:
!
      integer nf_put_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! integer len,
! character(*) text)
      external nf_put_att_text
      integer nf_get_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! character(*) text)
      external nf_get_att_text
      integer nf_put_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int1_t i1vals(1))
      external nf_put_att_int1
      integer nf_get_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int1_t i1vals(1))
      external nf_get_att_int1
      integer nf_put_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int2_t i2vals(1))
      external nf_put_att_int2
      integer nf_get_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int2_t i2vals(1))
      external nf_get_att_int2
      integer nf_put_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! integer ivals(1))
      external nf_put_att_int
      integer nf_get_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer ivals(1))
      external nf_get_att_int
      integer nf_put_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! real rvals(1))
      external nf_put_att_real
      integer nf_get_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! real rvals(1))
      external nf_get_att_real
      integer nf_put_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! double dvals(1))
      external nf_put_att_double
      integer nf_get_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! double dvals(1))
      external nf_get_att_double
!
! general variable routines:
!
      integer nf_def_var
! (integer ncid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer varid)
      external nf_def_var
      integer nf_inq_var
! (integer ncid,
! integer varid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer natts)
      external nf_inq_var
      integer nf_inq_varid
! (integer ncid,
! character(*) name,
! integer varid)
      external nf_inq_varid
      integer nf_inq_varname
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_inq_varname
      integer nf_inq_vartype
! (integer ncid,
! integer varid,
! integer xtype)
      external nf_inq_vartype
      integer nf_inq_varndims
! (integer ncid,
! integer varid,
! integer ndims)
      external nf_inq_varndims
      integer nf_inq_vardimid
! (integer ncid,
! integer varid,
! integer dimids(1))
      external nf_inq_vardimid
      integer nf_inq_varnatts
! (integer ncid,
! integer varid,
! integer natts)
      external nf_inq_varnatts
      integer nf_rename_var
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_rename_var
      integer nf_copy_var
! (integer ncid_in,
! integer varid,
! integer ncid_out)
      external nf_copy_var
!
! entire variable put/get routines:
!
      integer nf_put_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_put_var_text
      integer nf_get_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_get_var_text
      integer nf_put_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_put_var_int1
      integer nf_get_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_get_var_int1
      integer nf_put_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_put_var_int2
      integer nf_get_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_get_var_int2
      integer nf_put_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_put_var_int
      integer nf_get_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_get_var_int
      integer nf_put_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_put_var_real
      integer nf_get_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_get_var_real
      integer nf_put_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_put_var_double
      integer nf_get_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_get_var_double
!
! single variable put/get routines:
!
      integer nf_put_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_put_var1_text
      integer nf_get_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_get_var1_text
      integer nf_put_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_put_var1_int1
      integer nf_get_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_get_var1_int1
      integer nf_put_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_put_var1_int2
      integer nf_get_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_get_var1_int2
      integer nf_put_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_put_var1_int
      integer nf_get_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_get_var1_int
      integer nf_put_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_put_var1_real
      integer nf_get_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_get_var1_real
      integer nf_put_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_put_var1_double
      integer nf_get_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_get_var1_double
!
! variable array put/get routines:
!
      integer nf_put_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_put_vara_text
      integer nf_get_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_get_vara_text
      integer nf_put_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_put_vara_int1
      integer nf_get_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_get_vara_int1
      integer nf_put_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_put_vara_int2
      integer nf_get_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_get_vara_int2
      integer nf_put_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_put_vara_int
      integer nf_get_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_get_vara_int
      integer nf_put_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_put_vara_real
      integer nf_get_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_get_vara_real
      integer nf_put_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_put_vara_double
      integer nf_get_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_get_vara_double
!
! strided variable put/get routines:
!
      integer nf_put_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_put_vars_text
      integer nf_get_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_get_vars_text
      integer nf_put_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_put_vars_int1
      integer nf_get_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_get_vars_int1
      integer nf_put_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_put_vars_int2
      integer nf_get_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_get_vars_int2
      integer nf_put_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_put_vars_int
      integer nf_get_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_get_vars_int
      integer nf_put_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_put_vars_real
      integer nf_get_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_get_vars_real
      integer nf_put_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_put_vars_double
      integer nf_get_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_get_vars_double
!
! mapped variable put/get routines:
!
      integer nf_put_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_put_varm_text
      integer nf_get_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_get_varm_text
      integer nf_put_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_put_varm_int1
      integer nf_get_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_get_varm_int1
      integer nf_put_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_put_varm_int2
      integer nf_get_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_get_varm_int2
      integer nf_put_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_put_varm_int
      integer nf_get_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_get_varm_int
      integer nf_put_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_put_varm_real
      integer nf_get_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_get_varm_real
      integer nf_put_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_put_varm_double
      integer nf_get_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_get_varm_double
! NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!
!
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil
      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil
      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool
!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble
      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)
!
! masks for the struct nc flag field; passed in as 'mode' arg to
! nccreate and ncopen.
!
! read/write, 0 => readonly
      parameter(ncrdwr = 1)
! in create phase, cleared by ncendef
      parameter(nccreat = 2)
! on create destroy existing file
      parameter(ncexcl = 4)
! in define mode, cleared by ncendef
      parameter(ncindef = 8)
! synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
! synchronise whole header on change (x'20')
      parameter(nchsync = 32)
! numrecs has changed (x'40')
      parameter(ncndirty = 64)
! header info has changed (x'80')
      parameter(nchdirty = 128)
! prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
! do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
! isa link (x'8000')
      parameter(nclink = 32768)
!
! 'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)
!
! 'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)
!
! attribute id to put/get a global attribute
!
      parameter(ncglobal = 0)
!
! advisory maximums:
!
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
! not enforced
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)
!
! global netcdf error status variable
! initialized in error.c
!
! no error
      parameter(ncnoerr = nf_noerr)
! not a netcdf id
      parameter(ncebadid = nf_ebadid)
! too many netcdfs open
      parameter(ncenfile = -31) ! nc_syserr
! netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
! invalid argument
      parameter(nceinval = nf_einval)
! write to read only
      parameter(nceperm = nf_eperm)
! operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine )
! operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
! coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
! maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
! string match to name in use
      parameter(ncename = nf_enameinuse)
! attribute not found
      parameter(ncenoatt = nf_enotatt)
! maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
! not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
! invalid dimension id
      parameter(ncebadd = nf_ebaddim)
! ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
! maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
! variable not found
      parameter(ncenotvr = nf_enotvar)
! action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
! not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)
!
! global options variable. used to determine behavior of error handler.
! initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)
!
! default fill values. these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub
      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   character (len=*), intent(in) :: file
   character (len=*), intent(in) :: var, att_name, text
   logical, intent(in) :: debug
   integer :: cdfid, status, varid, n, natts
   character (len=256) :: loc_att_name
   ! not for update_bc
   ! if (trace_use) call da_trace_entry("da_put_att_cdf")
   status = NF_OPEN(file, NF_WRITE, cdfid)
   if (status == 0) then
      if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
      write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
      stop
   end if
   status = NF_inQ_VARID(cdfid, var, varid)
   status = NF_inQ_VARNATTS(cdfid, varid, natts)
   do n=1, natts
      status = NF_inQ_ATTNAME(cdfid, varid, n, loc_att_name)
      write(unit=stdout, fmt='(a,i2,2a)') &
        'loc_att_name(',n,')=', trim(loc_att_name)
      if (trim(loc_att_name) == trim(att_name)) then
         write(unit=stdout, fmt='(2a)') &
           'att_name=', trim(att_name)
         status = NF_PUT_ATT_TEXT(cdfid, varid, trim(att_name), len(text), trim(text))
         if (status == 0) then
            if (debug) then
               write(unit=stdout, fmt='(4a)') &
                    'write ', trim(att_name), 'to netcdf file ', trim(file)
            end if
         else
            write(unit=stdout, fmt='(a, i8)') &
                   'Status= ', status
            write(unit=stdout, fmt='(4a)') &
                   'Failed to write ', trim(att_name), 'to netcdf file ', trim(file)
            ! if (status /= NF_NOERR) call handle_err(status)
            stop
         end if
         exit
      end if
   end do
   status = NF_close(cdfid)
   !if (trace_use) call da_trace_exit("da_put_att_cdf")
end subroutine da_put_att_cdf
subroutine da_get_bdyfrq(this_datestr, next_datestr, bdyfrq, debug)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none
! NetCDF-3.
!
! netcdf version 3 fortran interface:
!
!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
!
! default fill values:
!
      integer nf_fill_byte
      integer nf_fill_int1
      integer nf_fill_char
      integer nf_fill_short
      integer nf_fill_int2
      integer nf_fill_int
      real nf_fill_float
      real nf_fill_real
      doubleprecision nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)
!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)
!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc
      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer nf_fatal
      integer nf_verbose
      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)
!
! miscellaneous routines:
!
      character*80 nf_inq_libvers
      external nf_inq_libvers
      character*80 nf_strerror
! (integer ncerr)
      external nf_strerror
      logical nf_issyserr
! (integer ncerr)
      external nf_issyserr
!
! control routines:
!
      integer nf_inq_base_pe
! (integer ncid,
! integer pe)
      external nf_inq_base_pe
      integer nf_set_base_pe
! (integer ncid,
! integer pe)
      external nf_set_base_pe
      integer nf_create
! (character*(*) path,
! integer cmode,
! integer ncid)
      external nf_create
      integer nf__create
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer chunksizehint,
! integer ncid)
      external nf__create
      integer nf__create_mp
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__create_mp
      integer nf_open
! (character*(*) path,
! integer mode,
! integer ncid)
      external nf_open
      integer nf__open
! (character*(*) path,
! integer mode,
! integer chunksizehint,
! integer ncid)
      external nf__open
      integer nf__open_mp
! (character*(*) path,
! integer mode,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__open_mp
      integer nf_set_fill
! (integer ncid,
! integer fillmode,
! integer old_mode)
      external nf_set_fill
      integer nf_set_default_format
! (integer format,
! integer old_format)
      external nf_set_default_format
      integer nf_redef
! (integer ncid)
      external nf_redef
      integer nf_enddef
! (integer ncid)
      external nf_enddef
      integer nf__enddef
! (integer ncid,
! integer h_minfree,
! integer v_align,
! integer v_minfree,
! integer r_align)
      external nf__enddef
      integer nf_sync
! (integer ncid)
      external nf_sync
      integer nf_abort
! (integer ncid)
      external nf_abort
      integer nf_close
! (integer ncid)
      external nf_close
      integer nf_delete
! (character*(*) ncid)
      external nf_delete
!
! general inquiry routines:
!
      integer nf_inq
! (integer ncid,
! integer ndims,
! integer nvars,
! integer ngatts,
! integer unlimdimid)
      external nf_inq
      integer nf_inq_ndims
! (integer ncid,
! integer ndims)
      external nf_inq_ndims
      integer nf_inq_nvars
! (integer ncid,
! integer nvars)
      external nf_inq_nvars
      integer nf_inq_natts
! (integer ncid,
! integer ngatts)
      external nf_inq_natts
      integer nf_inq_unlimdim
! (integer ncid,
! integer unlimdimid)
      external nf_inq_unlimdim
      integer nf_inq_format
! (integer ncid,
! integer format)
      external nf_inq_format
!
! dimension routines:
!
      integer nf_def_dim
! (integer ncid,
! character(*) name,
! integer len,
! integer dimid)
      external nf_def_dim
      integer nf_inq_dimid
! (integer ncid,
! character(*) name,
! integer dimid)
      external nf_inq_dimid
      integer nf_inq_dim
! (integer ncid,
! integer dimid,
! character(*) name,
! integer len)
      external nf_inq_dim
      integer nf_inq_dimname
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_inq_dimname
      integer nf_inq_dimlen
! (integer ncid,
! integer dimid,
! integer len)
      external nf_inq_dimlen
      integer nf_rename_dim
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_rename_dim
!
! general attribute routines:
!
      integer nf_inq_att
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len)
      external nf_inq_att
      integer nf_inq_attid
! (integer ncid,
! integer varid,
! character(*) name,
! integer attnum)
      external nf_inq_attid
      integer nf_inq_atttype
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype)
      external nf_inq_atttype
      integer nf_inq_attlen
! (integer ncid,
! integer varid,
! character(*) name,
! integer len)
      external nf_inq_attlen
      integer nf_inq_attname
! (integer ncid,
! integer varid,
! integer attnum,
! character(*) name)
      external nf_inq_attname
      integer nf_copy_att
! (integer ncid_in,
! integer varid_in,
! character(*) name,
! integer ncid_out,
! integer varid_out)
      external nf_copy_att
      integer nf_rename_att
! (integer ncid,
! integer varid,
! character(*) curname,
! character(*) newname)
      external nf_rename_att
      integer nf_del_att
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_del_att
!
! attribute put/get routines:
!
      integer nf_put_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! integer len,
! character(*) text)
      external nf_put_att_text
      integer nf_get_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! character(*) text)
      external nf_get_att_text
      integer nf_put_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int1_t i1vals(1))
      external nf_put_att_int1
      integer nf_get_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int1_t i1vals(1))
      external nf_get_att_int1
      integer nf_put_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int2_t i2vals(1))
      external nf_put_att_int2
      integer nf_get_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int2_t i2vals(1))
      external nf_get_att_int2
      integer nf_put_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! integer ivals(1))
      external nf_put_att_int
      integer nf_get_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer ivals(1))
      external nf_get_att_int
      integer nf_put_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! real rvals(1))
      external nf_put_att_real
      integer nf_get_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! real rvals(1))
      external nf_get_att_real
      integer nf_put_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! double dvals(1))
      external nf_put_att_double
      integer nf_get_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! double dvals(1))
      external nf_get_att_double
!
! general variable routines:
!
      integer nf_def_var
! (integer ncid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer varid)
      external nf_def_var
      integer nf_inq_var
! (integer ncid,
! integer varid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer natts)
      external nf_inq_var
      integer nf_inq_varid
! (integer ncid,
! character(*) name,
! integer varid)
      external nf_inq_varid
      integer nf_inq_varname
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_inq_varname
      integer nf_inq_vartype
! (integer ncid,
! integer varid,
! integer xtype)
      external nf_inq_vartype
      integer nf_inq_varndims
! (integer ncid,
! integer varid,
! integer ndims)
      external nf_inq_varndims
      integer nf_inq_vardimid
! (integer ncid,
! integer varid,
! integer dimids(1))
      external nf_inq_vardimid
      integer nf_inq_varnatts
! (integer ncid,
! integer varid,
! integer natts)
      external nf_inq_varnatts
      integer nf_rename_var
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_rename_var
      integer nf_copy_var
! (integer ncid_in,
! integer varid,
! integer ncid_out)
      external nf_copy_var
!
! entire variable put/get routines:
!
      integer nf_put_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_put_var_text
      integer nf_get_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_get_var_text
      integer nf_put_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_put_var_int1
      integer nf_get_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_get_var_int1
      integer nf_put_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_put_var_int2
      integer nf_get_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_get_var_int2
      integer nf_put_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_put_var_int
      integer nf_get_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_get_var_int
      integer nf_put_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_put_var_real
      integer nf_get_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_get_var_real
      integer nf_put_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_put_var_double
      integer nf_get_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_get_var_double
!
! single variable put/get routines:
!
      integer nf_put_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_put_var1_text
      integer nf_get_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_get_var1_text
      integer nf_put_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_put_var1_int1
      integer nf_get_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_get_var1_int1
      integer nf_put_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_put_var1_int2
      integer nf_get_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_get_var1_int2
      integer nf_put_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_put_var1_int
      integer nf_get_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_get_var1_int
      integer nf_put_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_put_var1_real
      integer nf_get_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_get_var1_real
      integer nf_put_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_put_var1_double
      integer nf_get_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_get_var1_double
!
! variable array put/get routines:
!
      integer nf_put_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_put_vara_text
      integer nf_get_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_get_vara_text
      integer nf_put_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_put_vara_int1
      integer nf_get_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_get_vara_int1
      integer nf_put_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_put_vara_int2
      integer nf_get_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_get_vara_int2
      integer nf_put_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_put_vara_int
      integer nf_get_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_get_vara_int
      integer nf_put_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_put_vara_real
      integer nf_get_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_get_vara_real
      integer nf_put_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_put_vara_double
      integer nf_get_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_get_vara_double
!
! strided variable put/get routines:
!
      integer nf_put_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_put_vars_text
      integer nf_get_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_get_vars_text
      integer nf_put_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_put_vars_int1
      integer nf_get_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_get_vars_int1
      integer nf_put_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_put_vars_int2
      integer nf_get_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_get_vars_int2
      integer nf_put_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_put_vars_int
      integer nf_get_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_get_vars_int
      integer nf_put_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_put_vars_real
      integer nf_get_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_get_vars_real
      integer nf_put_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_put_vars_double
      integer nf_get_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_get_vars_double
!
! mapped variable put/get routines:
!
      integer nf_put_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_put_varm_text
      integer nf_get_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_get_varm_text
      integer nf_put_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_put_varm_int1
      integer nf_get_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_get_varm_int1
      integer nf_put_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_put_varm_int2
      integer nf_get_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_get_varm_int2
      integer nf_put_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_put_varm_int
      integer nf_get_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_get_varm_int
      integer nf_put_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_put_varm_real
      integer nf_get_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_get_varm_real
      integer nf_put_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_put_varm_double
      integer nf_get_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_get_varm_double
! NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!
!
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil
      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil
      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool
!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble
      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)
!
! masks for the struct nc flag field; passed in as 'mode' arg to
! nccreate and ncopen.
!
! read/write, 0 => readonly
      parameter(ncrdwr = 1)
! in create phase, cleared by ncendef
      parameter(nccreat = 2)
! on create destroy existing file
      parameter(ncexcl = 4)
! in define mode, cleared by ncendef
      parameter(ncindef = 8)
! synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
! synchronise whole header on change (x'20')
      parameter(nchsync = 32)
! numrecs has changed (x'40')
      parameter(ncndirty = 64)
! header info has changed (x'80')
      parameter(nchdirty = 128)
! prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
! do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
! isa link (x'8000')
      parameter(nclink = 32768)
!
! 'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)
!
! 'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)
!
! attribute id to put/get a global attribute
!
      parameter(ncglobal = 0)
!
! advisory maximums:
!
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
! not enforced
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)
!
! global netcdf error status variable
! initialized in error.c
!
! no error
      parameter(ncnoerr = nf_noerr)
! not a netcdf id
      parameter(ncebadid = nf_ebadid)
! too many netcdfs open
      parameter(ncenfile = -31) ! nc_syserr
! netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
! invalid argument
      parameter(nceinval = nf_einval)
! write to read only
      parameter(nceperm = nf_eperm)
! operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine )
! operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
! coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
! maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
! string match to name in use
      parameter(ncename = nf_enameinuse)
! attribute not found
      parameter(ncenoatt = nf_enotatt)
! maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
! not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
! invalid dimension id
      parameter(ncebadd = nf_ebaddim)
! ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
! maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
! variable not found
      parameter(ncenotvr = nf_enotvar)
! action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
! not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)
!
! global options variable. used to determine behavior of error handler.
! initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)
!
! default fill values. these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub
      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   character(len=*), intent(in) :: this_datestr, next_datestr
   real, intent(out) :: bdyfrq
   logical, intent(in) :: debug
   real :: this_bdy_time, next_bdy_time
   ! if (trace_use) call da_trace_entry("da_get_bdyfrq")
   call da_atotime(this_datestr, this_bdy_time)
   call da_atotime(next_datestr, next_bdy_time)
   bdyfrq = next_bdy_time - this_bdy_time
   if (debug) then
      write(unit=stdout, fmt='(a, f20.1)') &
           'next_bdy_time=', next_bdy_time, &
           'this_bdy_time=', this_bdy_time, &
           'bdyfrq       =', bdyfrq
   end if
   ! if (trace_use) call da_trace_exit("da_get_bdyfrq")
end subroutine da_get_bdyfrq
subroutine da_get_bdytimestr_cdf(file, time_flag, bdytimestr, max_times, debug)
   implicit none
! NetCDF-3.
!
! netcdf version 3 fortran interface:
!
!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
!
! default fill values:
!
      integer nf_fill_byte
      integer nf_fill_int1
      integer nf_fill_char
      integer nf_fill_short
      integer nf_fill_int2
      integer nf_fill_int
      real nf_fill_float
      real nf_fill_real
      doubleprecision nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)
!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)
!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc
      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer nf_fatal
      integer nf_verbose
      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)
!
! miscellaneous routines:
!
      character*80 nf_inq_libvers
      external nf_inq_libvers
      character*80 nf_strerror
! (integer ncerr)
      external nf_strerror
      logical nf_issyserr
! (integer ncerr)
      external nf_issyserr
!
! control routines:
!
      integer nf_inq_base_pe
! (integer ncid,
! integer pe)
      external nf_inq_base_pe
      integer nf_set_base_pe
! (integer ncid,
! integer pe)
      external nf_set_base_pe
      integer nf_create
! (character*(*) path,
! integer cmode,
! integer ncid)
      external nf_create
      integer nf__create
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer chunksizehint,
! integer ncid)
      external nf__create
      integer nf__create_mp
! (character*(*) path,
! integer cmode,
! integer initialsz,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__create_mp
      integer nf_open
! (character*(*) path,
! integer mode,
! integer ncid)
      external nf_open
      integer nf__open
! (character*(*) path,
! integer mode,
! integer chunksizehint,
! integer ncid)
      external nf__open
      integer nf__open_mp
! (character*(*) path,
! integer mode,
! integer basepe,
! integer chunksizehint,
! integer ncid)
      external nf__open_mp
      integer nf_set_fill
! (integer ncid,
! integer fillmode,
! integer old_mode)
      external nf_set_fill
      integer nf_set_default_format
! (integer format,
! integer old_format)
      external nf_set_default_format
      integer nf_redef
! (integer ncid)
      external nf_redef
      integer nf_enddef
! (integer ncid)
      external nf_enddef
      integer nf__enddef
! (integer ncid,
! integer h_minfree,
! integer v_align,
! integer v_minfree,
! integer r_align)
      external nf__enddef
      integer nf_sync
! (integer ncid)
      external nf_sync
      integer nf_abort
! (integer ncid)
      external nf_abort
      integer nf_close
! (integer ncid)
      external nf_close
      integer nf_delete
! (character*(*) ncid)
      external nf_delete
!
! general inquiry routines:
!
      integer nf_inq
! (integer ncid,
! integer ndims,
! integer nvars,
! integer ngatts,
! integer unlimdimid)
      external nf_inq
      integer nf_inq_ndims
! (integer ncid,
! integer ndims)
      external nf_inq_ndims
      integer nf_inq_nvars
! (integer ncid,
! integer nvars)
      external nf_inq_nvars
      integer nf_inq_natts
! (integer ncid,
! integer ngatts)
      external nf_inq_natts
      integer nf_inq_unlimdim
! (integer ncid,
! integer unlimdimid)
      external nf_inq_unlimdim
      integer nf_inq_format
! (integer ncid,
! integer format)
      external nf_inq_format
!
! dimension routines:
!
      integer nf_def_dim
! (integer ncid,
! character(*) name,
! integer len,
! integer dimid)
      external nf_def_dim
      integer nf_inq_dimid
! (integer ncid,
! character(*) name,
! integer dimid)
      external nf_inq_dimid
      integer nf_inq_dim
! (integer ncid,
! integer dimid,
! character(*) name,
! integer len)
      external nf_inq_dim
      integer nf_inq_dimname
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_inq_dimname
      integer nf_inq_dimlen
! (integer ncid,
! integer dimid,
! integer len)
      external nf_inq_dimlen
      integer nf_rename_dim
! (integer ncid,
! integer dimid,
! character(*) name)
      external nf_rename_dim
!
! general attribute routines:
!
      integer nf_inq_att
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len)
      external nf_inq_att
      integer nf_inq_attid
! (integer ncid,
! integer varid,
! character(*) name,
! integer attnum)
      external nf_inq_attid
      integer nf_inq_atttype
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype)
      external nf_inq_atttype
      integer nf_inq_attlen
! (integer ncid,
! integer varid,
! character(*) name,
! integer len)
      external nf_inq_attlen
      integer nf_inq_attname
! (integer ncid,
! integer varid,
! integer attnum,
! character(*) name)
      external nf_inq_attname
      integer nf_copy_att
! (integer ncid_in,
! integer varid_in,
! character(*) name,
! integer ncid_out,
! integer varid_out)
      external nf_copy_att
      integer nf_rename_att
! (integer ncid,
! integer varid,
! character(*) curname,
! character(*) newname)
      external nf_rename_att
      integer nf_del_att
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_del_att
!
! attribute put/get routines:
!
      integer nf_put_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! integer len,
! character(*) text)
      external nf_put_att_text
      integer nf_get_att_text
! (integer ncid,
! integer varid,
! character(*) name,
! character(*) text)
      external nf_get_att_text
      integer nf_put_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int1_t i1vals(1))
      external nf_put_att_int1
      integer nf_get_att_int1
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int1_t i1vals(1))
      external nf_get_att_int1
      integer nf_put_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! nf_int2_t i2vals(1))
      external nf_put_att_int2
      integer nf_get_att_int2
! (integer ncid,
! integer varid,
! character(*) name,
! nf_int2_t i2vals(1))
      external nf_get_att_int2
      integer nf_put_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! integer ivals(1))
      external nf_put_att_int
      integer nf_get_att_int
! (integer ncid,
! integer varid,
! character(*) name,
! integer ivals(1))
      external nf_get_att_int
      integer nf_put_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! real rvals(1))
      external nf_put_att_real
      integer nf_get_att_real
! (integer ncid,
! integer varid,
! character(*) name,
! real rvals(1))
      external nf_get_att_real
      integer nf_put_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! integer xtype,
! integer len,
! double dvals(1))
      external nf_put_att_double
      integer nf_get_att_double
! (integer ncid,
! integer varid,
! character(*) name,
! double dvals(1))
      external nf_get_att_double
!
! general variable routines:
!
      integer nf_def_var
! (integer ncid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer varid)
      external nf_def_var
      integer nf_inq_var
! (integer ncid,
! integer varid,
! character(*) name,
! integer datatype,
! integer ndims,
! integer dimids(1),
! integer natts)
      external nf_inq_var
      integer nf_inq_varid
! (integer ncid,
! character(*) name,
! integer varid)
      external nf_inq_varid
      integer nf_inq_varname
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_inq_varname
      integer nf_inq_vartype
! (integer ncid,
! integer varid,
! integer xtype)
      external nf_inq_vartype
      integer nf_inq_varndims
! (integer ncid,
! integer varid,
! integer ndims)
      external nf_inq_varndims
      integer nf_inq_vardimid
! (integer ncid,
! integer varid,
! integer dimids(1))
      external nf_inq_vardimid
      integer nf_inq_varnatts
! (integer ncid,
! integer varid,
! integer natts)
      external nf_inq_varnatts
      integer nf_rename_var
! (integer ncid,
! integer varid,
! character(*) name)
      external nf_rename_var
      integer nf_copy_var
! (integer ncid_in,
! integer varid,
! integer ncid_out)
      external nf_copy_var
!
! entire variable put/get routines:
!
      integer nf_put_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_put_var_text
      integer nf_get_var_text
! (integer ncid,
! integer varid,
! character(*) text)
      external nf_get_var_text
      integer nf_put_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_put_var_int1
      integer nf_get_var_int1
! (integer ncid,
! integer varid,
! nf_int1_t i1vals(1))
      external nf_get_var_int1
      integer nf_put_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_put_var_int2
      integer nf_get_var_int2
! (integer ncid,
! integer varid,
! nf_int2_t i2vals(1))
      external nf_get_var_int2
      integer nf_put_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_put_var_int
      integer nf_get_var_int
! (integer ncid,
! integer varid,
! integer ivals(1))
      external nf_get_var_int
      integer nf_put_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_put_var_real
      integer nf_get_var_real
! (integer ncid,
! integer varid,
! real rvals(1))
      external nf_get_var_real
      integer nf_put_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_put_var_double
      integer nf_get_var_double
! (integer ncid,
! integer varid,
! doubleprecision dvals(1))
      external nf_get_var_double
!
! single variable put/get routines:
!
      integer nf_put_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_put_var1_text
      integer nf_get_var1_text
! (integer ncid,
! integer varid,
! integer index(1),
! character*1 text)
      external nf_get_var1_text
      integer nf_put_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_put_var1_int1
      integer nf_get_var1_int1
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int1_t i1val)
      external nf_get_var1_int1
      integer nf_put_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_put_var1_int2
      integer nf_get_var1_int2
! (integer ncid,
! integer varid,
! integer index(1),
! nf_int2_t i2val)
      external nf_get_var1_int2
      integer nf_put_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_put_var1_int
      integer nf_get_var1_int
! (integer ncid,
! integer varid,
! integer index(1),
! integer ival)
      external nf_get_var1_int
      integer nf_put_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_put_var1_real
      integer nf_get_var1_real
! (integer ncid,
! integer varid,
! integer index(1),
! real rval)
      external nf_get_var1_real
      integer nf_put_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_put_var1_double
      integer nf_get_var1_double
! (integer ncid,
! integer varid,
! integer index(1),
! doubleprecision dval)
      external nf_get_var1_double
!
! variable array put/get routines:
!
      integer nf_put_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_put_vara_text
      integer nf_get_vara_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! character(*) text)
      external nf_get_vara_text
      integer nf_put_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_put_vara_int1
      integer nf_get_vara_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int1_t i1vals(1))
      external nf_get_vara_int1
      integer nf_put_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_put_vara_int2
      integer nf_get_vara_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! nf_int2_t i2vals(1))
      external nf_get_vara_int2
      integer nf_put_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_put_vara_int
      integer nf_get_vara_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer ivals(1))
      external nf_get_vara_int
      integer nf_put_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_put_vara_real
      integer nf_get_vara_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! real rvals(1))
      external nf_get_vara_real
      integer nf_put_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_put_vara_double
      integer nf_get_vara_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! doubleprecision dvals(1))
      external nf_get_vara_double
!
! strided variable put/get routines:
!
      integer nf_put_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_put_vars_text
      integer nf_get_vars_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! character(*) text)
      external nf_get_vars_text
      integer nf_put_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_put_vars_int1
      integer nf_get_vars_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int1_t i1vals(1))
      external nf_get_vars_int1
      integer nf_put_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_put_vars_int2
      integer nf_get_vars_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! nf_int2_t i2vals(1))
      external nf_get_vars_int2
      integer nf_put_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_put_vars_int
      integer nf_get_vars_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer ivals(1))
      external nf_get_vars_int
      integer nf_put_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_put_vars_real
      integer nf_get_vars_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! real rvals(1))
      external nf_get_vars_real
      integer nf_put_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_put_vars_double
      integer nf_get_vars_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! doubleprecision dvals(1))
      external nf_get_vars_double
!
! mapped variable put/get routines:
!
      integer nf_put_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_put_varm_text
      integer nf_get_varm_text
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! character(*) text)
      external nf_get_varm_text
      integer nf_put_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_put_varm_int1
      integer nf_get_varm_int1
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int1_t i1vals(1))
      external nf_get_varm_int1
      integer nf_put_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_put_varm_int2
      integer nf_get_varm_int2
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! nf_int2_t i2vals(1))
      external nf_get_varm_int2
      integer nf_put_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_put_varm_int
      integer nf_get_varm_int
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! integer ivals(1))
      external nf_get_varm_int
      integer nf_put_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_put_varm_real
      integer nf_get_varm_real
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! real rvals(1))
      external nf_get_varm_real
      integer nf_put_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_put_varm_double
      integer nf_get_varm_double
! (integer ncid,
! integer varid,
! integer start(1),
! integer count(1),
! integer stride(1),
! integer imap(1),
! doubleprecision dvals(1))
      external nf_get_varm_double
! NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!
!
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil
      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil
      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool
!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble
      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)
!
! masks for the struct nc flag field; passed in as 'mode' arg to
! nccreate and ncopen.
!
! read/write, 0 => readonly
      parameter(ncrdwr = 1)
! in create phase, cleared by ncendef
      parameter(nccreat = 2)
! on create destroy existing file
      parameter(ncexcl = 4)
! in define mode, cleared by ncendef
      parameter(ncindef = 8)
! synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
! synchronise whole header on change (x'20')
      parameter(nchsync = 32)
! numrecs has changed (x'40')
      parameter(ncndirty = 64)
! header info has changed (x'80')
      parameter(nchdirty = 128)
! prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
! do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
! isa link (x'8000')
      parameter(nclink = 32768)
!
! 'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)
!
! 'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)
!
! attribute id to put/get a global attribute
!
      parameter(ncglobal = 0)
!
! advisory maximums:
!
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
! not enforced
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)
!
! global netcdf error status variable
! initialized in error.c
!
! no error
      parameter(ncnoerr = nf_noerr)
! not a netcdf id
      parameter(ncebadid = nf_ebadid)
! too many netcdfs open
      parameter(ncenfile = -31) ! nc_syserr
! netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
! invalid argument
      parameter(nceinval = nf_einval)
! write to read only
      parameter(nceperm = nf_eperm)
! operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine )
! operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
! coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
! maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
! string match to name in use
      parameter(ncename = nf_enameinuse)
! attribute not found
      parameter(ncenoatt = nf_enotatt)
! maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
! not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
! invalid dimension id
      parameter(ncebadd = nf_ebaddim)
! ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
! maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
! variable not found
      parameter(ncenotvr = nf_enotvar)
! action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
! not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)
!
! global options variable. used to determine behavior of error handler.
! initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)
!
! default fill values. these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub
      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   integer, intent(in) :: max_times
   logical, intent(in) :: debug
   character(len=*), intent(in) :: file
   character(len=*), intent(in) :: time_flag
   character(len=80), intent(out) :: bdytimestr(max_times)
   character(len=80) :: varnam, time1
   integer :: cdfid, rcode, id_time
   integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
   integer :: i, ivtype
   ! if (trace_use) call da_trace_entry("da_get_bdytimestr_cdf")
   cdfid = ncopn(file, NCNOWRIT, rcode)
   if (rcode == 0) then
      if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
      write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
      stop
   end if
   write(varnam,'(a,a,a)') 'md___',trim(time_flag),'e_x_t_d_o_m_a_i_n_m_e_t_a_data_'
   id_time = ncvid(cdfid, varnam, rcode)
   rcode = nf_inq_var(cdfid, id_time, varnam, ivtype, ndims, dimids, natts)
   if (debug) then
     write(unit=stdout,fmt=*) ' number of dims for bdytime ',ndims
   end if
   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), idims(i))
      if (debug) write(unit=stdout,fmt=*) ' dimension ',i,idims(i)
   end do
   ! get the bdytime
   do i=1,idims(2)
      istart(1) = 1
      iend(1) = idims(1)
      istart(2) = i
      iend(2) = 1
      rcode = NF_GET_VARA_TEXT (cdfid, id_time, &
                                 istart, iend, &
                                 bdytimestr(i))
      time1 = bdytimestr(i)
      if (debug) write(unit=stdout,fmt=*) trim(file), time1(1:19)
      bdytimestr(i) = time1(1:19)
   end do
   call ncclos(cdfid,rcode)
   ! if (trace_use) call da_trace_exit("da_get_bdytimestr_cdf")
end subroutine da_get_bdytimestr_cdf
subroutine da_atotime(date_char, st)
   !-----------------------------------------------------------------------
   ! Purpose: Input a date character string in WRF format (CCYY-MM-DD_hh:mm:ss)
   ! Output the number of seconds since Dec 31, 1999, 00:00:00
   !-----------------------------------------------------------------------
   implicit none
   character(len=*), intent(in) :: date_char
   real, intent(out) :: st
   integer :: ccyy,mo,dd,hh,mi,ss,i
   integer, dimension(12) :: mmday
   integer :: dayssince2000
   mmday=(/31,28,31,30,31,30,31,31,30,31,30,31/)
   read(date_char(1:19),'(i4,1x,4(i2,1x),i2)') &
        ccyy, mo, dd, hh, mi, ss
   if (mod(ccyy,4) == 0) then
      mmday(2) = 29
      if (mod(ccyy,400) == 0) then
         mmday(2) = 29
      else if (mod(ccyy,100) == 0) then
         mmday(2) = 28
      end if
   end if
   dayssince2000 = 0;
   ! This set of if statements sets "dayssince2000" to the number of days from the beginning of
   ! the year 2000 to the beginning of the current year (for example, 2000 returns "0",
   ! 2001 returns 366, 2012 returns 4018, 1999 returns -365, etc.)
   if (ccyy < 2000) then
      do i=ccyy,1999
         dayssince2000 = dayssince2000 - 365
         !If statements to cover leap year cases
         if (mod(i,4) == 0) then
            dayssince2000 = dayssince2000 - 1
            if (mod(i,100) == 0) then
               dayssince2000 = dayssince2000 + 1
               if (mod(i,400) == 0) then
                  dayssince2000 = dayssince2000 - 1
               end if
            end if
         end if
      end do
   else if (ccyy > 2000) then
      do i=2000,ccyy-1
         dayssince2000 = dayssince2000 + 365
         !If statements to cover leap year cases
         if (mod(i,4) == 0) then
            dayssince2000 = dayssince2000 + 1
            if (mod(i,100) == 0) then
               dayssince2000 = dayssince2000 - 1
               if (mod(i,400) == 0) then
                  dayssince2000 = dayssince2000 + 1
               end if
            end if
         end if
      end do
   end if
   dd=dd+dayssince2000
   do i=1,mo-1
      dd=dd+mmday(i)
   end do
   st = real(ss) + 60.0*(real(mi) + 60.0*(real(hh) + 24.0* real(dd)))
end subroutine da_atotime
end module da_netcdf_interface
