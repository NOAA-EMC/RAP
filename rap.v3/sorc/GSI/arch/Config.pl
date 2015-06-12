#!/usr/bin/perl
#
# Configuration script for GSI code
# 
# Be sure to run as ./configure (to avoid getting a system configure command by mistake)
#
select((select(STDOUT), $|=1)[0]);
$sw_netcdf_path = "" ;
$sw_wrf_path = "" ;
$sw_core_path = "" ;
$sw_os = "ARCH" ;           # ARCH will match any
$sw_mach = "ARCH" ;         # ARCH will match any
$sw_dmparallel = "" ;
$sw_ompparallel = "" ;
$sw_sfc = "" ;
$sw_fc = "\$(SFC)" ;
$sw_cc = "\$(SCC)" ;
$sw_f90 = "\$(SF90)" ;
$sw_usenetcdff = "" ;    # for 3.6.2 and greater, the fortran bindings might be in a separate lib file
$iflinux = '';
$ifintel = '';

while ( substr( $ARGV[0], 0, 1 ) eq "-" )
 {
  if ( substr( $ARGV[0], 1, 7 ) eq "netcdf=" )
  {
    $sw_netcdf_path = substr( $ARGV[0], 8 ) ;
  }
  if ( substr( $ARGV[0], 1, 9 ) eq "corepath=" )
  {
    $sw_core_path = substr( $ARGV[0], 10 ) ;
  }
  if ( substr( $ARGV[0], 1, 8 ) eq "wrfpath=" )
  {
    $sw_wrf_path = substr( $ARGV[0], 9 ) ;
  }
  if ( substr( $ARGV[0], 1, 3 ) eq "os=" )
  {
    $sw_os = substr( $ARGV[0], 4 ) ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "mach=" )
  {
    $sw_mach = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 11 ) eq "dmparallel=" )
  {
    $sw_dmparallel=substr( $ARGV[0], 12 ) ;
  }
  if ( substr( $ARGV[0], 1, 12 ) eq "ompparallel=" )
  {
    $sw_ompparallel=substr( $ARGV[0], 13 ) ;
  }
  if ( substr( $ARGV[0], 1, 11 ) eq "USENETCDFF=" )
  {
    $sw_usenetcdff = substr( $ARGV[0], 12 ) ;
  }
  shift @ARGV ;
 }


# parse the configure.defaults file

$validresponse = 0 ;
@platforms = qw ( serial dmpar,debug dmpar,optimize ) ;
$JASPTERLIB = "";
$JASPERLIB  =  $ENV{JASPERLIB};
if ($JASPERLIB) {
  $sw_grib2_libs="-L$sw_wrf_path/external/io_grib2 -lio_grib2 -L$JASPERLIB -ljasper";
 print "grib2lib = $sw_grib2_libs";
}

# Display the choices to the user and get selection
until ( $validresponse ) {
  printf "------------------------------------------------------------------------\n" ;
  printf "Please select from among the following supported platforms.\n\n" ;

  $opt = 1 ;
  open CONFIGURE_DEFAULTS, "< ./arch/configure.defaults" 
      or die "Cannot open ./arch/configure.defaults for reading" ;
  while ( <CONFIGURE_DEFAULTS> )
  {
    for $paropt ( @platforms ) 
    {
      if ( substr( $_, 0, 5 ) eq "#ARCH" && ( index( $_, $sw_os ) >= 0 ) && ( index( $_, $sw_mach ) >= 0 )
	   && ( index($_, $paropt) >= 0 )  )
      {
        $optstr[$opt] = substr($_,6) ;
        $optstr[$opt] =~ s/^[ 	]*// ;
        $optstr[$opt] =~ s/#.*$//g ;
        chomp($optstr[$opt]) ;
        $optstr[$opt] = $optstr[$opt]." (".$paropt.")" ;
        if ( substr( $optstr[$opt], 0,4 ) ne "NULL" )
        {
          printf "  %2d.  %s\n",$opt,$optstr[$opt] ;
          $opt++ ;
        }
      }
    }
  }
  close CONFIGURE_DEFAULTS ;

  $opt -- ;

  printf "\nEnter selection [%d-%d] : ",1,$opt ;
  $response = <STDIN> ;

  if ( $response == -1 ) { exit ; }

  if ( $response >= 1 && $response <= $opt ) 
  { $validresponse = 1 ; }
  else
  { printf("\nInvalid response (%d)\n",$response);}
}
printf "------------------------------------------------------------------------\n" ;

$optchoice = $response ;

open CONFIGURE_DEFAULTS, "< ./arch/configure.defaults" 
      or die "Cannot open ./arch/configure.defaults for reading" ;
$latchon = 0 ;

while ( <CONFIGURE_DEFAULTS> )
{
  if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 1 )
  {
    $latchon = 0 ;
  }
  if ( $latchon == 1 )
  {
    $_ =~ s/CONFIGURE_NETCDF_PATH/$sw_netcdf_path/g ;
    $_ =~ s/CONFIGURE_NETCDF_LIBS/$sw_usenetcdff -lnetcdf/g ;
    $_ =~ s/CONFIGURE_WRF_PATH/$sw_wrf_path/g ;
    $_ =~ s/CONFIGURE_CORE_PATH/$sw_core_path/g ;
    $_ =~ s/CONFIGURE_FC/$sw_fc/g ;
    $_ =~ s/CONFIGURE_F90/$sw_f90/g ;
    $_ =~ s/CONFIGURE_CC/$sw_cc/g ;

    @machopts = ( @machopts, $_ ) ;
  }
  for $paropt ( @platforms )
  {
    if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 0
          && ( index( $_, $sw_os ) >= 0 ) && ( index( $_, $sw_mach ) >= 0 )
          && ( index($_, $paropt) >= 0 ) )
    {

    $x=substr($_,6) ;
    $x=~s/^[     ]*// ;
    $x =~ s/#.*$//g ;
    chomp($x) ;
    $x = $x." (".$paropt.")" ;
    if ( $x eq $optstr[$optchoice] )
    {
      $latchon = 1 ;
      $sw_ompparallel = "" ;
      $sw_dmparallel = "" ;
      $validresponse = 0 ;
# search x for Linux and Intel, if found set flag
      if ( $x =~ /Linux/ ) { $iflinux = 1 };
      if ( $x =~ /Intel/ ) { $ifintel = 1 };

      if ( ($paropt eq 'dmpar,debug') || ($paropt eq 'dmpar,optimize') ) 
      {
        $sw_dmparallel = "RSL_LITE" ;
        $sw_dmparallelflag = "-DDM_PARALLEL" ;
	$sw_fc = "\$(DM_FC)" ;
	$sw_f90 = "\$(DM_F90)" ;
	$sw_cc = "\$(DM_CC)" ;
      }
    }
    }

  }
}
close CONFIGURE_DEFAULTS ;

# test Intel with Linux build, requires explicit path to LAPACK libs
$LAPACK_PATH = $ENV{LAPACK_PATH};
if ( $iflinux  )
{ 
   if ( $ifintel ) 
   {
      if ( !$LAPACK_PATH )
      {
        printf "The LAPACK math library is required to compile GSI with Intel. This is";
        printf " required \nwhen building with Intel ifort. The library path variable ";
        printf "\$LAPACK_PATH has not \nbeen set. Typically the required path is ";
        printf " available through a system variable such as: \n"; 
        printf " $MKLROOT on Jet and Zeus \n";
        printf "Please set the path, and rerun the configure script. \n";
        printf "Configuration unsuccessful. \n" ;
        printf "----------------------------------------------------------";
        printf "--------------\n" ;
        die;
      }
   }
}

open CONFIGURE_GSI, "> configure.gsi" or die "cannot append configure.gsi" ;
open ARCH_PREAMBLE, "< arch/preamble" or die "cannot open arch/preamble" ;
my @preamble;
# apply substitutions to the preamble...
while ( <ARCH_PREAMBLE> )
  {
  @preamble = ( @preamble, $_ ) ;
  }
close ARCH_PREAMBLE ;
print CONFIGURE_GSI @preamble  ;
close ARCH_PREAMBLE ;
printf CONFIGURE_GSI "# Settings for %s", $optstr[$optchoice] ;
print CONFIGURE_GSI @machopts  ;
open ARCH_POSTAMBLE, "< arch/postamble" or die "cannot open arch/postamble" ;
$HWRF = $ENV{HWRF};
if ($HWRF) {
close ARCH_POSTAMBLE ;
open ARCH_POSTAMBLE, "< arch/postamble.hwrf" or die "cannot open arch/postamble.hwrf" ;
}
while ( <ARCH_POSTAMBLE> ) { 
    $_ =~ s/CONFIGURE_NETCDF_PATH/$sw_netcdf_path/g ;
    $_ =~ s/CONFIGURE_NETCDF_LIBS/$sw_usenetcdff -lnetcdf/g ;
    $_ =~ s/CONFIGURE_WRF_PATH/$sw_wrf_path/g ;
    $_ =~ s/CONFIGURE_GRIB2_LIBS/$sw_grib2_libs/g ;
  print CONFIGURE_GSI;
 }
close ARCH_POSTAMBLE ;
close CONFIGURE_GSI ;

printf "Configuration successful. To build the GSI, type: compile \n" ;
printf "------------------------------------------------------------------------\n" ;


