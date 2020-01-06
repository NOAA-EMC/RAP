# CMake generated Testfile for 
# Source directory: /gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd
# Build directory: /gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(global_T62 "regression_driver.sh" "global_T62" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(global_T62 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
add_test(global_T62_ozonly "regression_driver.sh" "global_T62_ozonly" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(global_T62_ozonly PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
add_test(global_4dvar_T62 "regression_driver.sh" "global_4dvar_T62" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(global_4dvar_T62 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
add_test(global_4denvar_T126 "regression_driver.sh" "global_4denvar_T126" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(global_4denvar_T126 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
add_test(global_fv3_4denvar_T126 "regression_driver.sh" "global_fv3_4denvar_T126" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(global_fv3_4denvar_T126 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
add_test(global_lanczos_T62 "regression_driver.sh" "global_lanczos_T62" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(global_lanczos_T62 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
add_test(arw_netcdf "regression_driver.sh" "arw_netcdf" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(arw_netcdf PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
add_test(
          arw_binary "regression_driver.sh" "
          arw_binary" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(
          arw_binary PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
add_test(nmm_binary "regression_driver.sh" "nmm_binary" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(nmm_binary PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
add_test(nmm_netcdf "regression_driver.sh" "nmm_netcdf" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(nmm_netcdf PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
add_test(nmmb_nems_4denvar "regression_driver.sh" "nmmb_nems_4denvar" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(nmmb_nems_4denvar PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
add_test(hwrf_nmm_d2 "regression_driver.sh" "hwrf_nmm_d2" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(hwrf_nmm_d2 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
add_test(hwrf_nmm_d3 "regression_driver.sh" "hwrf_nmm_d3" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(hwrf_nmm_d3 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
add_test(rtma "regression_driver.sh" "rtma" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(rtma PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
add_test(global_enkf_T62 "regression_driver.sh" "global_enkf_T62" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(global_enkf_T62 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
add_test(netcdf_fv3_regional "regression_driver.sh" "netcdf_fv3_regional" "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/build")
set_tests_properties(netcdf_fv3_regional PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_gsi.fd/regression")
subdirs(libsrc/wrflib)
subdirs(src/ncdiag)
subdirs(libsrc/GSD/gsdcloud)
subdirs(src/gsi)
subdirs(src/enkf)
subdirs(util/EnKF/arw/src)
