set(CMAKE_C_COMPILER "/opt/cray/craype/2.3.0/bin/cc")
set(CMAKE_C_COMPILER_ARG1 "")
set(CMAKE_C_COMPILER_ID "Intel")
set(CMAKE_C_COMPILER_VERSION "15.0.3.20150407")
set(CMAKE_C_COMPILER_WRAPPER "CrayPrgEnv")
set(CMAKE_C_STANDARD_COMPUTED_DEFAULT "90")
set(CMAKE_C_COMPILE_FEATURES "c_function_prototypes;c_restrict;c_static_assert;c_variadic_macros")
set(CMAKE_C90_COMPILE_FEATURES "c_function_prototypes")
set(CMAKE_C99_COMPILE_FEATURES "c_restrict;c_static_assert;c_variadic_macros")
set(CMAKE_C11_COMPILE_FEATURES "")

set(CMAKE_C_PLATFORM_ID "Linux")
set(CMAKE_C_SIMULATE_ID "")
set(CMAKE_C_SIMULATE_VERSION "")

set(CMAKE_AR "/usr/bin/ar")
set(CMAKE_RANLIB "/usr/bin/ranlib")
set(CMAKE_LINKER "/usr/bin/ld")
set(CMAKE_COMPILER_IS_GNUCC )
set(CMAKE_C_COMPILER_LOADED 1)
set(CMAKE_C_COMPILER_WORKS TRUE)
set(CMAKE_C_ABI_COMPILED TRUE)
set(CMAKE_COMPILER_IS_MINGW )
set(CMAKE_COMPILER_IS_CYGWIN )
if(CMAKE_COMPILER_IS_CYGWIN)
  set(CYGWIN 1)
  set(UNIX 1)
endif()

set(CMAKE_C_COMPILER_ENV_VAR "CC")

if(CMAKE_COMPILER_IS_MINGW)
  set(MINGW 1)
endif()
set(CMAKE_C_COMPILER_ID_RUN 1)
set(CMAKE_C_SOURCE_FILE_EXTENSIONS c;m)
set(CMAKE_C_IGNORE_EXTENSIONS h;H;o;O;obj;OBJ;def;DEF;rc;RC)
set(CMAKE_C_LINKER_PREFERENCE 10)

# Save compiler ABI information.
set(CMAKE_C_SIZEOF_DATA_PTR "8")
set(CMAKE_C_COMPILER_ABI "ELF")
set(CMAKE_C_LIBRARY_ARCHITECTURE "")

if(CMAKE_C_SIZEOF_DATA_PTR)
  set(CMAKE_SIZEOF_VOID_P "${CMAKE_C_SIZEOF_DATA_PTR}")
endif()

if(CMAKE_C_COMPILER_ABI)
  set(CMAKE_INTERNAL_PLATFORM_ABI "${CMAKE_C_COMPILER_ABI}")
endif()

if(CMAKE_C_LIBRARY_ARCHITECTURE)
  set(CMAKE_LIBRARY_ARCHITECTURE "")
endif()

set(CMAKE_C_CL_SHOWINCLUDES_PREFIX "")
if(CMAKE_C_CL_SHOWINCLUDES_PREFIX)
  set(CMAKE_CL_SHOWINCLUDES_PREFIX "${CMAKE_C_CL_SHOWINCLUDES_PREFIX}")
endif()




set(CMAKE_C_IMPLICIT_LINK_LIBRARIES "intlc;hugetlbfs;mpich_intel;AtpSigHandler;AtpSigHCommData;rca;sci_intel_mpi;sci_intel;imf;m;ifcore;ifport;pthread;imf;svml;irng;m;ipgo;decimal;cilkrts;stdc++;irc;svml;c;irc_s;dl;c")
set(CMAKE_C_IMPLICIT_LINK_DIRECTORIES "/opt/cray/dmapp/default/lib64;/opt/cray/mpt/7.2.0/gni/mpich2-intel/140/lib;/opt/cray/libsci/13.0.3/INTEL/140/x86_64/lib;/opt/cray/rca/1.0.0-2.0502.60530.1.62.ari/lib64;/opt/cray/atp/1.8.1/libApp;/opt/intel/composer_xe_2015.3.187/compiler/lib/intel64;/opt/intel/composer_xe_2015.3.187/ipp/lib/intel64;/opt/intel/composer_xe_2015.3.187/mkl/lib/intel64;/opt/intel/composer_xe_2015.3.187/tbb/lib/intel64/gcc4.1;/usr/lib64/gcc/x86_64-suse-linux/4.3;/usr/lib64;/lib64;/usr/x86_64-suse-linux/lib;/lib;/usr/lib")
set(CMAKE_C_IMPLICIT_LINK_FRAMEWORK_DIRECTORIES "")
