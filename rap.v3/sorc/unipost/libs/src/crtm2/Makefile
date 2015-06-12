SHELL=/bin/sh

#==============================================================================
#
# Community Radiate Transfer Model (CRTM2) Makefile
#
# !REVISION HISTORY:
#
# 2004-08-16  Treadon - initial makefile
# 2004-10-26  Mirvis  - Profile_Utility_Parameters.o was missing from lib
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Parent make (calls child make) --
#-----------------------------------------------------------------------------

# -----------------------------------------------------------
# Default configuration, possibily redefined in Makefile.conf
# -----------------------------------------------------------

SED      = sed
DASPERL  = /usr/bin/perl
COREROOT = ../..
COREBIN  = $(COREROOT)/bin
CORELIB  = $(COREROOT)
COREINC  = $(COREROOT)/incmod/crtm2
COREETC  = $(COREROOT)/etc

# -------------
# General Rules
# -------------

CP              = /bin/cp -p
RM              = /bin/rm -f
MKDIR           = /bin/mkdir -p
AR              = ar cq
PROTEX          = protex -f # -l
ProTexMake      = protex -S # -l
LATEX           = pdflatex
DVIPS           = dvips

# ---------
# Libraries
# ---------
LIBmpeu = $(CORELIB)/libmpeu.a
LIBcrtm = $(CORELIB)/libcrtm2.a

#------------
# Include machine dependent compile & load options
#------------

include ./Makefile.conf


# -------------
# This makefile
# -------------

  MAKE_FILE = Makefile


# -----------
# Load module
# -----------

  EXE_FILE = crtm2


# --------------------
# Installing directory
# --------------------

  INSTALL_DIR = $(CORELIB)


# --------
# Log file
# --------

  LOG_FILE = log.make.$(EXE_FILE)


# ---------------
# Call child make
# ---------------

"" :
	@$(MAKE) -f $(MAKE_FILE) all


# ------------
# Make install
# ------------

install:
	@echo
	@echo '==== INSTALL ================================================='
	@if [ -e $(INSTALL_DIR) ]; then \
	  if [ ! -d $(INSTALL_DIR) ]; then \
	    echo '### Fail to create installing directory ###' ;\
	    echo '### Stop the installation               ###' ;\
	    exit ;\
	  fi ;\
	else \
	  echo "	mkdir -p $(INSTALL_DIR)" ;\
	  mkdir -p $(INSTALL_DIR) ;\
	fi
	cp $(EXE_FILE) $(INSTALL_DIR)
	@cd $(INSTALL_DIR) ; ls -l `pwd`/$(EXE_FILE)


# ----------
# Make clean
# ----------

clean:
	@echo
	@echo '==== CLEAN ==================================================='
	- $(RM) $(EXE_FILE) *.o *.mod *.MOD *.lst *.a *.x
	- $(RM) loadmap.txt log.make.crtm2
	- $(MAKE) doclean


#-----------------------------------------------------------------------------
#                          -- Child make --
#-----------------------------------------------------------------------------

# ------------
# Source files
# ------------

  SRCSF90C = \
            Type_Kinds.f90 \
            File_Utility.f90 \
            Error_Handler.f90 \
            Compare_Float_Numbers.f90 \
            CRTM_RTlocal.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            SpcCoeff_Define.f90 \
            SpcCoeff_Binary_IO.f90 \
            TauCoeff_Define.f90 \
            TauCoeff_Binary_IO.f90 \
            CloudCoeff_Define.f90 \
            CloudCoeff_Binary_IO.f90 \
            AerosolCoeff_Define.f90 \
            AerosolCoeff_Binary_IO.f90 \
            EmisCoeff_Define.f90 \
            EmisCoeff_Binary_IO.f90 \
            NESDIS_AMSRE_SICEEM_Module.f90 \
            NESDIS_AMSRE_SNOWEM_Module.f90 \
            NESDIS_AMSU_SICEEM_Module.f90 \
            NESDIS_AMSU_SnowEM_Module.f90 \
            NESDIS_LandEM_Module.f90 \
            NESDIS_OCEANEM_Module.f90 \
            NESDIS_SEAICE_PHYEM_MODULE.f90 \
            NESDIS_SnowEM_Parameters.f90 \
            NESDIS_SSMI_Module.f90 \
            NESDIS_SSMI_SIceEM_Module.f90 \
            NESDIS_SSMI_SnowEM_Module.f90 \
            CRTM_Utility.f90 \
            CRTM_Interpolation.f90 \
            CRTM_SpcCoeff.f90 \
            CRTM_TauCoeff.f90 \
            CRTM_AerosolCoeff.f90 \
            CRTM_CloudCoeff.f90 \
            CRTM_EmisCoeff.f90 \
            CRTM_Atmosphere_Define.f90 \
            CRTM_Cloud_Define.f90 \
            CRTM_Aerosol_Define.f90 \
            CRTM_Atmosphere_Binary_IO.f90 \
            CRTM_Cloud_Binary_IO.f90 \
            CRTM_Aerosol_Binary_IO.f90 \
            CRTM_Surface_Define.f90 \
            CRTM_SensorData_Define.f90 \
            CRTM_SensorInfo.f90 \
            CRTM_Surface_Binary_IO.f90 \
            CRTM_ChannelInfo_Define.f90 \
            CRTM_ChannelInfo.f90 \
            CRTM_GeometryInfo_Define.f90 \
            CRTM_AtmAbsorption_IntAbsorber.f90 \
            CRTM_AtmAbsorption_Predictor.f90 \
            CRTM_AtmAbsorption_Define.f90 \
            CRTM_AtmAbsorption.f90 \
            CRTM_AtmScatter_Define.f90 \
            CRTM_AerosolScatter.f90 \
            CRTM_CloudScatter.f90 \
            CRTM_SfcOptics_Define.f90 \
            CRTM_SfcOptics.f90 \
            CRTM_IR_Ice_SfcOptics.f90 \
            CRTM_IR_Land_SfcOptics.f90 \
            CRTM_IR_Snow_SfcOptics.f90 \
            CRTM_IR_Water_SfcOptics.f90 \
            CRTM_MW_Ice_SfcOptics.f90 \
            CRTM_MW_Land_SfcOptics.f90 \
            CRTM_MW_Snow_SfcOptics.f90 \
            CRTM_Fastem1.f90 \
            CRTM_MW_Water_SfcOptics.f90 \
            CRTM_IRSSEM.f90 \
            CRTM_surface_ir_emissivity.f90 \
            CRTM_AtmOptics.f90 \
            CRTM_RTSolution_Define.f90 \
            CRTM_RTSolution.f90 \
            CRTM_Planck_Functions.f90 \
            CRTM_Options_Define.f90 \
            CRTM_Parameters.f90 \
            CRTM_LifeCycle.f90 \
            CRTM_Forward_Module.f90 \
            CRTM_Tangent_Linear_Module.f90 \
            CRTM_Adjoint_Module.f90 \
            CRTM_K_Matrix_Module.f90 \
            CRTM_Module.f90


  SRCSF77 =

  SRCSC =

  SRCS = $(SRCSF90C) $(SRCSF77) $(SRCSC) $(XSRCSC)

  DOCSRCS = *.f90 *.F90

# ------------
# Object files
# ------------

  SRCSF90	= ${SRCSF90C:.F90=.f90}

  OBJS 		= ${SRCSF90:.f90=.o} ${SRCSF77:.f=.o} ${SRCSC:.c=.o}


# -----------------------
# Default compiling rules
# -----------------------

.SUFFIXES : .F90 .f90 .f .c

.F90.o  :
	@echo
	@echo '---> Compiling $<'
	$(CF) $(FFLAGS) -c $<

.f90.o  :
	@echo
	@echo '---> Compiling $<'
	$(CF) $(FFLAGS) -c $<

.f.o  :
	@echo
	@echo '---> Compiling $<'
	$(CF) $(FFLAGS_f) -c $<

.c.o  :
	@echo
	@echo '---> Compiling $<'
	$(CC) $(CFLAGS) -c $<


# ------------
# Dependencies
# ------------
include Makefile.dependency


# ------------------------
# Call compiler and linker
# ------------------------

all :
	@$(MAKE) -f $(MAKE_FILE) "COMP_MODE=$@" check_mode
	@echo
	@echo '==== COMPILE ================================================='
	@$(MAKE) -f $(MAKE_FILE) \
		"FFLAGS=$(FFLAGS_N)" \
		"CFLAGS=$(CFLAGS_N)" \
		$(OBJS) 
	@echo
	@echo '==== ARCHIVE ===================================================='
	@$(MAKE) -f $(MAKE_FILE) lib


debug :
	@$(MAKE) -f $(MAKE_FILE) "COMP_MODE=$@" check_mode
	@echo
	@echo '==== COMPILE ================================================='
	@$(MAKE) -f $(MAKE_FILE) \
		"FFLAGS=$(FFLAGS_D)" \
		"CFLAGS=$(CFLAGS_D)" \
		$(OBJS) 
	@echo
	@echo '==== ARCHIVE ===================================================='
	@$(MAKE) -f $(MAKE_FILE) lib 

check_mode :
	@if [ -e $(LOG_FILE) ]; then \
	  if [ '$(COMP_MODE)' != `head -n 1 $(LOG_FILE)` ]; then \
	    echo ;\
	    echo "### COMPILE MODE WAS CHANGED ###" ;\
	    make clean ;\
	  fi ;\
	else \
	  echo ;\
	  echo "### NO LOG FILE ###" ;\
	  make clean ;\
	fi
	@echo $(COMP_MODE) > $(LOG_FILE)

# -------------------------
# GMAO Nomenclature/targets
# -------------------------
LIB =   libcrtm2.a

lib: $(LIB)

$(LIB): $(OBJS)
	$(RM) $(LIB)
	$(AR) $@ $(OBJS)
	@if [ ! -d $(CORELIB) ]; then \
	   $(MKDIR) $(CORELIB) ;\
	fi
	$(CP) $(LIB) $(CORELIB)/
	@if [ ! -d $(COREINC) ]; then \
	   $(MKDIR) $(COREINC) ;\
	fi
	$(CP) *.mod  $(COREINC)/

doc:              AnIntro $(DOCSRC)
	$(PROTEX) AnIntro *.f90 *.F90 >  crtm2.tex
	$(LATEX) crtm2.tex
	$(LATEX) crtm2.tex

doclean:
	- $(RM) *.tex *.dvi *.aux *.toc *.log *.ps *.pdf

help:
	@ echo "Available targets:"
	@ echo "make             creates crtm2 library"
	@ echo "make debug       created crtm2 library for debugging purposes"
	@ echo "make install     creates crtm2 exec & places it in bin"
	@ echo "make lib         creates crtm2 library"
	@ echo "make clean       cleans objects, exec, and alien files"  
	@ echo "make doc         creates documentation"
	@ echo "make doclean     clean doc-related temporary files"

