#-
#- $Id: AA_make.ldef 1042 2012-10-26 14:32:10Z josefine.ghattas $
#-
SHELL = /bin/sh
#---------------------------------------------------------------------
LIBDIR = ../../../lib
MODDIR = $(LIBDIR)
BINDIR = ../../../bin
SCPDIR = ../../scripts
ORDIR  = ..
#---------------------------------------------------------------------
MODEL_LIB = $(LIBDIR)/liborchidee.a
SXMODEL_LIB = $(MODEL_LIB)
#-
IOIPSL_LIB = $(LIBDIR)/libioipsl.a
SXIOIPSL_LIB = $(IOIPSL_LIB)
#-
PARAM_LIB = $(LIBDIR)/libparameters.a
SXPARAM_LIB = $(PARAM_LIB)
#-
PARALLEL_LIB = $(LIBDIR)/libparallel.a
SXPARALLEL_LIB = $(PARALLEL_LIB)
#-
ORGLOB_LIB = $(LIBDIR)/liborglob.a
SXORGLOB_LIB = $(ORGLOB_LIB)
#-
SECHIBA_LIB = $(LIBDIR)/libsechiba.a
SXSECHIBA_LIB = $(SECHIBA_LIB)
#-
STOMATE_LIB = $(LIBDIR)/libstomate.a
SXSTOMATE_LIB = $(STOMATE_LIB)
#-
#- $Id: AA_make.gdef 4976 2020-02-25 10:06:58Z aclsce $
#-
#- Validate the correlation between the target and the environment
#-
UTIL_DIR = ../../../util
#-
#-Q-
######-Q- ada      F_O = -DCPP_PARA -p -g -traceback -fp-stack-check -ftrapuv -check bounds $(F_D) $(F_P) -I$(MODDIR) -module $(MODDIR)
######-Q- curie  F_O = -DCPP_PARA -p -g -traceback -fp-stack-check -ftrapuv -check bounds $(F_D) $(F_P) -I$(MODDIR) -module $(MODDIR)
######-Q- cur_mono  F_O = -DCPP_PARA -p -g -traceback -fp-stack-check -ftrapuv -check bounds $(F_D) $(F_P) -I$(MODDIR) -module $(MODDIR)
#- Global definitions for Intel compiler (ifort) with MPI at obelix/LSCE
FCM_ARCH=ifort_LSCE
M_K = gmake
P_C = cpp
P_O = -P -C $(P_P)
F_C = mpif90 -c -cpp
F_D =
F_P = -i4 -r8
F_O = -DCPP_PARA -O3 $(F_D) $(F_P) -I$(MODDIR) -module $(MODDIR) -fp-model precise
####-Q- lxiv8    F_O = -DCPP_PARA -p -g -traceback -fp-stack-check -ftrapuv -check bounds $(F_D) $(F_P) -I$(MODDIR) -module $(MODDIR)
F_L = mpif90
M_M = 0
L_X = 0
L_O = -Vaxlib
A_C = ar -r
A_G = ar -x
C_C = cc -c
C_O =
C_L = cc
#-
NCDF_INC = /usr/local/install/netcdf-4.3.2p/include 
NCDF_LIB = -L/usr/local/install/netcdf-4.3.2p/lib -lnetcdff -lnetcdf
#-
######-Q- occigen  F_O = -DCPP_PARA -p -g -traceback -fp-stack-check -ftrapuv -check bounds $(F_D) $(F_P) -I$(MODDIR) -module $(MODDIR)
RM      = rm -f
STRIP   = strip
SIZE    = size

#-
#- $Id: AA_make 1042 2012-10-26 14:32:10Z josefine.ghattas $
#-
MODS1 = dim2_driver.f90 \
        weather.f90 \
        readdim2.f90 \
        teststomate.f90 \
        forcesoil.f90 \
        getprec.f90

all : orchidee_ol teststomate forcesoil

orchidee_ol:
	$(M_K) m_orchidee_ol
	@echo orchidee_ol is OK

m_orchidee_ol: $(BINDIR)/orchidee_ol
$(BINDIR)/orchidee_ol: \
  $(SXIOIPSL_LIB) $(SXPARAM_LIB) $(SXPARALLEL_LIB) $(SXORGLOB_LIB) $(SXSTOMATE_LIB) $(SXSECHIBA_LIB) \
  $(MODEL_LIB)(weather.o) $(MODEL_LIB)(readdim2.o) \
  $(MODEL_LIB)(dim2_driver.o)
	$(A_G) $(MODEL_LIB) dim2_driver.o
	$(F_L) -o $(BINDIR)/orchidee_ol dim2_driver.o $(L_O) \
          $(SXMODEL_LIB) $(SXSECHIBA_LIB) $(SXSTOMATE_LIB) \
          $(SXPARALLEL_LIB) $(SXORGLOB_LIB) $(SXPARAM_LIB) $(SXIOIPSL_LIB) $(NCDF_LIB) 
	$(RM) dim2_driver.o

teststomate:
	$(M_K) m_teststomate
	@echo teststomate is OK

m_teststomate: $(BINDIR)/teststomate
$(BINDIR)/teststomate: \
  $(SXIOIPSL_LIB) $(SXPARALLEL_LIB) $(SXPARAM_LIB) $(SXORGLOB_LIB) $(SXSTOMATE_LIB) $(SXSECHIBA_LIB) \
  $(MODEL_LIB)(teststomate.o)
	$(A_G) $(MODEL_LIB) teststomate.o
	$(F_L) -o $(BINDIR)/teststomate teststomate.o $(L_O) \
          $(SXMODEL_LIB) $(SXSECHIBA_LIB) $(SXSTOMATE_LIB) \
          $(SXPARALLEL_LIB) $(SXORGLOB_LIB) $(SXPARAM_LIB) $(SXIOIPSL_LIB) $(NCDF_LIB)
	$(RM) teststomate.o

forcesoil:
	$(M_K) m_forcesoil
	@echo m_forcesoil is OK

m_forcesoil: $(BINDIR)/forcesoil
$(BINDIR)/forcesoil: \
  $(SXIOIPSL_LIB) $(SXPARALLEL_LIB) $(SXPARAM_LIB) $(SXORGLOB_LIB) $(SXSTOMATE_LIB) $(SXSECHIBA_LIB) \
  $(MODEL_LIB)(forcesoil.o)
	$(A_G) $(MODEL_LIB) forcesoil.o
	$(F_L) -o $(BINDIR)/forcesoil forcesoil.o $(L_O) \
          $(SXMODEL_LIB) $(SXSECHIBA_LIB) $(SXSTOMATE_LIB) \
          $(SXPARALLEL_LIB) $(SXORGLOB_LIB) $(SXPARAM_LIB) $(SXIOIPSL_LIB) $(NCDF_LIB)
	$(RM) forcesoil.o

libioipsl:
	(cd ../../IOIPSL/src; $(M_K) -f Makefile)

clean:
	$(RM) $(MODEL_LIB)

$(MODEL_LIB)(%.o): %.f90
	$(F_C) $(F_O) -I$(NCDF_INC) $*.f90
	$(A_C) $(MODEL_LIB) $*.o
	$(RM) $*.o

