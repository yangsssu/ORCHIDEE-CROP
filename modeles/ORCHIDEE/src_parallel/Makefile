#-
#- $Id: AA_make.ldef 1536 2013-10-14 13:38:24Z josefine.ghattas $
#-
#---------------------------------------------------------------------
#- Creation des elements relatifs a PARALLEL
#---------------------------------------------------------------------
SHELL = /bin/sh
#---------------------------------------------------------------------
LIBDIR = ../../../lib
BINDIR = ../../../bin
MODDIR = $(LIBDIR)
ORDIR  = ..
#---------------------------------------------------------------------
ORCHIDEE_LIB = $(LIBDIR)/liborchidee.a
MODEL_LIB = $(LIBDIR)/libparallel.a
SXMODEL_LIB = $(MODEL_LIB)
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
#- $Id: AA_make 3026 2015-11-16 13:26:03Z albert.jornet $
#-
MODS1 = data_para.f90 \
        timer.f90 \
        transfert_para.f90 \
	ioipsl_para.f90 \
	tools_para.f90 \
	parallel.f90
#\
# 	orch_write_field.f90 \
# 	write_field.f90 \
# 	orch_write_field_p.f90\
# 	write_field_p.f90

OBJSMODS1 = $(MODS1:.f90=.o)
#-
.PRECIOUS : $(MODEL_LIB)
#-
all:
	$(M_K) m_all
	@echo parallel is OK

m_all: $(MODEL_LIB)($(OBJSMODS1))

$(MODEL_LIB)(%.o) : %.f90
	$(F_C) $(F_O) -I$(NCDF_INC) $*.f90
	$(A_C) $(MODEL_LIB) $*.o
	$(RM) $*.o

config : 
	$(BINDIR)/Fparser -name PARALLEL $(MODS1)
	echo 'Configuration of PARALLEL done'

clean:
	$(RM) $(MODEL_LIB)

$(MODEL_LIB)(mpi_dummy.o):

$(MODEL_LIB)(timer.o):

$(MODEL_LIB)(data_para.o):

$(MODEL_LIB)(orch_write_field.o):

$(MODEL_LIB)(write_field_p.o): \
  $(MODEL_LIB)(orch_write_field.o)

$(MODEL_LIB)(transfert_para.o): \
  $(MODEL_LIB)(timer.o) \
  $(MODEL_LIB)(data_para.o) \

$(MODEL_LIB)(ioipsl_para.o): \
  $(MODEL_LIB)(transfert_para.o) \
  $(MODEL_LIB)(data_para.o)

$(MODEL_LIB)(tools_para.o): \
  $(MODEL_LIB)(timer.o) \
  $(MODEL_LIB)(data_para.o)

$(MODEL_LIB)(orch_write_field_p.o): \
  $(MODEL_LIB)(parallel.o)

$(MODEL_LIB)(write_field_p.o): \
  $(MODEL_LIB)(parallel.o) \
  $(MODEL_LIB)(orch_write_field_p.o)

$(MODEL_LIB)(parallel.o): \
  $(MODEL_LIB)(data_para.o) \
  $(MODEL_LIB)(transfert_para.o) \
  $(MODEL_LIB)(ioipsl_para.o) 
