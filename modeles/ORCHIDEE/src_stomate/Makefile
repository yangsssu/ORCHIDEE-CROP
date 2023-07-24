#-
#- $Id: AA_make.ldef 1536 2013-10-14 13:38:24Z josefine.ghattas $
#-
#---------------------------------------------------------------------
#- Creation des elements relatifs a STOMATE
#---------------------------------------------------------------------
SHELL = /bin/sh
#---------------------------------------------------------------------
LIBDIR = ../../../lib
BINDIR = ../../../bin
MODDIR = $(LIBDIR)
#---------------------------------------------------------------------
ORCHIDEE_LIB = $(LIBDIR)/liborchidee.a
MODEL_LIB = $(LIBDIR)/libstomate.a
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
#- $Id: AA_make 3306 2016-03-22 12:22:18Z albert.jornet $
#-
PARALLEL_LIB = $(LIBDIR)/libparallel.a
SXPARALLEL_LIB = $(PARALLEL_LIB)
#-
PARAM_LIB = $(LIBDIR)/libparameters.a
SXPARAM_LIB = $(PARAM_LIB)
#-
ORGLOB_LIB = $(LIBDIR)/liborglob.a
SXORGLOB_LIB = $(ORGLOB_LIB)
#-
MODS1 = stomate_data.f90      \
        lpj_constraints.f90        \
        lpj_cover.f90              \
        lpj_crown.f90              \
        lpj_establish.f90          \
        lpj_spitfire.f90               \
        lpj_gap.f90                \
        lpj_kill.f90               \
        lpj_light.f90              \
        lpj_pftinout.f90           \
        stomate_alloc.f90          \
        stomate_data.f90           \
        stomate_io.f90             \
        stomate_litter.f90         \
        stomate_npp.f90            \
        stomate_phenology.f90      \
        stomate_prescribe.f90      \
        stomate_season.f90         \
        stomate_soilcarbon.f90     \
        stomate_turnover.f90       \
        stomate_vmax.f90           \
        stomate_lcchange.f90       \
        stomate_glcchange_fh.f90       \
        stomate_glcchange_SinAgeC_fh.f90       \
        constantes_PaSim.f90         \
        fonctions_PaSim.f90          \
        Applic_Plantes.f90           \
        Fauche_Orchidee.f90          \
        Fertilisation_Orchidee.f90   \
        Animals_Orchidee.f90         \
        Grassland_Management.f90     \
        stomate_lpj.f90            \
        stomate_resp.f90           \
        stomate_permafrost_soilcarbon.f90  \
        stomate_wet_ch4_pt_ter_0.f90 \
        stomate_wet_ch4_pt_ter_wet1.f90 \
        stomate_wet_ch4_pt_ter_wet2.f90 \
        stomate_wet_ch4_pt_ter_wet3.f90 \
        stomate_wet_ch4_pt_ter_wet4.f90 \
        stomate.f90

OBJSMODS1 = $(MODS1:.f90=.o)
#-
.PRECIOUS : $(MODEL_LIB)
#-
all:
	$(M_K) libparallel
	$(M_K) libparameters
	$(M_K) liborglob
	$(M_K) m_all
	@echo stomate is OK

m_all: $(MODEL_LIB)($(OBJSMODS1))

memory:

libparallel:
	(cd ../src_parallel; $(M_K) -f Makefile)

libparameters:
	(cd ../src_parameters; $(M_K) -f Makefile)

liborglob:
	(cd ../src_global; $(M_K) -f Makefile)

$(MODEL_LIB)(%.o): %.f90
	$(F_C) $(F_O) -I$(NCDF_INC) $*.f90
	$(A_C) $(MODEL_LIB) $*.o
	$(A_C) $(ORCHIDEE_LIB) $*.o
	$(RM) $*.o

config: 
	$(BINDIR)/Fparser -name STOMATE $(MODS1)
	echo 'Configuration of STOMATE done'

clean:
	$(RM) $(MODEL_LIB)

$(MODEL_LIB)(stomate.o):          \
  $(PARAM_LIB)(pft_parameters.o)  \
  $(MODEL_LIB)(stomate_io.o)      \
  $(MODEL_LIB)(stomate_permafrost_soilcarbon.o)  \
  $(MODEL_LIB)(stomate_lpj.o)

$(MODEL_LIB)(stomate_permafrost_soilcarbon.o): \
  $(MODEL_LIB)(stomate_data.o) \
  $(PARAM_LIB)(pft_parameters.o) \
  $(PARAM_LIB)(constantes_soil.o) \
  $(PARAM_LIB)(constantes_soil_var.o) \
  $(PARAM_LIB)(constantes_var.o)
 
$(MODEL_LIB)(stomate_data.o): \
  $(PARAM_LIB)(pft_parameters.o)	\
  $(PARAM_LIB)(constantes.o)  

$(MODEL_LIB)(lpj_constraints.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(lpj_cover.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(lpj_crown.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(lpj_establish.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(lpj_fire.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(lpj_gap.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(lpj_kill.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(lpj_light.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(lpj_pftinout.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(stomate_alloc.o): \
  $(PARAM_LIB)(pft_parameters.o)	\
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(stomate_io.o): \
  $(MODEL_LIB)(stomate_data.o) \
  $(PARALLEL_LIB)(mod_orchidee_para.o)


$(MODEL_LIB)(stomate_litter.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(stomate_npp.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(stomate_wet_ch4_pt_ter_0.o): \
  $(PARAM_LIB)(pft_parameters.o)

$(MODEL_LIB)(stomate_wet_ch4_pt_ter_wet1.o): \
  $(PARAM_LIB)(pft_parameters.o)

$(MODEL_LIB)(stomate_wet_ch4_pt_ter_wet2.o): \
  $(PARAM_LIB)(pft_parameters.o)

$(MODEL_LIB)(stomate_wet_ch4_pt_ter_wet3.o): \
  $(PARAM_LIB)(pft_parameters.o)

$(MODEL_LIB)(stomate_wet_ch4_pt_ter_wet4.o): \
  $(PARAM_LIB)(pft_parameters.o)

$(MODEL_LIB)(stomate_phenology.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(stomate_prescribe.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(stomate_resp.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(stomate_season.o): \
  $(PARAM_LIB)(pft_parameters.o)	\
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(stomate_soilcarbon.o): \
  $(MODEL_LIB)(stomate_data.o) \
  $(ORGLOB_LIB)(grid.o)\
  $(ORGLOB_LIB)(solar.o)

$(MODEL_LIB)(stomate_turnover.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(stomate_vmax.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(stomate_lcchange.o): \
  $(MODEL_LIB)(stomate_data.o)

$(MODEL_LIB)(constantes_PaSim.o): \
  $(PARAM_LIB)(pft_parameters.o)        \
  $(PARAM_LIB)(constantes.o)

$(MODEL_LIB)(fonctions_PaSim.o): \
  $(PARAM_LIB)(pft_parameters.o)        \
  $(PARAM_LIB)(constantes.o)            \
  $(MODEL_LIB)(constantes_PaSim.o)

$(MODEL_LIB)(Applic_Plantes.o): \
  $(PARAM_LIB)(pft_parameters.o)        \
  $(PARAM_LIB)(constantes.o)            \
  $(MODEL_LIB)(constantes_PaSim.o)      \
  $(MODEL_LIB)(fonctions_PaSim.o)

$(MODEL_LIB)(Fertilisation_Orchidee.o): \
  $(PARAM_LIB)(pft_parameters.o)        \
  $(PARAM_LIB)(constantes.o)            \
  $(MODEL_LIB)(constantes_PaSim.o)      \
  $(MODEL_LIB)(fonctions_PaSim.o)

$(MODEL_LIB)(Fauche_Orchidee.o): \
  $(PARAM_LIB)(pft_parameters.o)        \
  $(PARAM_LIB)(constantes.o)            \
  $(MODEL_LIB)(constantes_PaSim.o)      \
  $(MODEL_LIB)(fonctions_PaSim.o)

$(MODEL_LIB)(Animals_Orchidee.o): \
  $(PARAM_LIB)(pft_parameters.o)        \
  $(PARAM_LIB)(constantes.o)            \
  $(MODEL_LIB)(constantes_PaSim.o)      \
  $(MODEL_LIB)(fonctions_PaSim.o)

$(MODEL_LIB)(Grassland_Management.o): \
  $(PARAM_LIB)(pft_parameters.o)        \
  $(PARAM_LIB)(constantes.o)            \
  $(MODEL_LIB)(constantes_PaSim.o)      \
  $(MODEL_LIB)(fonctions_PaSim.o)       \
  $(MODEL_LIB)(Animals_Orchidee.o)      \
  $(MODEL_LIB)(Fauche_Orchidee.o)       \
  $(MODEL_LIB)(Applic_Plantes.o)        \
  $(MODEL_LIB)(Fertilisation_Orchidee.o)

$(MODEL_LIB)(stomate_lpj.o):            \
  $(MODEL_LIB)(lpj_constraints.o)       \
  $(MODEL_LIB)(lpj_cover.o)             \
  $(MODEL_LIB)(lpj_crown.o)             \
  $(MODEL_LIB)(lpj_establish.o)         \
  $(MODEL_LIB)(lpj_spitfire.o)          \
  $(MODEL_LIB)(lpj_fire.o)              \
  $(MODEL_LIB)(lpj_gap.o)               \
  $(MODEL_LIB)(lpj_kill.o)              \
  $(MODEL_LIB)(lpj_light.o)             \
  $(MODEL_LIB)(lpj_pftinout.o)          \
  $(MODEL_LIB)(stomate_alloc.o)         \
  $(MODEL_LIB)(stomate_data.o)          \
  $(MODEL_LIB)(stomate_litter.o)        \
  $(MODEL_LIB)(stomate_npp.o)           \
  $(MODEL_LIB)(stomate_phenology.o)     \
  $(MODEL_LIB)(stomate_prescribe.o)     \
  $(MODEL_LIB)(stomate_season.o)        \
  $(MODEL_LIB)(stomate_soilcarbon.o)    \
  $(MODEL_LIB)(stomate_turnover.o)      \
  $(MODEL_LIB)(stomate_vmax.o)          \
  $(MODEL_LIB)(stomate_resp.o)          \
  $(MODEL_LIB)(stomate_wet_ch4_pt_ter_0.o)             \
  $(MODEL_LIB)(stomate_wet_ch4_pt_ter_wet1.o)          \
  $(MODEL_LIB)(stomate_wet_ch4_pt_ter_wet2.o)          \
  $(MODEL_LIB)(stomate_wet_ch4_pt_ter_wet3.o)          \
  $(MODEL_LIB)(stomate_wet_ch4_pt_ter_wet4.o)          \
  $(MODEL_LIB)(stomate_lcchange.o)      \
  $(MODEL_LIB)(Grassland_Management.o)
