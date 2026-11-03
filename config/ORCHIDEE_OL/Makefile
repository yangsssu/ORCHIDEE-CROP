#- $Id$
#---------------------------------------------------------------------
#- Initialisations for ORCHIDEE_OL configuration
#---------------------------------------------------------------------
SHELL = /bin/sh
#---------------------------------------------------------------------
LIBDIR = ../../lib
BINDIR = ../../bin
MODDIR = $(LIBDIR)
#---------------------------------------------------------------------
#-

#-
#- $Id: AA_make.gdef 4976 2020-02-25 10:06:58Z aclsce $
#-
#- Validate the correlation between the target and the environment
#-
UTIL_DIR = ../../util
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

# $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/trunk/ORCHIDEE_OL/AA_make $
# $Date: 2016-01-11 15:09:09 +0100 (Mon, 11 Jan 2016) $
# $Revision: 3108 $
#
#- AA_make for ORCHIDEE_OL configuration
#- This is the portable part of the main Makefile. AA_make toghether with machine 
#- dependent options in modipsl/util/AA_make.gdef and AA_make.ldef will form the 
#- complete Makefile. This Makefile will only call the Makefile of ORCHIDEE.
#- 
#- Compile using one of the main targets.

### Main targets
#############################################################################################
# Default method : Compiling ORCHIDEE library and offline executables with XIOS
all : with_xios

# without_xios : Compiling ORCHIDEE and IOIPSL. Do not compile or link to XIOS.
without_xios : ioipsl driver verif

# with_xios : Compiling ORCHIDEE, XIOS and IOIPSL.
with_xios : xios ioipsl driver_xios verif


# clean : Remove everything created during compilation including the executables
clean :
	(cd ../../modeles/IOIPSL/src ; ${M_K} clean ; )
	(cd ../../modeles/ORCHIDEE ; ./makeorchidee_fcm -clean ; )
	(rm -rf ../../modeles/XIOS/bin/ ../../modeles/XIOS/inc ../../modeles/XIOS/obj ../../modeles/XIOS/lib ; )
	(rm -f ../../bin/orchidee_ol ; rm -f ../../bin/teststomate ; rm -f ../../bin/forcesoil ; rm -f ../../bin/xios_server.exe ; )

### Internal targets
#############################################################################################
# xios : Only compiling XIOS (using fcm)
xios :
	(cd  ../../modeles/XIOS ; ./make_xios --prod --arch ${FCM_ARCH} --job 8 ; cp bin/xios_server.exe ../../bin/. ; )

# ioipsl : Only compiling IOIPSL (standard Makefile)
ioipsl :
	(cd ../../modeles/IOIPSL/src ; $(M_K) ; )

# driver : Only compiling ORCHIDEE without linking to XIOS (with fcm method)
driver :
	(cd ../../modeles/ORCHIDEE ; ./makeorchidee_fcm        -j 8 -parallel mpi -prod -arch ${FCM_ARCH} -driver ; )

# driver_xios : Only compiling ORCHIDEE and linking to XIOS 1.0 (with fcm method)
#               Note: For linking with XIOS 2.0 change -xios into -xios2
driver_xios :
	(cd ../../modeles/ORCHIDEE ; ./makeorchidee_fcm -j 8 -parallel mpi -prod -arch ${FCM_ARCH} -driver ; )

#driver_xios :
#        (cd ../../modeles/ORCHIDEE ; ./makeorchidee_fcm -xios -j 8 -parallel mpi -prod -arch ${FCM_ARCH} -driver ; )


# verif : List contents in executable directory
verif: ../../bin
	ls -lrt ../../bin



