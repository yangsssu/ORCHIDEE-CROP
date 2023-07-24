# Automatic Make rule for global

SRCDIR0__global = /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/modeles/ORCHIDEE/src_global

PPSRCDIR0__global = /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/modeles/ORCHIDEE/.config/ppsrc/global

global.etc : \
          $(SRCDIR0__global)/AA_make \
          $(SRCDIR0__global)/AA_make.ldef \
          $(SRCDIR0__global)/Makefile \
          $(FCM_DONEDIR)/FCM_CP.dummy
	cp $^ $(FCM_ETCDIR)
	touch $(FCM_DONEDIR)/$@

FFLAGS__global__solar.flags: \
          FFLAGS__global.flags
	touch $(FCM_FLAGSDIR)/$@

solar.done: \
          solar.o \
          constantes.done \
          ioipsl_para.done
	touch $(FCM_DONEDIR)/$@

solar.o: \
          $(PPSRCDIR0__global)/solar.f90 \
          FFLAGS__global__solar.flags \
          constantes.o \
          ioipsl_para.o
	fcm_internal compile:F global $< $@

FFLAGS__global__gauss_jordan_method.flags: \
          FFLAGS__global.flags
	touch $(FCM_FLAGSDIR)/$@

matrix_resolution.done: \
          matrix_resolution.o \
          constantes.done
	touch $(FCM_DONEDIR)/$@

matrix_resolution.o: \
          $(PPSRCDIR0__global)/gauss_jordan_method.f90 \
          FFLAGS__global__gauss_jordan_method.flags \
          constantes.o
	fcm_internal compile:F global $< $@

FFLAGS__global__interpol_help.flags: \
          FFLAGS__global.flags
	touch $(FCM_FLAGSDIR)/$@

interpol_help.done: \
          interpol_help.o \
          constantes.done \
          grid.done \
          mod_orchidee_para.done
	touch $(FCM_DONEDIR)/$@

interpol_help.o: \
          $(PPSRCDIR0__global)/interpol_help.f90 \
          FFLAGS__global__interpol_help.flags \
          constantes.o \
          grid.o \
          mod_orchidee_para.o
	fcm_internal compile:F global $< $@

FFLAGS__global__grid.flags: \
          FFLAGS__global.flags
	touch $(FCM_FLAGSDIR)/$@

grid.done: \
          grid.o \
          constantes.done \
          mod_orchidee_para.done
	touch $(FCM_DONEDIR)/$@

grid.o: \
          $(PPSRCDIR0__global)/grid.f90 \
          FFLAGS__global__grid.flags \
          constantes.o \
          mod_orchidee_para.o
	fcm_internal compile:F global $< $@

