# Automatic Make rule for parameters

SRCDIR0__parameters = /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/modeles/ORCHIDEE/src_parameters

PPSRCDIR0__parameters = /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/modeles/ORCHIDEE/.config/ppsrc/parameters

parameters.etc : \
          $(SRCDIR0__parameters)/constantes_soil_var.f90~ \
          $(SRCDIR0__parameters)/pft_parameters.f90~ \
          $(SRCDIR0__parameters)/AA_make \
          $(SRCDIR0__parameters)/vertical_soil.f90~ \
          $(SRCDIR0__parameters)/constantes_mtc.f90.ini \
          $(SRCDIR0__parameters)/constantes_soil.f90~ \
          $(SRCDIR0__parameters)/constantes_var.f90~ \
          $(SRCDIR0__parameters)/AA_make.ldef \
          $(SRCDIR0__parameters)/Makefile \
          $(FCM_DONEDIR)/FCM_CP.dummy
	cp $^ $(FCM_ETCDIR)
	touch $(FCM_DONEDIR)/$@

FFLAGS__parameters__constantes.flags: \
          FFLAGS__parameters.flags
	touch $(FCM_FLAGSDIR)/$@

constantes.done: \
          constantes.o \
          constantes_var.done \
          ioipsl_para.done \
          mod_orchidee_para.done
	touch $(FCM_DONEDIR)/$@

constantes.o: \
          $(PPSRCDIR0__parameters)/constantes.f90 \
          FFLAGS__parameters__constantes.flags \
          constantes_var.o \
          ioipsl_para.o \
          mod_orchidee_para.o
	fcm_internal compile:F parameters $< $@

FFLAGS__parameters__vertical_soil_var.flags: \
          FFLAGS__parameters.flags
	touch $(FCM_FLAGSDIR)/$@

vertical_soil_var.done: \
          vertical_soil_var.o
	touch $(FCM_DONEDIR)/$@

vertical_soil_var.o: \
          $(PPSRCDIR0__parameters)/vertical_soil_var.f90 \
          FFLAGS__parameters__vertical_soil_var.flags
	fcm_internal compile:F parameters $< $@

FFLAGS__parameters__vertical_soil.flags: \
          FFLAGS__parameters.flags
	touch $(FCM_FLAGSDIR)/$@

vertical_soil.done: \
          vertical_soil.o \
          ioipsl_para.done \
          vertical_soil_var.done
	touch $(FCM_DONEDIR)/$@

vertical_soil.o: \
          $(PPSRCDIR0__parameters)/vertical_soil.f90 \
          FFLAGS__parameters__vertical_soil.flags \
          ioipsl_para.o \
          vertical_soil_var.o
	fcm_internal compile:F parameters $< $@

FFLAGS__parameters__constantes_soil.flags: \
          FFLAGS__parameters.flags
	touch $(FCM_FLAGSDIR)/$@

constantes_soil.done: \
          constantes_soil.o \
          constantes.done \
          constantes_soil_var.done \
          ioipsl_para.done
	touch $(FCM_DONEDIR)/$@

constantes_soil.o: \
          $(PPSRCDIR0__parameters)/constantes_soil.f90 \
          FFLAGS__parameters__constantes_soil.flags \
          constantes.o \
          constantes_soil_var.o \
          ioipsl_para.o
	fcm_internal compile:F parameters $< $@

FFLAGS__parameters__pft_parameters.flags: \
          FFLAGS__parameters.flags
	touch $(FCM_FLAGSDIR)/$@

pft_parameters.done: \
          pft_parameters.o \
          constantes.done \
          constantes_mtc.done \
          constantes_soil_var.done \
          ioipsl_para.done \
          pft_parameters_var.done \
          vertical_soil_var.done
	touch $(FCM_DONEDIR)/$@

pft_parameters.o: \
          $(PPSRCDIR0__parameters)/pft_parameters.f90 \
          FFLAGS__parameters__pft_parameters.flags \
          constantes.o \
          constantes_mtc.o \
          constantes_soil_var.o \
          ioipsl_para.o \
          pft_parameters_var.o \
          vertical_soil_var.o
	fcm_internal compile:F parameters $< $@

FFLAGS__parameters__constantes_soil_var.flags: \
          FFLAGS__parameters.flags
	touch $(FCM_FLAGSDIR)/$@

constantes_soil_var.done: \
          constantes_soil_var.o \
          vertical_soil_var.done
	touch $(FCM_DONEDIR)/$@

constantes_soil_var.o: \
          $(PPSRCDIR0__parameters)/constantes_soil_var.f90 \
          FFLAGS__parameters__constantes_soil_var.flags \
          vertical_soil_var.o
	fcm_internal compile:F parameters $< $@

FFLAGS__parameters__control.flags: \
          FFLAGS__parameters.flags
	touch $(FCM_FLAGSDIR)/$@

control.done: \
          control.o \
          constantes_soil.done \
          constantes_var.done \
          pft_parameters.done \
          vertical_soil.done
	touch $(FCM_DONEDIR)/$@

control.o: \
          $(PPSRCDIR0__parameters)/control.f90 \
          FFLAGS__parameters__control.flags \
          constantes_soil.o \
          constantes_var.o \
          pft_parameters.o \
          vertical_soil.o
	fcm_internal compile:F parameters $< $@

FFLAGS__parameters__constantes_var.flags: \
          FFLAGS__parameters.flags
	touch $(FCM_FLAGSDIR)/$@

constantes_var.done: \
          constantes_var.o
	touch $(FCM_DONEDIR)/$@

constantes_var.o: \
          $(PPSRCDIR0__parameters)/constantes_var.f90 \
          FFLAGS__parameters__constantes_var.flags
	fcm_internal compile:F parameters $< $@

FFLAGS__parameters__pft_parameters_var.flags: \
          FFLAGS__parameters.flags
	touch $(FCM_FLAGSDIR)/$@

pft_parameters_var.done: \
          pft_parameters_var.o
	touch $(FCM_DONEDIR)/$@

pft_parameters_var.o: \
          $(PPSRCDIR0__parameters)/pft_parameters_var.f90 \
          FFLAGS__parameters__pft_parameters_var.flags
	fcm_internal compile:F parameters $< $@

FFLAGS__parameters__constantes_mtc.flags: \
          FFLAGS__parameters.flags
	touch $(FCM_FLAGSDIR)/$@

constantes_mtc.done: \
          constantes_mtc.o \
          constantes.done
	touch $(FCM_DONEDIR)/$@

constantes_mtc.o: \
          $(PPSRCDIR0__parameters)/constantes_mtc.f90 \
          FFLAGS__parameters__constantes_mtc.flags \
          constantes.o
	fcm_internal compile:F parameters $< $@

