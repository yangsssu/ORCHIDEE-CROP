# Automatic Make rule for orchidee_ol

SRCDIR0__orchidee_ol = /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/modeles/ORCHIDEE/src_driver

PPSRCDIR0__orchidee_ol = /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/modeles/ORCHIDEE/.config/ppsrc/orchidee_ol

orchidee_ol.etc : \
          $(SRCDIR0__orchidee_ol)/AA_make \
          $(SRCDIR0__orchidee_ol)/AA_make.ldef \
          $(SRCDIR0__orchidee_ol)/Makefile \
          $(FCM_DONEDIR)/FCM_CP.dummy
	cp $^ $(FCM_ETCDIR)
	touch $(FCM_DONEDIR)/$@

FFLAGS__orchidee_ol__teststomate.flags: \
          FFLAGS__orchidee_ol.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__orchidee_ol__teststomate.flags: \
          LDFLAGS__orchidee_ol.flags
	touch $(FCM_FLAGSDIR)/$@

LD__orchidee_ol__teststomate.flags: \
          LD__orchidee_ol.flags
	touch $(FCM_FLAGSDIR)/$@

teststomate.exe: \
          teststomate.o \
          LD__orchidee_ol__teststomate.flags \
          LDFLAGS__orchidee_ol__teststomate.flags \
          $(OBJECTS__global) \
          $(OBJECTS__parallel) \
          $(OBJECTS__ext_src) \
          $(OBJECTS__stomate) \
          $(OBJECTS__parameters) \
          $(OBJECTS__sticslai) \
          $(OBJECTS__orchidee_ol) \
          $(OBJECTS__sechiba) \
          constantes.done \
          constantes_soil.done \
          grid.done \
          intersurf.done \
          ioipsl_para.done \
          ioipslctrl.done \
          mod_orchidee_para.done \
          pft_parameters.done \
          slowproc.done \
          stomate.done \
          stomate_data.done \
          tools_para.done
	fcm_internal load orchidee_ol $< $@

teststomate.o: \
          $(PPSRCDIR0__orchidee_ol)/teststomate.f90 \
          FFLAGS__orchidee_ol__teststomate.flags \
          constantes.o \
          constantes_soil.o \
          grid.o \
          intersurf.o \
          ioipsl_para.o \
          ioipslctrl.o \
          mod_orchidee_para.o \
          pft_parameters.o \
          slowproc.o \
          stomate.o \
          stomate_data.o \
          tools_para.o
	fcm_internal compile:F orchidee_ol $< $@

FFLAGS__orchidee_ol__weather.flags: \
          FFLAGS__orchidee_ol.flags
	touch $(FCM_FLAGSDIR)/$@

weather.done: \
          weather.o \
          constantes.done \
          grid.done \
          ioipsl_para.done \
          mod_orchidee_para.done \
          solar.done
	touch $(FCM_DONEDIR)/$@

weather.o: \
          $(PPSRCDIR0__orchidee_ol)/weather.f90 \
          FFLAGS__orchidee_ol__weather.flags \
          constantes.o \
          grid.o \
          ioipsl_para.o \
          mod_orchidee_para.o \
          solar.o
	fcm_internal compile:F orchidee_ol $< $@

FFLAGS__orchidee_ol__forcesoil.flags: \
          FFLAGS__orchidee_ol.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__orchidee_ol__forcesoil.flags: \
          LDFLAGS__orchidee_ol.flags
	touch $(FCM_FLAGSDIR)/$@

LD__orchidee_ol__forcesoil.flags: \
          LD__orchidee_ol.flags
	touch $(FCM_FLAGSDIR)/$@

forcesoil.exe: \
          forcesoil.o \
          LD__orchidee_ol__forcesoil.flags \
          LDFLAGS__orchidee_ol__forcesoil.flags \
          $(OBJECTS__global) \
          $(OBJECTS__parallel) \
          $(OBJECTS__ext_src) \
          $(OBJECTS__stomate) \
          $(OBJECTS__parameters) \
          $(OBJECTS__sticslai) \
          $(OBJECTS__orchidee_ol) \
          $(OBJECTS__sechiba) \
          constantes.done \
          constantes_mtc.done \
          constantes_soil.done \
          constantes_soil_var.done \
          ioipsl_para.done \
          mod_orchidee_para.done \
          pft_parameters.done \
          stomate_data.done \
          stomate_permafrost_soilcarbon.done \
          stomate_soilcarbon.done
	fcm_internal load orchidee_ol $< $@

forcesoil.o: \
          $(PPSRCDIR0__orchidee_ol)/forcesoil.f90 \
          FFLAGS__orchidee_ol__forcesoil.flags \
          constantes.o \
          constantes_mtc.o \
          constantes_soil.o \
          constantes_soil_var.o \
          ioipsl_para.o \
          mod_orchidee_para.o \
          pft_parameters.o \
          stomate_data.o \
          stomate_permafrost_soilcarbon.o \
          stomate_soilcarbon.o
	fcm_internal compile:F orchidee_ol $< $@

FFLAGS__orchidee_ol__dim2_driver.flags: \
          FFLAGS__orchidee_ol.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__orchidee_ol__dim2_driver.flags: \
          LDFLAGS__orchidee_ol.flags
	touch $(FCM_FLAGSDIR)/$@

LD__orchidee_ol__dim2_driver.flags: \
          LD__orchidee_ol.flags
	touch $(FCM_FLAGSDIR)/$@

dim2_driver.exe: \
          driver.o \
          LD__orchidee_ol__dim2_driver.flags \
          LDFLAGS__orchidee_ol__dim2_driver.flags \
          $(OBJECTS__global) \
          $(OBJECTS__parallel) \
          $(OBJECTS__ext_src) \
          $(OBJECTS__stomate) \
          $(OBJECTS__parameters) \
          $(OBJECTS__sticslai) \
          $(OBJECTS__orchidee_ol) \
          $(OBJECTS__sechiba) \
          constantes.done \
          grid.done \
          intersurf.done \
          ioipsl_para.done \
          mod_orchidee_para.done \
          readdim2.done \
          timer.done
	fcm_internal load orchidee_ol $< $@

driver.o: \
          $(PPSRCDIR0__orchidee_ol)/dim2_driver.f90 \
          FFLAGS__orchidee_ol__dim2_driver.flags \
          constantes.o \
          grid.o \
          intersurf.o \
          ioipsl_para.o \
          mod_orchidee_para.o \
          readdim2.o \
          timer.o
	fcm_internal compile:F orchidee_ol $< $@

FFLAGS__orchidee_ol__getprec.flags: \
          FFLAGS__orchidee_ol.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__orchidee_ol__getprec.flags: \
          LDFLAGS__orchidee_ol.flags
	touch $(FCM_FLAGSDIR)/$@

LD__orchidee_ol__getprec.flags: \
          LD__orchidee_ol.flags
	touch $(FCM_FLAGSDIR)/$@

getprec.exe: \
          getprec.o \
          LD__orchidee_ol__getprec.flags \
          LDFLAGS__orchidee_ol__getprec.flags \
          $(OBJECTS__global) \
          $(OBJECTS__parallel) \
          $(OBJECTS__ext_src) \
          $(OBJECTS__stomate) \
          $(OBJECTS__parameters) \
          $(OBJECTS__sticslai) \
          $(OBJECTS__orchidee_ol) \
          $(OBJECTS__sechiba)
	fcm_internal load orchidee_ol $< $@

getprec.o: \
          $(PPSRCDIR0__orchidee_ol)/getprec.f90 \
          FFLAGS__orchidee_ol__getprec.flags
	fcm_internal compile:F orchidee_ol $< $@

FFLAGS__orchidee_ol__readdim2.flags: \
          FFLAGS__orchidee_ol.flags
	touch $(FCM_FLAGSDIR)/$@

readdim2.done: \
          readdim2.o \
          timer.done \
          constantes.done \
          grid.done \
          ioipsl_para.done \
          mod_orchidee_para.done \
          solar.done \
          weather.done
	touch $(FCM_DONEDIR)/$@

readdim2.o: \
          $(PPSRCDIR0__orchidee_ol)/readdim2.f90 \
          FFLAGS__orchidee_ol__readdim2.flags \
          timer.o \
          constantes.o \
          grid.o \
          ioipsl_para.o \
          mod_orchidee_para.o \
          solar.o \
          weather.o
	fcm_internal compile:F orchidee_ol $< $@

