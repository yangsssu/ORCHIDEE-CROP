# Automatic Make rule for sechiba

SRCDIR0__sechiba = /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/modeles/ORCHIDEE/src_sechiba

PPSRCDIR0__sechiba = /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/modeles/ORCHIDEE/.config/ppsrc/sechiba

sechiba.etc : \
          $(SRCDIR0__sechiba)/diffuco.f90~ \
          $(SRCDIR0__sechiba)/tmp.txt~ \
          $(SRCDIR0__sechiba)/routing.f90~ \
          $(SRCDIR0__sechiba)/thermosoil.f90~ \
          $(SRCDIR0__sechiba)/ioipslctrl.ini \
          $(SRCDIR0__sechiba)/sechiba.f90~ \
          $(SRCDIR0__sechiba)/slowproc.f90~ \
          $(SRCDIR0__sechiba)/hydrol.f90~ \
          $(SRCDIR0__sechiba)/hydrol.f90.lowtranspir \
          $(SRCDIR0__sechiba)/enerbil.f90~ \
          $(SRCDIR0__sechiba)/#hydrol.f90# \
          $(SRCDIR0__sechiba)/hydrol.f90.ini \
          $(SRCDIR0__sechiba)/intersurf.f90~ \
          $(SRCDIR0__sechiba)/hydrol.f90.notimesplit \
          $(SRCDIR0__sechiba)/AA_make \
          $(SRCDIR0__sechiba)/ioipslctrl.f90~ \
          $(SRCDIR0__sechiba)/tmp.txt \
          $(SRCDIR0__sechiba)/AA_make.ldef \
          $(SRCDIR0__sechiba)/Makefile \
          $(FCM_DONEDIR)/FCM_CP.dummy
	cp $^ $(FCM_ETCDIR)
	touch $(FCM_DONEDIR)/$@

FFLAGS__sechiba__intersurf.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

intersurf.done: \
          intersurf.o \
          orch_write_field_p.done \
          constantes.done \
          constantes_soil.done \
          control.done \
          grid.done \
          ioipsl_para.done \
          ioipslctrl.done \
          mod_orchidee_para.done \
          pft_parameters.done \
          sechiba.done \
          solar.done \
          thermosoilc.done \
          timer.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

intersurf.o: \
          $(PPSRCDIR0__sechiba)/intersurf.f90 \
          FFLAGS__sechiba__intersurf.flags \
          orch_write_field_p.o \
          constantes.o \
          constantes_soil.o \
          control.o \
          grid.o \
          ioipsl_para.o \
          ioipslctrl.o \
          mod_orchidee_para.o \
          pft_parameters.o \
          sechiba.o \
          solar.o \
          thermosoilc.o \
          timer.o \
          xios_orchidee.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__chemistry.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

chemistry.done: \
          chemistry.o \
          constantes.done \
          grid.done \
          ioipsl_para.done \
          mod_orchidee_para.done \
          pft_parameters.done \
          qsat_moisture.done \
          sechiba_io.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

chemistry.o: \
          $(PPSRCDIR0__sechiba)/chemistry.f90 \
          FFLAGS__sechiba__chemistry.flags \
          constantes.o \
          grid.o \
          ioipsl_para.o \
          mod_orchidee_para.o \
          pft_parameters.o \
          qsat_moisture.o \
          sechiba_io.o \
          xios_orchidee.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__sechiba_io.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

sechiba_io.done: \
          sechiba_io.o \
          constantes.done \
          sechiba_io_p.done
	touch $(FCM_DONEDIR)/$@

sechiba_io.o: \
          $(PPSRCDIR0__sechiba)/sechiba_io.f90 \
          FFLAGS__sechiba__sechiba_io.flags \
          constantes.o \
          sechiba_io_p.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__thermosoil.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

thermosoil.done: \
          thermosoil.o \
          constantes.done \
          constantes_soil.done \
          grid.done \
          ioipsl_para.done \
          pft_parameters_var.done \
          sechiba_io.done \
          vertical_soil.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

thermosoil.o: \
          $(PPSRCDIR0__sechiba)/thermosoil.f90 \
          FFLAGS__sechiba__thermosoil.flags \
          constantes.o \
          constantes_soil.o \
          grid.o \
          ioipsl_para.o \
          pft_parameters_var.o \
          sechiba_io.o \
          vertical_soil.o \
          xios_orchidee.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__enerbil.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

enerbil.done: \
          enerbil.o \
          constantes.done \
          ioipsl_para.done \
          pft_parameters.done \
          qsat_moisture.done \
          sechiba_io.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

enerbil.o: \
          $(PPSRCDIR0__sechiba)/enerbil.f90 \
          FFLAGS__sechiba__enerbil.flags \
          constantes.o \
          ioipsl_para.o \
          pft_parameters.o \
          qsat_moisture.o \
          sechiba_io.o \
          xios_orchidee.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__sechiba.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

sechiba.done: \
          sechiba.o \
          condveg.done \
          constantes.done \
          constantes_soil.done \
          diffuco.done \
          enerbil.done \
          hydrol.done \
          hydrolc.done \
          ioipsl_para.done \
          pft_parameters.done \
          routing.done \
          sechiba_io.done \
          slowproc.done \
          thermosoil.done \
          thermosoilc.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

sechiba.o: \
          $(PPSRCDIR0__sechiba)/sechiba.f90 \
          FFLAGS__sechiba__sechiba.flags \
          condveg.o \
          constantes.o \
          constantes_soil.o \
          diffuco.o \
          enerbil.o \
          hydrol.o \
          hydrolc.o \
          ioipsl_para.o \
          pft_parameters.o \
          routing.o \
          sechiba_io.o \
          slowproc.o \
          thermosoil.o \
          thermosoilc.o \
          xios_orchidee.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__condveg.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

condveg.done: \
          condveg.o \
          constantes.done \
          constantes_soil.done \
          grid.done \
          interpol_help.done \
          ioipsl_para.done \
          mod_orchidee_para.done \
          pft_parameters.done \
          qsat_moisture.done \
          sechiba_io.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

condveg.o: \
          $(PPSRCDIR0__sechiba)/condveg.f90 \
          FFLAGS__sechiba__condveg.flags \
          constantes.o \
          constantes_soil.o \
          grid.o \
          interpol_help.o \
          ioipsl_para.o \
          mod_orchidee_para.o \
          pft_parameters.o \
          qsat_moisture.o \
          sechiba_io.o \
          xios_orchidee.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__sechiba_io_p.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

sechiba_io_p.done: \
          sechiba_io_p.o \
          constantes.done \
          ioipsl_para.done \
          mod_orchidee_para.done
	touch $(FCM_DONEDIR)/$@

sechiba_io_p.o: \
          $(PPSRCDIR0__sechiba)/sechiba_io_p.f90 \
          FFLAGS__sechiba__sechiba_io_p.flags \
          constantes.o \
          ioipsl_para.o \
          mod_orchidee_para.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__qsat_moisture.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

qsat_moisture.done: \
          qsat_moisture.o \
          constantes.done \
          constantes_soil.done
	touch $(FCM_DONEDIR)/$@

qsat_moisture.o: \
          $(PPSRCDIR0__sechiba)/qsat_moisture.f90 \
          FFLAGS__sechiba__qsat_moisture.flags \
          constantes.o \
          constantes_soil.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__hydrol.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

hydrol.done: \
          hydrol.o \
          constantes.done \
          constantes_soil.done \
          explicitsnow.done \
          grid.done \
          hydro_subgrid.done \
          init_top.done \
          ioipsl_para.done \
          pft_parameters.done \
          sechiba_io.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

hydrol.o: \
          $(PPSRCDIR0__sechiba)/hydrol.f90 \
          FFLAGS__sechiba__hydrol.flags \
          constantes.o \
          constantes_soil.o \
          explicitsnow.o \
          grid.o \
          hydro_subgrid.o \
          init_top.o \
          ioipsl_para.o \
          pft_parameters.o \
          sechiba_io.o \
          xios_orchidee.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__explicitsnow.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

explicitsnow.done: \
          explicitsnow.o \
          constantes.done \
          constantes_soil.done \
          constantes_var.done \
          ioipsl_para.done \
          pft_parameters.done \
          qsat_moisture.done \
          sechiba_io.done
	touch $(FCM_DONEDIR)/$@

explicitsnow.o: \
          $(PPSRCDIR0__sechiba)/explicitsnow.f90 \
          FFLAGS__sechiba__explicitsnow.flags \
          constantes.o \
          constantes_soil.o \
          constantes_var.o \
          ioipsl_para.o \
          pft_parameters.o \
          qsat_moisture.o \
          sechiba_io.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__routing.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

routing.done: \
          routing.o \
          constantes.done \
          constantes_soil.done \
          grid.done \
          interpol_help.done \
          ioipsl_para.done \
          mod_orchidee_para.done \
          pft_parameters.done \
          sechiba_io.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

routing.o: \
          $(PPSRCDIR0__sechiba)/routing.f90 \
          FFLAGS__sechiba__routing.flags \
          constantes.o \
          constantes_soil.o \
          grid.o \
          interpol_help.o \
          ioipsl_para.o \
          mod_orchidee_para.o \
          pft_parameters.o \
          sechiba_io.o \
          xios_orchidee.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__gammad_inc.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

gammad_inc.done: \
          gammad_inc.o
	touch $(FCM_DONEDIR)/$@

gammad_inc.o: \
          $(PPSRCDIR0__sechiba)/gammad_inc.f90 \
          FFLAGS__sechiba__gammad_inc.flags
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__thermosoilc.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

thermosoilc.done: \
          thermosoilc.o \
          constantes.done \
          constantes_soil.done \
          constantes_var.done \
          grid.done \
          ioipsl_para.done \
          mod_orchidee_para.done \
          pft_parameters_var.done \
          sechiba_io.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

thermosoilc.o: \
          $(PPSRCDIR0__sechiba)/thermosoilc.f90 \
          FFLAGS__sechiba__thermosoilc.flags \
          constantes.o \
          constantes_soil.o \
          constantes_var.o \
          grid.o \
          ioipsl_para.o \
          mod_orchidee_para.o \
          pft_parameters_var.o \
          sechiba_io.o \
          xios_orchidee.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__ioipslctrl.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

ioipslctrl.done: \
          ioipslctrl.o \
          constantes.done \
          constantes_soil.done \
          ioipsl_para.done \
          mod_orchidee_para.done \
          pft_parameters.done \
          thermosoilc.done
	touch $(FCM_DONEDIR)/$@

ioipslctrl.o: \
          $(PPSRCDIR0__sechiba)/ioipslctrl.f90 \
          FFLAGS__sechiba__ioipslctrl.flags \
          constantes.o \
          constantes_soil.o \
          ioipsl_para.o \
          mod_orchidee_para.o \
          pft_parameters.o \
          thermosoilc.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__slowproc.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

slowproc.done: \
          slowproc.o \
          constantes.done \
          constantes_soil.done \
          constantes_soil_var.done \
          grid.done \
          interpol_help.done \
          ioipsl_para.done \
          mod_orchidee_para.done \
          pft_parameters.done \
          sechiba_io.done \
          stomate.done \
          stomate_data.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

slowproc.o: \
          $(PPSRCDIR0__sechiba)/slowproc.f90 \
          FFLAGS__sechiba__slowproc.flags \
          constantes.o \
          constantes_soil.o \
          constantes_soil_var.o \
          grid.o \
          interpol_help.o \
          ioipsl_para.o \
          mod_orchidee_para.o \
          pft_parameters.o \
          sechiba_io.o \
          stomate.o \
          stomate_data.o \
          xios_orchidee.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__hydro_subgrid.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

hydro_subgrid.done: \
          hydro_subgrid.o \
          constantes.done \
          constantes_soil.done
	touch $(FCM_DONEDIR)/$@

hydro_subgrid.o: \
          $(PPSRCDIR0__sechiba)/hydro_subgrid.f90 \
          FFLAGS__sechiba__hydro_subgrid.flags \
          constantes.o \
          constantes_soil.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__hydrolc.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

hydrolc.done: \
          hydrolc.o \
          constantes.done \
          constantes_soil.done \
          explicitsnow.done \
          grid.done \
          ioipsl_para.done \
          mod_orchidee_para.done \
          pft_parameters.done \
          sechiba_io.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

hydrolc.o: \
          $(PPSRCDIR0__sechiba)/hydrolc.f90 \
          FFLAGS__sechiba__hydrolc.flags \
          constantes.o \
          constantes_soil.o \
          explicitsnow.o \
          grid.o \
          ioipsl_para.o \
          mod_orchidee_para.o \
          pft_parameters.o \
          sechiba_io.o \
          xios_orchidee.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__diffuco.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

diffuco.done: \
          diffuco.o \
          chemistry.done \
          constantes.done \
          grid.done \
          ioipsl_para.done \
          pft_parameters.done \
          qsat_moisture.done \
          sechiba_io.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

diffuco.o: \
          $(PPSRCDIR0__sechiba)/diffuco.f90 \
          FFLAGS__sechiba__diffuco.flags \
          chemistry.o \
          constantes.o \
          grid.o \
          ioipsl_para.o \
          pft_parameters.o \
          qsat_moisture.o \
          sechiba_io.o \
          xios_orchidee.o
	fcm_internal compile:F sechiba $< $@

FFLAGS__sechiba__init_top.flags: \
          FFLAGS__sechiba.flags
	touch $(FCM_FLAGSDIR)/$@

init_top.done: \
          init_top.o \
          constantes.done \
          gammad_inc.done \
          pft_parameters.done
	touch $(FCM_DONEDIR)/$@

init_top.o: \
          $(PPSRCDIR0__sechiba)/init_top.f90 \
          FFLAGS__sechiba__init_top.flags \
          constantes.o \
          gammad_inc.o \
          pft_parameters.o
	fcm_internal compile:F sechiba $< $@

