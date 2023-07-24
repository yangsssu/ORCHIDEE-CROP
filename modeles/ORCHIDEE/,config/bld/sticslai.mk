# Automatic Make rule for sticslai

SRCDIR0__sticslai = /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/modeles/ORCHIDEE/src_sticslai

PPSRCDIR0__sticslai = /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/modeles/ORCHIDEE/.config/ppsrc/sticslai

sticslai.etc : \
          $(SRCDIR0__sticslai)/crop_alloc.f90.new \
          $(SRCDIR0__sticslai)/crop_alloc.f90.backup \
          $(SRCDIR0__sticslai)/#Stics_Recolte.f90# \
          $(FCM_DONEDIR)/FCM_CP.dummy
	cp $^ $(FCM_ETCDIR)
	touch $(FCM_DONEDIR)/$@

FFLAGS__sticslai__Stics.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

stics.done: \
          stics.o \
          grid.done
	touch $(FCM_DONEDIR)/$@

stics.o: \
          $(PPSRCDIR0__sticslai)/Stics.f90 \
          FFLAGS__sticslai__Stics.flags \
          grid.o
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__crop_alloc.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

crop_alloc.done: \
          crop_alloc.o \
          constantes.done \
          pft_parameters.done
	touch $(FCM_DONEDIR)/$@

crop_alloc.o: \
          $(PPSRCDIR0__sticslai)/crop_alloc.f90 \
          FFLAGS__sticslai__crop_alloc.flags \
          constantes.o \
          pft_parameters.o
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__gel.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

divers_gel.done: \
          divers_gel.o \
          stics.done
	touch $(FCM_DONEDIR)/$@

divers_gel.o: \
          $(PPSRCDIR0__sticslai)/gel.f90 \
          FFLAGS__sticslai__gel.flags \
          stics.o
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__Stics_Recolte.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

recolte.done: \
          recolte.o \
          stics.done \
          constantes.done
	touch $(FCM_DONEDIR)/$@

recolte.o: \
          $(PPSRCDIR0__sticslai)/Stics_Recolte.f90 \
          FFLAGS__sticslai__Stics_Recolte.flags \
          stics.o \
          constantes.o
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__reprac_calc.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

reprac_calc.done: \
          reprac_calc.o
	touch $(FCM_DONEDIR)/$@

reprac_calc.o: \
          $(PPSRCDIR0__sticslai)/reprac_calc.f90 \
          FFLAGS__sticslai__reprac_calc.flags
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__Stics_Calcul_LAI.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

calai_.done: \
          calai_.o \
          besoins_en_froid.done \
          stics.done
	touch $(FCM_DONEDIR)/$@

calai_.o: \
          $(PPSRCDIR0__sticslai)/Stics_Calcul_LAI.f90 \
          FFLAGS__sticslai__Stics_Calcul_LAI.flags \
          besoins_en_froid.o \
          stics.o
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__Stics_Develop.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

develop2.done: \
          develop2.o \
          besoins_en_froid.done \
          divers_develop.done \
          stics.done \
          constantes.done
	touch $(FCM_DONEDIR)/$@

develop2.o: \
          $(PPSRCDIR0__sticslai)/Stics_Develop.f90 \
          FFLAGS__sticslai__Stics_Develop.flags \
          besoins_en_froid.o \
          divers_develop.o \
          stics.o \
          constantes.o
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__Divers_develop.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

divers_develop.done: \
          divers_develop.o \
          stics.done
	touch $(FCM_DONEDIR)/$@

divers_develop.o: \
          $(PPSRCDIR0__sticslai)/Divers_develop.f90 \
          FFLAGS__sticslai__Divers_develop.flags \
          stics.o
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__Besoins_en_froid.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

besoins_en_froid.done: \
          besoins_en_froid.o \
          stics.done
	touch $(FCM_DONEDIR)/$@

besoins_en_froid.o: \
          $(PPSRCDIR0__sticslai)/Besoins_en_froid.f90 \
          FFLAGS__sticslai__Besoins_en_froid.flags \
          stics.o
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__CalculNombreDeFeuilles.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

calculnombredefeuilles.done: \
          calculnombredefeuilles.o \
          stics.done
	touch $(FCM_DONEDIR)/$@

calculnombredefeuilles.o: \
          $(PPSRCDIR0__sticslai)/CalculNombreDeFeuilles.f90 \
          FFLAGS__sticslai__CalculNombreDeFeuilles.flags \
          stics.o
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__F_humirac.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

divers_water.done: \
          divers_water.o \
          stics.done
	touch $(FCM_DONEDIR)/$@

divers_water.o: \
          $(PPSRCDIR0__sticslai)/F_humirac.f90 \
          FFLAGS__sticslai__F_humirac.flags \
          stics.o
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__senescen.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

senescen.done: \
          senescen.o \
          divers_gel.done \
          stics.done \
          constantes.done
	touch $(FCM_DONEDIR)/$@

senescen.o: \
          $(PPSRCDIR0__sticslai)/senescen.f90 \
          FFLAGS__sticslai__senescen.flags \
          divers_gel.o \
          stics.o \
          constantes.o
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__Stics_Levee.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

levee.done: \
          levee.o \
          besoins_en_froid.done \
          divers_water.done \
          stics.done
	touch $(FCM_DONEDIR)/$@

levee.o: \
          $(PPSRCDIR0__sticslai)/Stics_Levee.f90 \
          FFLAGS__sticslai__Stics_Levee.flags \
          besoins_en_froid.o \
          divers_water.o \
          stics.o
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__Stics_Lai_Developpement.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

laidev.done: \
          laidev.o \
          besoins_en_froid.done \
          stics.done
	touch $(FCM_DONEDIR)/$@

laidev.o: \
          $(PPSRCDIR0__sticslai)/Stics_Lai_Developpement.f90 \
          FFLAGS__sticslai__Stics_Lai_Developpement.flags \
          besoins_en_froid.o \
          stics.o
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__stress.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

stress.done: \
          stress.o \
          stics.done \
          constantes.done
	touch $(FCM_DONEDIR)/$@

stress.o: \
          $(PPSRCDIR0__sticslai)/stress.f90 \
          FFLAGS__sticslai__stress.flags \
          stics.o \
          constantes.o
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__Stics_init.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

stics_init.done: \
          stics_init.o \
          constantes.done \
          pft_parameters.done
	touch $(FCM_DONEDIR)/$@

stics_init.o: \
          $(PPSRCDIR0__sticslai)/Stics_init.f90 \
          FFLAGS__sticslai__Stics_init.flags \
          constantes.o \
          pft_parameters.o
	fcm_internal compile:F sticslai $< $@

FFLAGS__sticslai__grain.flags: \
          FFLAGS__sticslai.flags
	touch $(FCM_FLAGSDIR)/$@

grain.done: \
          grain.o \
          divers_gel.done \
          constantes.done
	touch $(FCM_DONEDIR)/$@

grain.o: \
          $(PPSRCDIR0__sticslai)/grain.f90 \
          FFLAGS__sticslai__grain.flags \
          divers_gel.o \
          constantes.o
	fcm_internal compile:F sticslai $< $@

