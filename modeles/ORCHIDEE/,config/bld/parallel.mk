# Automatic Make rule for parallel

SRCDIR0__parallel = /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/modeles/ORCHIDEE/src_parallel

PPSRCDIR0__parallel = /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/modeles/ORCHIDEE/.config/ppsrc/parallel

parallel.etc : \
          $(SRCDIR0__parallel)/AA_make \
          $(SRCDIR0__parallel)/AA_make.ldef \
          $(SRCDIR0__parallel)/Makefile \
          $(FCM_DONEDIR)/FCM_CP.dummy
	cp $^ $(FCM_ETCDIR)
	touch $(FCM_DONEDIR)/$@

FFLAGS__parallel__mod_orchidee_para.flags: \
          FFLAGS__parallel.flags
	touch $(FCM_FLAGSDIR)/$@

mod_orchidee_para.done: \
          mod_orchidee_para.o \
          mod_orchidee_mpi_data.done \
          mod_orchidee_omp_data.done \
          mod_orchidee_para_var.done \
          mod_orchidee_transfert_para.done
	touch $(FCM_DONEDIR)/$@

mod_orchidee_para.o: \
          $(PPSRCDIR0__parallel)/mod_orchidee_para.f90 \
          FFLAGS__parallel__mod_orchidee_para.flags \
          mod_orchidee_mpi_data.o \
          mod_orchidee_omp_data.o \
          mod_orchidee_para_var.o \
          mod_orchidee_transfert_para.o
	fcm_internal compile:F parallel $< $@

src_parallel.h: \
          $(SRCDIR0__parallel)/src_parallel.h
	cp $< $(FCM_INCDIR)
	chmod u+w $(FCM_INCDIR)/$@

src_parallel.h.idone: \
          $(SRCDIR0__parallel)/src_parallel.h
	touch $(FCM_DONEDIR)/$@

FFLAGS__parallel__orch_write_field.flags: \
          FFLAGS__parallel.flags
	touch $(FCM_FLAGSDIR)/$@

orch_write_field.done: \
          orch_write_field.o \
          mod_orchidee_para.done \
          orch_write_field.done
	touch $(FCM_DONEDIR)/$@

orch_write_field.o: \
          $(PPSRCDIR0__parallel)/orch_write_field.f90 \
          FFLAGS__parallel__orch_write_field.flags \
          mod_orchidee_para.o \
          orch_write_field.o
	fcm_internal compile:F parallel $< $@

FFLAGS__parallel__mod_orchidee_omp_transfert.flags: \
          FFLAGS__parallel.flags
	touch $(FCM_FLAGSDIR)/$@

mod_orchidee_omp_transfert.done: \
          mod_orchidee_omp_transfert.o \
          mod_orchidee_omp_data.done
	touch $(FCM_DONEDIR)/$@

mod_orchidee_omp_transfert.o: \
          $(PPSRCDIR0__parallel)/mod_orchidee_omp_transfert.f90 \
          FFLAGS__parallel__mod_orchidee_omp_transfert.flags \
          mod_orchidee_omp_data.o
	fcm_internal compile:F parallel $< $@

FFLAGS__parallel__mod_orchidee_mpi_transfert.flags: \
          FFLAGS__parallel.flags
	touch $(FCM_FLAGSDIR)/$@

mod_orchidee_mpi_transfert.done: \
          mod_orchidee_mpi_transfert.o \
          mod_orchidee_para_var.done \
          timer.done
	touch $(FCM_DONEDIR)/$@

mod_orchidee_mpi_transfert.o: \
          $(PPSRCDIR0__parallel)/mod_orchidee_mpi_transfert.f90 \
          FFLAGS__parallel__mod_orchidee_mpi_transfert.flags \
          mod_orchidee_para_var.o \
          timer.o
	fcm_internal compile:F parallel $< $@

FFLAGS__parallel__xios_orchidee.flags: \
          FFLAGS__parallel.flags
	touch $(FCM_FLAGSDIR)/$@

xios_orchidee.done: \
          xios_orchidee.o \
          constantes_soil_var.done \
          constantes_var.done \
          ioipsl_para.done \
          mod_orchidee_para_var.done \
          mod_orchidee_transfert_para.done \
          pft_parameters_var.done \
          vertical_soil_var.done
	touch $(FCM_DONEDIR)/$@

xios_orchidee.o: \
          $(PPSRCDIR0__parallel)/xios_orchidee.f90 \
          FFLAGS__parallel__xios_orchidee.flags \
          constantes_soil_var.o \
          constantes_var.o \
          ioipsl_para.o \
          mod_orchidee_para_var.o \
          mod_orchidee_transfert_para.o \
          pft_parameters_var.o \
          vertical_soil_var.o
	fcm_internal compile:F parallel $< $@

FFLAGS__parallel__write_field_p.flags: \
          FFLAGS__parallel.flags
	touch $(FCM_FLAGSDIR)/$@

write_field_p.done: \
          write_field_p.o \
          orch_write_field_p.done
	touch $(FCM_DONEDIR)/$@

write_field_p.o: \
          $(PPSRCDIR0__parallel)/write_field_p.f90 \
          FFLAGS__parallel__write_field_p.flags \
          orch_write_field_p.o
	fcm_internal compile:F parallel $< $@

mpi_dummy.h: \
          $(SRCDIR0__parallel)/mpi_dummy.h
	cp $< $(FCM_INCDIR)
	chmod u+w $(FCM_INCDIR)/$@

mpi_dummy.h.idone: \
          $(SRCDIR0__parallel)/mpi_dummy.h
	touch $(FCM_DONEDIR)/$@

FFLAGS__parallel__write_field.flags: \
          FFLAGS__parallel.flags
	touch $(FCM_FLAGSDIR)/$@

write_field.done: \
          write_field.o \
          orch_write_field.done
	touch $(FCM_DONEDIR)/$@

write_field.o: \
          $(PPSRCDIR0__parallel)/write_field.f90 \
          FFLAGS__parallel__write_field.flags \
          orch_write_field.o
	fcm_internal compile:F parallel $< $@

FFLAGS__parallel__mod_orchidee_mpi_data.flags: \
          FFLAGS__parallel.flags
	touch $(FCM_FLAGSDIR)/$@

mod_orchidee_mpi_data.done: \
          mod_orchidee_mpi_data.o \
          mod_orchidee_para_var.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

mod_orchidee_mpi_data.o: \
          $(PPSRCDIR0__parallel)/mod_orchidee_mpi_data.f90 \
          FFLAGS__parallel__mod_orchidee_mpi_data.flags \
          mod_orchidee_para_var.o \
          xios_orchidee.o
	fcm_internal compile:F parallel $< $@

FFLAGS__parallel__orch_write_field_p.flags: \
          FFLAGS__parallel.flags
	touch $(FCM_FLAGSDIR)/$@

orch_write_field_p.done: \
          orch_write_field_p.o \
          write_field.done \
          write_field.done \
          mod_orchidee_para.done
	touch $(FCM_DONEDIR)/$@

orch_write_field_p.o: \
          $(PPSRCDIR0__parallel)/orch_write_field_p.f90 \
          FFLAGS__parallel__orch_write_field_p.flags \
          write_field.o \
          write_field.o \
          mod_orchidee_para.o
	fcm_internal compile:F parallel $< $@

FFLAGS__parallel__ioipsl_para.flags: \
          FFLAGS__parallel.flags
	touch $(FCM_FLAGSDIR)/$@

ioipsl_para.done: \
          ioipsl_para.o \
          mod_orchidee_para_var.done \
          mod_orchidee_transfert_para.done
	touch $(FCM_DONEDIR)/$@

ioipsl_para.o: \
          $(PPSRCDIR0__parallel)/ioipsl_para.f90 \
          FFLAGS__parallel__ioipsl_para.flags \
          mod_orchidee_para_var.o \
          mod_orchidee_transfert_para.o
	fcm_internal compile:F parallel $< $@

FFLAGS__parallel__tools_para.flags: \
          FFLAGS__parallel.flags
	touch $(FCM_FLAGSDIR)/$@

tools_para.done: \
          tools_para.o \
          mod_orchidee_para_var.done
	touch $(FCM_DONEDIR)/$@

tools_para.o: \
          $(PPSRCDIR0__parallel)/tools_para.f90 \
          FFLAGS__parallel__tools_para.flags \
          mod_orchidee_para_var.o
	fcm_internal compile:F parallel $< $@

FFLAGS__parallel__mod_orchidee_transfert_para.flags: \
          FFLAGS__parallel.flags
	touch $(FCM_FLAGSDIR)/$@

mod_orchidee_transfert_para.done: \
          mod_orchidee_transfert_para.o \
          mod_orchidee_mpi_transfert.done \
          mod_orchidee_omp_transfert.done \
          mod_orchidee_para_var.done
	touch $(FCM_DONEDIR)/$@

mod_orchidee_transfert_para.o: \
          $(PPSRCDIR0__parallel)/mod_orchidee_transfert_para.f90 \
          FFLAGS__parallel__mod_orchidee_transfert_para.flags \
          mod_orchidee_mpi_transfert.o \
          mod_orchidee_omp_transfert.o \
          mod_orchidee_para_var.o
	fcm_internal compile:F parallel $< $@

FFLAGS__parallel__mod_orchidee_para_var.flags: \
          FFLAGS__parallel.flags
	touch $(FCM_FLAGSDIR)/$@

mod_orchidee_para_var.done: \
          mod_orchidee_para_var.o
	touch $(FCM_DONEDIR)/$@

mod_orchidee_para_var.o: \
          $(PPSRCDIR0__parallel)/mod_orchidee_para_var.f90 \
          FFLAGS__parallel__mod_orchidee_para_var.flags
	fcm_internal compile:F parallel $< $@

FFLAGS__parallel__timer.flags: \
          FFLAGS__parallel.flags
	touch $(FCM_FLAGSDIR)/$@

timer.done: \
          timer.o \
          mod_orchidee_para_var.done
	touch $(FCM_DONEDIR)/$@

timer.o: \
          $(PPSRCDIR0__parallel)/timer.f90 \
          FFLAGS__parallel__timer.flags \
          mod_orchidee_para_var.o
	fcm_internal compile:F parallel $< $@

FFLAGS__parallel__mod_orchidee_omp_data.flags: \
          FFLAGS__parallel.flags
	touch $(FCM_FLAGSDIR)/$@

mod_orchidee_omp_data.done: \
          mod_orchidee_omp_data.o \
          mod_orchidee_para_var.done
	touch $(FCM_DONEDIR)/$@

mod_orchidee_omp_data.o: \
          $(PPSRCDIR0__parallel)/mod_orchidee_omp_data.f90 \
          FFLAGS__parallel__mod_orchidee_omp_data.flags \
          mod_orchidee_para_var.o
	fcm_internal compile:F parallel $< $@

