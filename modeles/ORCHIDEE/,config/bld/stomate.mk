# Automatic Make rule for stomate

SRCDIR0__stomate = /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/modeles/ORCHIDEE/src_stomate

PPSRCDIR0__stomate = /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/modeles/ORCHIDEE/.config/ppsrc/stomate

stomate.etc : \
          $(SRCDIR0__stomate)/AA_make \
          $(SRCDIR0__stomate)/AA_make.ldef \
          $(SRCDIR0__stomate)/Makefile \
          $(FCM_DONEDIR)/FCM_CP.dummy
	cp $^ $(FCM_ETCDIR)
	touch $(FCM_DONEDIR)/$@

FFLAGS__stomate__stomate_lpj.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_lpj.done: \
          stomate_lpj.o \
          grassland_management.done \
          constantes.done \
          constantes_soil.done \
          grid.done \
          ioipsl_para.done \
          lpj_constraints.done \
          lpj_cover.done \
          lpj_crown.done \
          lpj_establish.done \
          lpj_fire.done \
          lpj_gap.done \
          lpj_kill.done \
          lpj_light.done \
          lpj_pftinout.done \
          lpj_spitfire.done \
          pft_parameters.done \
          stomate_alloc.done \
          stomate_data.done \
          stomate_glcchange_sinagec_fh.done \
          stomate_glcchange_fh.done \
          stomate_lcchange.done \
          stomate_litter.done \
          stomate_npp.done \
          stomate_phenology.done \
          stomate_prescribe.done \
          stomate_soilcarbon.done \
          stomate_turnover.done \
          stomate_vmax.done \
          stomate_wet_ch4_pt_ter_0.done \
          stomate_wet_ch4_pt_ter_wet1.done \
          stomate_wet_ch4_pt_ter_wet2.done \
          stomate_wet_ch4_pt_ter_wet3.done \
          stomate_wet_ch4_pt_ter_wet4.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

stomate_lpj.o: \
          $(PPSRCDIR0__stomate)/stomate_lpj.f90 \
          FFLAGS__stomate__stomate_lpj.flags \
          grassland_management.o \
          constantes.o \
          constantes_soil.o \
          grid.o \
          ioipsl_para.o \
          lpj_constraints.o \
          lpj_cover.o \
          lpj_crown.o \
          lpj_establish.o \
          lpj_fire.o \
          lpj_gap.o \
          lpj_kill.o \
          lpj_light.o \
          lpj_pftinout.o \
          lpj_spitfire.o \
          pft_parameters.o \
          stomate_alloc.o \
          stomate_data.o \
          stomate_glcchange_sinagec_fh.o \
          stomate_glcchange_fh.o \
          stomate_lcchange.o \
          stomate_litter.o \
          stomate_npp.o \
          stomate_phenology.o \
          stomate_prescribe.o \
          stomate_soilcarbon.o \
          stomate_turnover.o \
          stomate_vmax.o \
          stomate_wet_ch4_pt_ter_0.o \
          stomate_wet_ch4_pt_ter_wet1.o \
          stomate_wet_ch4_pt_ter_wet2.o \
          stomate_wet_ch4_pt_ter_wet3.o \
          stomate_wet_ch4_pt_ter_wet4.o \
          xios_orchidee.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_wet_ch4_pt_ter_wet4.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_wet_ch4_pt_ter_wet4.done: \
          stomate_wet_ch4_pt_ter_wet4.o \
          constantes.done \
          constantes_soil.done \
          pft_parameters.done
	touch $(FCM_DONEDIR)/$@

stomate_wet_ch4_pt_ter_wet4.o: \
          $(PPSRCDIR0__stomate)/stomate_wet_ch4_pt_ter_wet4.f90 \
          FFLAGS__stomate__stomate_wet_ch4_pt_ter_wet4.flags \
          constantes.o \
          constantes_soil.o \
          pft_parameters.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_io.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_io.done: \
          stomate_io.o \
          constantes.done \
          constantes_soil.done \
          ioipsl_para.done \
          mod_orchidee_para.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

stomate_io.o: \
          $(PPSRCDIR0__stomate)/stomate_io.f90 \
          FFLAGS__stomate__stomate_io.flags \
          constantes.o \
          constantes_soil.o \
          ioipsl_para.o \
          mod_orchidee_para.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__lpj_light.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

lpj_light.done: \
          lpj_light.o \
          constantes.done \
          ioipsl_para.done \
          stomate_data.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

lpj_light.o: \
          $(PPSRCDIR0__stomate)/lpj_light.f90 \
          FFLAGS__stomate__lpj_light.flags \
          constantes.o \
          ioipsl_para.o \
          stomate_data.o \
          xios_orchidee.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_glcchange_SinAgeC_fh.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_glcchange_sinagec_fh.done: \
          stomate_glcchange_sinagec_fh.o \
          constantes.done \
          constantes_soil_var.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

stomate_glcchange_sinagec_fh.o: \
          $(PPSRCDIR0__stomate)/stomate_glcchange_SinAgeC_fh.f90 \
          FFLAGS__stomate__stomate_glcchange_SinAgeC_fh.flags \
          constantes.o \
          constantes_soil_var.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__lpj_kill.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

lpj_kill.done: \
          lpj_kill.o \
          constantes.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

lpj_kill.o: \
          $(PPSRCDIR0__stomate)/lpj_kill.f90 \
          FFLAGS__stomate__lpj_kill.flags \
          constantes.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_prescribe.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_prescribe.done: \
          stomate_prescribe.o \
          constantes.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

stomate_prescribe.o: \
          $(PPSRCDIR0__stomate)/stomate_prescribe.f90 \
          FFLAGS__stomate__stomate_prescribe.flags \
          constantes.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_litter.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_litter.done: \
          stomate_litter.o \
          constantes.done \
          constantes_soil.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

stomate_litter.o: \
          $(PPSRCDIR0__stomate)/stomate_litter.f90 \
          FFLAGS__stomate__stomate_litter.flags \
          constantes.o \
          constantes_soil.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__Fauche_Orchidee.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

fauche.done: \
          fauche.o \
          constantes.done \
          constantes_pasim.done \
          fonctions_pasim.done \
          pft_parameters.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

fauche.o: \
          $(PPSRCDIR0__stomate)/Fauche_Orchidee.f90 \
          FFLAGS__stomate__Fauche_Orchidee.flags \
          constantes.o \
          constantes_pasim.o \
          fonctions_pasim.o \
          pft_parameters.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__Animals_Orchidee.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

animaux.done: \
          animaux.o \
          constantes.done \
          constantes_pasim.done \
          fonctions_pasim.done \
          ioipsl_para.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

animaux.o: \
          $(PPSRCDIR0__stomate)/Animals_Orchidee.f90 \
          FFLAGS__stomate__Animals_Orchidee.flags \
          constantes.o \
          constantes_pasim.o \
          fonctions_pasim.o \
          ioipsl_para.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_permafrost_soilcarbon.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_permafrost_soilcarbon.done: \
          stomate_permafrost_soilcarbon.o \
          constantes_soil.done \
          constantes_soil_var.done \
          constantes_var.done \
          grid.done \
          ioipsl_para.done \
          mod_orchidee_para.done \
          pft_parameters.done \
          stomate_data.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

stomate_permafrost_soilcarbon.o: \
          $(PPSRCDIR0__stomate)/stomate_permafrost_soilcarbon.f90 \
          FFLAGS__stomate__stomate_permafrost_soilcarbon.flags \
          constantes_soil.o \
          constantes_soil_var.o \
          constantes_var.o \
          grid.o \
          ioipsl_para.o \
          mod_orchidee_para.o \
          pft_parameters.o \
          stomate_data.o \
          xios_orchidee.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_resp.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_resp.done: \
          stomate_resp.o \
          constantes.done \
          constantes_soil.done \
          pft_parameters.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

stomate_resp.o: \
          $(PPSRCDIR0__stomate)/stomate_resp.f90 \
          FFLAGS__stomate__stomate_resp.flags \
          constantes.o \
          constantes_soil.o \
          pft_parameters.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_alloc.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_alloc.done: \
          stomate_alloc.o \
          constantes.done \
          constantes_soil.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

stomate_alloc.o: \
          $(PPSRCDIR0__stomate)/stomate_alloc.f90 \
          FFLAGS__stomate__stomate_alloc.flags \
          constantes.o \
          constantes_soil.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__lpj_constraints.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

lpj_constraints.done: \
          lpj_constraints.o \
          constantes.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

lpj_constraints.o: \
          $(PPSRCDIR0__stomate)/lpj_constraints.f90 \
          FFLAGS__stomate__lpj_constraints.flags \
          constantes.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o \
          xios_orchidee.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_lcchange.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_lcchange.done: \
          stomate_lcchange.o \
          constantes.done \
          constantes_soil_var.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

stomate_lcchange.o: \
          $(PPSRCDIR0__stomate)/stomate_lcchange.f90 \
          FFLAGS__stomate__stomate_lcchange.flags \
          constantes.o \
          constantes_soil_var.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_wet_ch4_pt_ter_wet2.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_wet_ch4_pt_ter_wet2.done: \
          stomate_wet_ch4_pt_ter_wet2.o \
          constantes.done \
          constantes_soil.done \
          pft_parameters.done
	touch $(FCM_DONEDIR)/$@

stomate_wet_ch4_pt_ter_wet2.o: \
          $(PPSRCDIR0__stomate)/stomate_wet_ch4_pt_ter_wet2.f90 \
          FFLAGS__stomate__stomate_wet_ch4_pt_ter_wet2.flags \
          constantes.o \
          constantes_soil.o \
          pft_parameters.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__Fertilisation_Orchidee.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

fertilisation.done: \
          fertilisation.o \
          constantes.done \
          constantes_pasim.done \
          fonctions_pasim.done \
          pft_parameters.done
	touch $(FCM_DONEDIR)/$@

fertilisation.o: \
          $(PPSRCDIR0__stomate)/Fertilisation_Orchidee.f90 \
          FFLAGS__stomate__Fertilisation_Orchidee.flags \
          constantes.o \
          constantes_pasim.o \
          fonctions_pasim.o \
          pft_parameters.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_vmax.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_vmax.done: \
          stomate_vmax.o \
          constantes.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

stomate_vmax.o: \
          $(PPSRCDIR0__stomate)/stomate_vmax.f90 \
          FFLAGS__stomate__stomate_vmax.flags \
          constantes.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_phenology.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_phenology.done: \
          stomate_phenology.o \
          constantes.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

stomate_phenology.o: \
          $(PPSRCDIR0__stomate)/stomate_phenology.f90 \
          FFLAGS__stomate__stomate_phenology.flags \
          constantes.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o \
          xios_orchidee.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_turnover.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_turnover.done: \
          stomate_turnover.o \
          constantes.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

stomate_turnover.o: \
          $(PPSRCDIR0__stomate)/stomate_turnover.f90 \
          FFLAGS__stomate__stomate_turnover.flags \
          constantes.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o \
          xios_orchidee.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__lpj_pftinout.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

lpj_pftinout.done: \
          lpj_pftinout.o \
          constantes.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

lpj_pftinout.o: \
          $(PPSRCDIR0__stomate)/lpj_pftinout.f90 \
          FFLAGS__stomate__lpj_pftinout.flags \
          constantes.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_wet_ch4_pt_ter_wet1.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_wet_ch4_pt_ter_wet1.done: \
          stomate_wet_ch4_pt_ter_wet1.o \
          constantes.done \
          constantes_soil.done \
          pft_parameters.done
	touch $(FCM_DONEDIR)/$@

stomate_wet_ch4_pt_ter_wet1.o: \
          $(PPSRCDIR0__stomate)/stomate_wet_ch4_pt_ter_wet1.f90 \
          FFLAGS__stomate__stomate_wet_ch4_pt_ter_wet1.flags \
          constantes.o \
          constantes_soil.o \
          pft_parameters.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__lpj_fire.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

lpj_fire.done: \
          lpj_fire.o \
          constantes.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

lpj_fire.o: \
          $(PPSRCDIR0__stomate)/lpj_fire.f90 \
          FFLAGS__stomate__lpj_fire.flags \
          constantes.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o \
          xios_orchidee.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate.done: \
          stomate.o \
          divers_develop.done \
          stics.done \
          constantes.done \
          constantes_soil.done \
          grid.done \
          interpol_help.done \
          ioipsl_para.done \
          matrix_resolution.done \
          mod_orchidee_para.done \
          pft_parameters.done \
          stomate_data.done \
          stomate_io.done \
          stomate_litter.done \
          stomate_lpj.done \
          stomate_permafrost_soilcarbon.done \
          stomate_resp.done \
          stomate_season.done \
          stomate_soilcarbon.done \
          stomate_vmax.done \
          stomate_wet_ch4_pt_ter_0.done \
          stomate_wet_ch4_pt_ter_wet1.done \
          stomate_wet_ch4_pt_ter_wet2.done \
          stomate_wet_ch4_pt_ter_wet3.done \
          stomate_wet_ch4_pt_ter_wet4.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

stomate.o: \
          $(PPSRCDIR0__stomate)/stomate.f90 \
          FFLAGS__stomate__stomate.flags \
          divers_develop.o \
          stics.o \
          constantes.o \
          constantes_soil.o \
          grid.o \
          interpol_help.o \
          ioipsl_para.o \
          matrix_resolution.o \
          mod_orchidee_para.o \
          pft_parameters.o \
          stomate_data.o \
          stomate_io.o \
          stomate_litter.o \
          stomate_lpj.o \
          stomate_permafrost_soilcarbon.o \
          stomate_resp.o \
          stomate_season.o \
          stomate_soilcarbon.o \
          stomate_vmax.o \
          stomate_wet_ch4_pt_ter_0.o \
          stomate_wet_ch4_pt_ter_wet1.o \
          stomate_wet_ch4_pt_ter_wet2.o \
          stomate_wet_ch4_pt_ter_wet3.o \
          stomate_wet_ch4_pt_ter_wet4.o \
          xios_orchidee.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__lpj_gap.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

lpj_gap.done: \
          lpj_gap.o \
          constantes.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

lpj_gap.o: \
          $(PPSRCDIR0__stomate)/lpj_gap.f90 \
          FFLAGS__stomate__lpj_gap.flags \
          constantes.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o \
          xios_orchidee.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__lpj_cover.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

lpj_cover.done: \
          lpj_cover.o \
          constantes_soil_var.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

lpj_cover.o: \
          $(PPSRCDIR0__stomate)/lpj_cover.f90 \
          FFLAGS__stomate__lpj_cover.flags \
          constantes_soil_var.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_soilcarbon.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_soilcarbon.done: \
          stomate_soilcarbon.o \
          constantes.done \
          ioipsl_para.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

stomate_soilcarbon.o: \
          $(PPSRCDIR0__stomate)/stomate_soilcarbon.f90 \
          FFLAGS__stomate__stomate_soilcarbon.flags \
          constantes.o \
          ioipsl_para.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__Applic_Plantes.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

applic_plant.done: \
          applic_plant.o \
          constantes.done \
          constantes_pasim.done \
          fonctions_pasim.done \
          ioipsl_para.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

applic_plant.o: \
          $(PPSRCDIR0__stomate)/Applic_Plantes.f90 \
          FFLAGS__stomate__Applic_Plantes.flags \
          constantes.o \
          constantes_pasim.o \
          fonctions_pasim.o \
          ioipsl_para.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_npp.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_npp.done: \
          stomate_npp.o \
          constantes.done \
          constantes_soil.done \
          crop_alloc.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

stomate_npp.o: \
          $(PPSRCDIR0__stomate)/stomate_npp.f90 \
          FFLAGS__stomate__stomate_npp.flags \
          constantes.o \
          constantes_soil.o \
          crop_alloc.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o \
          xios_orchidee.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_wet_ch4_pt_ter_wet3.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_wet_ch4_pt_ter_wet3.done: \
          stomate_wet_ch4_pt_ter_wet3.o \
          constantes.done \
          constantes_soil.done \
          pft_parameters.done
	touch $(FCM_DONEDIR)/$@

stomate_wet_ch4_pt_ter_wet3.o: \
          $(PPSRCDIR0__stomate)/stomate_wet_ch4_pt_ter_wet3.f90 \
          FFLAGS__stomate__stomate_wet_ch4_pt_ter_wet3.flags \
          constantes.o \
          constantes_soil.o \
          pft_parameters.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__lpj_crown.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

lpj_crown.done: \
          lpj_crown.o \
          constantes.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

lpj_crown.o: \
          $(PPSRCDIR0__stomate)/lpj_crown.f90 \
          FFLAGS__stomate__lpj_crown.flags \
          constantes.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_season.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_season.done: \
          stomate_season.o \
          constantes.done \
          constantes_soil.done \
          grid.done \
          ioipsl_para.done \
          pft_parameters.done \
          solar.done \
          stomate_data.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

stomate_season.o: \
          $(PPSRCDIR0__stomate)/stomate_season.f90 \
          FFLAGS__stomate__stomate_season.flags \
          constantes.o \
          constantes_soil.o \
          grid.o \
          ioipsl_para.o \
          pft_parameters.o \
          solar.o \
          stomate_data.o \
          xios_orchidee.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__fonctions_PaSim.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

fonctions_pasim.done: \
          fonctions_pasim.o \
          constantes.done \
          constantes_pasim.done
	touch $(FCM_DONEDIR)/$@

fonctions_pasim.o: \
          $(PPSRCDIR0__stomate)/fonctions_PaSim.f90 \
          FFLAGS__stomate__fonctions_PaSim.flags \
          constantes.o \
          constantes_pasim.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__Grassland_Management.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

grassland_management.done: \
          grassland_management.o \
          animaux.done \
          fauche.done \
          fertilisation.done \
          applic_plant.done \
          constantes.done \
          constantes_pasim.done \
          fonctions_pasim.done \
          grid.done \
          interpol_help.done \
          ioipsl_para.done \
          matrix_resolution.done \
          mod_orchidee_para.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

grassland_management.o: \
          $(PPSRCDIR0__stomate)/Grassland_Management.f90 \
          FFLAGS__stomate__Grassland_Management.flags \
          animaux.o \
          fauche.o \
          fertilisation.o \
          applic_plant.o \
          constantes.o \
          constantes_pasim.o \
          fonctions_pasim.o \
          grid.o \
          interpol_help.o \
          ioipsl_para.o \
          matrix_resolution.o \
          mod_orchidee_para.o \
          xios_orchidee.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__lpj_establish.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

lpj_establish.done: \
          lpj_establish.o \
          constantes.done \
          ioipsl_para.done \
          stomate_data.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

lpj_establish.o: \
          $(PPSRCDIR0__stomate)/lpj_establish.f90 \
          FFLAGS__stomate__lpj_establish.flags \
          constantes.o \
          ioipsl_para.o \
          stomate_data.o \
          xios_orchidee.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_wet_ch4_pt_ter_0.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_wet_ch4_pt_ter_0.done: \
          stomate_wet_ch4_pt_ter_0.o \
          constantes.done \
          constantes_soil.done \
          pft_parameters.done
	touch $(FCM_DONEDIR)/$@

stomate_wet_ch4_pt_ter_0.o: \
          $(PPSRCDIR0__stomate)/stomate_wet_ch4_pt_ter_0.f90 \
          FFLAGS__stomate__stomate_wet_ch4_pt_ter_0.flags \
          constantes.o \
          constantes_soil.o \
          pft_parameters.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_data.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_data.done: \
          stomate_data.o \
          constantes.done \
          pft_parameters.done
	touch $(FCM_DONEDIR)/$@

stomate_data.o: \
          $(PPSRCDIR0__stomate)/stomate_data.f90 \
          FFLAGS__stomate__stomate_data.flags \
          constantes.o \
          pft_parameters.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__lpj_spitfire.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

lpj_spitfire.done: \
          lpj_spitfire.o \
          constantes.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done \
          xios_orchidee.done
	touch $(FCM_DONEDIR)/$@

lpj_spitfire.o: \
          $(PPSRCDIR0__stomate)/lpj_spitfire.f90 \
          FFLAGS__stomate__lpj_spitfire.flags \
          constantes.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o \
          xios_orchidee.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__constantes_PaSim.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

constantes_pasim.done: \
          constantes_pasim.o \
          constantes.done
	touch $(FCM_DONEDIR)/$@

constantes_pasim.o: \
          $(PPSRCDIR0__stomate)/constantes_PaSim.f90 \
          FFLAGS__stomate__constantes_PaSim.flags \
          constantes.o
	fcm_internal compile:F stomate $< $@

FFLAGS__stomate__stomate_glcchange_fh.flags: \
          FFLAGS__stomate.flags
	touch $(FCM_FLAGSDIR)/$@

stomate_glcchange_fh.done: \
          stomate_glcchange_fh.o \
          constantes.done \
          constantes_soil_var.done \
          ioipsl_para.done \
          pft_parameters.done \
          stomate_data.done
	touch $(FCM_DONEDIR)/$@

stomate_glcchange_fh.o: \
          $(PPSRCDIR0__stomate)/stomate_glcchange_fh.f90 \
          FFLAGS__stomate__stomate_glcchange_fh.flags \
          constantes.o \
          constantes_soil_var.o \
          ioipsl_para.o \
          pft_parameters.o \
          stomate_data.o
	fcm_internal compile:F stomate $< $@

