This version of ORCHIDEE-CROP (v2.2) is developed by Shuaishuai Deng based on the model version developed by Ke Yu and Yang Su, incorporating a process-based parameterization of hydraulic activation of stomata (HAS). This version is used in the study, "A process-based representation of hydraulic activation of stomata induced by hygroscopic fertilizer salts in the ORCHIDEE-CROP model (v2.2)".

Authors: Shuaishuai Deng*, Jürgen Burkhardt, Ronny Lauerwald, Christophe Flechard, Jürgen Knauer, Benjamin Dumont, Emilio Baud-Fraile, Yang Su, Ke Yu, Nicolas Viovy, Benjamin Loubet

Corresponding author: Shuaishuai Deng, shuaishuai.deng@inrae.fr

To use the model and data, please contact the corresponding author for more details.

The model is written in Fortran. To use this model, you first need to set up the necessary environment. Please follow the tutorial on this website: https://forge.ipsl.jussieu.fr/orchidee/wiki/Documentation/UserGuide.

Our model runs on the Obelix server at LSCE. You can check the environment setup at: http://forge.ipsl.jussieu.fr/igcmg_doc/wiki/Doc/ComputingCenters/LSCE. The input files can be found at R_IN=/home/orchideeshare/igcmg/IGCM.

After installing the necessary environment, navigate to the util folder and run ins_make to install the Makefiles. Then navigate to config/ORCHIDEE_OL/ and use make_without_xios to compile the model.

The folder inside config/ORCHIDEE_OL/ contains the simulation configuration for the winter wheat HAS experiment at FR-Gri.

The model has no specific hardware requirements and can be run in a Linux environment with the required packages and dependencies correctly installed. All necessary information can be found in the User Guide mentioned above (see "Install and compile ORCHIDEE for offline use"). For further questions, please contact the ORCHIDEE support team at https://orchidee.ipsl.fr/contact/.
