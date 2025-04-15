# ORCHIDEE-CROP-RES
This version is created by Ke Yu, which included the crop residue inside the model. This version of ORCHIDEE-CROP-RES model is used in the study, "Quantifying albedo impact and radiative forcing of management practices in European wheat cropping systems"

Authors:
Ke Yu, Yang Su, Philippe Ciais, Ronny Lauerwald, Eric Ceschia, David Makowski, Yidi Xu, Ezzeddine Abbessi, Hassan Bazzi, Tiphaine Tallec, Aurore Brut, Bernard Heinesch, Christian Brümmer, Marius Schmidt, Manuel Acosta, Pauline Buysse, Thomas Gruenwald and Daniel S Goll

Corresponding author: Ke Yu, ke.yu@lsce.ipsl.fr

To use the model and data, please contact the correspondin author for more details.

The model is written in Fortran, and to use this model, you first need to install the necessary environment, please follow the tutorial on this website, https://forge.ipsl.jussieu.fr/orchidee/wiki/Documentation/UserGuide .

Our model is running on Obelix server at LSCE, you can check the environmental set up at: ​http://forge.ipsl.jussieu.fr/igcmg_doc/wiki/Doc/ComputingCenters/LSCE, the input files can be found in R_IN=/home/orchideeshare/igcmg/IGCM.

After you install the necessary environment, you need to go to the util folder, run ins_make to install the make files, and then go to Config/ORCHIDEE_OL/ to use make_without_xios to compile the model.

The folders inside Config/ORCHIDEE_OL/ are the cases simulated, including Maize/wheat monocropping, maize/wheat double cropping, single cropping with maize/wheat in rotation.

The model has no specific hardware requirement, it can be run in linux environment with correct installation of packages and dependencies, all information can be found in the above mentioned UserGuide website (check "Install and compile ORCHIDEE for offline use "), for further question, please contact ORCHIDEE support team through (https://orchidee.ipsl.fr/contact/).

