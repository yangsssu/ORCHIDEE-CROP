# ORCHIDEE-CROP
This is the initial version of the ORCHIDEE-CROP used in the paper, "Datasets used in the paper Global warming increases the chance of success of maize-wheat double cropping in Europe", which is in review in Nature Communications.

Authors 

Yang Su a, b, Ronny Lauerwald a, David Makowski c, Nicolas Viovy b, Nicolas Guilpart d, Peng Zhu b, e, Benoit Gabrielle a, Philippe Ciais b 

Affiliations 
a UMR ECOSYS, INRAE AgroParisTech, Université Paris-Saclay, 91120 Palaiseau, France 
b Laboratoire des Sciences du Climat et de l’Environnement, CEA CNRS UVSQ Orme des Merisiers, 91190 Gif-sur-Yvette, France 
c Unit Applied mathematics and computer science (MIA 518), INRAE AgroParisTech, Université Paris-Saclay, 91120 Palaiseau, France 
d UMR Agronomie, INRAE AgroParisTech, Université Paris-Saclay, 91120 Palaiseau, France 
e Department of Geography, The University of Hong Kong, Hong Kong SAR, China 

Corresponding Author 

Yang Su 
yang.su@inrae.fr 
+33 1 89 10 07 67 
INRAE AgroParisTech 

To use the model and data, please contact the correspondin author for more details.

The model is written in Fortran, and to use this model, you first need to install the necessary environment, please follow the tutorial on this website, https://forge.ipsl.jussieu.fr/orchidee/wiki/Documentation/UserGuide .

Our model is running on Obelix server at LSCE, you can check the set up environment at: ​http://forge.ipsl.jussieu.fr/igcmg_doc/wiki/Doc/ComputingCenters/LSCE, the input files can be found in R_IN=/home/orchideeshare/igcmg/IGCM.

After you install the necessary environment, you need to go to the util folder, run ins_make to install the make files, and then go to Config/ORCHIDEE_OL/ to use make_without_xios to compile the model.

The folders inside Config/ORCHIDEE_OL/ are the cases simulated, including Maize/wheat monocropping, maize/wheat double cropping, single cropping with maize/wheat in rotation.
