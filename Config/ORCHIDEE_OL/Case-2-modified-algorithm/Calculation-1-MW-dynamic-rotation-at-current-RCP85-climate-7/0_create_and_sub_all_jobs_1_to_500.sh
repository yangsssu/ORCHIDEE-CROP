for i in {1..1}

do

cd /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-current-RCP85-7

#rm -rf Pixel-$i

cd /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-current-RCP85-climate-7

rm -rf Pixel-$i

cd /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-current-RCP85-climate-7

mkdir Pixel-$i

cp -r Pixel-0/* Pixel-$i/

cd /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-current-RCP85-climate-7/Pixel-$i/

rm -rf run.card run.card.bak Debug Script*

rm -f Job_* run.card.init

cd /home/orchidee04/yangsu/Other_datasets/c_Final_calculation_case_2/Year_current_RCP85_7

cp config.card.$i.txt /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-current-RCP85-climate-7/Pixel-$i/config.card

cd /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-current-RCP85-climate-7/Pixel-$i/

../../../../../libIGCM/ins_job

cd /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-current-RCP85-climate-7/Pixel-$i/

cp Job_Pixel-$i Job_Pixel-$i.txt

sed '71s/.*/PeriodNb=1/' Job_Pixel-$i.txt > new_Job_Pixel-$i.txt

cp new_Job_Pixel-$i.txt Job_Pixel-$i

rm -f Job_Pixel-$i.txt new_Job_Pixel-$i.txt

cp Job_Pixel-$i Job_Pixel-$i.txt

sed '7s/.*/#PBS -q medium/' Job_Pixel-$i.txt > new_Job_Pixel-$i.txt

cp new_Job_Pixel-$i.txt Job_Pixel-$i

cd /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-current-RCP85-climate-7/Pixel-$i/PARAM

rm -f run.def

cd /home/orchidee04/yangsu/Other_datasets/c_Final_calculation_case_2/Year_current_RCP85_7

cp run$i.def.txt /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-current-RCP85-climate-7/Pixel-$i/PARAM/run.def

cd /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-current-RCP85-climate-7/Pixel-$i/

qsub -m n Job_Pixel-$i

rm -f new_Job_*

rm -f Job_Pixel-*.txt

done

sleep 10s

qstat
