for i in {1..841}

do

cd /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-future-NW-RCP85-3_5

rm -rf Pixel-$i

cd /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-future-NW-RCP85-climate-3_5

rm -rf Pixel-$i

cd /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-future-NW-RCP85-climate-3_5

mkdir Pixel-$i

cp -r Pixel-0/* Pixel-$i/

cd /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-future-NW-RCP85-climate-3_5/Pixel-$i/

rm -rf run.card run.card.bak Debug Script*

rm -f Job_* run.card.init

cd /home/orchidee04/yangsu/Other_datasets/c_Final_calculation_case_2/Year_future-NW_RCP85_3_5

cp config.card.$i.txt /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-future-NW-RCP85-climate-3_5/Pixel-$i/config.card

cd /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-future-NW-RCP85-climate-3_5/Pixel-$i/

../../../../../libIGCM/ins_job

cd /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-future-NW-RCP85-climate-3_5/Pixel-$i/

cp Job_Pixel-$i Job_Pixel-$i.txt

sed '71s/.*/PeriodNb=1/' Job_Pixel-$i.txt > new_Job_Pixel-$i.txt

cp new_Job_Pixel-$i.txt Job_Pixel-$i

rm -f Job_Pixel-$i.txt new_Job_Pixel-$i.txt

cp Job_Pixel-$i Job_Pixel-$i.txt

sed '7s/.*/#PBS -q medium/' Job_Pixel-$i.txt > new_Job_Pixel-$i.txt

cp new_Job_Pixel-$i.txt Job_Pixel-$i

cd /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-future-NW-RCP85-climate-3_5/Pixel-$i/PARAM

rm -f run.def

cd /home/orchidee04/yangsu/Other_datasets/c_Final_calculation_case_2/Year_future-NW_RCP85_3_5

cp run$i.def.txt /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-future-NW-RCP85-climate-3_5/Pixel-$i/PARAM/run.def

cd /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/Config/ORCHIDEE_OL/Case-2-modified-algorithm/Calculation-1-MW-dynamic-rotation-at-future-NW-RCP85-climate-3_5/Pixel-$i/

qsub -m n Job_Pixel-$i

rm -f new_Job_*

rm -f Job_Pixel-*.txt

done

sleep 10s

qstat

sleep 10m

for k in {1..841}

do

cd /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-future-NW-RCP85-3_5/Pixel-$k/SBG/Output/MO

for n in `ls|tail -1`; do cp $n /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-future-NW-RCP85-3_5/Final-total_pixel-year-3_5; done

for n in `ls|head -1`; do cp $n /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-future-NW-RCP85-3_5/Final-total_pixel-year-3; done

cd /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-future-NW-RCP85-3_5/Pixel-$k/SRF/Output/MO

for n in `ls|tail -1`; do cp $n /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-future-NW-RCP85-3_5/Final-total_pixel-year-3_5; done

for n in `ls|head -1`; do cp $n /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-future-NW-RCP85-3_5/Final-total_pixel-year-3; done

done

