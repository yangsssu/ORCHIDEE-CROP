for k in {1..841}

do

cd /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-future-RCP85-9_5/Pixel-$k/SBG/Output/MO

for n in `ls|tail -1`; do cp $n /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-future-RCP85-9_5/Final-total_pixel-year-9_5; done

cd /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-future-RCP85-9_5/Pixel-$k/SRF/Output/MO

for n in `ls|tail -1`; do cp $n /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-future-RCP85-9_5/Final-total_pixel-year-9_5; done

done
