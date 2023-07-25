for k in {1..841}

do

cd /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-current-NW-RCP85-3_5/Pixel-$k/SBG/Output/MO

for n in `ls|tail -1`; do cp $n /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-current-NW-RCP85-3_5/Final-total_pixel-year-3_5; done

for n in `ls|head -1`; do cp $n /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-current-NW-RCP85-3_5/Final-total_pixel-year-3; done

cd /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-current-NW-RCP85-3_5/Pixel-$k/SRF/Output/MO

for n in `ls|tail -1`; do cp $n /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-current-NW-RCP85-3_5/Final-total_pixel-year-3_5; done

for n in `ls|head -1`; do cp $n /home/orchidee04/yangsu/IGCM_OUT/OL2/DEVT/Case-2-modified-algorithm/Calculation-current-NW-RCP85-3_5/Final-total_pixel-year-3; done

done
