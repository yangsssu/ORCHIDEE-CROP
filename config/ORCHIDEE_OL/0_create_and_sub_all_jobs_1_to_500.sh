cd /home/orchidee01/yuke/orchidee-CROP/util/

chmod 777 *

cd /home/orchidee01/yuke/orchidee-CROP/modeles/ORCHIDEE/

chmod 777 *

cd /home/orchidee01/yuke/orchidee-CROP/modeles/ORCHIDEE/tools/FCM_V1.2/bin/

chmod 777 *

cd /home/orchidee01/yuke/orchidee-CROP/modeles/IOIPSL/src/

chmod 777 *

cd /home/orchidee01/yuke/orchidee-CROP/libIGCM/

chmod 777 *

cd /home/orchidee01/yuke/orchidee-CROP/libIGCM/libIGCM_sys/ 

chmod 777 *

cd /home/orchidee01/yuke/orchidee-CROP/libIGCM/libIGCM_debug/

chmod 777 *

cd /home/orchidee01/yuke/orchidee-CROP/config/ORCHIDEE_OL/

make clean

make without_xios



sleep 5s

cd /home/orchidee01/yuke/orchidee-CROP/config/ORCHIDEE_OL/Wheat/
../../../libIGCM/ins_job


qsub -m n Job_*



cd /home/orchidee01/yuke/orchidee-CROP/config/ORCHIDEE_OL/Wheat/

sed -i '14s/.*/JobName=frlam2020resno-true2-10y/' config.card
sed -i '21s/.*/ExperimentName=WW-test-res-yel-frlam2020resno-true2-10y/' config.card
sed -i '28s/.*/DateBegin=2020-01-01/' config.card
sed -i '29s/.*/DateEnd=2020-12-31/' config.card
sed -i '62s/.*/RestartDate=2019-12-31/' config.card
sed -i '65s/.*/RestartJobName=frlam2019resno-true2-10y/' config.card
sed -i '68s|.*|RestartPath=/home/orchidee01/yuke/IGCM_OUT/OL2/DEVT/WW-test-res-yel-frlam2019resno-true2-10y/|' config.card
sed -i '99s/.*/RestartDate=2019-12-31/' config.card
sed -i '100s/.*/RestartJobName=frlam2019resno-true2-10y/' config.card
sed -i '101s|.*|RestartPath=/home/orchidee01/yuke/IGCM_OUT/OL2/DEVT/WW-test-res-yel-frlam2019resno-true2-10y/|' config.card
sed -i '110s/.*/RestartDate=2019-12-31/' config.card
sed -i '111s/.*/RestartJobName=frlam2019resno-true2-10y/' config.card
sed -i '112s|.*|RestartPath=/home/orchidee01/yuke/IGCM_OUT/OL2/DEVT/WW-test-res-yel-frlam2019resno-true2-10y/|' config.card

cd /home/orchidee01/yuke/orchidee-CROP/config/ORCHIDEE_OL/Wheat/
../../../libIGCM/ins_job


qsub -m n Job_*



for year in {2010..2020}; do
    
    prev_year=$((year - 1))

    cd /home/orchidee01/yuke/orchidee-CROP/config/ORCHIDEE_OL/Wheat/
    
    rm -rf Job* run.card run.card.bak Scrip*

    sed -i "14s/.*/JobName=frlam${year}res-true2-10y/" config.card
    sed -i "21s/.*/ExperimentName=WW-test-res-yel-frlam${year}res-true2-10y/" config.card
    sed -i "28s/.*/DateBegin=${year}-01-01/" config.card
    sed -i "29s/.*/DateEnd=${year}-12-31/" config.card
    sed -i "62s/.*/RestartDate=${prev_year}-12-31/" config.card
    sed -i "65s/.*/RestartJobName=frlam${prev_year}res-true2-10y/" config.card
    sed -i "68s|.*|RestartPath=/home/orchidee01/yuke/IGCM_OUT/OL2/DEVT/WW-test-res-yel-frlam${prev_year}res-true2-10y/|" config.card
    sed -i "99s/.*/RestartDate=${prev_year}-12-31/" config.card
    sed -i "100s/.*/RestartJobName=frlam${prev_year}res-true2-10y/" config.card
    sed -i "101s|.*|RestartPath=/home/orchidee01/yuke/IGCM_OUT/OL2/DEVT/WW-test-res-yel-frlam${prev_year}res-true2-10y/|" config.card
    sed -i "110s/.*/RestartDate=${prev_year}-12-31/" config.card
    sed -i "111s/.*/RestartJobName=frlam${prev_year}res-true2-10y/" config.card
    sed -i "112s|.*|RestartPath=/home/orchidee01/yuke/IGCM_OUT/OL2/DEVT/WW-test-res-yel-frlam${prev_year}res-true2-10y/|" config.card

    ../../../libIGCM/ins_job

    qsub -m n Job_*
    
    sleep 200s
    
done





cd /home/orchidee01/yuke/orchidee-CROP/config/ORCHIDEE_OL/Wheat-nores/

../../../libIGCM/ins_job


qsub -m n Job_*


