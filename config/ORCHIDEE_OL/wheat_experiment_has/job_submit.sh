#!/bin/bash

cd /home/orchidee03/ssdeng/ORCHIDEE-CROP/config/ORCHIDEE_OL/wheat_experiment_has || {
    echo "ERROR: cannot cd to directory"
    exit 1
}

# clean previous files
rm -rf Job* run.card* Scrip*

# generate new job files
../../../libIGCM/ins_job

# submit real job
qsub -m n Job_*