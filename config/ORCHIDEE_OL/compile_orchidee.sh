#!/bin/bash
#======================= PBS settings =======================
#PBS -N orch_comp
#PBS -q mediump
#PBS -l nodes=1:ppn=4
#PBS -l walltime=02:00:00
#PBS -o /home/orchidee03/ssdeng/ORCHIDEE-CROP/config/ORCHIDEE_OL/orch_compile.out
#PBS -e /home/orchidee03/ssdeng/ORCHIDEE-CROP/config/ORCHIDEE_OL/orch_compile.err
#PBS -m n

#====================== Base directory =======================
BASE=/home/orchidee03/ssdeng/ORCHIDEE-CROP

echo "===== ORCHIDEE compile job started at: $(date) ====="
echo "BASE directory: $BASE"
echo

#================== 1. Set permissions safely =================
DIRS=(
  "$BASE/util"
  "$BASE/modeles/ORCHIDEE"
  "$BASE/modeles/ORCHIDEE/tools/FCM_V1.2/bin"
  "$BASE/modeles/IOIPSL/src"
  "$BASE/libIGCM"
  "$BASE/libIGCM/libIGCM_sys"
  "$BASE/libIGCM/libIGCM_debug"
)

for d in "${DIRS[@]}"; do
    if [ -d "$d" ]; then
        echo ">>> chmod 777 * in: $d"
        cd "$d" || { echo "!!! ERROR: cannot cd to $d"; exit 1; }
        chmod 777 *
    else
        echo "!!! WARNING: directory not found: $d"
    fi
done

#================= 2. Go to ORCHIDEE_OL config dir =================
ORCH_CONF_DIR="$BASE/config/ORCHIDEE_OL"
cd "$ORCH_CONF_DIR" || { echo "!!! ERROR: cannot cd to $ORCH_CONF_DIR"; exit 1; }

#=================== 3. Clean + Compile ====================
make clean
make without_xios