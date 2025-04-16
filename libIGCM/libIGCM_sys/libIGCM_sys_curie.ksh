#!/bin/ksh

#**************************************************************
# Author: Sebastien Denvil, Martial Mancip, Arnaud Caubel
# Contact: Arnaud.Caubel__at__lsce.ipsl.fr
# $Revision:: 1321                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2016-04-14 14:06:46 +0200 (Thu, 14 Apr 2016) $ Date of last commit
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************

#=========================================================
# The documentation of this file can be automatically generated
# if you use the prefix #D- for comments to be extracted.
# Extract with command: cat lib* | grep "^#D-" | cut -c "4-"
#=========================================================

#D-#==================================================
#D-LibIGCM_sys for Curie
#D-#==================================================
#D-
#D- This ksh library if a layer under some usefull
#D-environment variables and shell commands.
#D-All those definitions depend on host particularities.
#D-It manages a stack mechanism and test validity of operations.
#D-All function described bellow must be prefixed by IGCM_sys.

#====================================================
# libIGCM_sys PARAMETERS
#====================================================

#====================================================
# set DEBUG_sys to true to output calls of function
typeset -r DEBUG_sys=${DEBUG_sys:=true}

#====================================================
# Turn in dry run mode ? (sys_Put_Rest, sys_Put_Out, sys_Get)
typeset -r DRYRUN=${DRYRUN:=0}

# YOU MUST COMPILE YOUR EXE FILES FOR DRYRUN MODE !
# -------------------------------------------------------------------------------------
# | DRYRUN=  |  Date computations, | sys_Get    |  Exe    | sys_Put_Out; sys_Put_Rest |
# |          |  Cp/Exe/param/files |            |         |                           |
# |          |  Chmod Qsub         |            |         |                           |
# -------------------------------------------------------------------------------------
# |    0     |       yes           |    yes     |  yes    |      yes                  |
# -------------------------------------------------------------------------------------
# |    1     |       yes           |    yes     |  yes    |      no                   |
# -------------------------------------------------------------------------------------
# |    2     |       yes           |    yes     |  no     |      no                   |
# -------------------------------------------------------------------------------------
# |    3     |       yes           |    no      |  no     |      no                   |
# -------------------------------------------------------------------------------------

#=====================================================
# Global Variables :
#=====================================================
# Language : "fr" or "en"
typeset -r MYLANG="fr"

#=====================================================
# Host user names project maxCpuTime
# $hostname ou hostname
typeset HOST=${HOST:=$( hostname )}
# $username ou whoami
typeset LOGIN=${LOGIN:=$( whoami )}
# $hostname of the MASTER job
typeset MASTER=curie
# add default project on curie
typeset PROJECT=$(echo ${BRIDGE_MSUB_PROJECT:=gen0826} | cut -d@ -f1 )
# jobWarningDelay in seconds
typeset jobWarningDelay=${BRIDGE_MSUB_MAXTIME}

#D-
#D-#==================================================
#D-Program used in libIGCM
#D-#==================================================

# Submit command
typeset SUBMIT=${SUBMIT:=ccc_msub}
# rsync with path
typeset -r RSYNC=/usr/bin/rsync
# RSYNC_opt args to rsync
typeset -r RSYNC_opt="-va"
# ie storage filesystem
typeset -r STOREHOST=${MASTER}
typeset -r REMOTE_RSYNC=/usr/bin/rsync

#====================================================
# Set environment tools (ferret, nco, cdo, rebuild, ...)
#====================================================
if [ X${TaskType} = Xcomputing ] ; then
  . /ccc/cont003/home/dsm/p86ipsl/.atlas_env_netcdf4.3.3.1_hdf5_parallel_curie_ksh > /dev/null 2>&1
# to run with netcdf 3.6.3 ie compilation done before 17/2/2014
# uncomment 2 lines :
#  module unload netcdf
#  module load netcdf/3.6.3
else
  . /ccc/cont003/home/dsm/p86ipsl/.atlas_env_netcdf4.3.3.1_hdf5_parallel_curie_ksh > /dev/null 2>&1
  PCMDI_MP=/ccc/work/cont003/igcmg/igcmg/PCMDI-MP
  export UVCDAT_ANONYMOUS_LOG=no
fi

# Use CMIP6 storage space when using CMIP6 cpu ressources
[ "X${PROJECT}" = "Xdevcmip6" ] && module switch dfldatadir dfldatadir/gencmip6 > /dev/null 2>&1

# FYI
[ ! X${TaskType} = Xchecking ] && IGCM_debug_Print 1 "List of loaded modules:"
[ ! X${TaskType} = Xchecking ] && module list

export PATH=${PATH}:/ccc/cont003/home/dsm/p86ipsl/AddNoise/src_X64_CURIE/bin
export PATH=${PATH}:/ccc/cont003/home/dsm/p86ipsl/AddPerturbation/src_X64_CURIE/bin
export PATH=${PATH}:$( ccc_home -u p86ipsl )/rebuild/src_X64_CURIE/modipsl_v2_2_2_netcdf4.2/bin/

#====================================================
# Specific for ocean additionnal diagnostic
export FER_GO="$FER_GO /home/cont003/p86denv/IGCM_POST_UTIL/JNL /home/cont003/p86denv/GRAF /home/cont003/p86denv/GRAF/GO"
export FER_PALETTE="$FER_PALETTE /home/cont003/p86denv/GRAF/PALET"

#====================================================
# Host specific DIRECTORIES
#====================================================

# ============ CESIUM START ============ #

#====================================================
#- Mirror libIGCM from titane to cesium if needed
#ROOTSYS=$( echo ${libIGCM} | gawk -F"/" '{print $3}' )
#if [ ! ${ROOTSYS} = "home" ] ; then
#  typeset -r MirrorlibIGCM=${MirrorlibIGCM:=true}
#else
#  typeset -r MirrorlibIGCM=${MirrorlibIGCM:=false}
#fi

#====================================================
#- libIGCM_POST
#if ( ${MirrorlibIGCM} ) ; then
#  PATHlibIGCM=$( echo ${libIGCM} | gawk -F"${LOGIN}/" '{print $2}' | sed -e "s&/libIGCM&&" )
#  typeset -r libIGCM_POST=${HOME}/MIRROR/${PATHlibIGCM}/libIGCM
#else
#  typeset -r libIGCM_POST=${libIGCM}
#fi

# ============ CESIUM  END  ============ #

#====================================================
#- MirrorlibIGCM for frontend
typeset -r MirrorlibIGCM=${MirrorlibIGCM:=false}

#====================================================
#- libIGCM_POST for frontend
typeset -r libIGCM_POST=${libIGCM}

#====================================================
#- R_EXE   (==> BIN_DIR = ${MODIPSL}/bin )
typeset -r R_EXE="${MODIPSL}/bin"

#====================================================
#- SUBMIT_DIR : submission dir
typeset -x SUBMIT_DIR=${SUBMIT_DIR:=${BRIDGE_MSUB_PWD}}

#====================================================
#- IN
typeset -r R_IN=${R_IN:=/ccc/work/cont003/igcmg/igcmg/IGCM}
typeset -r R_IN_ECMWF=${R_IN_ECMWF:=/ccc/work/cont003/dsm/p24data}

#====================================================
#- RUN_DIR_PATH : Temporary working directory (=> TMP)
typeset -r RUN_DIR_PATH=${RUN_DIR_PATH:=${SCRATCHDIR}/RUN_DIR/${BRIDGE_MSUB_JOBID}_${$}}

#====================================================
#- OUTCOMMAND_PATH : tmp place to store command lines standard error and outputs
typeset -r OUTCOMMAND_PATH=/tmp

#====================================================
#- HOST_MPIRUN_COMMAND
typeset -r HOST_MPIRUN_COMMAND=${HOST_MPIRUN_COMMAND:="/usr/bin/time ccc_mprun -E-K1"}

#====================================================
#- Max number of arguments passed to nco operator or demigration command
UNIX_MAX_LIMIT=360

#====================================================
#- set PackDefault to true on curie
PackDefault=true

#====================================================
#- Default number of MPI task for IPSL coupled model
#- required for backward compatibility
#-
DEFAULT_NUM_PROC_OCE=5
DEFAULT_NUM_PROC_CPL=1
(( DEFAULT_NUM_PROC_ATM = BATCH_NUM_PROC_TOT - DEFAULT_NUM_PROC_OCE - DEFAULT_NUM_PROC_CPL ))

#D-#==================================================
#D-function IGCM_sys_defineArchives
#D-* Purpose:
#D-* Define ARCHIVE : Dedicated to large files
#D-* Define STORAGE : Dedicated to small/medium files
#D-* Define R_OUT   : Output tree located on ARCHIVE
#D-* Define R_FIG   : Output tree located on STORAGE hosting figures (monitoring and atlas, and/or small files)
#D-* Define R_BUF   : Output tree located on SCRATCHDIR hosting files waiting for rebuild or pack processes
#D-* if SpaceName=TEST everything is stored on SCRATCHDIR
#D-* Examples:
#D-
function IGCM_sys_defineArchives {
  IGCM_debug_PushStack "IGCM_sys_defineArchives"

  if [ ! X${config_UserChoices_ARCHIVE} = X ]; then
    #====================================================
    #- ARCHIVE (dedicated to large files)
    ARCHIVE=${config_UserChoices_ARCHIVE}
  else
    #====================================================
    #- ARCHIVE (dedicated to large files)
    ARCHIVE=${CCCSTOREDIR}
  fi

  if [ ! X${config_UserChoices_STORAGE} = X ]; then
    #====================================================
    #- STORAGE (dedicated to small/medium files)
    STORAGE=${config_UserChoices_STORAGE}
  else
    #====================================================
    #- STORAGE (dedicated to small/medium files)
    STORAGE=${CCCWORKDIR}
  fi

  if [ X${config_UserChoices_SpaceName} = XTEST ]; then
    #====================================================
    #- R_OUT
    R_OUT=${SCRATCHDIR}/IGCM_OUT

    #====================================================
    #- R_FIG (hosting figures : monitoring and atlas, and/or small files)
    R_FIG=${SCRATCHDIR}/IGCM_OUT

    IGCM_debug_Print 1 "SpaceName=TEST ==> OVERRULE destination path directories"

  else
    #====================================================
    #- R_OUT
    R_OUT=${ARCHIVE}/IGCM_OUT

    #====================================================
    #- R_FIG (hosting figures : monitoring and atlas, and/or small files)
    R_FIG=${STORAGE}/IGCM_OUT
  fi

  #====================================================
  #- R_BUF (ONLY FOR double copy an scratch)
  R_BUF=${SCRATCHDIR}/IGCM_OUT

  IGCM_debug_Print 1 "R_OUT has been defined = ${R_OUT}"
  IGCM_debug_Print 1 "R_BUF has been defined = ${R_BUF}"
  IGCM_debug_Print 1 "R_FIG has been defined = ${R_FIG}"

  IGCM_debug_PopStack "IGCM_sys_defineArchives"
}

#D-#==================================================
#D-function IGCM_sys_RshArchive
#D-* Purpose: Archive rsh command
#D-* Examples:
#D-
function IGCM_sys_RshArchive {
  IGCM_debug_PushStack "IGCM_sys_RshArchive" $@
  /bin/ksh <<-EOF
    ${@}
EOF
  status=$?
  if [ ${status} -gt 0 ] ; then
    IGCM_debug_Print 2 "IGCM_sys_RshArchive : command failed error code ${status}"
    IGCM_debug_Exit "IGCM_sys_RshArchive"
  fi
  IGCM_debug_PopStack "IGCM_sys_RshArchive"
}

#D-#==================================================
#D-function IGCM_sys_RshArchive_NoError
#D-* Purpose: Archive rsh command, without error
#D-*          used only in monitoring.job
#D-* Examples:
#D-
function IGCM_sys_RshArchive_NoError {
  IGCM_debug_PushStack "IGCM_sys_RshArchive_NoError" $@
  /bin/ksh <<-EOF
    ${@} 2> /dev/null
EOF
  IGCM_debug_PopStack "IGCM_sys_RshArchive_NoError"
}

#D-#==================================================
#D-function IGCM_sys_MkdirArchive
#D-* Purpose: Mkdir on Archive
#D-* Examples:
#D-
function IGCM_sys_MkdirArchive {
  IGCM_debug_PushStack "IGCM_sys_MkdirArchive" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_MkdirArchive :" $@
  fi
  #- creation de repertoire sur le serveur fichier
  if [ ! -d ${1} ]; then
    \mkdir -p $1
    status=$?

    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_MkdirArchive : mkdir failed error code ${status}"
      IGCM_debug_Exit "IGCM_sys_MkdirArchive"
    fi
  fi
  IGCM_debug_PopStack "IGCM_sys_MkdirArchive"
}

#D-#==================================================
#D-function IGCM_sys_TestDirArchive
#D-* Purpose: Test Directory that must exists on Archive
#D-* Examples:
#D-
function IGCM_sys_TestDirArchive {
  IGCM_debug_PushStack "IGCM_sys_TestDirArchive" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_TestDirArchive :" $@
  fi
  typeset ExistFlag
  ExistFlag=$( [ -d $1 ] && echo 0 || echo 1 )
  IGCM_debug_PopStack "IGCM_sys_TestDirArchive"
  return ${ExistFlag}
}

#D-#==================================================
#D-function IGCM_sys_IsFileArchived
#D-* Purpose: Test file that must NOT EXISTS on Archive based on filename only
#D-* Examples:
#D-
function IGCM_sys_IsFileArchived {
  IGCM_debug_PushStack "IGCM_sys_IsFileArchived" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_IsFileArchived :" $@
  fi
  typeset IsArchivedFlag
  IsArchivedFlag=$( [ X$( echo $@ | grep ^\/ccc\/store ) != X ] && echo 0 || echo 1 )
  IGCM_debug_PopStack "IGCM_sys_IsFileArchived"

  return ${IsArchivedFlag}
}

#D-#==================================================
#D-function IGCM_sys_TestFileArchive
#D-* Purpose: Test file that must NOT EXISTS on Archive
#D-* Examples:
#D-
function IGCM_sys_TestFileArchive {
  IGCM_debug_PushStack "IGCM_sys_TestFileArchive" $@
  typeset ExistFlag
  ExistFlag=$( [ -f $1 ] && echo 0 || echo 1 )
  IGCM_debug_PopStack "IGCM_sys_TestFileArchive"

  return ${ExistFlag}
}

#D-#==================================================
#D-function IGCM_sys_CountFileArchive
#D-* Purpose: Count files on Archive filesystem
#D-* Examples:
#D-
function IGCM_sys_CountFileArchive {
  IGCM_debug_PushStack "IGCM_sys_CountFileArchive" $@
  ls ${@} 2>/dev/null | wc -l
  if [ $? -gt 0 ] ; then
    echo "IGCM_sys_CountFileArchive : erreur."
  fi
  IGCM_debug_PopStack "IGCM_sys_CountFileArchive"
}

#D-#==================================================
#D-function IGCM_sys_Tree
#D-* Purpose: Tree directories with files on ${ARCHIVE}
#D-* Examples: IGCM_sys_Tree ${R_IN} ${R_OUT}
#D-
function IGCM_sys_Tree {
  IGCM_debug_PushStack "IGCM_sys_Tree" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Tree :" $@
  fi

  \ls -lR ${@}

  IGCM_debug_PopStack "IGCM_sys_Tree"
}

#D-#==================================================
#D-function IGCM_sys_Qsub
#D-* Purpose: Qsub new job
#D-* Examples:
#D-
function IGCM_sys_Qsub {
  IGCM_debug_PushStack "IGCM_sys_Qsub" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Qsub :" $@
  fi
  typeset options status
  options="-o ${SUBMIT_DIR}/${Script_Output} -e ${SUBMIT_DIR}/${Script_Output}"

  /usr/bin/ccc_msub ${options} $1 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
  status=$?

  cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
  if [ ${status} -gt 0 ] ; then
    IGCM_debug_Print 2 "IGCM_sys_Qsub ${options} $1 : error code ${status}"
    IGCM_debug_Exit "IGCM_sys_Qsub"
  else
    JobID=$( gawk {'print $4'} ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ )
  fi
  IGCM_debug_PopStack "IGCM_sys_Qsub"
}

#D-#==================================================
#D-function IGCM_sys_QsubPost
#D-* Purpose: Qsub new job on scalaire
#D-* Examples:
#D-
function IGCM_sys_QsubPost {
  IGCM_debug_PushStack "IGCM_sys_QsubPost" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_QsubPost :" $@
  fi
  typeset options status

  # EASIER TO DO THIS INSTEAD OF DUPLICATING libIGCM_sys_curie.ksh
  case $( hostname -s ) in
  curie*)
    options="-Q normal -A ${PROJECT} -o ${POST_DIR}/${Script_Post_Output}.out -e ${POST_DIR}/${Script_Post_Output}.out";;
  airain*)
    options="-q ivybridge -A dsm -o ${POST_DIR}/${Script_Post_Output}.out -e ${POST_DIR}/${Script_Post_Output}.out"
  esac

  /usr/bin/ccc_msub ${options} ${libIGCM_POST}/$1.job > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
  status=$?

  cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
  if [ ${status} -gt 0 ] ; then
    IGCM_debug_Print 2 "IGCM_sys_QsubPost ${options} ${libIGCM_POST}/$1.job : error code ${status}"
    IGCM_debug_Exit "IGCM_sys_QsubPost"
  else
    JobID=$( gawk {'print $4'} ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ )
  fi
  IGCM_debug_PopStack "IGCM_sys_QsubPost"
}

#D-*************************
#D- File transfer functions
#D-*************************
#D-

#D-#==================================================
#D-function IGCM_sys_RmRunDir
#D-* Purpose: rm tmpdir (dummy function most of the time batch
#D-                      scheduler will do the job)
#D-* Examples:
#D-
function IGCM_sys_RmRunDir {
  IGCM_debug_PushStack "IGCM_sys_RmRunDir" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_RmRunDir :" $@
  fi

  typeset status

  echo rm $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
  \rm $@ >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
  status=$?

  if [ ${status} -gt 0 ] ; then
    IGCM_debug_Print 1 "IGCM_sys_RmRunDir : rm error code is ${status}."
    cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    IGCM_debug_Exit "IGCM_sys_RmRunDir"
  else
    \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
  fi
  IGCM_debug_PopStack "IGCM_sys_RmRunDir"
}

#D-#==================================================
#D-function IGCM_sys_Put_Dir
#D-* Purpose: Copy a complete directory on $(ARCHIVE)
#D-* Examples:
#D-
function IGCM_sys_Put_Dir {
  IGCM_debug_PushStack "IGCM_sys_Put_Dir" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Put_Dir :" $@
  fi
  if [ $DRYRUN = 0 ]; then
    if [ ! -d ${1} ] ; then
      echo "WARNING : IGCM_sys_Put_Dir ${1} DOES NOT EXIST ."
      IGCM_debug_PopStack "IGCM_sys_Put_Dir"
      return
    fi

    typeset status

    # Only if we use rsync
    #IGCM_sys_TestDirArchive $( dirname $2 )
    #
    #USUAL WAY
    \cp -r $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?

    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_Put_Dir : cp failed error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Exit "IGCM_sys_Put_Dir"
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    fi
  fi
  IGCM_debug_PopStack "IGCM_sys_Put_Dir"
}

#D-#==================================================
#D-function IGCM_sys_Get_Dir
#D-* Purpose: Copy a complete directory from ${ARCHIVE}
#D-* Examples:
#D-
function IGCM_sys_Get_Dir {
  IGCM_debug_PushStack "IGCM_sys_Get_Dir" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Get_Dir :" $@
  fi
  if [ $DRYRUN = 0 ]; then
    typeset NB_ESSAI DELAI status i
    # number of tentative
    NB_ESSAI=3
    # time delay between tentative
    DELAI=2

    #
    # USUAL WAY
    # add 'ccc_hsm get' (to demigrate all offline files) to reduce time of this command :
    ccc_hsm get -r $1

    i=0
    while [ $i -lt $NB_ESSAI ] ; do
      \cp -ur $1 $2 >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
      status=$?
      if [ ${status} -gt 0 ] ; then
        IGCM_debug_Print 2 "IGCM_sys_Get_Dir : cp failed error code ${status} ${i}/${NB_ESSAI}"
        IGCM_debug_Print 2 "IGCM_sys_Get_Dir : sleep ${DELAI} seconds and try again."
        sleep $DELAI
      else
        break
      fi
      (( i = i + 1 ))
    done

    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_Get_Dir : cp failed error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Exit "IGCM_sys_Get_Dir"
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    fi
  fi
  IGCM_debug_PopStack "IGCM_sys_Get_Dir"
}

#D-#==================================================
#D-function IGCM_sys_Put_Rest
#D-* Purpose: Put computied restarts on ${ARCHIVE}.
#D-           File and target directory must exist.
#D-* Examples:
#D-
function IGCM_sys_Put_Rest {
  IGCM_debug_PushStack "IGCM_sys_Put_Rest" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Put_Rest :" $@
  fi
  if [ $DRYRUN = 0 ]; then
    if [ ! -f ${1} ] ; then
      echo "ERROR : IGCM_sys_Put_Rest ${1} DOES NOT EXIST ."
      IGCM_debug_Exit "IGCM_sys_Put_Rest"
    fi

    typeset status
    #
    # USUAL WAY
    \cp $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?

#       #RSYNC WITH NETWORK SSH CALL
#       echo ${RSYNC} ${RSYNC_opt} -e ssh ${RUN_DIR}/$1 ${STOREHOST}:${2} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
#       ${RSYNC} ${RSYNC_opt} -e ssh ${RUN_DIR}/$1 ${STOREHOST}:${2} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1

#       #RSYNC WITH NFS USE
#       echo ${RSYNC} ${RSYNC_opt} ${RUN_DIR}/$1 ${2} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
#       ${RSYNC} ${RSYNC_opt} ${RUN_DIR}/$1 ${2} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1

#       status=$?
#       IGCM_sys_Rsync_out $status

#       ${libIGCM}/libIGCM_sys/IGCM_analyse_rsync_out.awk ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
#       (( status=status+$? ))

    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_Put_Rest : cp failed error code ${status}"
      [ -f ${1} ] && ls -l ${1}
      [ -f ${2} ] && ls -l ${2}
      [ -f ${2}/${1} ] && ls -l ${2}/${1}
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Exit "IGCM_sys_Put_Rest"
    else

      if [ X${JobType} = XRUN ] ; then
        [ -f ${2} ] && IGCM_sys_Chmod 444 ${2}
        [ -f ${2}/${1} ] && IGCM_sys_Chmod 444 ${2}/${1}
      fi

      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    fi
  fi
  IGCM_debug_PopStack "IGCM_sys_Put_Rest"
}

#D-#==================================================
#D-function IGCM_sys_Put_Out
#D-* Purpose: Copy a file on ${ARCHIVE} after having chmod it in readonly
#D-* Examples:
#D-
function IGCM_sys_Put_Out {
  IGCM_debug_PushStack "IGCM_sys_Put_Out" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Put_Out :" $@
  fi

  typeset NB_ESSAI DELAI status i exist skip
  typeset fileDeviceNumberInHex directoryDeviceNumberInHex

  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  if [ $DRYRUN = 0 ]; then
    if [ ! -f ${1} ] ; then
      echo "WARNING : IGCM_sys_Put_Out ${1} DOES NOT EXIST ."
      IGCM_debug_PopStack "IGCM_sys_Put_Out"
      return 1
    fi
    #
    IGCM_sys_MkdirArchive $( dirname $2 )
    #
    exist=false
    skip=false
    if [ -f $2 ] ; then
      IGCM_debug_Print 1 "$2 already exist"
      ccc_hsm get $2
      exist=true
      if [ "X$( diff $1 $2 )" = X ] ; then
        IGCM_debug_Print 2 "$1 and $2 are the same file, we skip the copy"
        skip=true
      else
        IGCM_debug_Print 2 "$1 and $2 are not the same file, we force the copy"
        skip=false
      fi
    fi
    #
    if ( [ X${exist} = Xtrue ] && [ X${skip} = Xfalse ] ) ; then
      IGCM_sys_Chmod u+w $2
    fi

    if [ X${skip} = Xfalse ] ; then
      i=0
      while [ $i -lt $NB_ESSAI ] ; do
        # Identify file system
        fileDeviceNumberInHex=$( stat -c %d $1 )
        status=$?
        if [ ${status} -gt 0 ] ; then
          IGCM_debug_Exit "IGCM_sys_Put_Out"
        fi
        # Identify file system
        directoryDeviceNumberInHex=$( stat -c %d $( dirname $2 ) )
        status=$?
        if [ ${status} -gt 0 ] ; then
          IGCM_debug_Exit "IGCM_sys_Put_Out"
        fi

        if [ ${fileDeviceNumberInHex} -ne ${directoryDeviceNumberInHex} ] ; then
          # They are not on the same device. USUAL WAY
          \cp $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
          status=$?
        else
          # They are on the same device. NOT SO USUAL WAY
          \mv $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
          status=$?
        fi
        if [ ${status} -gt 0 ]; then
          IGCM_debug_Print 2 "IGCM_sys_Put_Out : cp failed error code ${status} ${i}/${NB_ESSAI}"
          IGCM_debug_Print 2 "IGCM_sys_Put_Out : sleep ${DELAI} seconds and try again."
          [ -f ${1} ] && ls -l ${1}
          [ -f ${2} ] && ls -l ${2}
          [ -f ${2}/${1} ] && ls -l ${2}/${1}
          sleep $DELAI
        else
          break
        fi
        (( i = i + 1 ))
      done
    fi

#       #RSYNC WITH NETWORK SSH CALL
#       echo ${RSYNC} ${RSYNC_opt} -e ssh ${RUN_DIR}/$1 ${STOREHOST}:${2} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
#       ${RSYNC} ${RSYNC_opt} -e ssh ${RUN_DIR}/$1 ${STOREHOST}:${2} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1

#       #RSYNC WITH NFS USE
#       echo ${RSYNC} ${RSYNC_opt} ${RUN_DIR}/$1 ${2} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
#       ${RSYNC} ${RSYNC_opt} ${RUN_DIR}/$1 ${2} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1

#       status=$?
#       IGCM_sys_Rsync_out $status

#       ${libIGCM}/libIGCM_sys/IGCM_analyse_rsync_out.awk ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
#       (( status=status+$? ))

    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_Put_Out : cp failed error code ${status}"
      [ -f ${1} ] && ls -l ${1}
      [ -f ${2} ] && ls -l ${2}
      [ -f ${2}/${1} ] && ls -l ${2}/${1}
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Exit "IGCM_sys_Put_Out"
    else

      if [ X${JobType} = XRUN ] ; then
        if [ X${3} = X ] ; then
          [ -f ${2} ] && IGCM_sys_Chmod 444 ${2}
          [ -f ${2}/${1} ] && IGCM_sys_Chmod 444 ${2}/${1}
        fi
      fi

      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    fi
  fi
  IGCM_debug_PopStack "IGCM_sys_Put_Out"
  return 0
}

#D-#==================================================
#D-function IGCM_sys_Get
#D-* Purpose: Get a file from ${ARCHIVE}
#D-* Examples: IGCM_sys_Get myfile /destpath/myfile_with_PREFIX
#D-            IGCM_sys_Get /l Array_contain_myfiles /destpath/
function IGCM_sys_Get {
  IGCM_debug_PushStack "IGCM_sys_Get" $@

  typeset DEST dm_liste target file_work
  typeset NB_ESSAI DELAI status i

  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Get :" $@
  fi

  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  if [ $DRYRUN -le 2 ]; then
    if [ X${1} = X'/l' ] ; then
      eval set +A dm_liste \${${2}}
    else
      eval set +A dm_liste ${1}
    fi
    eval DEST=\${${#}}
    ccc_hsm get ${dm_liste[*]} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?
    if [ ${status} -gt 0 ] ; then
      echo "WARNING IGCM_sys_Get : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      echo "WARNING IGCM_sys_Get : will stop later if the cp fails."
    fi

    #if [ ${status} -gt 0 ] ; then
    #  if [ ! "X$( grep "Lost dmusrcmd connection" ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ )" = "X" ] ; then
    #    cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    #    echo "WARNING IGCM_sys_Get : Lost dmusrcmd connection : "
    #    sleep 30
    #    echo "We try another time"
    ##    dmget ${dm_liste[*]} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    #    ccc_hsm get ${dm_liste[*]} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    #    status=$?
    #    if [ ${status} -gt 0 ] ; then
    #      echo "ERROR IGCM_sys_Get : again demigration error :"
    #      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    #      IGCM_debug_Exit "IGCM_sys_Get"
    #    fi
    #  else
    #    echo "ERROR IGCM_sys_Get : demigration error :"
    #    cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    #    IGCM_debug_Exit "IGCM_sys_Get"
    #  fi
    #fi

    #   #RSYNC WITH NETWORK SSH CALL
    #   echo ${RSYNC} ${RSYNC_opt} -e ssh ${STOREHOST}:"${dm_liste}" ${STOREHOST}:${RUN_DIR}/${DEST} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    #   ${RSYNC} ${RSYNC_opt} -e ssh ${STOREHOST}:"${dm_liste}" ${STOREHOST}:${RUN_DIR}/${DEST} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1

    #   #RSYNC WITH NFS USE
    #   echo ${RSYNC} ${RSYNC_opt} ${dm_liste} ${RUN_DIR}/${DEST} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    #   ${RSYNC} ${RSYNC_opt} ${dm_liste} ${RUN_DIR}/${DEST} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1

    #   status=$?
    #   IGCM_sys_Rsync_out $status

    #   ${libIGCM}/libIGCM_sys/IGCM_analyse_rsync_out.awk ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    #   (( status=status+$? ))

    #USUAL WAY
    if [ X${1} = X'/l' ] ; then
      for target in ${dm_liste[*]} ; do
        local_file=$( basename ${target} )
        # test if the target file is present before the loop
        IGCM_sys_TestFileArchive ${target}
        status=$?
        if [ ${status} -gt 0 ] ; then
          echo "IGCM_sys_Get, ERROR : regular file ${target} DOES NOT EXIST ."
          IGCM_debug_Exit "IGCM_sys_Get"
        else
          i=0
          while [ $i -lt $NB_ESSAI ] ; do
            #if [ X${DoLink} = Xtrue ] ; then
            #  \ln -s ${target} ${DEST}/${local_file} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
            #  status=$?
            #  else
            #  \cp ${target} ${DEST}/${local_file} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
            #  status=$?
            #fi
            \ln -s ${target} ${DEST}/${local_file} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
            status=$?
            if [ ${status} -gt 0 ]; then
              IGCM_debug_Print 2 "IGCM_sys_Get : cp failed error code ${status} ${i}/${NB_ESSAI}"
              IGCM_debug_Print 2 "IGCM_sys_Get : sleep ${DELAI} seconds and try again."
              sleep $DELAI
            else
              break
            fi
            (( i = i + 1 ))
          done
          if [ ${status} -gt 0 ] ; then
            echo "IGCM_sys_Get : error"
            cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
            \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
            IGCM_debug_Exit "IGCM_sys_Get"
          else
            \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
          fi
        fi
      done
    else
      i=0
      while [ $i -lt $NB_ESSAI ] ; do
        \cp ${dm_liste} ${DEST} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
        status=$?
        if [ ${status} -gt 0 ]; then
          IGCM_debug_Print 2 "IGCM_sys_Get : cp failed error code ${status} ${i}/${NB_ESSAI}"
          IGCM_debug_Print 2 "IGCM_sys_Get : sleep ${DELAI} seconds and try again."
          sleep $DELAI
        else
          break
        fi
        (( i = i + 1 ))
      done
      if [ ${status} -gt 0 ] ; then
        echo "IGCM_sys_Get : error"
        cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
        \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
        IGCM_debug_Exit "IGCM_sys_Get"
      else
        \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      fi
    fi
  fi
  IGCM_debug_PopStack "IGCM_sys_Get"
}

#D-#==================================================
#D-function IGCM_sys_GetDate_Monitoring
#D-* Purpose: get the last year for which the monitoring has been computed
#D-* Examples:
#D-
function IGCM_sys_GetDate_Monitoring {
  IGCM_debug_PushStack "IGCM_sys_GetDate_Monitoring" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_GetDate_Monitoring :" $@
  fi

  eval ${2}=$( cdo showyear ${1} 2> /dev/null | gawk '{ print $NF }' )

  IGCM_debug_PopStack "IGCM_sys_GetDate_Monitoring"
}

#D-#==================================================
#D-function IGCM_sys_Dods_Rm
#D-* Purpose: DO NOTHING ! Put ${ARCHIVE} files on DODS internet protocole.
#D-* Examples:
#D-
function IGCM_sys_Dods_Rm {
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Dods_Rm :" $@
  fi
  typeset status
  if [ $DRYRUN = 0 ]; then

#    if [ ! -d /dmnfs/cont003/dods/public/${LOGIN}/${R_DODS}/${1} ] ; then
#      echo "WARNING : IGCM_sys_Dods_Rm /dmnfs/cont003/dods/public/${LOGIN}/${R_DODS}/${1} DOES NOT EXIST ."
#      echo "Nothing has been done."
#      return
#    fi

    /ccc/cont003/home/dsm/p86ipsl/bin/dods_rm public/${LOGIN}/${R_DODS}/${1} # > out_dods_rm 2>&1
    status=$?

#    if [ ${status} -gt 0 ] ; then
#      echo "IGCM_sys_Dods_Rm : error."
#      cat out_dods_rm
#      IGCM_debug_Exit "IGCM_sys_Dods_Rm"
#    else
#      rm out_dods_rm
#    fi

  fi
  return $status
}

#D-#==================================================
#D-function IGCM_sys_Dods_Cp
#D-* Purpose: Copy $(ARCHIVE) files on DODS internet protocole.
#D-* Examples:
#D-
function IGCM_sys_Dods_Cp {
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Dods_Cp :" $@
  fi
  typeset status
  if [ $DRYRUN = 0 ]; then

#    if [ ! -d ${R_SAVE}/${1} ] ; then
#      echo "WARNING : IGCM_sys_Dods_Cp ${R_SAVE}/${1} DOES NOT EXIST ."
#      echo "Nothing has been done."
#      return
#    fi

    /ccc/cont003/home/dsm/p86ipsl/bin/dods_cp ${1} public/${LOGIN}/${R_DODS} # > out_dods_cp 2>&1
    status=$?

#       if [ ${status} -gt 0 ] ; then
#           echo "IGCM_sys_Dods_Cp : error."
#           cat out_dods_cp
#           IGCM_debug_Exit "IGCM_sys_Dods_Cp"
#       else
#           rm out_dods_cp
#       fi

  fi
  return $status
}

#D-#==================================================
#D-function IGCM_sys_Put_Dods
#D-* Purpose: Put ${ARCHIVE} files on DODS internet protocole.
#D-* Examples:
#D-
function IGCM_sys_Put_Dods {
  IGCM_debug_PushStack "IGCM_sys_Put_Dods" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Put_Dods :" $@
  fi
  typeset status
  if [ $DRYRUN = 0 ]; then
    if ( [ ! -d ${R_SAVE}/${1} ] && [ ! -d ${R_FIGR}/${1} ] ) ; then
      echo "WARNING IGCM_sys_Put_Dods : None of the following directories exist. Exactly one should."
      echo "WARNING IGCM_sys_Put_Dods : ${R_SAVE}/${1} DOES NOT EXIST."
      echo "WARNING IGCM_sys_Put_Dods : ${R_FIGR}/${1} DOES NOT EXIST."
      IGCM_debug_PopStack "IGCM_sys_Put_Dods"
      return
    fi

    if ( [ -d ${R_SAVE}/${1} ] && [ -d ${R_FIGR}/${1} ] ) ; then
      echo "WARNING IGCM_sys_Put_Dods : Both of the following directories exist. Exactly one should."
      echo "WARNING IGCM_sys_Put_Dods : ${R_SAVE}/${1} EXISTS."
      echo "WARNING IGCM_sys_Put_Dods : ${R_FIGR}/${1} EXISTS."
      IGCM_debug_PopStack "IGCM_sys_Put_Dods"
      return
    fi
    #
    if [ -d ${R_SAVE}/${1} ] ; then
      cd ${R_SAVE}
    elif [ -d ${R_FIGR}/${1} ] ; then
      cd ${R_FIGR}
    fi

    IGCM_sys_Dods_Rm ${1}
    IGCM_sys_Dods_Cp ${1}
    status=0

    if [ ${status} -gt 0 ] ; then
      echo "IGCM_sys_Put_Dods : error."
      IGCM_debug_Exit "IGCM_sys_Put_Dods"
    fi
  fi
  IGCM_debug_PopStack "IGCM_sys_Put_Dods"
}

##############################################################
# REBUILD OPERATOR

#D-#==================================================
#D-function IGCM_sys_sync
#D-* Purpose: flush buffer on disk
#D-* Examples:
#D-
function IGCM_sys_sync {
  IGCM_debug_PushStack "IGCM_sys_sync" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_sync :" $@
  fi

  /bin/sync

  IGCM_debug_PopStack "IGCM_sys_sync"
}

############################################################
# Activate Running Environnment Variables

#D-#==================================================
#D-function IGCM_sys_activ_variables
#D-* Purpose: set environement variables prior to execution
#D-* Examples:
#D-
function IGCM_sys_activ_variables {
  IGCM_debug_PushStack "IGCM_sys_activ_variables"
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_activ_variables"
  fi

# --------------------------------------------------------------------
#D- MPI specifications
# --------------------------------------------------------------------

# --------------------------------------------------------------------
#D- Other specifications
# --------------------------------------------------------------------

  ulimit -s unlimited

  IGCM_debug_PopStack "IGCM_sys_activ_variables"
}

############################################################
# Desactivate Running Environnment Variables

#D-#==================================================
#D-function IGCM_sys_desactiv_variables
#D-* Purpose: unset environement variables after execution
#D-* Examples:
#D-
function IGCM_sys_desactiv_variables {
  IGCM_debug_PushStack "IGCM_sys_desactiv_variables"
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_desactiv_variables"
  fi
# --------------------------------------------------------------------
#D- MPI specifications
# --------------------------------------------------------------------

# --------------------------------------------------------------------
#D- Other specifications
# --------------------------------------------------------------------

  IGCM_debug_PopStack "IGCM_sys_desactiv_variables"
}

############################################################
# Update job headers to be used by the scheduler

#D-#==================================================
#D-function IGCM_sys_updateHeaders
#D-* Purpose: Update job headers to be used by the scheduler
#D-* Examples: IGCM_sys_updateHeaders /path/to/Job_MYEXP
#D-
function IGCM_sys_updateHeaders {
  IGCM_debug_PushStack "IGCM_sys_updateHeaders"
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_updateHeaders"
  fi
  typeset file
  file=$1

  if [ ${executionType} -eq 1 ] ; then
    # MPMD + MPI
    sed -e "/::openMPthreads::/d"                  \
        -e "s/::JobNumProcTot::/${coreNumber}/"    \
        -e "/#MSUB -x/d"                           \
        -e "/--cpu_bind=none/d"                    \
      ${file} > ${file}.tmp

  elif [ ${executionType} -eq 2 ] ; then
    # MPMD + MPI + OMP
    sed -e "/::openMPthreads::/d"                  \
        -e "s/::JobNumProcTot::/${coreNumber}/"    \
      ${file} > ${file}.tmp

  elif [ ${executionType} -eq 3 ] ; then
    # SPMD + MPI/OMP
    sed -e "s/::openMPthreads::/${openMPthreads}/" \
        -e "s/::JobNumProcTot::/${mpiTasks}/"      \
        -e "/#MSUB -x/d"                           \
        -e "/--cpu_bind=none/d"                    \
      ${file} > ${file}.tmp

  elif [ ${executionType} -eq 4 ] ; then
    # SPMD + MPI only
    sed -e "s/::JobNumProcTot::/${mpiTasks}/"      \
        -e "/::openMPthreads::/d"                  \
        -e "/#MSUB -x/d"                           \
        -e "/--cpu_bind=none/d"                    \
      ${file} > ${file}.tmp

  elif [ ${executionType} -eq 5 ] ; then
    # SPMD + OMP only
    sed -e "s/::openMPthreads::/${openMPthreads}/" \
        -e "/::JobNumProcTot::/d"                  \
        -e "/#MSUB -x/d"                           \
        -e "/--cpu_bind=none/d"                    \
      ${file} > ${file}.tmp

  elif [ ${executionType} -eq 6 ] ; then
    # SEQUENTIAL THEN
    sed -e "s/::JobNumProcTot::/1/"                \
        -e "/::openMPthreads::/d"                  \
        -e "/#MSUB -x/d"                           \
        -e "/--cpu_bind=none/d"                    \
      ${file} > ${file}.tmp

  fi

  IGCM_sys_Mv ${file}.tmp ${file}

  IGCM_debug_PopStack "IGCM_sys_updateHeaders"
}

############################################################
# Build MPI/OMP scripts run file (dummy function)

#D-#==================================================
#D-function IGCM_sys_build_run_file
#D-* Purpose: build run file (deprecated)
#D-* Examples:
#D-
function IGCM_sys_build_run_file {
  IGCM_debug_Print 3 " dummy function : IGCM_sys_build_run_file "
}

############################################################
# Build MPI/OMP scripts

#D-#==================================================
#D-function IGCM_sys_build_execution_scripts
#D-* Purpose: build execution scripts to be launch by ${HOST_MPIRUN_COMMAND}
#D-* Examples:
#D-
function IGCM_sys_build_execution_scripts
{
  IGCM_debug_PushStack "IGCM_sys_build_execution_scripts" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_build_execution_scripts " $@
  fi

  EXECUTION=${HOST_MPIRUN_COMMAND}

  # MPMD mode
  if ( ${OK_PARA_MPMD} ) ; then

    # Only MPI (MPMD)
    if  ( ! ${OK_PARA_OMP} ) ; then

      if [ -f run_file ] ; then
        IGCM_sys_Rm -f run_file
      fi
      touch run_file

      # Build run_file

      # First loop on the components for the coupler ie oasis (only if oasis3)
      # the coupler ie oasis3 must be the first one
      for comp in ${config_ListOfComponents[*]} ; do

        eval ExeNameIn=\${config_Executable_${comp}[0]}
        eval ExeNameOut=\${config_Executable_${comp}[1]}

        # for CPL component only
        if [ "X${comp}" = "XCPL" ] && [ "X${ExeNameOut}" != X\"\" ] ; then

          eval comp_proc_mpi_loc=\${${comp}_PROC_MPI}
          eval comp_proc_omp_loc=\${${comp}_PROC_OMP}
          echo "${comp_proc_mpi_loc} ./${ExeNameOut}" >> run_file
        fi
      done

      # Then second loop on the components
      for comp in ${config_ListOfComponents[*]} ; do

        eval ExeNameIn=\${config_Executable_${comp}[0]}
        eval ExeNameOut=\${config_Executable_${comp}[1]}

        # Only if we really have an executable for the component and not the coupler ie oasis:
        if ( [ "X${ExeNameOut}" != X\"\" ] && [ "X${comp}" != "XCPL" ] ) ; then

          eval comp_proc_mpi_loc=\${${comp}_PROC_MPI}
          eval comp_proc_omp_loc=\${${comp}_PROC_OMP}
          echo "${comp_proc_mpi_loc} ./${ExeNameOut}" >> run_file
        fi
      done

      EXECUTION="${HOST_MPIRUN_COMMAND} -f ./run_file"

      IGCM_sys_Chmod u+x run_file
      if ( $DEBUG_sys ) ; then
        echo "run_file contains : "
        cat run_file
      fi

    # MPI-OpenMP (MPMD)
    else

      # Use of mpirun instead of ccc_mprun
      EXECUTION="time mpirun"

      #  Hosts treatment
      ${EXECUTION} hostname | sort | uniq > hosts.tmp

      i=0
      rm -f hosts rankfile
      IGCM_debug_Print 1 "sys Curie, Hosts available :"
      for nodes in `cat hosts.tmp`
      do
        host[$i]=$nodes
        echo "${host[$i]}" >> hosts
        IGCM_debug_Print 1 ${host[$i]}
        i=$((i+1))
      done
      rm -f hosts.tmp

      listnodes=${host[*]}

      EXECUTION="${EXECUTION} -hostfile hosts -rankfile rankfile"

      # Initialisation
      rank=0
      current_core=0
      core_per_node=16
      init_exec=n

      # Loop on the components
      for comp in ${config_ListOfComponents[*]} ; do

        eval ExeNameIn=\${config_Executable_${comp}[0]}
        eval ExeNameOut=\${config_Executable_${comp}[1]}

        # Not possible if oasis has an executable (i.e old version of oasis3)
        if ( [ "X${ExeNameOut}" != X\"\" ] &&  [ "X${comp}" = "XCPL" ] ) ; then
          IGCM_debug_Exit "ERROR MPMD with hybrid MPI-OpenMP is not available with oasis3 version"
          IGCM_debug_Print 2 "Only available with oasis3-MCT version coupler"
          IGCM_debug_Verif_Exit
        fi

        # Only if we really have an executable for the component :
        if [ "X${ExeNameOut}" != X\"\" ] ; then

          eval comp_proc_mpi_loc=\${${comp}_PROC_MPI}
          eval comp_proc_omp_loc=\${${comp}_PROC_OMP}

          echo "#!/bin/ksh" > script_${ExeNameOut}.ksh
          echo ""  >> script_${ExeNameOut}.ksh
          if [ ${comp_proc_omp_loc} -gt 1 ] ; then

            # Check if the number of threads is correct
            case ${comp_proc_omp_loc} in
            2|4|8|16)
              IGCM_debug_Print 1 "You run ${ExeNameOut} on ${comp_proc_omp_loc} OMP threads"
              ;;
            *)
              IGCM_debug_Exit "ERROR with OMP parameters !"
              IGCM_debug_Print 2 "${comp_proc_omp_loc} is not possible as number of OMP threads"
              IGCM_debug_Print 2 "Only 2,4,8,16 as number of OMP threads are possible "
              IGCM_debug_Verif_Exit
              ;;
            esac
            echo "export KMP_STACKSIZE=3g"  >> script_${ExeNameOut}.ksh
            echo "export KMP_LIBRARY=turnaround"  >> script_${ExeNameOut}.ksh
            echo "export MKL_SERIAL=YES"  >> script_${ExeNameOut}.ksh
            echo "OMP_NUM_THREADS=${comp_proc_omp_loc}" >> script_${ExeNameOut}.ksh
          fi

          #echo "./${ExeNameOut}" >> script_${ExeNameOut}.ksh
	  echo "(( MYMPIRANK = OMPI_COMM_WORLD_RANK )) " >> script_${ExeNameOut}.ksh
	  echo "MYMPIRANK=\$(printf '%3.3d\n' \${MYMPIRANK})" >> script_${ExeNameOut}.ksh
	  echo "./${ExeNameOut} > out_${ExeNameOut}.out.\${MYMPIRANK} 2>out_${ExeNameOut}.err.\${MYMPIRANK}" >> script_${ExeNameOut}.ksh
	  IGCM_sys_Chmod u+x script_${ExeNameOut}.ksh

          if [ ${init_exec} = y ] ; then
            EXECUTION="${EXECUTION} : -np ${comp_proc_mpi_loc} ./script_${ExeNameOut}.ksh"
          else
            EXECUTION="${EXECUTION} -np ${comp_proc_mpi_loc} ./script_${ExeNameOut}.ksh"
            init_exec=y
          fi

          # Build rankfile : method used to assign cores and nodes for the MPI process
          # Ex :
          #rank 0=curie5296 slot=0,1,2,3
          #rank 1=curie5296 slot=4,5,6,7
          # Example of final command :
          # mpirun -hostfile hosts -rankfile rankfile -np 27 ./script_lmdz.x.ksh : -np 5 ./script_opa.xx.ksh
          # with script_lmdz.x.ksh :
          # #!/bin/ksh
          #export KMP_STACKSIZE=3g
          #export KMP_LIBRARY=turnaround
          #export MKL_SERIAL=YES
          #OMP_NUM_THREADS=4
          #./lmdz.x

          for nb_proc_mpi in `seq 0 $(($comp_proc_mpi_loc-1))`; do
            (( index_host = current_core / core_per_node ))
            host_value=${host[${index_host}]}
            (( slot =  current_core % core_per_node ))
            virg=","
            string_final=""
            for index in `seq $slot $(($slot+$comp_proc_omp_loc-1))`; do
              string=$index$virg
              string_final=$string_final$string
            done
            string_final=$( echo $string_final | sed "s/.$//" )
            echo "rank $rank=$host_value slot=$string_final" >> rankfile
            (( rank = rank + 1 ))
            (( current_core = current_core + comp_proc_omp_loc ))
          done
        fi
      done
    fi

  # Only one executable (SPMD mode).
  else

    for comp in ${config_ListOfComponents[*]} ; do

      # Only if we really have an executable for the component :
      eval ExeNameOut=\${config_Executable_${comp}[1]}
      if ( [ "X${ExeNameOut}" != X\"\" ] && [ "X${ExeNameOut}" != "Xinca.dat" ] ) ; then

        echo "#!/bin/ksh" > script_${ExeNameOut}.ksh
        echo ""  >> script_${ExeNameOut}.ksh
        IGCM_sys_Chmod u+x script_${ExeNameOut}.ksh

        if ( ${OK_PARA_OMP} ) ; then
          eval comp_proc_omp_loc=\${${comp}_PROC_OMP}
          echo ""  >> script_${ExeNameOut}.ksh
          echo "export KMP_STACKSIZE=3g"  >> script_${ExeNameOut}.ksh
          echo "export KMP_LIBRARY=turnaround"  >> script_${ExeNameOut}.ksh
          echo "export MKL_SERIAL=YES"  >> script_${ExeNameOut}.ksh
          echo "OMP_NUM_THREADS=${comp_proc_omp_loc}" >> script_${ExeNameOut}.ksh
        fi

        if  ( ${OK_PARA_MPI} ) ; then
          eval comp_proc_mpi_loc=\${${comp}_PROC_MPI}
          # Default : ccc_mprun used if nb_proc gt 1
          # to have out/err per process on different files
          echo "./${ExeNameOut} > out_${ExeNameOut}.out.\${SLURM_PROCID} 2>out_${ExeNameOut}.err.\${SLURM_PROCID}"  >> script_${ExeNameOut}.ksh
          #echo "./${ExeNameOut}" >> script_${ExeNameOut}.ksh
          EXECUTION="${HOST_MPIRUN_COMMAND} -n ${comp_proc_mpi_loc} ./script_${ExeNameOut}.ksh"
        else
          # Default : ccc_mprun is NOT used if nb_proc eq 1
          # to have out/err per process on different files
          echo "./${ExeNameOut} > out_${ExeNameOut}.out 2>out_${ExeNameOut}.err" >> script_${ExeNameOut}.ksh
          #echo "./${ExeNameOut}" >> script_${ExeNameOut}.ksh
          EXECUTION="/usr/bin/time ./script_${ExeNameOut}.ksh"
        fi

        IGCM_debug_Print 1 "sys Curie : script_${ExeNameOut}.ksh contains"
        cat script_${ExeNameOut}.ksh

      fi

    done

  fi

  IGCM_debug_Print 1 "sys Curie : execution command is "
  IGCM_debug_Print 1 "$EXECUTION"

  IGCM_debug_PopStack "IGCM_sys_build_execution_scripts"
}

#D-#==================================================
#D-function IGCM_sys_check_path
#D-* Purpose: check that RUN_DIR_PATH that will be removed on some machine
#D-* do not point to an important use directory. Stop immediately in that case.
#D-* Examples:
#D-
function IGCM_sys_check_path {
  IGCM_debug_PushStack "IGCM_sys_check_path"
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_check_path"
  fi

  if ( [ X${RUN_DIR_PATH} = X${HOME} ] || [ X${RUN_DIR_PATH} = X${WORKDIR} ] || [ X${RUN_DIR_PATH} = X${SCRATCHDIR} ] || [ X${RUN_DIR_PATH} = X${CCCWORKDIR} ] || [ X${RUN_DIR_PATH} = X${CCCSTOREDIR} ] ) ; then
    IGCM_debug_Print 1 "Variable RUN_DIR_PATH is pointing to an important directory : ${RUN_DIR_PATH}"
    IGCM_debug_Print 1 "Please check the RUN_DIR_PATH definition in your Job : Job_${config_UserChoices_JobName}"
    IGCM_debug_Exit "This will stop the job"
  fi
  IGCM_debug_PopStack "IGCM_sys_check_path"
}

#D-#==================================================
#D-function IGCM_sys_check_quota
#D-* Purpose: check user quota. Stop the simulation if quota above 90%
#D-* Examples:
#D-
function IGCM_sys_check_quota {
  IGCM_debug_PushStack "IGCM_sys_check_quota"
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_check_quota"
  fi
  # Limit of quota (in %)
  limit_quota=90

  # Check of the volume
  volume_quota=$(ccc_quota | grep ' scratch' | gawk '{print $2}')
  volume_avail=$(ccc_quota | grep ' scratch' | gawk '{print $3}')

  if ( [ ! X${volume_quota} = X ] && [ ! ${volume_quota} = "-" ] ) ; then

    unit_avail=${volume_avail: -1}
    unit_quota=${volume_quota: -1}

    if [ "${unit_quota}" = "*" ] ; then
      IGCM_debug_Print 1 "Please, check your quota of volume on scratch"
      IGCM_debug_Print 1 "More than 100% of your quota is used"
      IGCM_debug_Print 1 "Use the ccc_quota command to check"
      IGCM_debug_Print 1 "You must have more than 10% available to run"
      IGCM_debug_Exit "Not enough space to run ! STOP HERE"
      IGCM_debug_Verif_Exit
    fi

    temp_avail=${volume_avail%%${volume_avail: -1}*}
    temp_quota=${volume_quota%%${volume_quota: -1}*}

    if [ ! ${unit_avail} = ${unit_quota} ] ; then

    # Convertion
      if [ ${volume_avail: -1} = "T" ] ; then
        (( temp_avail = temp_avail * 1000000000000 ))
      elif [ ${volume_avail: -1} = "G" ] ; then
        (( temp_avail = temp_avail * 1000000000 ))
      elif [ ${volume_avail: -1} = "M" ] ; then
        (( temp_avail = temp_avail * 1000000 ))
      elif [ ${volume_avail: -1} = "k" ] ; then
        (( temp_avail = temp_avail * 1000 ))
      else
        (( temp_avail = volume_avail ))
      fi
      if [ ${volume_quota: -1} = "T" ] ; then
        (( temp_quota = temp_quota * 1000000000000 ))
      elif [ ${volume_quota: -1} = "G" ] ; then
        (( temp_quota = temp_quota * 1000000000 ))
      elif [ ${volume_quota: -1} = "M" ] ; then
        (( temp_quota = temp_quota * 1000000 ))
      elif [ ${volume_quota: -1} = "k" ] ; then
        (( temp_quota = temp_quota * 1000 ))
      else
        (( temp_quota = volume_quota ))
      fi
    fi

    quota_volume=$(echo "scale=2 ; $temp_quota/$temp_avail*100" | bc)
#    echo "volume ratio is " $quota_volume

    if [ ${quota_volume} -ge ${limit_quota} ] ; then
      IGCM_debug_Print 1 "Please, check your quota of volume on scratch"
      IGCM_debug_Print 1 "${quota_volume}% of your quota is used"
      IGCM_debug_Print 1 "Use the ccc_quota command to check"
      IGCM_debug_Print 1 "You must have more than 10% available to run"
      IGCM_debug_Exit "Not enough space to run ! STOP HERE"
      IGCM_debug_Verif_Exit
    fi

  fi

# Check of the number of inodes

  inode_quota=$(ccc_quota | grep ' scratch' | gawk '{print $6}')
  inode_avail=$(ccc_quota | grep ' scratch' | gawk '{print $7}')

  if ( [ ! X${inode_quota} = X ] && [ ! ${inode_quota} = "-" ] ) ; then

    unit_avail=${inode_avail: -1}
    unit_quota=${inode_quota: -1}

    if [ "${unit_quota}" = "*" ] ; then
      IGCM_debug_Print 1 "Please, check your quota of inode on scratch"
      IGCM_debug_Print 1 "More than 100% of your quota is used"
      IGCM_debug_Print 1 "Use the ccc_quota command to check"
      IGCM_debug_Print 1 "You must have more than 10% available to run"
      IGCM_debug_Exit "Not enough space to run ! STOP HERE"
      IGCM_debug_Verif_Exit
    fi

    temp_avail=${inode_avail%%${inode_avail: -1}*}
    temp_quota=${inode_quota%%${inode_quota: -1}*}

    if [ ! ${unit_avail} = ${unit_quota} ] ; then

    # Convertion
      if [ ${inode_avail: -1} = "T" ] ; then
        (( temp_avail = temp_avail * 1000000000000 ))
      elif [ ${inode_avail: -1} = "G" ] ; then
        (( temp_avail = temp_avail * 1000000000 ))
      elif [ ${inode_avail: -1} = "M" ] ; then
        (( temp_avail = temp_avail * 1000000 ))
      elif [ ${inode_avail: -1} = "k" ] ; then
        (( temp_avail = temp_avail * 1000 ))
      else
        (( temp_avail = inode_avail ))
      fi

      if [ ${inode_quota: -1} = "T" ] ; then
        (( temp_quota = temp_quota * 1000000000000 ))
      elif [ ${inode_quota: -1} = "G" ] ; then
        (( temp_quota = temp_quota * 1000000000 ))
      elif [ ${inode_quota: -1} = "M" ] ; then
        (( temp_quota = temp_quota * 1000000 ))
      elif [ ${inode_quota: -1} = "k" ] ; then
        (( temp_quota = temp_quota * 1000 ))
      else
        (( temp_quota = inode_quota ))
      fi
    fi
    quota_inode=$(echo "scale=2 ; $temp_quota/$temp_avail*100" | bc)
#    echo "inode ratio is " $quota_inode

    if [ ${quota_inode} -ge ${limit_quota} ] ; then
      IGCM_debug_Print 1 "Please, check your quota of inode on scratch"
      IGCM_debug_Print 1 "${quota_inode}% of your quota is used"
      IGCM_debug_Print 1 "Use the ccc_quota command to check"
      IGCM_debug_Print 1 "You must have more than 10% available to run"
      IGCM_debug_Exit "Not enough space to run ! STOP HERE"
      IGCM_debug_Verif_Exit
    fi
  fi
  IGCM_debug_PopStack "IGCM_sys_check_quota"
}

#D-#==================================================
#D-function IGCM_sys_projectAccounting
#D-* Purpose: store project accounting information in a file
#D-* Examples:
#D-
function IGCM_sys_projectAccounting {
  IGCM_debug_PushStack "IGCM_sys_projectAccounting"
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_check_quota"
  fi

  ssh curie91 /usr/bin/ccc_myproject > $1

  IGCM_debug_PopStack "IGCM_sys_projectAccounting"
}

#D-#==================================================
#D-function IGCM_sys_getJobSchedulerID
#D-* Purpose: Get the job ID during execution
#D-* Examples: IGCM_sys_getJobSchedulerID jobSchedulerID
#D-
function IGCM_sys_getJobSchedulerID {
  IGCM_debug_PushStack "IGCM_sys_getJobSchedulerID"
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_getJobSchedulerID"
  fi

  eval ${1}=${BRIDGE_MSUB_JOBID}

  IGCM_debug_PopStack "IGCM_sys_getJobSchedulerID"
}

#D-#==================================================
#D-function IGCM_sys_GetJobID
#D-* Purpose: Get the job ID from the JobName
#D-* Examples: IGCM_sys_GetJobID ${JobName} ${TargetUsr} JobID
#D-
function IGCM_sys_GetJobID {
  IGCM_debug_PushStack "IGCM_sys_GetJobID"
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_GetJobID"
  fi

  # With -f option, the full job name is given in the last column
  ID=$( ccc_mstat -f -u $2 | \
        gawk -v JobName=$1 '( $NF ~ JobName ) { print $1 }' )

  eval ${3}=${ID}
  IGCM_debug_PopStack "IGCM_sys_GetJobID"
}

#D-#==================================================
#D-function IGCM_sys_CountJobInQueue
#D-* Purpose: Count number of users job
#D-* Examples: IGCM_sys_CountJobInQueue ${JobName} NbRun
#D-
function IGCM_sys_CountJobInQueue {
  IGCM_debug_PushStack "IGCM_sys_CountJobInQueue"
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_CountJobInQueue"
  fi

  # With -f option, the full job name is given in the last column
  NbRun=$( ccc_mstat -f | gawk -v JobName=$1 'BEGIN { x=0 } ( $NF ~ JobName ) { x=x+1 } END { print x }' )

  eval ${2}=${NbRun}

  IGCM_debug_PopStack "IGCM_sys_CountJobInQueue"
}

#D-#==================================================
#D-function IGCM_sys_ListJobInQueue
#D-* Purpose: Produce a list of users computing jobs (excluding post-processing)
#D-* Examples: IGCM_sys_ListJobInQueue ${User} JobNameList
#D-
function IGCM_sys_ListJobInQueue {
  IGCM_debug_PushStack "IGCM_sys_ListJobInQueue"
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_ListJobInQueue"
  fi

  # With -f option, the full job name is given in the last column
  set -A JobList $( ccc_mstat -f | gawk -v User=$1             \
                                        '( $2  == User      && \
                                           $NF != /TS/      && \
                                           $NF !~ /PACK/    && \
                                           $NF !~ /REBUILD/ && \
                                           $NF !~ /pack/ )     \
                                         { print $NF }' | sed -e "s/\(.*\)\.[0-9]*/\1/" )

  eval set -A ${2} ${JobList[*]}

  IGCM_debug_PopStack "IGCM_sys_ListJobInQueue"
}

#D-#==================================================
#D-function IGCM_sys_atlas
#D-* Purpose: encapsulate atlas call so as to manage error code and curie specificity
#D-* Examples:
#D-
function IGCM_sys_atlas {
  IGCM_debug_PushStack "IGCM_sys_atlas" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_atlas :" $@
  fi

  typeset status

  \ccc_mprun atlas $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
  status=$?
  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_atlas : error code ${status}"
    cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    IGCM_debug_PopStack "IGCM_sys_atlas"
    return 1
  else
    IGCM_debug_PopStack "IGCM_sys_atlas"
    return 0
  fi

  IGCM_debug_PopStack "IGCM_sys_atlas"
}

