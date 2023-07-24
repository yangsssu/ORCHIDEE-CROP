#!/bin/ksh

#**************************************************************
# Author: Sébastien Denvil
# Contact: Sebastien.Denvil__at__ipsl.jussieu.fr
# $Revision:: 1244                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2015-10-09 16:04:04 +0200 (Fri, 09 Oct 2015) $ Date of last commit
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
#D-LibIGCM_sys for IITM IBM machine
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
# Host and user names
# $hostname ou hostname
typeset HOST=${HOST:=$( hostname )}
# $username ou whoami
typeset LOGIN=${LOGIN:=$( whoami )}
# $hostname of the MASTER job
typeset MASTER=iitm01
# project name
typeset PROJECT=NONE
# jobWarningDelay in seconds
typeset jobWarningDelay=NONE

#D-
#D-#==================================================
#D-Program used in libIGCM
#D-#==================================================

# Submit command
typeset SUBMIT=${SUBMIT:=/usr/lpp/LoadL/full/bin/llsubmit}
# rsync with path
typeset -r RSYNC=/usr/bin/rsync
# RSYNC_opt args to rsync
typeset -r RSYNC_opt="-va"
# ie storage filesystem
typeset -r STOREHOST=iitm01
typeset -r REMOTE_RSYNC=/usr/bin/rsync

#====================================================
# Set environment tools (ferret, nco, cdo, rebuild, ...)
#====================================================
# Not applicable here. Only rebuild path
export export PATH=${PATH}:/gpfs1/home/sebastien/REBUILD/

#====================================================
# Host specific DIRECTORIES
#====================================================

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
typeset SUBMIT_DIR=${SUBMIT_DIR:=${LOADL_STEP_INITDIR}}

#====================================================
#- IN
typeset -r R_IN=${R_IN:=/gpfs1/home/sabin}
typeset -r R_IN_ECMWF=${R_IN_ECMWF:=/gpfs1/home/sabin}

#====================================================
#- RUN_DIR_PATH : Temporary working directory (=> TMP)
typeset -r RUN_DIR_PATH=${RUN_DIR_PATH:=$( echo ${HOME} | sed -e "s/gpfs1/gpfs3/" )/RUN/${LOADL_STEP_ID}}

#====================================================
#- OUTCOMMAND_PATH : tmp place to store command lines standard error and outputs
typeset -r OUTCOMMAND_PATH=/tmp

#====================================================
#- HOST_MPIRUN_COMMAND
typeset -r HOST_MPIRUN_COMMAND=${HOST_MPIRUN_COMMAND:="/usr/bin/time poe"}

#====================================================
#- Max number of arguments passed to nco operator or demigration command
UNIX_MAX_LIMIT=360

#====================================================
#- set PackDefault to false on iitm
PackDefault=false

#====================================================
#- Number of core per node (max number of OpenMP task)
NUM_COREPERNODE=2

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
#D-* Define R_BUF   : Output tree located on STORAGE hosting files waiting for rebuild or pack processes
#D-* if SpaceName=TEST nothing special will hapen
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
    ARCHIVE=$( echo ${HOME} | sed -e "s/gpfs1/gpfs3/" )
  fi

  if [ ! X${config_UserChoices_STORAGE} = X ]; then
    #====================================================
    #- STORAGE (dedicated to small/medium files)
    STORAGE=${config_UserChoices_STORAGE}
  else
    #====================================================
    #- STORAGE (dedicated to small/medium files)
    STORAGE=${ARCHIVE}
  fi

  # ON OBELIX NO SPECIAL CASE WHEN X${config_UserChoices_SpaceName} = XTEST

  #====================================================
  #- R_OUT
  R_OUT=${ARCHIVE}/IGCM_OUT

  #====================================================
  #- R_FIG (hosting figures : monitoring and atlas, and/or small files)
  R_FIG=${WORKDIR}/IGCM_OUT

  #====================================================
  #- R_BUF (ONLY FOR double copy an scratch)
  R_BUF=${WORKDIR}/IGCM_OUT

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
  ExistFlag=$( IGCM_sys_RshArchive "[ -d $1 ] && echo 0 || echo 1" )
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
  IsArchivedFlag=$( [ "X$( echo $@ | grep \/gpfs3 )" != "X" ] && echo 0 || echo 1 )
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
  ExistFlag=$( IGCM_sys_RshArchive "[ -f $1 ] && echo 0 || echo 1" )
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
  #Command depends on targeted file system
  DEBUG_sys=false IGCM_sys_IsFileArchived $1
  if [ $? = 0 ] ; then
    IGCM_sys_RshArchive "ls ${@} 2>/dev/null | wc -l"
    status=$?
  else
    ls ${@} 2>/dev/null | wc -l
    status=$?
  fi
  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_CountFileArchive : erreur."
  fi
  IGCM_debug_PopStack "IGCM_sys_CountFileArchive"
}

#D-#==================================================
#D-function IGCM_sys_Qsub
#D-* Purpose: Qsub new job
#D-* Examples:
#D-
function IGCM_sys_Qsub {
  IGCM_debug_PushStack "IGCM_sys_Qsub" $@

  typeset status

  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Qsub :" $@
  fi
  # We have to change output/error file
  [ ${#@} = 1 ] &&  REP_FOR_JOB=${SUBMIT_DIR}
  [ ${#@} = 2 ] &&  REP_FOR_JOB=${2}
  sed -e "s:\# \@ output *= .*:\# \@ output = ${Script_Output}:" \
    -e "s:\# \@ error *= .*:\# \@ error = ${Script_Output}:"   \
    $1 > ${REP_FOR_JOB}/JOB_FOR_IGCM
  cd $REP_FOR_JOB ; /usr/lpp/LoadL/full/bin/llsubmit JOB_FOR_IGCM > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1 ; status=$? ; cd - ;

  cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
  if [ ${status} -gt 0 ] ; then
    IGCM_debug_Print 2 "IGCM_sys_Qsub $1 : error code ${status}"
    IGCM_debug_Exit "IGCM_sys_Qsub"
  else
    JobID=$( gawk {'print $4'} ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ | tail -1 | sed -e s/\"//g )
    IGCM_sys_Rm ${REP_FOR_JOB}/JOB_FOR_IGCM
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

  typeset status

  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_QsubPost :" $@
  fi
  # We have to change output/error file
  [ ${#@} = 1 ] &&  REP_FOR_JOB=${POST_DIR}
  [ ${#@} = 2 ] &&  REP_FOR_JOB=${2}

  sed -e "s:\# \@ output *= .*:\# \@ output = ${Script_Post_Output}.out:" \
    -e "s:\# \@ error *= .*:\# \@ error = ${Script_Post_Output}.out:"   \
    ${libIGCM_POST}/$1.job > ${REP_FOR_JOB}/JOB_FOR_IGCM

  cd $REP_FOR_JOB ; /usr/lpp/LoadL/full/bin/llsubmit JOB_FOR_IGCM > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1 ; status=$? ; cd - ;

  cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
  if [ ${status} -gt 0 ] ; then
    IGCM_debug_Print 2 "IGCM_sys_QsubPost $1 : error code ${status}"
    IGCM_debug_Exit "IGCM_sys_QsubPost"
  else
    JobID=$( gawk {'print $4'} ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ | tail -1 | sed -e s/\"//g )
    IGCM_sys_Rm ${REP_FOR_JOB}/JOB_FOR_IGCM
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
    if [ ! -d ${1} ] ; then
      echo "WARNING : IGCM_sys_Get_Dir ${1} DOES NOT EXIST ."
      IGCM_debug_PopStack "IGCM_sys_Get_Dir"
      return
    fi

    typeset status

    #USUAL WAY
    \cp -ur $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?

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
    if [ X${JobType} = XRUN ] ; then
      IGCM_sys_Chmod 444 ${1}
    fi
    #
    # Only if we use rsync
    #IGCM_sys_MkdirArchive $( dirname $2 )
    #
    #USUAL WAY
    \cp $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?

    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_Put_Rest : cp failed error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Exit "IGCM_sys_Put_Rest"
    else
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

  typeset status

  if [ $DRYRUN = 0 ]; then
    if [ ! -f ${1} ] ; then
      echo "WARNING : IGCM_sys_Put_Out ${1} DOES NOT EXIST ."
      IGCM_debug_PopStack "IGCM_sys_Put_Out"
      return 1
    fi
    #
    IGCM_sys_MkdirArchive $( dirname $2 )
    #
    if [ X${JobType} = XRUN ] ; then
      if [ X${3} = X ] ; then
        IGCM_sys_Chmod 444 ${1}
      fi
    fi
    #
    #USUAL WAY
    \cp $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?

    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_Put_Out : cp failed error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Exit "IGCM_sys_Put_Out"
    else
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

  typeset DEST status dm_liste

  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Get :" $@
  fi
  if [ $DRYRUN -le 2 ]; then
    if [ X${1} = X'/l' ] ; then
      eval set +A dm_liste \${${2}}
    else
      dm_liste=${1}
    fi
    eval DEST=\${${#}}

    # test if the (first) file is present in the old computation :
    DEBUG_sys=false IGCM_sys_IsFileArchived ${dm_liste[0]}
    if [ $? = 0 ] ; then
      IGCM_sys_TestFileArchive ${dm_liste[0]}
      status=$?
    else
      IGCM_sys_TestFileBuffer ${dm_liste[0]}
      status=$?
    fi

    if [ ${status} -gt 0 ] ; then
      echo "IGCM_sys_Get, ERROR : regular file ${dm_liste[0]} DOES NOT EXIST ."
      IGCM_debug_Exit "IGCM_sys_Get"
      return
    fi

    #USUAL WAY
    \cp ${dm_liste[*]} ${DEST} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?

    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_Get : cp failed error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Exit "IGCM_sys_Get"
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    fi
  fi
  IGCM_debug_PopStack "IGCM_sys_Get"
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
  return 0
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
  return 0
}

#D-#==================================================
#D-function IGCM_sys_Put_Dods
#D-* Purpose: Put ${ARCHIVE} files on DODS internet protocole. Dummy function here
#D-* Examples:
#D-
function IGCM_sys_Put_Dods {
  IGCM_debug_PushStack "IGCM_sys_Put_Dods" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Put_Dods :" $@
  fi
  IGCM_debug_PopStack "IGCM_sys_Put_Dods"
}

##############################################################
# REBUILD OPERATOR

#D-#==================================================
#D-function IGCM_sys_sync
#D-* Purpose: flush buffer on disk (dummy function on Ada)
#D-* Examples:
#D-
function IGCM_sys_sync {
  IGCM_debug_PushStack "IGCM_sys_sync" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_sync :" $@
    echo "Dummy call, let the system do that."
  fi
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
      ${file} > ${file}.tmp

  elif [ ${executionType} -eq 2 ] ; then
    # MPMD + MPI + OMP
    sed -e "s/::openMPthreads::/${openMPthreads}/" \
        -e "s/::JobNumProcTot::/${coreNumber}/"    \
      ${file} > ${file}.tmp

  elif [ ${executionType} -eq 3 ] ; then
    # SPMD + MPI/OMP
    sed -e "s/::openMPthreads::/${openMPthreads}/" \
        -e "s/::JobNumProcTot::/${mpiTasks}/"      \
      ${file} > ${file}.tmp

  elif [ ${executionType} -eq 4 ] ; then
    # SPMD + MPI only
    sed -e "s/::JobNumProcTot::/${mpiTasks}/"      \
        -e "/::openMPthreads::/d"                  \
      ${file} > ${file}.tmp

  elif [ ${executionType} -eq 5 ] ; then
    # SPMD + OMP only
    sed -e "s/::openMPthreads::/${openMPthreads}/" \
        -e "/::JobNumProcTot::/d"                  \
      ${file} > ${file}.tmp

  elif [ ${executionType} -eq 6 ] ; then
    # SEQUENTIAL THEN
    sed -e "s/::JobNumProcTot::/1/"                \
        -e "/::openMPthreads::/d"                  \
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

  typeset NbNodes_Job NbProc_Job comp_proc_mpi_loc comp_proc_omp_loc mpi_count

  if [ ! -f ${SUBMIT_DIR}/Job_${config_UserChoices_JobName} ]  ; then
    IGCM_debug_Exit "IGCM_sys_iitm build_execution_scripts : Job_${config_UserChoices_JobName} don't exist in SUBMIT_DIR : ${SUBMIT_DIR} "
  fi

  if ( ${OK_PARA_MPMD} ) ; then

    if [ -f run_file ] ; then
      IGCM_sys_Rm -f run_file
    fi
    touch run_file

    if ( ${OK_PARA_OMP} ) ; then
# NEW : 2 Noeuds
# @ task_geometry={(0)(1,2,3)}
# Nombre de processus demandes
      echo "Job_${config_UserChoices_JobName} includes task_geometry = \c"
      cat ${SUBMIT_DIR}/Job_${config_UserChoices_JobName} | grep "task_geometry" | sed -e "s/[^(]*([^(]*/(/g"
      echo "Job_${config_UserChoices_JobName} includes task_geometry  with NbNodes = \c"
      cat ${SUBMIT_DIR}/Job_${config_UserChoices_JobName} | grep "task_geometry" | sed -e "s/[^(]*([^(]*/(/g" | wc -c
      NbNodes_Job=$(( $( cat ${SUBMIT_DIR}/Job_${config_UserChoices_JobName} | grep "task_geometry" | sed -e "s/[^(]*([^(]*/(/g" | wc -c ) - 1 ))

      if [ ${NbNodes_Job} -eq 0 ] ; then
        IGCM_debug_Print 2 "Error in Job_${config_UserChoices_JobName} ressources : no task_geometry defined with OpenMP run."
        exit 1
      fi
    else

# OLD :
# @ total_tasks = 3
# @ environment = "BATCH_NUM_PROC_TOT=3"
# Pas d OpenMP
# @ resources = ConsumableCpus(1)

      echo "Job_${config_UserChoices_JobName} includes total_tasks = \c"
      cat ${SUBMIT_DIR}/Job_${config_UserChoices_JobName} | grep "total_tasks" | sed -e "s/.*total_tasks = //"
      NbProc_Job=$( cat ${SUBMIT_DIR}/Job_${config_UserChoices_JobName} | grep "total_tasks" | sed -e "s/.*total_tasks = //" )
      NbProc_Job=${NbProc_Job:=0}
      if [ ${NbProc_Job} -eq 0 ] ; then
        IGCM_debug_Print 2 "Error in Job_${config_UserChoices_JobName} ressources : no total_tasks defined with MPI only run."
        exit 1
      fi
      if ( $( egrep '^# *@ *resources *= *ConsumableCpus\(1\)' ${SUBMIT_DIR}/Job_${config_UserChoices_JobName} >/dev/null 2>&1 ) ) ; then
        IGCM_debug_Print 2 "ressources =  ConsumableCpus(1) line found into Job_${config_UserChoices_JobName}"
      else
        IGCM_debug_Print 2 "Error in Job_${config_UserChoices_JobName} ressources = line not found. Job should include resources = ConsumableCpus(1) "
        exit 1
      fi
    fi

# run_file construction

# Then first loop on the components for the coupler ie oasis

### the coupler ie oasis must be the first one
    for comp in ${config_ListOfComponents[*]} ; do

      eval ExeNameIn=\${config_Executable_${comp}[0]}
      eval ExeNameOut=\${config_Executable_${comp}[1]}

      # for CPL component only
      if [ "X${comp}" = "XCPL" ] ; then

        eval comp_proc_mpi_loc=\${${comp}_PROC_MPI}
        eval comp_proc_omp_loc=\${${comp}_PROC_OMP}

        if ( ${OK_PARA_MPI} ) ; then
          (( mpi_count = 1 ))
          until [ ${mpi_count} -gt ${comp_proc_mpi_loc} ] ; do
            if ( ${OK_PARA_OMP} ) ; then
              echo "env OMP_NUM_THREADS=${comp_proc_omp_loc} ./${ExeNameOut} " >> run_file
            else
              echo "./${ExeNameOut}" >> run_file
            fi
            (( mpi_count = mpi_count + 1 ))
          done
        else
          if ( ${OK_PARA_OMP} ) ; then
            echo "env OMP_NUM_THREADS=${comp_proc_omp_loc} ./${ExeNameOut} " >> run_file
          else
            echo "./${ExeNameOut} " >> run_file
          fi
        fi
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

        if ( ${OK_PARA_MPI} ) ; then
          (( mpi_count = 1 ))
          until [ ${mpi_count} -gt ${comp_proc_mpi_loc} ] ; do
            if ( ${OK_PARA_OMP} ) ; then
              echo "env OMP_NUM_THREADS=${comp_proc_omp_loc} ./${ExeNameOut} " >> run_file
            else
              echo "./${ExeNameOut}" >> run_file
            fi
            (( mpi_count = mpi_count + 1 ))
          done
        else
          if ( ${OK_PARA_OMP} ) ; then
            echo "env OMP_NUM_THREADS=${comp_proc_omp_loc} ./${ExeNameOut} " >> run_file
          else
                    # to be tested : no MPI only OpenMP into MPMD mode
            echo "./${ExeNameOut} " >> run_file
          fi
        fi
      fi
    done

    EXECUTION="${HOST_MPIRUN_COMMAND} -pgmmodel mpmd -cmdfile ./run_file"

    IGCM_sys_Chmod u+x run_file
    if ( $DEBUG_sys ) ; then
      echo "run_file contains : "
      cat run_file
    fi

  # Only one executable (SPMD mode).
  else

    for comp in ${config_ListOfComponents[*]} ; do

      # Only if we really have an executable for the component :
      eval ExeNameOut=\${config_Executable_${comp}[1]}
      if ( [ "X${ExeNameOut}" != X\"\" ] && [ "X${ExeNameOut}" != "Xinca.dat" ] ) ; then
        EXECUTION="${HOST_MPIRUN_COMMAND} ./${ExeNameOut}"
      fi

    done

  fi

  IGCM_debug_Print 1 "sys iitm : execution command is"
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

  if ( [ X${RUN_DIR_PATH} = X${HOME} ] || [ X${RUN_DIR_PATH} = X${WORKDIR} ] || [ X${RUN_DIR_PATH} = X${ARCHIVE} ] ) ; then
    IGCM_debug_Print 1 "Variable RUN_DIR_PATH is pointing to an important directory : ${RUN_DIR_PATH}"
    IGCM_debug_Print 1 "Please check the RUN_DIR_PATH definition in your Job : Job_${config_UserChoices_JobName}"
    IGCM_debug_Exit "This will stop the job"
  fi
  IGCM_debug_PopStack "IGCM_sys_check_path"
}

#D-#==================================================
#D-function IGCM_sys_check_quota. Dummy call here
#D-* Purpose: check user quota. Stop the simulation if quota above 90%
#D-* Examples:
#D-
function IGCM_sys_check_quota {
  IGCM_debug_PushStack "IGCM_sys_check_quota"
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_check_quota"
  fi
  IGCM_debug_PopStack "IGCM_sys_check_quota"
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

  # Print only the full (-W) JobName (%jn)
  NbRun=$( llq -W -f %jn | grep -c "$1" )

  eval ${2}=${NbRun}

  IGCM_debug_PopStack "IGCM_sys_CountJobInQueue"
}
