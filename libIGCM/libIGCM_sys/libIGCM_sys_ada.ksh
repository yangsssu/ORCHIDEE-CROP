#!/bin/ksh

#**************************************************************
# Author: Sébastien Denvil
# Contact: Sebastien.Denvil__at__ipsl.jussieu.fr
# $Revision:: 1313                                     $ Revision of last commit
# $Author:: aclsce                                     $ Author of last commit
# $Date:: 2016-04-01 17:47:04 +0200 (Fri, 01 Apr 2016) $ Date of last commit
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
#D-LibIGCM_sys for Ada
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
typeset MASTER=ada
# project name
typeset PROJECT=$(echo ${LOADL_STEP_GROUP:=NONE})
# jobWarningDelay in seconds
typeset jobWarningDelay=$( TZ=utc date -d '1970-01-01 '${wall_clock_limit} +%s )

#D-
#D-#==================================================
#D-Program used in libIGCM
#D-#==================================================

# Submit command
typeset SUBMIT=${SUBMIT:=llsubmit}
# rsync with path
typeset -r RSYNC=/usr/bin/rsync
# RSYNC_opt args to rsync
typeset -r RSYNC_opt="-va"
# ie storage filesystem
typeset -r STOREHOST=ergon
typeset -r REMOTE_RSYNC=/usr/bin/rsync

#====================================================
# Set environment tools (ferret, nco, cdo, rebuild, ...)
#====================================================
source /smplocal/pub/Modules/default/init/ksh
if ( [ "X${LOADL_STEP_TYPE}" = "XPARALLEL" ] || [ "X${LOADL_STEP_TYPE}" = "XSERIAL" ] ) ; then
  . /smphome/rech/psl/rpsl035/.atlas_env_ada_intel_2013_0_bash > /dev/null 2>&1
else
  module load intel/2016.2 > /dev/null 2>&1
  . /smphome/rech/psl/rpsl035/.atlas_env_ada_bash > /dev/null 2>&1
  PCMDI_MP=/workgpfs/rech/psl/rpsl035/PCMDI-MP
  export UVCDAT_ANONYMOUS_LOG=no
fi
[ ! X${TaskType} = Xchecking ] && IGCM_debug_Print 1 "List of loaded modules:"
[ ! X${TaskType} = Xchecking ] && module list

export PATH=${PATH}:/smphome/rech/psl/rpsl035/AddNoise/src_X64_ADA/bin
export PATH=${PATH}:/smphome/rech/psl/rpsl035/AddPerturbation/src_X64_ADA/bin
export PATH=${PATH}:/smphome/rech/psl/rpsl035/bin/

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
typeset -x SUBMIT_DIR=${SUBMIT_DIR:=${LOADL_STEP_INITDIR}}

#====================================================
#- IN
typeset -r R_IN=${R_IN:=/workgpfs/rech/psl/rpsl035/IGCM}
typeset -r R_IN_ECMWF=${R_IN_ECMWF:=/u/rech/psl/rpsl376}

#====================================================
#- RUN_DIR_PATH : Temporary working directory (=> TMP)
typeset -r RUN_DIR_PATH=${RUN_DIR_PATH:=${TMPDIR}}

#====================================================
#- OUTCOMMAND_PATH : tmp place to store command lines standard error and outputs
typeset -r OUTCOMMAND_PATH=${RUN_DIR_PATH:-/tmp}

#====================================================
#- HOST_MPIRUN_COMMAND
if ( [ "X${LOADL_STEP_TYPE}" = "XPARALLEL" ] || [ "X${LOADL_STEP_TYPE}" = "XSERIAL" ] ) ; then
  typeset -r HOST_MPIRUN_COMMAND=${HOST_MPIRUN_COMMAND:="/usr/bin/time poe"}
else
  typeset -r HOST_MPIRUN_COMMAND=${HOST_MPIRUN_COMMAND:="/usr/bin/time mpirun"}
fi
#====================================================
#- Max number of arguments passed to nco operator or demigration command
UNIX_MAX_LIMIT=360

#====================================================
#- set PackDefault to true on ada
PackDefault=true

#====================================================
#- Number of cores per node
core_per_node=32

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
#D-* if SpaceName=TEST everything is stored on WORKDIR
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
    ARCHIVE=$(echo ${HOME} | sed -e "s,/.*home/,/arch/home/,")
  fi

  if [ ! X${config_UserChoices_STORAGE} = X ]; then
    #====================================================
    #- STORAGE (dedicated to small/medium files)
    STORAGE=${config_UserChoices_STORAGE}
  else
    #====================================================
    #- STORAGE (dedicated to small/medium files)
    STORAGE=${WORKDIR}
  fi

  if [ X${config_UserChoices_SpaceName} = XTEST ]; then
    #====================================================
    #- R_OUT
    R_OUT=${WORKDIR}/IGCM_OUT

    #====================================================
    #- R_FIG (hosting figures : monitoring and atlas, and/or small files)
    R_FIG=${WORKDIR}/IGCM_OUT

    IGCM_debug_Print 1 "SpaceName=TEST ==> OVERRULE destination path directories"

  else
    #====================================================
    #- R_OUT
    R_OUT=${ARCHIVE}/IGCM_OUT

    #====================================================
    #- R_FIG (hosting figures : monitoring and atlas, and/or small files)
    R_FIG=${ARCHIVE}/IGCM_OUT
  fi

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
  DEBUG_sys=false IGCM_sys_IsFileArchived "$@"
  if [ $? = 0 ] ; then
    rsh ${STOREHOST} exec /bin/ksh <<-EOF
    ${@}
EOF
    status=$?
  else
    /bin/ksh <<-EOF
    ${@}
EOF
    status=$?
  fi
  if [ ${status} -gt 0 ] ; then
    IGCM_debug_Print 2 "IGCM_sys_RshArchive : rsh or command failed error code ${status}"
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
  DEBUG_sys=false IGCM_sys_IsFileArchived "$@"
  if [ $? = 0 ] ; then
    rsh ${STOREHOST} exec /bin/ksh <<-EOF
    ${@} 2> /dev/null
EOF
  else
    /bin/ksh <<-EOF
    ${@} 2> /dev/null
EOF
  fi
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
  DEBUG_sys=false IGCM_sys_IsFileArchived $1
  if [ $? = 0 ] ; then
    rsh ${STOREHOST} -n mkdir -p $1
    status=$?
  else
    mkdir -p $1
    status=$?
  fi

  if [ ${status} -gt 0 ] ; then
    IGCM_debug_Print 2 "IGCM_sys_MkdirArchive : rsh or mkdir failed error code ${status}"
    IGCM_debug_Exit "IGCM_sys_MkdirArchive"
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
  #Command depends on targeted file system
  DEBUG_sys=false IGCM_sys_IsFileArchived $1
  if [ $? = 0 ] ; then
    ExistFlag=$( IGCM_sys_RshArchive "[ -d $1 ] && echo 0 || echo 1" )
  else
    ExistFlag=$( [ -d $1 ] && echo 0 || echo 1 )
  fi
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
  IsArchivedFlag=$( [ "X$( echo $@ | grep \/arch\/home )" != "X" ] && echo 0 || echo 1 )
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
#D-function IGCM_sys_Tree
#D-* Purpose: Tree directories with files on ${ARCHIVE}
#D-* Examples: IGCM_sys_Tree ${R_IN} ${R_OUT}
#D-
function IGCM_sys_Tree {
  IGCM_debug_PushStack "IGCM_sys_Tree" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Tree :" $@
  fi

  \mfls -R $@

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
  typeset status

  # We have to change output/error file
  [ ${#@} = 1 ] &&  REP_FOR_JOB=${SUBMIT_DIR}
  [ ${#@} = 2 ] &&  REP_FOR_JOB=${2}
  sed -e "s:\# \@ output *= .*:\# \@ output = ${Script_Output}:" \
    -e "s:\# \@ error *= .*:\# \@ error = ${Script_Output}:"   \
    $1 > ${REP_FOR_JOB}/JOB_FOR_IGCM
  cd $REP_FOR_JOB ; /usr/bin/llsubmit JOB_FOR_IGCM > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1 ; status=$? ; cd - ;

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
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_QsubPost :" $@
  fi
  typeset status

  # We have to change output/error file
  [ ${#@} = 1 ] &&  REP_FOR_JOB=${POST_DIR}
  [ ${#@} = 2 ] &&  REP_FOR_JOB=${2}

  sed -e "s:\# \@ output *= .*:\# \@ output = ${Script_Post_Output}.out:" \
    -e "s:\# \@ error *= .*:\# \@ error = ${Script_Post_Output}.out:"   \
    ${libIGCM_POST}/$1.job > ${REP_FOR_JOB}/JOB_FOR_IGCM_$$

  cd $REP_FOR_JOB ; /usr/bin/llsubmit JOB_FOR_IGCM_$$ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1 ; status=$? ; cd - ;

  cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
  if [ ${status} -gt 0 ] ; then
    IGCM_debug_Print 2 "IGCM_sys_QsubPost $1 : error code ${status}"
    IGCM_debug_Exit "IGCM_sys_QsubPost"
  else
    JobID=$( gawk {'print $4'} ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ | tail -1 | sed -e s/\"//g )
    IGCM_sys_Rm ${REP_FOR_JOB}/JOB_FOR_IGCM_$$
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
    IGCM_debug_Print 1 "Dummy call, let the scheduler do that."
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
    #Command depends on targeted file system
    DEBUG_sys=false IGCM_sys_IsFileArchived $2
    if [ $? = 0 ] ; then
      \rcp -r $1 ${STOREHOST}:$2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
      status=$?
    else
      \cp -r $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
      status=$?
    fi

    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_Put_Dir : rcp or cp failed error code ${status}"
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

    # Only if we use rsync
    #IGCM_sys_TestDirArchive $( dirname $2 )
    #
    # Command depends on targeted filesystem
    DEBUG_sys=false IGCM_sys_IsFileArchived $1
    if [ $? = 0 ] ; then
      # add dmget (to demigrate all offline files) to reduce time of this command :
      #IGCM_sys_RshArchive "dmfind $1 -state MIG -o -state OFL -o -state PAR | dmget -q -n"
      i=0
      while [ $i -lt $NB_ESSAI ] ; do
        \rcp -rp ${STOREHOST}:$1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
        status=$?
        if [ ${status} -gt 0 ]; then
          IGCM_debug_Print 2 "IGCM_sys_Get_Dir : rcp failed error code ${status} ${i}/${NB_ESSAI}"
          IGCM_debug_Print 2 "IGCM_sys_Get_Dir : sleep ${DELAI} seconds and try again."
          sleep $DELAI
        else
          break
        fi
        (( i = i + 1 ))
      done
    else
      \cp -rp $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
      status=$?
      if [ ${status} -gt 0 ] ; then
        IGCM_debug_Print 2 "IGCM_sys_Get_Dir : cp failed error code ${status}"
        cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
        IGCM_debug_Exit "IGCM_sys_Get_Dir"
      else
        \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      fi
    fi
    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_Get_Dir : (r)cp failed error code ${status}"
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
    #
    #Command depends on targeted file system
    DEBUG_sys=false IGCM_sys_IsFileArchived $2
    if [ $? = 0 ] ; then
      mfput $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
      status=$?
    else
      IGCM_sys_MkdirArchive $( dirname $2 )
      \cp $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
      status=$?
    fi

#       #RSYNC WITH NETWORK RSH CALL
#       IGCM_sys_MkdirArchive $( dirname $2 )
#       echo ${RSYNC} ${RSYNC_opt} --rsync-path=${REMOTE_RSYNC} -e rsh ${RUN_DIR}/$1 ${STOREHOST}:${2} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
#       ${RSYNC} ${RSYNC_opt} --rsync-path=${REMOTE_RSYNC} -e rsh ${RUN_DIR}/$1 ${STOREHOST}:${2} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1

#       #RSYNC WITH NFS USE
#       echo ${RSYNC} ${RSYNC_opt} ${RUN_DIR}/$1 ${2} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
#       ${RSYNC} ${RSYNC_opt} ${RUN_DIR}/$1 ${2} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1

#       status=$?
#       IGCM_sys_Rsync_out $status

#       ${libIGCM}/libIGCM_sys/IGCM_analyse_rsync_out.awk ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
#       (( status=status+$? ))

    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_Put_Rest : mfput or cp failed error code ${status}"
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
    if [ X${JobType} = XRUN ] ; then
      if [ X${3} = X ] ; then
        IGCM_sys_Chmod 444 ${1}
      fi
    fi
    #
    #
    #Command depends on targeted file system
    DEBUG_sys=false IGCM_sys_IsFileArchived $2
    if [ $? = 0 ] ; then
      mfput $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
      status=$?
    else
      IGCM_sys_MkdirArchive $( dirname $2 )
      \cp $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
      status=$?
    fi

#       #RSYNC WITH NETWORK RSH CALL
#       IGCM_sys_MkdirArchive $( dirname $2 )
#       echo ${RSYNC} ${RSYNC_opt} --rsync-path=${REMOTE_RSYNC} -e rsh ${RUN_DIR}/$1 ${STOREHOST}:${2} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
#       ${RSYNC} ${RSYNC_opt} --rsync-path=${REMOTE_RSYNC} -e rsh ${RUN_DIR}/$1 ${STOREHOST}:${2} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1

#       #RSYNC WITH NFS USE
#       echo ${RSYNC} ${RSYNC_opt} ${RUN_DIR}/$1 ${2} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
#       ${RSYNC} ${RSYNC_opt} ${RUN_DIR}/$1 ${2} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1

#       status=$?
#       IGCM_sys_Rsync_out $status

#       ${libIGCM}/libIGCM_sys/IGCM_analyse_rsync_out.awk ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
#       (( status=status+$? ))

    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_Put_Out : mfput or cp failed error code ${status}"
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

    #Command depends on targeted file system
    DEBUG_sys=false IGCM_sys_IsFileArchived ${dm_liste[0]}
    if [ $? = 0 ] ; then
      mfget ${dm_liste[*]} ${DEST} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
      status=$?
    else
      \cp ${dm_liste[*]} ${DEST} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
      status=$?
    fi

#       #RSYNC WITH NETWORK RSH CALL
#       echo ${RSYNC} ${RSYNC_opt} --rsync-path=${REMOTE_RSYNC} -e rsh ${STOREHOST}:"${dm_liste}" ${STOREHOST}:${RUN_DIR}/${DEST} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
#       ${RSYNC} ${RSYNC_opt} --rsync-path=${REMOTE_RSYNC} -e rsh ${STOREHOST}:"${dm_liste}" ${STOREHOST}:${RUN_DIR}/${DEST} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1

#       #RSYNC WITH NFS USE
#       echo ${RSYNC} ${RSYNC_opt} ${dm_liste} ${RUN_DIR}/${DEST} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
#       ${RSYNC} ${RSYNC_opt} ${dm_liste} ${RUN_DIR}/${DEST} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1

#       status=$?
#       IGCM_sys_Rsync_out $status

#       ${libIGCM}/libIGCM_sys/IGCM_analyse_rsync_out.awk ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
#       (( status=status+$? ))

    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_Get : mfget or cp failed error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Exit "IGCM_sys_Get"
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
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

  IGCM_sys_Cd /tmp
  # NASTY HACK. We force /tmp here. This function is called only from the front-end and interactively.
  RUN_DIR_PATH=/tmp IGCM_sys_Get ${1} .
  eval ${2}=$( cdo showyear $( basename ${1} ) 2> /dev/null | gawk '{ print $NF }' )
  # Need to erase this file to avoid collision (permission denied getting file) between users on the front-end
  IGCM_sys_Rm -f $( basename ${1} )
  IGCM_sys_Cd -

  IGCM_debug_PopStack "IGCM_sys_GetDate_Monitoring"
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
    # We take our time on that
    sleep 10
    IGCM_sys_TestDirArchive ${R_SAVE}/${1}
    if [ $? != 0 ] ; then
      echo "WARNING : IGCM_sys_Put_Dods ${R_SAVE}/${1} DOES NOT EXIST ."
      IGCM_debug_PopStack "IGCM_sys_Put_Dods"
      return
    fi

    rsh ${STOREHOST} exec /bin/ksh <<EOF
          cd ${R_SAVE}
          /arch/local/bin/dods_rm DODS/pub/${LOGIN}/${R_DODS}/${1} > /dev/null 2>&1
          /bin/chmod -R u+w ${R_SAVE}/${1}
          /arch/local/bin/dods_cp ${1} DODS/pub/${LOGIN}/${R_DODS} > /dev/null 2>&1
          /bin/chmod -R +rX ${R_SAVE}/${1}
          /bin/chmod -R u+w ${R_SAVE}/${1}
EOF
    status=$?

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

  ulimit -s unlimited
## to be done only one time
## export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/smplocal/pub/NetCDF/4.1.3/lib:/smplocal/pub/HDF5/1.8.9/seq/lib
##  echo ${LD_LIBRARY_PATH} | grep -i netcdf >/dev/null 2>&1 || export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/smplocal/pub/NetCDF/4.1.3/lib:/smplocal/pub/HDF5/1.8.9/seq/lib
  IGCM_debug_Print 1 "set LD_LIBRARY_PATH=${LD_LIBRARY_PATH}"

  export MP_STDOUTMODE=combined
  IGCM_debug_Print 1 "set MP_STDOUTMODE=${MP_STDOUTMODE}"

  ## fix to reduce memory usage. Required since 2014/22/04. On ada IDRIS.
  #export MP_EUILIBPATH=/smplocal/lib/ibmhpc/pe12012/ppe.pami/gnu/lib64/pami64
  #IGCM_debug_Print 1 "set MP_EUILIBPATH=${MP_EUILIBPATH}"

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
    if [ X${MPIEnvironment} = XIBM ] ; then
      sed -e "/::openMPthreads::/d"                  \
        -e "s/::JobNumProcTot::/${coreNumber}/"    \
        ${file} > ${file}.tmp
    else
      sed -e "/::openMPthreads::/d"                  \
        -e "s/@ job_type = parallel/@ job_type = mpich/" \
        -e "s/::JobNumProcTot::/${coreNumber}/"    \
        ${file} > ${file}.tmp
    fi

  elif [ ${executionType} -eq 2 ] ; then
    # MPMD + MPI + OMP
    if [ X${MPIEnvironment} = XIBM ] ; then
      sed -e "s/::openMPthreads::/${openMPthreads}/" \
        -e "s/::JobNumProcTot::/${mpiTasks}/"    \
        ${file} > ${file}.tmp
    else
      (( nodeNumber = coreNumber / core_per_node ))
      [ $(( ${coreNumber} % ${core_per_node} )) -ne 0 ] && (( nodeNumber = nodeNumber + 1 ))
      sed -e "/::openMPthreads::/d"                  \
        -e "s/@ job_type = parallel/@ job_type = mpich/" \
        -e "s/@ total_tasks = ::JobNumProcTot::/@ node = ${nodeNumber} /"    \
        -e "/@ as_limit = 3.5gb/d"      \
        -e "s/::JobNumProcTot::/${mpiTasks}/"      \
        ${file} > ${file}.tmp
    fi

  elif [ ${executionType} -eq 3 ] ; then
    # SPMD + MPI/OMP
    if [ X${MPIEnvironment} = XIBM ] ; then
      sed -e "s/::openMPthreads::/${openMPthreads}/" \
        -e "s/::JobNumProcTot::/${mpiTasks}/"      \
        ${file} > ${file}.tmp
    else
      sed -e "s/::openMPthreads::/${openMPthreads}/" \
        -e "s/@ job_type = parallel/@ job_type = mpich/" \
        -e "s/::JobNumProcTot::/${mpiTasks}/"      \
        ${file} > ${file}.tmp
    fi

  elif [ ${executionType} -eq 4 ] ; then
    # SPMD + MPI only
    if [ X${MPIEnvironment} = XIBM ] ; then
      sed -e "s/::JobNumProcTot::/${mpiTasks}/"      \
        -e "/::openMPthreads::/d"                  \
        ${file} > ${file}.tmp
    else
      sed -e "s/::JobNumProcTot::/${mpiTasks}/"      \
        -e "s/@ job_type = parallel/@ job_type = mpich/" \
        -e "/::openMPthreads::/d"                  \
        ${file} > ${file}.tmp
    fi

  elif [ ${executionType} -eq 5 ] ; then
    # SPMD + OMP only
    sed -e "s/::openMPthreads::/${openMPthreads}/" \
        -e "s/@ job_type = parallel/@ job_type = serial/" \
        -e "/::JobNumProcTot::/d"                  \
      ${file} > ${file}.tmp

  elif [ ${executionType} -eq 6 ] ; then
    # SEQUENTIAL THEN
    sed -e "s/::JobNumProcTot::/1/"                \
        -e "s/@ job_type = parallel/@ job_type = serial/" \
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

  EXECUTION=${HOST_MPIRUN_COMMAND}

  # MPMD mode
  if ( ${OK_PARA_MPMD} ) ; then

    # MPI IBM Environment
    if ( [ "X${LOADL_STEP_TYPE}" = "XPARALLEL" ] || [ "X${LOADL_STEP_TYPE}" = "XSERIAL" ] ) ; then
      IGCM_debug_Print 1 "You use IBM MPI environment"
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
        if [ "X${comp}" = "XCPL" ] ; then

          eval comp_proc_mpi_loc=\${${comp}_PROC_MPI}
          eval comp_proc_omp_loc=\${${comp}_PROC_OMP}

          if ( ${OK_PARA_MPI} ) ; then
            (( mpi_count = 1 ))
            until [ ${mpi_count} -gt ${comp_proc_mpi_loc} ] ; do
              echo "./${ExeNameOut}" >> run_file
              (( mpi_count = mpi_count + 1 ))
            done
          else
            echo "./${ExeNameOut} " >> run_file
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

          if ( ${OK_PARA_OMP} ) ; then
            # Check if the number of threads is correct
            case ${comp_proc_omp_loc} in
            1|2|4)
              IGCM_debug_Print 1 "You run ${ExeNameOut} on ${comp_proc_omp_loc} OMP threads on IBM MPI Environment"
              IGCM_debug_Print 1 "Beware : it may you use more CPU than needed."
              IGCM_debug_Print 1 "Think to the Intel MPI Environment to do what you want to do !"
              ;;
            8|16)
              IGCM_debug_Exit "ERROR with OMP parameters !"
              IGCM_debug_Print 2 "Beware : ${comp_proc_omp_loc} is too much for MPMD mode"
              IGCM_debug_Print 2 "You will use more CPU than needed : try to use Intel-MPI environment to do such a thing !"
              IGCM_debug_Verif_Exit
              ;;
            *)
              IGCM_debug_Exit "ERROR with OMP parameters !"
              IGCM_debug_Print 2 "${comp_proc_omp_loc} is not possible as number of OMP threads"
              IGCM_debug_Print 2 "Only 1,2,4,8,16 as number of OMP threads are possible "
              IGCM_debug_Verif_Exit
              ;;
            esac

          fi

          if ( ${OK_PARA_MPI} ) ; then
            (( mpi_count = 1 ))
            until [ ${mpi_count} -gt ${comp_proc_mpi_loc} ] ; do
              if ( ${OK_PARA_OMP} ) ; then
                echo "env OMP_NUM_THREADS=$comp_proc_omp_loc ./${ExeNameOut}" >> run_file
              else
                echo "./${ExeNameOut}" >> run_file
              fi
              (( mpi_count = mpi_count + 1 ))
            done
          else
            echo "./${ExeNameOut} " >> run_file
          fi
        fi
      done
      if ( ${OK_PARA_OMP} ) ; then
        export KMP_STACKSIZE=200m
      fi

      EXECUTION="${HOST_MPIRUN_COMMAND} -pgmmodel mpmd -cmdfile ./run_file"

      IGCM_sys_Chmod u+x run_file
      if ( $DEBUG_sys ) ; then
        echo "run_file contains : "
        cat run_file
      fi


    # MPI Intel Environment
    else
      IGCM_debug_Print 1 "You use Intel MPI environment"

      # Only MPI (MPMD)
      if  ( ! ${OK_PARA_OMP} ) ; then
	init_exec=n
  
        # First loop on the components for the coupler ie oasis (only if oasis3)
        # the coupler ie oasis3 must be the first one
        for comp in ${config_ListOfComponents[*]} ; do

          eval ExeNameIn=\${config_Executable_${comp}[0]}
          eval ExeNameOut=\${config_Executable_${comp}[1]}

          # for CPL component only
          if [ "X${comp}" = "XCPL"  ]  && [ "X${ExeNameOut}" != X\"\" ] ; then

            eval comp_proc_mpi_loc=\${${comp}_PROC_MPI}
            eval comp_proc_omp_loc=\${${comp}_PROC_OMP}

            echo "#!/bin/ksh" > script_${ExeNameOut}.ksh
            echo ""  >> script_${ExeNameOut}.ksh
            echo "./${ExeNameOut}" >> script_${ExeNameOut}.ksh
            IGCM_sys_Chmod u+x script_${ExeNameOut}.ksh
            EXECUTION="${EXECUTION} -np ${comp_proc_mpi_loc} ./script_${ExeNameOut}.ksh"
            init_exec=y
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

            echo "#!/bin/ksh" > script_${ExeNameOut}.ksh
            echo ""  >> script_${ExeNameOut}.ksh
            echo "./${ExeNameOut}" >> script_${ExeNameOut}.ksh
            IGCM_sys_Chmod u+x script_${ExeNameOut}.ksh

            if [ ${init_exec} = y ] ; then
              EXECUTION="${EXECUTION} : -np ${comp_proc_mpi_loc} ./script_${ExeNameOut}.ksh"
            else
              EXECUTION="${EXECUTION} -np ${comp_proc_mpi_loc} ./script_${ExeNameOut}.ksh"
              init_exec=y
            fi
          fi
        done
      # MPI-OpenMP (MPMD)
      else

        # Execution specifications
        EXECUTION="${EXECUTION} -configfile run_file"
        export KMP_STACKSIZE=200m
        if [ -f run_file ] ; then
          IGCM_sys_Rm -f run_file
        fi
        touch run_file

        # Initialisation of variables
        string_final=""
        string_final_hexa=""
        current_core=0
        executable_nb=1

        #  Hosts treatment
        for nodes in `echo $LOADL_PROCESSOR_LIST`
        do
          host[$i]=$nodes
          i=$((i+1))
        done

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

            # Check if the number of threads is correct
            case ${comp_proc_omp_loc} in
            1|2|4|8|16)
              IGCM_debug_Print 1 "You run ${ExeNameOut} on ${comp_proc_omp_loc} OMP threads"
              ;;
            *)
              IGCM_debug_Exit "ERROR with OMP parameters !"
              IGCM_debug_Print 2 "${comp_proc_omp_loc} is not possible as number of OMP threads"
              IGCM_debug_Print 2 "Only 1,2,4,8,16 as number of OMP threads are possible "
              IGCM_debug_Verif_Exit
              ;;
            esac


            # Build run_file for Ada Intel-MPI environment : method used to assign cores and nodes for the MPI process by using hexadecimal mask
            # Example of run_file :
            #-host host0 -n 4 -env I_MPI_PIN_DOMAIN=[ff,ff00,ff0000,ff000000] ./a.out
            #-host host1 -n 4 -env I_MPI_PIN_DOMAIN=[ff,ff00,ff0000,ff000000] ./a.out
            #-host host2 -n 10 -env I_MPI_PIN_DOMAIN=[3,c,30,c0,300,c00,3000,c000,30000,c0000,100000,200000,400000,800000,1000000,2000000,4000000,8000000,10000000,20000000] ./b.out
            #-host host2 -n 10 ./c.out
            # Example of final command :
            # mpirun -configfile run_file

            rank=0
            # For one specific executable, loop on mpi process
            for nb_proc_mpi in `seq 0 $(($comp_proc_mpi_loc-1))`; do
              (( index_host = current_core / core_per_node ))
              host_value=${host[${index_host}]}
              (( slot =  current_core % core_per_node ))
              # loop on omp threads for each mpi process (set the appropriate bit to 1 and append it to previous one)
              for index in `seq $slot $(($slot+$comp_proc_omp_loc-1))`; do
                string_final="1"$string_final
              done
              # convert binary mask to hexadecimal mask
              if [ $rank -ne 0 ] ; then
                string_final_hexa=$string_final_hexa","$( printf '%x\n' "$((2#$string_final))" )
              else
                string_final_hexa=$( printf '%x\n' "$((2#$string_final))" )
              fi
              # replace bit 1 by bit 0 in order to append next one (next one wil be 1)
              string_final=$( echo $string_final | sed "s/1/0/g" )
              # mpi rank = mpi_rank + 1
              (( rank = rank + 1 ))
              # current core takes into account the number of omp threads which was previously append
              (( current_core = current_core + comp_proc_omp_loc ))
              # We write to the configuration file either we switch to the next node or we switch to the next executable
              if ( [ $(( current_core / core_per_node )) -ne $index_host ] || [ $nb_proc_mpi -eq $(($comp_proc_mpi_loc-1)) ] ) ; then
                # I_MPI_PIN_DOMAIN variable must be given once per node
                if [ $executable_nb -eq 1 ] ; then
                  echo "-host $host_value -n $rank -env I_MPI_PIN_DOMAIN=[$string_final_hexa] ./$ExeNameOut" >> run_file
                else
                  sed -i "/$host_value/s/\]/\,$string_final_hexa\]/g" run_file
                  echo "-host $host_value -n $rank ./$ExeNameOut" >> run_file
                fi
                # +1 for the number of executbale on the same node
                if [ $nb_proc_mpi -eq $(($comp_proc_mpi_loc-1)) ] ; then
                  (( executable_nb = executable_nb + 1 ))
                fi
                # Some initializations if we switch to the next node
                if [ $(( current_core / core_per_node )) -ne $index_host ] ; then
                  string_final=""
                  string_final_hexa=""
                  rank=0
                  executable_nb=1
                fi
              fi
            done

          fi
        done
        IGCM_sys_Chmod u+x run_file
        if ( $DEBUG_sys ) ; then
          echo "run_file contains : "
          cat run_file
        fi
      fi
    fi

  # Only one executable (SPMD mode).
  else

    for comp in ${config_ListOfComponents[*]} ; do

      # Only if we really have an executable for the component :
      eval ExeNameOut=\${config_Executable_${comp}[1]}
      if ( [ "X${ExeNameOut}" != X\"\" ] && [ "X${ExeNameOut}" != "Xinca.dat" ] ) ; then

        if ( ${OK_PARA_OMP} ) ; then
          eval comp_proc_omp_loc=\${${comp}_PROC_OMP}
          export KMP_STACKSIZE=200m
          #export KMP_LIBRARY=turnaround
          #echo "export KMP_STACKSIZE=3g"  >> script_${ExeNameOut}.ksh
          #echo "export KMP_LIBRARY=turnaround"  >> script_${ExeNameOut}.ksh
          #echo "export MKL_SERIAL=YES"  >> script_${ExeNameOut}.ksh
        fi
        if  ( ${OK_PARA_MPI} ) ; then
          if ( [ "X${LOADL_STEP_TYPE}" = "XPARALLEL" ] || [ "X${LOADL_STEP_TYPE}" = "XSERIAL" ] ) ; then
            EXECUTION="${HOST_MPIRUN_COMMAND} ./${ExeNameOut}"
          else
            eval comp_proc_mpi_loc=\${${comp}_PROC_MPI}
            EXECUTION="${HOST_MPIRUN_COMMAND} -np ${comp_proc_mpi_loc} ./${ExeNameOut}"
          fi
        else
          EXECUTION="/usr/bin/time ./${ExeNameOut}"
        fi
      fi

    done

  fi

  IGCM_debug_Print 1 "sys ada : execution command is "
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
  volume_quota=$(quota_u -w 2>/dev/null| grep 'Quota soft' | gawk '{print $5}')
  if [ ! X${volume_quota} = X ] ; then
    quota_volume=${volume_quota%%\%}
#    echo $quota_volume
    if [ $(echo "${quota_volume} > ${limit_quota}" | bc) -eq 1 ] ; then
      IGCM_debug_Print 1 "Please, check your quota of volume on workgpfs"
      IGCM_debug_Print 1 "${quota_volume}% of your quota is used"
      IGCM_debug_Print 1 "Use the quota_u -w command to check"
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

  cpt > $1

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

  eval ${1}=$( echo $LOADL_STEP_ID | awk -F. '{print $4}' )

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

  # Print only the full (-W) JobID (%id) and JobName (%jn)
  ID=$( llq -u $2 -W -f %id %jn | \
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

  # Print only the full (-W) JobName (%jn)
  NbRun=$( llq -W -f %jn | grep -c "$1" )

  eval ${2}=${NbRun}

  IGCM_debug_PopStack "IGCM_sys_CountJobInQueue"
}

#D-#==================================================
#D-function IGCM_sys_ListJobInQueue
#D-* Purpose: Produce a list of users jobs
#D-* Examples: IGCM_sys_ListJobInQueue ${User} JobNameList
#D-
function IGCM_sys_ListJobInQueue {
  IGCM_debug_PushStack "IGCM_sys_ListJobInQueue"
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_ListJobInQueue"
  fi

  # With -W option, column width is as large as necessary
  set -A JobList $( llq -u $1 -W -f %jn | head -n -2 | tail -n +3 | \
    gawk '( $1 != /TS/      && \
                            $1 !~ /PACK/    && \
                            $1 !~ /REBUILD/ && \
                            $1 !~ /pack/ )     \
                          { print $1 }' | sed -e "s/\(.*\)\.[0-9]*/\1/" )

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

  \atlas $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
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

