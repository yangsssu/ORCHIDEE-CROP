#!/bin/ksh

#**************************************************************
# Author: Martial Mancip
# Contact: Martial.Mancip__at__ipsl.jussieu.fr
# $Revision:: 1314                                     $ Revision of last commit
# $Author:: jgipsl                                     $ Author of last commit
# $Date:: 2016-04-05 16:05:18 +0200 (Tue, 05 Apr 2016) $ Date of last commit
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************

# bypass specific internationalization (for gawk)
export LC_ALL="C"

# By default, all libIGCM_sys save functions will protect output datas (RUN execution mode)
# other values : DEB(ug), DEV(elopment).
JobType=${JobType:=RUN}

#====================================================
# set PackDefault false by default
PackDefault=false

#====================================================
# set BigBrother false by default
BigBrother=${BigBrother:=false}

#====================================================
# set BigBrother channel (AMQP or MAIL)
# only MAIL working at present due to firewall constraint
BigBrotherChannel=MAIL

# no need to be so verbose in checking mode
if [ ! X${TaskType} = Xchecking ] ; then
  echo
  echo "===================================================="
  echo "Where do we run ?" $( hostname )
  uname -a
  echo "===================================================="
  echo
fi

if [ X${JobType} = XDEB ] ; then
    echo "DEBUG mode : activation of 'set -vx' mode."
    set -vx
    DEBUG_debug=true
    DEBUG_sys=true
fi

#====================================================
case $( hostname -s ) in
    ada*)
        [ ! X${TaskType} = Xchecking ] && echo "sys source ada Intel X-64 lib."
        CENTER=IDRIS
        SYSTEM=ada
        . ${libIGCM}/libIGCM_sys/libIGCM_sys_ada.ksh;;
    curie*)
        [ ! X${TaskType} = Xchecking ] && echo "sys source curie Intel X-64 lib."
        CENTER=TGCC
        SYSTEM=curie
        . ${libIGCM}/libIGCM_sys/libIGCM_sys_curie.ksh;;
    airain*)
        [ ! X${TaskType} = Xchecking ] && echo "sys source airain Intel X-64 lib."
        CENTER=TGCC
        SYSTEM=curie
        . ${libIGCM}/libIGCM_sys/libIGCM_sys_curie.ksh;;
    asterix*|obelix*)
        [ ! X${TaskType} = Xchecking ] && echo "sys source obelix or asterix lib."
        CENTER=LSCE
        SYSTEM=lxiv8
        . ${libIGCM}/libIGCM_sys/libIGCM_sys_obelix.ksh;;
    ciclad*)
        [ ! X${TaskType} = Xchecking ] && echo "sys source ciclad lib for running at ciclad."
        CENTER=IPSL-ciclad
        SYSTEM=ifort_CICLAD
        . ${libIGCM}/libIGCM_sys/libIGCM_sys_ciclad.ksh;;
    camelot*|loholt1*|loholt2*|merlin*)
        [ ! X${TaskType} = Xchecking ] && echo "sys source ciclad lib for running at climserv."
        CENTER=IPSL-climserv
        SYSTEM=ifort_CICLAD
        . ${libIGCM}/libIGCM_sys/libIGCM_sys_ciclad.ksh;;
    iitm*)
        [ ! X${TaskType} = Xchecking ] && echo "sys source iitm lib."
        CENTER=IITM
        SYSTEM=iitm
        . ${libIGCM}/libIGCM_sys/libIGCM_sys_iitm.ksh;;
    *)
        [ ! X${TaskType} = Xchecking ] && echo "sys source default lib."
        CENTER=DEFAULT
        SYSTEM=default
        . ${libIGCM}/libIGCM_sys/libIGCM_sys_default.ksh;;
esac

# Set default umask (umask is 0027 on some machines : CCRT machine at least)
umask 0022

#D--------------------------------------------------------------------==
#D-
#D-    Define IGCM_sys functions that are common on every systems
#D-
#D--------------------------------------------------------------------==

#D-#==================================================
#D-function IGCM_sys_RshMaster
#D-* Purpose: Connection to frontend machine.
#D-* Examples:
#D-
function IGCM_sys_RshMaster {
  IGCM_debug_PushStack "IGCM_sys_RshMaster" $@
  OUTCOMMAND_PATH=${OUTCOMMAND_PATH} /bin/ksh <<-EOF
    export libIGCM=${libIGCM}
    export DEBUG_debug=${DEBUG_debug}
    . ${libIGCM}/libIGCM_debug/libIGCM_debug.ksh
    . ${libIGCM}/libIGCM_card/libIGCM_card.ksh
    ${@}
EOF
  if [ $? -gt 0 ] ; then
    echo "IGCM_sys_RshMaster : erreur."
    IGCM_debug_Exit "IGCM_sys_RshMaster"
  fi
  IGCM_debug_PopStack "IGCM_sys_RshMaster"
}

#D-#==================================================
#D-function IGCM_sys_RshPost
#D-* Purpose: Post-process rsh command
#D-* Examples:
#D-
function IGCM_sys_RshPost {
  IGCM_debug_PushStack "IGCM_sys_RshPost" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_RshPost :" $@
  fi
  # keep standard input (stdin) for the loop onto temporary file
  cat >${OUTCOMMAND_PATH}/tmp_IGCM_sys_RshPost_$$_${LOGNAME}

  OUTCOMMAND_PATH=${OUTCOMMAND_PATH} /bin/ksh <${OUTCOMMAND_PATH}/tmp_IGCM_sys_RshPost_$$_${LOGNAME}
  if [ $? -gt 0 ] ; then
    echo "IGCM_sys_RshPost : erreur."
    IGCM_debug_Exit "IGCM_sys_RshPost"
  fi
  # delete temporary file
  \rm ${OUTCOMMAND_PATH}/tmp_IGCM_sys_RshPost_$$_${LOGNAME}

# ============ FRONTEND  END  ============ #

# ============ CESIUM START ============ #
#  typeset NB_ESSAI DELAI status i
#  if [ "X$( grep rebuild_from tmp_IGCM_sys_RshPost_$$ )" != "X" ] ; then
#    #little hack so that rebuild submission is done on titane not an cesium
#
#    libIGCM_POST_sed=$( echo $libIGCM_POST | sed 's/\//\\\//g' )
#    POST_DIR_sed=$( echo ${POST_DIR} | sed 's/\//\\\//g' )
#    sed "s/IGCM_sys_QsubPost/IGCM_sys_Qsub/g" tmp_IGCM_sys_RshPost_$$ > tmp.txt
#    sed "s/ rebuild_fromWorkdir/ ${libIGCM_POST_sed}\/rebuild_fromWorkdir.job/g" tmp.txt > tmp_IGCM_sys_RshPost_$$
#    sed "s/ rebuild_fromArchive/ ${libIGCM_POST_sed}\/rebuild_fromArchive.job/g" tmp_IGCM_sys_RshPost_$$ > tmp.txt
#    sed "s/Script_Post_Output=/Script_Output=${POST_DIR_sed}\//g" tmp.txt > tmp_IGCM_sys_RshPost_$$
#    \mv tmp.txt tmp_IGCM_sys_RshPost_$$
#
#    echo cat tmp_IGCM_sys_RshPost_$$ AFTER
#    cat tmp_IGCM_sys_RshPost_$$
#
#    /bin/ksh <tmp_IGCM_sys_RshPost_$$
#    if [ $? -gt 0 ] ; then
#      echo "IGCM_sys_RshPost : erreur."
#      IGCM_debug_Exit "IGCM_sys_RshPost"
#    fi
#    # delete temporary file
#    \rm tmp_IGCM_sys_RshPost_$$
#
#  else
#    # number of tentative
#    NB_ESSAI=10
#    # time delay between tentative
#    DELAI=10
#    i=0
#    while [ $i -ne $NB_ESSAI ] ; do
#      ssh -t titane996 ssh cesium /bin/ksh <tmp_IGCM_sys_RshPost_$$
#      status=$?
#      if [ ${status} -ne 0 ];
#      then
#        sleep $DELAI
#      else
#        break
#      fi
#      let i=$i+1
#    done
#    # delete temporary file
#    \rm tmp_IGCM_sys_RshPost_$$
#
#    if [ ${status} -gt 0 ] ; then
#      echo "IGCM_sys_RshPost : erreur."
#      IGCM_debug_Exit "IGCM_sys_RshPost"
#    fi
#  fi

# ============ CESIUM  END  ============ #

  IGCM_debug_PopStack "IGCM_sys_RshPost"
}

#D-#==================================================
#D-function IGCM_sys_SendMail
#D-* Purpose: Send mail when simulation is over
#D-* Examples:
#D-

#D-#==================================================
#D-function IGCM_sys_Mkdir
#D-* Purpose: Master locale mkdir command
#D-* Examples:
#D-
function IGCM_sys_Mkdir {
  IGCM_debug_PushStack "IGCM_sys_Mkdir" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Mkdir :" $@
  fi
  if [ ! -d ${1} ]; then
    \mkdir -p $1
    if [ $? -gt 0 ] ; then
      echo "IGCM_sys_Mkdir : erreur."
      IGCM_debug_Exit "IGCM_sys_Mkdir"
    fi
  fi
  # vérification :
  if [ ! -d ${1} ] ; then
    echo "IGCM_sys_Mkdir : erreur."
    IGCM_debug_Exit "IGCM_sys_Mkdir"
  fi
  IGCM_debug_PopStack "IGCM_sys_Mkdir"
}

#D-#==================================================
#D-function IGCM_sys_MkdirWork
#D-* Purpose: Mkdir on Work
#D-* Examples:
#D-
function IGCM_sys_MkdirWork {
  IGCM_debug_PushStack "IGCM_sys_MkdirWork" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_MkdirWork :" $@
  fi
  #- creation de repertoire sur le serveur fichier
  if [ ! -d ${1} ]; then
    \mkdir -p $1
    if [ $? -gt 0 ] ; then
      echo "IGCM_sys_MkdirWork : erreur."
      IGCM_debug_Exit "IGCM_sys_MkdirWork"
    fi
  fi
  IGCM_debug_PopStack "IGCM_sys_MkdirWork"
}

#D-#==================================================
#D-function IGCM_sys_Cd
#D-* Purpose: master cd command
#D-* Examples:
#D-
function IGCM_sys_Cd {
  IGCM_debug_PushStack "IGCM_sys_Cd" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Cd :" $@
  fi
  \cd $1
  if [ $? -gt 0 ] ; then
    echo "IGCM_sys_Cd : erreur."
    IGCM_debug_Exit "IGCM_sys_Cd"
  fi
  IGCM_debug_PopStack "IGCM_sys_Cd"
}

#D-#==================================================
#D-function IGCM_sys_Chmod
#D-* Purpose: Chmod
#D-* Examples:
#D-
function IGCM_sys_Chmod {
  IGCM_debug_PushStack "IGCM_sys_Chmod" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Chmod :" $@
  fi
  \chmod $@
  if [ $? -gt 0 ] ; then
    echo "IGCM_sys_Chmod : erreur."
    IGCM_debug_Exit "IGCM_sys_Chmod"
  fi
  IGCM_debug_PopStack "IGCM_sys_Chmod"
}

#D-#==================================================
#D-function IGCM_sys_FileSize
#D-* Purpose: Filesize
#D-* Examples:
#D-
function IGCM_sys_FileSize {
  IGCM_debug_PushStack "IGCM_sys_FileSize" $@

  typeset sizeF
  set +A sizeF -- $( ls -la ${1} )
  if [ $? -gt 0 ] ; then
    IGCM_debug_Exit "IGCM_sys_FileSize"
  fi
  eval ${2}=${sizeF[4]}

  IGCM_debug_PopStack "IGCM_sys_FileSize"
}

#D-#==================================================
#D-function IGCM_sys_TestDir
#D-* Purpose: Test Directory that must exists
#D-* Examples:
#D-
function IGCM_sys_TestDir {
  IGCM_debug_PushStack "IGCM_sys_TestDir" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_TestDir :" $@
  fi
  typeset ExistFlag
  ExistFlag=$( [ -d $1 ] && echo 0 || echo 1 )
  IGCM_debug_PopStack "IGCM_sys_TestDir"

  return ${ExistFlag}
}

#D-#==================================================
#D-function IGCM_sys_TestFileBuffer
#D-* Purpose: Test file that must NOT EXISTS on Buffer
#D-* Examples:
#D-
function IGCM_sys_TestFileBuffer {
  IGCM_debug_PushStack "IGCM_sys_TestFileBuffer" $@
  typeset ExistFlag
  ExistFlag=$( [ -f $1 ] && echo 0 || echo 1 )
  IGCM_debug_PopStack "IGCM_sys_TestFileBuffer"

  return ${ExistFlag}
}

#D-#==================================================
#D-function IGCM_sys_CountFileBuffer
#D-* Purpose: Count files on Scratch filesystem
#D-* Examples:
#D-
function IGCM_sys_CountFileBuffer {
  IGCM_debug_PushStack "IGCM_sys_CountFileBuffer" $@
  ls ${@} 2>/dev/null | wc -l
  if [ $? -gt 0 ] ; then
    echo "IGCM_sys_CountFileBuffer : erreur."
  fi
  IGCM_debug_PopStack "IGCM_sys_CountFileBuffer"
}

#D-#==================================================
#D-function IGCM_sys_Tar
#D-* Purpose: master tar command
#D-* Examples:
#D-
function IGCM_sys_Tar {
  IGCM_debug_PushStack "IGCM_sys_Tar" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Tar :" $@
  fi
  \tar cf $@
  if [ $? -gt 0 ] ; then
    echo "IGCM_sys_Tar : erreur."
    IGCM_debug_Exit "IGCM_sys_Tar"
  fi
  IGCM_debug_PopStack "IGCM_sys_Tar"
}

#D-#==================================================
#D-function IGCM_sys_UnTar
#D-* Purpose: master un-tar command
#D-* Examples:
#D-
function IGCM_sys_UnTar {
  IGCM_debug_PushStack "IGCM_sys_UnTar" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_UnTar :" $@
  fi
  \tar xvf $1
  if [ $? -gt 0 ] ; then
    echo "IGCM_sys_UnTar : erreur."
    IGCM_debug_Exit "IGCM_sys_UnTar"
  fi
  IGCM_debug_PopStack "IGCM_sys_UnTar"
}

#D-*************************
#D- File transfer functions
#D-*************************
#D-

#D-#==================================================
#D-function IGCM_sys_Rsync_out
#D-* Purpose: treat return val of rsync
#D-* Examples: IGCM_sys_Rsync_out out_RET_rsync
#D-  Error values and explanations can depend on your system version.
function IGCM_sys_Rsync_out {
  IGCM_debug_PushStack "IGCM_sys_Rsync_out" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_UnTar :" $@
  fi

  typeset status
  status=$1

  if [ ! $status ] ; then
    IGCM_debug_Print 1 "IGCM_sys_Rsync_out need an argument"
    IGCM_debug_PopStack "IGCM_sys_Rsync_out"
    return
  fi

  case $status in
  0)  ;;
  1)  IGCM_debug_Print 1 "rsync error RERR_SYNTAX : Syntax or usage error";;
  2)  IGCM_debug_Print 1 "rsync error RERR_PROTOCOL : Protocol incompatibility";;
  3)  IGCM_debug_Print 1 "rsync error RERR_FILESELECT : Errors selecting input/output files, dirs";;
  4)  IGCM_debug_Print 1 "rsync error RERR_UNSUPPORTED : Requested action not supported."
      IGCM_debug_Print 1 "An attempt was made to manipulate 64-bit files on a platform that cannot support them"
      IGCM_debug_Print 1 "Or an option was specified that is supported by the client and not by the server.";;
  5)  IGCM_debug_Print 1 "rsync error  : Error starting client-server protocol";;
  10) IGCM_debug_Print 1 "rsync error RERR_SOCKETIO : Error in socket I/O";;
  11) IGCM_debug_Print 1 "rsync error RERR_FILEIO: Error in file I/O";;
  12) IGCM_debug_Print 1 "rsync error RERR_STREAMIO : Error in rsync protocol data stream";;
  13) IGCM_debug_Print 1 "rsync error RERR_MESSAGEIO : Errors with program diagnostics";;
  14) IGCM_debug_Print 1 "rsync error RERR_IPC : Error in IPC code";;
  20) IGCM_debug_Print 1 "rsync error RERR_SIGNAL : Received SIGUSR1 or SIGINT";;
  21) IGCM_debug_Print 1 "rsync error RERR_WAITCHILD : Some error returned by waitpid()";;
  22) IGCM_debug_Print 1 "rsync error RERR_MALLOC : Error allocating core memory buffers";;
  23) IGCM_debug_Print 1 "rsync error : Partial transfer due to error";;
  24) IGCM_debug_Print 1 "rsync error : Partial transfer due to vanished source files";;
  30) IGCM_debug_Print 1 "rsync error : Timeout in data send/receive";;
  *)  IGCM_debug_Print 1 "rsync error : return code of rsync unknown :" $status;;
  esac

  IGCM_debug_PopStack "IGCM_sys_Rsync_out"
}

#D-#==================================================
#D-function IGCM_sys_Miror_libIGCM
#D-* Purpose: Mirror libIGCM PATH and lib to frontend
#D-* Examples:
#D-
function IGCM_sys_Mirror_libIGCM {
  IGCM_debug_PushStack "IGCM_sys_Mirror_libIGCM"
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Mirror_libIGCM"
  fi

  typeset status

  mkdir -p ${HOME}/MIRROR/${PATHlibIGCM}

  echo ${RSYNC} ${RSYNC_opt} ${libIGCM} ${HOME}/MIRROR/${PATHlibIGCM} > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
  ${RSYNC} ${RSYNC_opt} ${libIGCM} ${HOME}/MIRROR/${PATHlibIGCM} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
  status=$?

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_Mirror_libIGCM Warning : no libIGCM on frontend."
    cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
  fi
  IGCM_debug_PopStack "IGCM_sys_Mirror_libIGCM"
}

#D-#==================================================
#D-function IGCM_sys_Cp
#D-* Purpose: generic cp
#D-* Examples:
#D-
function IGCM_sys_Cp {
  IGCM_debug_PushStack "IGCM_sys_Cp" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Cp :" $@
  fi

  typeset status

  echo cp $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
  \cp $@ >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
  status=$?

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_Cp : error code ${status}"
    cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    IGCM_debug_Exit "IGCM_sys_Cp"
  else
    \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
  fi
  IGCM_debug_PopStack "IGCM_sys_Cp"
}

#D-#==================================================
#D-function IGCM_sys_Rm
#D-* Purpose: generic rm
#D-* Examples:
#D-
function IGCM_sys_Rm {
  IGCM_debug_PushStack "IGCM_sys_Rm" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Rm :" $@
  fi

  typeset status

  echo rm $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
  \rm $@ >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
  status=$?

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_Rm : error code ${status}"
    cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    IGCM_debug_Exit "IGCM_sys_Rm"
  else
    \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
  fi
  IGCM_debug_PopStack "IGCM_sys_Rm"
}

#D-#==================================================
#D-function IGCM_sys_Mv
#D-* Purpose: generic move
#D-* Examples:
#D-
function IGCM_sys_Mv {
  IGCM_debug_PushStack "IGCM_sys_Mv" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Mv :" $@
  fi

  if [ $DRYRUN = 0 ]; then

    typeset status

    echo mv $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    \mv $@ >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?

    if [ ${status} -gt 0 ] ; then
      echo "IGCM_sys_Mv : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Exit "IGCM_sys_Mv"
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    fi
  fi
  IGCM_debug_PopStack "IGCM_sys_Mv"
}

#D-#==================================================
#D-function IGCM_sys_Get_Master
#D-* Purpose: Copy a complete directory from MASTER filesystem
#D-* Examples:
#D-
function IGCM_sys_Get_Master {
  IGCM_debug_PushStack "IGCM_sys_Get_Master" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_Get_Master :" $@
  fi
  if [ $DRYRUN = 0 ]; then
    if ( [ ! -d ${1} ] && [ ! -f ${1} ] ) ; then
      echo "WARNING : IGCM_sys_Get_Master ${1} DOES NOT EXIST ."
      IGCM_debug_PopStack "IGCM_sys_Get_Master"
      return
    fi

    typeset NB_ESSAI DELAI status i
    # number of tentative
    NB_ESSAI=3
    # time delay between tentative
    DELAI=2

    i=0
    while [ $i -lt $NB_ESSAI ] ; do
      \cp -urL $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
      status=$?
      if [ ${status} -gt 0 ]; then
        IGCM_debug_Print 2 "IGCM_sys_Get_Master : cp failed error code ${status} ${i}/${NB_ESSAI}"
        IGCM_debug_Print 2 "IGCM_sys_Get_Master : sleep ${DELAI} seconds and try again."
        sleep $DELAI
      else
        break
      fi
      (( i = i + 1 ))
    done

    if [ ${status} -gt 0 ] ; then
      echo "IGCM_sys_Get_Master : error."
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Exit "IGCM_sys_Get_Master"
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    fi
  fi
  IGCM_debug_PopStack "IGCM_sys_Get_Master"
}

#====================================================
#- Call IGCM_sys_Mirror_libIGCM now !
if ( $MirrorlibIGCM ) ; then
  IGCM_sys_Mirror_libIGCM
fi

#D-#==================================================
#D-function IGCM_sys_PutBuffer_Rest
#D-* Purpose: Put computied restarts on ${SCRATCHDIR}.
#D-           File and target directory must exist.
#D-* Examples:
#D-
function IGCM_sys_PutBuffer_Rest {
  IGCM_debug_PushStack "IGCM_sys_PutBuffer_Rest" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_PutBuffer_Rest :" $@
  fi
  if [ $DRYRUN = 0 ]; then
    if [ ! -f ${1} ] ; then
      echo "ERROR : IGCM_sys_PutBuffer_Rest ${1} DOES NOT EXIST ."
      IGCM_debug_Exit "IGCM_sys_PutBuffer_Rest"
    fi

    typeset status
    #
    # USUAL WAY
    \cp $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?

    if [ ${status} -gt 0 ] ; then
      echo "IGCM_sys_PutBuffer_Rest : error code ${status}"
      [ -f ${2} ] && ls -l ${2}
      [ -f ${2}/${1} ] && ls -l ${2}/${1}
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Exit "IGCM_sys_PutBuffer_Rest"
    else

      if [ X${JobType} = XRUN ] ; then
        [ -f ${2} ] && IGCM_sys_Chmod 444 ${2}
        [ -f ${2}/${1} ] && IGCM_sys_Chmod 444 ${2}/${1}
      fi

      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    fi
  fi
  IGCM_debug_PopStack "IGCM_sys_PutBuffer_Rest"
}

#D-#==================================================
#D-function IGCM_sys_PrepareTaredRestart
#D-* Purpose: Prepare tared restart to be access by computing job.
#D-* Examples:
#D-
function IGCM_sys_PrepareTaredRestart {
  IGCM_debug_PushStack "IGCM_sys_PrepareTaredRestart" $@
  if [ $DRYRUN = 0 ]; then
    [ ! -f $( basename $1 ) ] && IGCM_sys_Get $1 .
  fi
  IGCM_debug_PopStack "IGCM_sys_PrepareTaredRestart"
}

#D-#==================================================
#D-function IGCM_sys_PutBuffer_Out
#D-* Purpose: Copy a file on the buffer filesystem after having chmod it in readonly
#D-* Examples:
#D-
function IGCM_sys_PutBuffer_Out {
  IGCM_debug_PushStack "IGCM_sys_PutBuffer_Out" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_PutBuffer_Out :" $@
  fi

  typeset NB_ESSAI DELAI status i exist skip
  typeset fileDeviceNumberInHex directoryDeviceNumberInHex

  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  if [ $DRYRUN = 0 ]; then
    if [ ! -f ${1} ] ; then
      echo "WARNING : IGCM_sys_PutBuffer_Out ${1} DOES NOT EXIST ."
      IGCM_debug_PopStack "IGCM_sys_PutBuffer_Out"
      return 1
    fi
    #
    IGCM_sys_Mkdir $( dirname $2 )
    #

    exist=false
    skip=false
    if [ -f $2 ] ; then
      IGCM_debug_Print 1 "$2 already exist"
      exist=true
      if [ "X$( diff $1 $2 )" = X ] ; then
        IGCM_debug_Print 2 "$1 and $2 are the same file, we skip the copy"
        status=0
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
          IGCM_debug_Exit "IGCM_sys_PutBuffer_Out"
        fi
        # Identify file system
        directoryDeviceNumberInHex=$( stat -c %d $( dirname $2 ) )
        status=$?
        if [ ${status} -gt 0 ] ; then
          IGCM_debug_Exit "IGCM_sys_PutBuffer_Out"
        fi

        if [ ${fileDeviceNumberInHex} -ne ${directoryDeviceNumberInHex} ] ; then
          # They are not on the same device. USUAL WAY
          \cp $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
          status=$?
        else
          # They are on the same device. USUAL WAY
          \mv $1 $2 > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
          status=$?
        fi
        if [ ${status} -gt 0 ]; then
          IGCM_debug_Print 2 "IGCM_sys_PutBuffer_Out : cp failed error code ${status} ${i}/${NB_ESSAI}"
          IGCM_debug_Print 2 "IGCM_sys_PutBuffer_Out : sleep ${DELAI} seconds and try again."
          [ -f ${2} ] && ls -l ${2}
          [ -f ${2}/${1} ] && ls -l ${2}/${1}
          sleep $DELAI
        else
          break
        fi
        (( i = i + 1 ))
      done
    fi

    if [ ${status} -gt 0 ] ; then
      echo "IGCM_sys_PutBuffer_Out : error."
      [ -f ${2} ] && ls -l ${2}
      [ -f ${2}/${1} ] && ls -l ${2}/${1}
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Exit "IGCM_sys_PutBuffer_Out"
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
  IGCM_debug_PopStack "IGCM_sys_PutBuffer_Out"
  return 0
}

#D-#==================================================
#D-function IGCM_sys_GetBuffer
#D-* Purpose: Get a file from ${SCRATCHDIR}
#D-* Examples: IGCM_sys_GetBuffer myfile /destpath/myfile_with_PREFIX
#D-            IGCM_sys_GetBuffer /l Array_contain_myfiles /destpath/
function IGCM_sys_GetBuffer {
  IGCM_debug_PushStack "IGCM_sys_GetBuffer" $@

  typeset DEST buf_liste target file_work
  typeset NB_ESSAI DELAI status i

  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_GetBuffer :" $@
  fi

  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  if [ $DRYRUN -le 2 ]; then
    if [ X${1} = X'/l' ] ; then
      eval set +A buf_liste \${${2}}
    else
      eval set +A buf_liste ${1}
    fi
    eval DEST=\${${#}}

    #USUAL WAY
    if [ X${1} = X'/l' ] ; then
      for target in ${buf_liste[*]} ; do
        local_file=$( basename ${target} )
        i=0
        while [ $i -lt $NB_ESSAI ] ; do
          \cp ${target} ${DEST}/${local_file} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
          status=$?
          if [ ${status} -gt 0 ]; then
            IGCM_debug_Print 2 "IGCM_sys_GetBuffer : cp failed error code ${status} ${i}/${NB_ESSAI}"
            IGCM_debug_Print 2 "IGCM_sys_GetBuffer : sleep ${DELAI} seconds and try again."
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
          IGCM_debug_Exit "IGCM_sys_GetBuffer"
        else
          \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
        fi
      done
    else
      i=0
      while [ $i -lt $NB_ESSAI ] ; do
        \cp ${buf_liste} ${DEST} >> ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
        status=$?
        if [ ${status} -gt 0 ]; then
          IGCM_debug_Print 2 "IGCM_sys_GetBuffer : cp failed error code ${status} ${i}/${NB_ESSAI}"
          IGCM_debug_Print 2 "IGCM_sys_GetBuffer : sleep ${DELAI} seconds and try again."
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
        IGCM_debug_Exit "IGCM_sys_GetBuffer"
      else
        \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      fi
    fi
  fi
  IGCM_debug_PopStack "IGCM_sys_GetBuffer"
}

#D-#==================================================
#D-function IGCM_sys_GetDate_FichWork
#D-* Purpose: donne la date filesys d'un fichier sur le filesystem WORK
#D-* Examples:
#D-
function IGCM_sys_GetDate_FichWork {
  IGCM_debug_PushStack "IGCM_sys_GetDate_FichWork" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_GetDate_FichWork :" $@
  fi

  if [ $# -ge 3 ] ; then
    mode=$3
    TimeStyle=$4
  else
    mode="default"
    TimeStyle="%Y%m%d%H%M%S"
  fi

  typeset dateF
  set +A dateF -- $( ls -l --full-time --time-style=+"${TimeStyle}" ${1} )

  case $mode in
    "default")
      eval ${2}=${dateF[5]}
      ;;
    "SplitFields")
      eval ${2}="${dateF[5]}\ ${dateF[6]}"
      ;;
  esac

  # donne la date filesys d'un fichier sur la machine work
  IGCM_debug_PopStack "IGCM_sys_GetDate_FichWork"
}

#D-#==================================================
#D-function IGCM_sys_rebuild
#D-* Purpose: rebuild parallel files
#D-* Examples:
#D-
function IGCM_sys_rebuild {
  IGCM_debug_PushStack "IGCM_sys_rebuild" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_rebuild :" $@
  fi

  typeset NB_ESSAI DELAI status i firstArg
  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  i=0
  while [ $i -lt $NB_ESSAI ] ; do
    rebuild -f -o $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?
    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_rebuild : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Print 2 "IGCM_sys_rebuild : ${i}/${NB_ESSAI} sleep ${DELAI} seconds and try again."
      firstArg=${1}
      \rm ${firstArg}
      sleep $DELAI
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      break
    fi
    (( i = i + 1 ))
  done

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_rebuild : rebuild error code is ${status}"
    IGCM_debug_Exit "rebuild"
  fi

  IGCM_debug_PopStack "IGCM_sys_rebuild"
}

#D-#==================================================
#D-function IGCM_sys_rebuild_station
#D-* Purpose: rebuild parallel files describing station
#D-* Examples:
#D-
function IGCM_sys_rebuild_station {
  IGCM_debug_PushStack "IGCM_sys_rebuild_station" $@
  typeset i list_opt file_in file_out prefix_invert list_invert
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_rebuild_station :" $@
  fi

  # Station re-ordering is too expansive to be run within libIGCM
  # This is due to (ncpdq - nrcat - ncpdq) I/O sequence.
  # This re-ordering must be done "in memory" by the cmorization process

  # Only LMDZ text output contains the exact ordering of the station.
  # We isolate this in the code below:
  #  0  38  -157.5000000000000  70.98591549295774
  #  0  54  27.49999999999999   67.18309859154928
  #  0  56  -62.50000000000001  82.39436619718309
  #  0  79  12.49999999999999   78.59154929577466
  #  0  116 -165.0000000000000  76.05633802816901
  #  0  117 130.0000000000000   70.98591549295774
  #  0  118 110.0000000000000   87.46478873239437
  #  1  40  4.999999999999995   51.97183098591550

  list_opt=$@

  # Invert Axis : t,x -> x,t
  #               t,pres,x -> x,t,pres
  # So that we can concatenate along x
  i=0
  for file_in in ${list_opt} ; do
    (( i = i + 1))
    [ ${i} = 1 ] && file_out=${file_in} && continue
    # detect time counter and do the job only if present
    var_unlim=$(ncdump -h ${file_in} | grep UNLIMITED | cut -d ' ' -f 1 | sed -e 's/^[ \t]*//' -e 's/[ \t]*$//')
    if [ X${var_unlim} = Xtime_counter ] ; then
      prefix_invert=$( basename ${file_in} .nc )
      IGCM_sys_ncpdq -a x,time_counter -a x,time_counter,presnivs ${file_in} ${prefix_invert}_xt.nc
      list_invert[${#list_invert[*]}]=${prefix_invert}_xt.nc
    fi
  done

  # Concatenate
  IGCM_sys_ncrcat ${list_invert[*]} histstn_xt.nc

  # Re-ivert file
  IGCM_sys_ncpdq -a time_counter,x -a time_counter,presnivs,x histstn_xt.nc ${file_out}

  IGCM_debug_PopStack "IGCM_sys_rebuild_station"
}

##############################################################
# NCO OPERATOR

#D-#==================================================
#D-function IGCM_sys_ncap2
#D-* Purpose: encapsulate ncap2 call so as to manage error code and retry
#D-* Examples:
#D-
function IGCM_sys_ncap2 {
  IGCM_debug_PushStack "IGCM_sys_ncap2" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_ncap2 :" $@
  fi

  typeset NB_ESSAI DELAI status i
  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  i=0
  while [ $i -lt $NB_ESSAI ] ; do
    ncap2 -C --buffer_size 838860800 "$@" > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?
    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_ncap2 : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Print 2 "IGCM_sys_ncap2 : ${i}/${NB_ESSAI} sleep ${DELAI} seconds and try again."
      sleep $DELAI
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      break
    fi
    (( i = i + 1 ))
  done

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_ncap2 : ncap2 error"
    IGCM_debug_Exit "ncap2"
  fi

  IGCM_debug_PopStack "IGCM_sys_ncap2"
}

#D-#==================================================
#D-function IGCM_sys_ncatted
#D-* Purpose: encapsulate ncatted call so as to manage error code and retry
#D-* Examples:
#D-
function IGCM_sys_ncatted {
  IGCM_debug_PushStack "IGCM_sys_ncatted" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_ncatted :" $@
  fi

  typeset NB_ESSAI DELAI status i
  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  i=0
  while [ $i -lt $NB_ESSAI ] ; do
    ncatted "$@" > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?
    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_ncatted : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Print 2 "IGCM_sys_ncatted : ${i}/${NB_ESSAI} sleep ${DELAI} seconds and try again."
      sleep $DELAI
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      break
    fi
    (( i = i + 1 ))
  done

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_ncatted : ncatted error"
    IGCM_debug_Exit "ncatted"
  fi

  IGCM_debug_PopStack "IGCM_sys_ncatted"
}

#D-#==================================================
#D-function IGCM_sys_ncbo
#D-* Purpose: encapsulate ncbo call so as to manage error code and retry
#D-* Examples:
#D-
function IGCM_sys_ncbo {
  IGCM_debug_PushStack "IGCM_sys_ncbo" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_ncbo :" $@
  fi

  typeset NB_ESSAI DELAI status i
  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  i=0
  while [ $i -lt $NB_ESSAI ] ; do
    ncbo -C --buffer_size 838860800 $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?
    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_ncbo : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Print 2 "IGCM_sys_ncbo : ${i}/${NB_ESSAI} sleep ${DELAI} seconds and try again."
      sleep $DELAI
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      break
    fi
    (( i = i + 1 ))
  done

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_ncbo : ncbo error"
    IGCM_debug_Exit "ncbo"
  fi

  IGCM_debug_PopStack "IGCM_sys_ncbo"
}

#D-#==================================================
#D-function IGCM_sys_ncdif
#D-* Purpose: encapsulate ncdiff call so as to manage error code and retry
#D-* Examples:
#D-
function IGCM_sys_ncdiff {
  IGCM_debug_PushStack "IGCM_sys_ncdiff" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_ncdiff :" $@
  fi

  typeset NB_ESSAI DELAI status i
  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  i=0
  while [ $i -lt $NB_ESSAI ] ; do
    ncdiff -C --buffer_size 838860800 $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?
    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_ncdiff : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Print 2 "IGCM_sys_ncdiff : ${i}/${NB_ESSAI} sleep ${DELAI} seconds and try again."
      sleep $DELAI
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      break
    fi
    (( i = i + 1 ))
  done

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_ncdiff : ncdiff error"
    IGCM_debug_Exit "ncdiff"
  fi

  IGCM_debug_PopStack "IGCM_sys_ncdiff"
}

#D-#==================================================
#D-function IGCM_sys_ncea
#D-* Purpose: encapsulate ncea call so as to manage error code and retry
#D-* Examples:
#D-
function IGCM_sys_ncea {
  IGCM_debug_PushStack "IGCM_sys_ncea" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_ncea :" $@
  fi

  typeset NB_ESSAI DELAI status i
  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  i=0
  while [ $i -lt $NB_ESSAI ] ; do
    ncea -C --buffer_size 838860800 $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?
    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_ncea : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Print 2 "IGCM_sys_ncea : ${i}/${NB_ESSAI} sleep ${DELAI} seconds and try again."
      sleep $DELAI
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      break
    fi
    (( i = i + 1 ))
  done

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_ncea : ncea error"
    IGCM_debug_Exit "ncea"
  fi

  IGCM_debug_PopStack "IGCM_sys_ncea"
}

#D-#==================================================
#D-function IGCM_sys_ncecat
#D-* Purpose: encapsulate ncecat call so as to manage error code and retry
#D-* Examples:
#D-
function IGCM_sys_ncecat {
  IGCM_debug_PushStack "IGCM_sys_ncecat" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_ncecat :" $@
  fi

  typeset NB_ESSAI DELAI status i
  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  i=0
  while [ $i -lt $NB_ESSAI ] ; do
    ncecat -C --buffer_size 838860800 $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?
    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_ncecat : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Print 2 "IGCM_sys_ncecat : ${i}/${NB_ESSAI} sleep ${DELAI} seconds and try again."
      sleep $DELAI
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      break
    fi
    (( i = i + 1 ))
  done

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_ncecat : ncecat error"
    IGCM_debug_Exit "ncecat"
  fi

  IGCM_debug_PopStack "IGCM_sys_ncecat"
}

#D-#==================================================
#D-function IGCM_sys_ncflint
#D-* Purpose: encapsulate ncflint call so as to manage error code and retry
#D-* Examples:
#D-
function IGCM_sys_ncflint {
  IGCM_debug_PushStack "IGCM_sys_ncflint" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_ncflint :" $@
  fi

  typeset NB_ESSAI DELAI status i
  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  i=0
  while [ $i -lt $NB_ESSAI ] ; do
    ncflint -C --buffer_size 838860800 $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?
    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_ncflint : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Print 2 "IGCM_sys_ncflint : ${i}/${NB_ESSAI} sleep ${DELAI} seconds and try again."
      sleep $DELAI
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      break
    fi
    (( i = i + 1 ))
  done

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_ncflint : ncflint error"
    IGCM_debug_Exit "ncflint"
  fi

  IGCM_debug_PopStack "IGCM_sys_ncflint"
}

#D-#==================================================
#D-function IGCM_sys_ncks
#D-* Purpose: encapsulate ncks call so as to manage error code and retry
#D-* Examples:
#D-
function IGCM_sys_ncks {
  IGCM_debug_PushStack "IGCM_sys_ncks" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_ncks :" $@
  fi

  typeset NB_ESSAI DELAI status i
  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  i=0
  while [ $i -lt $NB_ESSAI ] ; do
    ncks -C --buffer_size 838860800 $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?
    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_ncks : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Print 2 "IGCM_sys_ncks : ${i}/${NB_ESSAI} sleep ${DELAI} seconds and try again."
      sleep $DELAI
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      break
    fi
    (( i = i + 1 ))
  done

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_ncks : ncks error"
    IGCM_debug_Exit "ncks"
  fi

  IGCM_debug_PopStack "IGCM_sys_ncks"
}

#D-#==================================================
#D-function IGCM_sys_ncpdq
#D-* Purpose: encapsulate ncpdq call so as to manage error code and retry
#D-* Examples:
#D-
function IGCM_sys_ncpdq {
  IGCM_debug_PushStack "IGCM_sys_ncpdq" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_ncpdq :" $@
  fi

  typeset NB_ESSAI DELAI status i
  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  i=0
  while [ $i -lt $NB_ESSAI ] ; do
    ncpdq -C --buffer_size 838860800 $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?
    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_ncpdq : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Print 2 "IGCM_sys_ncpdq : ${i}/${NB_ESSAI} sleep ${DELAI} seconds and try again."
      sleep $DELAI
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      break
    fi
    (( i = i + 1 ))
  done

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_ncpdq : ncpdq error"
    IGCM_debug_Exit "ncpdq"
  fi

  IGCM_debug_PopStack "IGCM_sys_ncpdq"
}

#D-#==================================================
#D-function IGCM_sys_ncra
#D-* Purpose: encapsulate ncra call so as to manage error code and retry
#D-* Examples:
#D-
function IGCM_sys_ncra {
  IGCM_debug_PushStack "IGCM_sys_ncra" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_ncra :" $@
  fi

  typeset NB_ESSAI DELAI status i
  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  i=0
  while [ $i -lt $NB_ESSAI ] ; do
    ncra -C --buffer_size 838860800 $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?
    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_ncra : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Print 2 "IGCM_sys_ncra : ${i}/${NB_ESSAI} sleep ${DELAI} seconds and try again."
      sleep $DELAI
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      break
    fi
    (( i = i + 1 ))
  done

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_ncra : ncra error"
    IGCM_debug_Exit "ncra"
  fi

  IGCM_debug_PopStack "IGCM_sys_ncra"
}

#D-#==================================================
#D-function IGCM_sys_ncrcat
#D-* Purpose: encapsulate ncrcat call so as to manage error code and retry
#D-* Examples:
#D-
function IGCM_sys_ncrcat {
  IGCM_debug_PushStack "IGCM_sys_ncrcat" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_ncrcat :" $@
  fi

  typeset NB_ESSAI DELAI status i lastArg
  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  i=0
  while [ $i -lt $NB_ESSAI ] ; do
    ncrcat -C --buffer_size 838860800 $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?
    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_ncrcat : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Print 2 "IGCM_sys_ncrcat : ${i}/${NB_ESSAI} sleep ${DELAI} seconds and try again."
      sleep $DELAI
    elif ( [ ! "X$( grep "WARNING Intra-file non-monotonicity" ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ )" = "X" ] && [ X${config_Post_IgnoreNonMonotonic} = XFALSE ] ) ; then
      IGCM_debug_Print 2 "IGCM_sys_ncrcat : WARNING Intra-file non-monotonicity"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      # remove files having corrupted time axis
      eval lastArg=\${$#}
      IGCM_debug_Print 2 "Remove files having corrupted time axis"
      IGCM_debug_Print 2 "IGCM_sys_ncrcat : Delete ${lastArg}"
      \rm ${lastArg}
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Print 2 "IGCM_sys_ncrcat : ${i}/${NB_ESSAI} sleep ${DELAI} seconds and try again."
      sleep $DELAI
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      break
    fi
    (( i = i + 1 ))
  done

  # Special case for spinup with cyclic time axis: change attribute calendar to none
  if [ X${config_Post_IgnoreNonMonotonic} = XTRUE ] ; then
      eval lastArg=\${$#}
      echo "IGCM_sys_ncrcat : change attribute calendar to none in file:" ${lastArg}
      IGCM_sys_ncatted -a calendar,time_counter,m,c,none ${lastArg}
  fi

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_ncrcat : ncrcat error"
    #IGCM_debug_Exit "ncrcat"
  fi

  IGCM_debug_PopStack "IGCM_sys_ncrcat"
}

#D-#==================================================
#D-function IGCM_sys_ncrename
#D-* Purpose: encapsulate ncrename call so as to manage error code and retry
#D-* Examples:
#D-
function IGCM_sys_ncrename {
  IGCM_debug_PushStack "IGCM_sys_ncrename" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_ncrename :" $@
  fi

  typeset NB_ESSAI DELAI status i
  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  i=0
  while [ $i -lt $NB_ESSAI ] ; do
    ncrename $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?
    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_ncrename : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Print 2 "IGCM_sys_ncrename : ${i}/${NB_ESSAI} sleep ${DELAI} seconds and try again."
      sleep $DELAI
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      break
    fi
    (( i = i + 1 ))
  done

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_ncrename : ncrename error"
    IGCM_debug_Exit "ncrename"
  fi

  IGCM_debug_PopStack "IGCM_sys_ncrename"
}

#D-#==================================================
#D-function IGCM_sys_ncwa
#D-* Purpose: encapsulate ncwa call so as to manage error code and retry
#D-* Examples:
#D-
function IGCM_sys_ncwa {
  IGCM_debug_PushStack "IGCM_sys_ncwa" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_ncwa :" $@
  fi

  typeset NB_ESSAI DELAI status i
  # number of tentative
  NB_ESSAI=3
  # time delay between tentative
  DELAI=2

  i=0
  while [ $i -lt $NB_ESSAI ] ; do
    ncwa -C --buffer_size 838860800 $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
    status=$?
    if [ ${status} -gt 0 ] ; then
      IGCM_debug_Print 2 "IGCM_sys_ncwa : error code ${status}"
      cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      IGCM_debug_Print 2 "IGCM_sys_ncwa : ${i}/${NB_ESSAI} sleep ${DELAI} seconds and try again."
      sleep $DELAI
    else
      \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
      break
    fi
    (( i = i + 1 ))
  done

  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_ncwa : ncwa error"
    IGCM_debug_Exit "ncwa"
  fi

  IGCM_debug_PopStack "IGCM_sys_ncwa"
}

##############################################################
# CDO OPERATOR

#D-#==================================================
#D-function IGCM_sys_cdo
#D-* Purpose: encapsulate cdo call so as to manage error code and retry
#D-* Examples:
#D-
function IGCM_sys_cdo {
  IGCM_debug_PushStack "IGCM_sys_cdo" $@
  if ( $DEBUG_sys ) ; then
    echo "IGCM_sys_cdo :" $@
  fi

  typeset status

  \cdo $@ > ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$ 2>&1
  status=$?
  if [ ${status} -gt 0 ] ; then
    echo "IGCM_sys_cdo : error code ${status}"
    cat ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    \rm ${OUTCOMMAND_PATH}/out_command_${LOGIN}.$$
    IGCM_debug_PopStack "IGCM_sys_cdo"
    return 1
  else
    IGCM_debug_PopStack "IGCM_sys_cdo"
    return 0
  fi

  IGCM_debug_PopStack "IGCM_sys_cdo"
}

# 
