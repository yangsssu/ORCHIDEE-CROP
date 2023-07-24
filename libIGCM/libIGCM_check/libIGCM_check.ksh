#!/bin/ksh

#**************************************************************
# Author: Sonia Labetoulle, Sébastien Denvil
# Contact: Sonia.Labetoulle__at__ipsl.jussieu.fr Sebastien.Denvil_at__ipsl.jussieu.fr
# $Revision:: 1059                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2014-09-23 12:05:29 +0200 (Tue, 23 Sep 2014) $ Date of last commit
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************

#D-#==================================================
#D-function IGCM_check_CommonDef
#D-* Purpose:
#D-* Examples:
#D-
function IGCM_check_CommonDef
{
  IGCM_debug_PushStack "IGCM_check_CommonDef" $@

  # Define colors
  # -------------
  ColEsc="\033["
  ColNon="${ColEsc}0m"       # Return to normal
  ColExp="${ColEsc}1m"       # Blanc - gras
  ColFat="${ColEsc}1;31m"    # Fatal
  ColCpl="${ColEsc}1;32m"    # Completed
  ColAtt="${ColEsc}1;30m"    # Waiting
  ColDef="${ColEsc}1;34m"    # Default
  ColRbl="${ColEsc}31m"      # Rebuild

  # Define date format
  # ------------------
  DateFormat="%d/%m/%y %R:%S"


  # What kind of rebuild ?
  # ----------------------
  RebuildJob="rebuild_fromWorkdir"
  IGCM_sys_TestDir ${REBUILD_DIR}
  RebuildExists=$?

  # Are packs activated or not ?
  # ----------------------------
  if ( [ ! X${config_Post_PackFrequency} = X${NULL_STR} ] && \
       [ ! X${config_Post_PackFrequency} = XNONE ] ) ; then
    Pack=true
  else
    Pack=false
  fi


  # Define input parameters
  # -----------------------
  set -A JobType_list "${RebuildJob}" "pack_output" "pack_restart" "pack_debug"

  for JobType in ${JobType_list[*]} ; do
    typeset    name1="${JobType}_String"
    typeset    name2="${JobType}_Field"
    typeset    name3="${JobType}_Activ"
    if [ X${JobType} == X${RebuildJob} ] ; then
      if ( ${Pack} ) ; then
        eval ${name1}=IGCM_sys_PutBuffer_Out
      else
        eval ${name1}=IGCM_sys_Put_Out
      fi
      eval ${name2}=4
      eval ${name3}=true
    else
      eval ${name1}=IGCM_sys_Put_Out
      eval ${name2}=3
      if ( ${Pack} ) ; then
        eval ${name3}=true
      else
        eval ${name3}=false
      fi
    fi
  done

  IGCM_debug_PopStack "IGCM_check_CommonDef"
}

#D-#==================================================
#D-function IGCM_check_ChangeUsr
#D-* Purpose:
#D-* Examples:
#D-
function IGCM_check_ChangeUsr
{
  IGCM_debug_PushStack "IGCM_check_ChangeUsr" $@

  CurrentGrp=$( groups $CurrentUsr | gawk '{print $3}' )
  TargetGrp=$( groups $TargetUsr | gawk '{print $3}' )

  echo $1 | sed -e "s/${CurrentUsr}/${TargetUsr}/" \
                -e "s/${CurrentGrp}/${TargetGrp}/"

  IGCM_debug_PopStack "IGCM_check_ChangeUsr"
}

#D-#==================================================
#D-function IGCM_check_SearchCatalog
#D-* Purpose:
#D-* Examples:
#D-
function IGCM_check_SearchCatalog
{
  IGCM_debug_PushStack "IGCM_check_SearchCatalog" $@

  typeset num
  unset SUBMIT_DIR

  fg_new=false

  if [ ! X${JobName} == X ] ; then
    NbOcc=$( gawk -v JobName=${JobName} \
             'BEGIN {x=0}  $1 ~ JobName {++x} END {print x}' ${SimuCatalog} )
  else
    NbOcc=0
  fi

  if ( [ ${NbOcc} -eq 0 ] && ( ${fg_path} ) ) ; then

    if [ ! "$( echo ${ConfigPath} | cut -c 1 )" == "/" ] ; then
      ConfigPath="${PWD}/${ConfigPath}"
    fi

    set -A FileList $( ls ${ConfigPath}/Job_* )
    if [ X$FileList == X ] ; then
      NbOcc=0
    else
      NbOcc=${#FileList[@]}
      fg_new=true
    fi
  fi

  if ( [ ${NbOcc} -eq 0 ] && ( ${fg_search} ) ) ; then
    SEARCH_DIR=${WORKDIR}
    if [ ${TargetUsr} != ${CurrentUsr} ] ; then
      SEARCH_DIR=$( IGCM_check_ChangeUsr ${SEARCH_DIR} )
    fi
    echo "${JobName} not in Catalog, we'll try to find it in ${SEARCH_DIR}"

    set -A FileList $( find ${SEARCH_DIR}/ \
                            -path ${SEARCH_DIR}/IGCM_OUT -prune -o \
                            -name Job_${JobName} -print )
    if [ X$FileList == X ] ; then
      NbOcc=0
    else
      NbOcc=${#FileList[@]}
      fg_new=true
    fi
  fi

  if [ ${NbOcc} -gt 1 ] ; then
    echo "More than one job"
    ind=0
    while [ ${ind} -lt ${NbOcc} ] ; do
      printf '%2i) %-30s\n' ${ind} ${FileList[${ind}]}
      (( ind = ind + 1 ))
    done
    echo "Give your choice number or 'q' to quit : "
    read Choice
    if [ X${Choice} == Xq ] ; then
      return 1
    else
      fg_new=true
      FileList=${FileList[${Choice}]}
      NbOcc=1
    fi
  fi

  case ${NbOcc} in
    0)
      if ( ${fg_search} ) ; then
        echo "${JobName} not found, neither in catalog nor in \$WORKDIR."
      else
        echo "${JobName} not found in catalog."
      fi
      echo "You can try : *) '-s' option to automatically search your \$WORKDIR ; "
      echo "              *) '-p' option to provide the directory (absolute path) "
      echo "                      containing the config.card ; "
      echo "              *)  manually editing your ${SimuCatalog}."
      return 1 ;;
    1)
      if ( ${fg_new} ) ; then
#        JobName=${JobName:=$( basename ${FileList} | gawk -F"_" '{ print $2 }' )}
        JobName=$( basename ${FileList} | gawk -F"_" '{ print $2 }' )
        SUBMIT_DIR=$( dirname ${FileList} )
        echo "${JobName} ${TargetUsr} ${HostName} ${SUBMIT_DIR}"
        echo "${JobName} ${TargetUsr} ${HostName} ${SUBMIT_DIR}" >> ${SimuCatalog}
        sort -u ${SimuCatalog} > ${SimuCatalog}.tmp
        mv ${SimuCatalog}.tmp ${SimuCatalog}
      elif ( [ ${TargetUsr} == $( gawk -v JobName=${JobName} \
                                      '$1 ~ JobName {print $2}' \
                                      ${SimuCatalog} ) ] \
          && [ ${HostName}  == $( gawk -v JobName=${JobName} \
                                      '$1 ~ JobName {print $3}' \
                                      ${SimuCatalog} ) ] ) ; then
        JobName=$( gawk -v JobName=${JobName} '$1 ~ JobName {print $1}' ${SimuCatalog} )
        SUBMIT_DIR=$( gawk -v JobName=${JobName} '$1 ~ JobName {print $4}' ${SimuCatalog} )
      else
        echo "${JobName} not in Catalog."
        return 1
      fi
      ;;
    *)
      break ;;
  esac

  IGCM_debug_PopStack "IGCM_check_SearchCatalog"
}

#D-#==================================================
#D-function IGCM_check_PrintHeader
#D-* Purpose:
#D-* Examples:
#D-
function IGCM_check_PrintHeader
{
  IGCM_debug_PushStack "IGCM_check_PrintHeader" $@

  echo "|===========================================================================================================|"
  printf "| JobName = ${ColExp}%-26.26s${ColNon} JobID = ${ColExp}%-36.36s${ColNon}date : ${ColExp}%-18s${ColNon}|\n" \
         ${JobName} ${JobID} "$( date +"${DateFormat}" )"

  printf "| User    = ${ColExp}%-56.56s${ColNon}last run.card write : ${ColExp}%-18s${ColNon}|\n" \
         ${TargetUsr} "${LastWrite}"

  printf "|-----------------------------------------------------------------------------------------------------------|\n"

  printf "| Submit  | ${ColExp}%-95.95s${ColNon} |\n" ${SUBMIT_DIR}
  printf "| Data    | ${ColExp}%-95.95s${ColNon} |\n" ${DATA_DIR}
  printf "| Rebuild | ${ColExp}%-95.95s${ColNon} |\n" ${REBUILD_DIR}
  printf "| Post    | ${ColExp}%-95.95s${ColNon} |\n" ${POST_DIR}
  printf "| Work    | ${ColExp}%-95.95s${ColNon} |\n" ${CWORK_DIR}

  echo "|-------------------------|-------------|-------------------------|-------------|-----:----------:----------|"
  echo "|                         |             |                         |             |     Pending Rebuilds      |"
  echo "| Date Begin - DateEnd    | PeriodState | Current Period          | CumulPeriod | Nb  : from     : to       |"
  echo "|-------------------------|-------------|-------------------------|-------------|-----:----------:----------|"

  printf "| %-10s - %-10s | " \
         $DateBegin $DateEnd

  case $PeriodState in
    Fatal)
      Color=${ColFat}
      ;;
    Completed)
      Color=${ColCpl}
      ;;
    Waiting|OnQueue)
      Color=${ColAtt}
      ;;
    *)
      Color=${ColDef}
      ;;
  esac
  printf "${Color}%-11s${ColNon} | " $PeriodState

  printf "%-10s - %-10s | %11s | " \
         $PeriodDateBegin $PeriodDateEnd $CumulPeriod

  if ( [ X${NbRebuild} != X. ] && [ X${NbRebuild} != X0 ] ) ; then
    printf "${ColRbl}%3s : %-8s : %-8s${ColNon} |\n" \
           $NbRebuild $FirstRebuild $LastRebuild
  else
    printf "%3s : %-8s : %-8s |\n" \
           $NbRebuild $FirstRebuild $LastRebuild
  fi

  if [ ${NbLines} -gt 0 ] ; then
    printf "|-----------------------------------------------------------------------------------------------------------|\n"
    printf "|                                                      Last                                                 |\n"
    printf "|     Rebuild      |   Pack_Output    |   Pack_Restart   |    Pack_Debug    |  Monitoring  |     Atlas      |\n"
    printf "|------------------|------------------|------------------|------------------|--------------|----------------|\n"
  fi

  IGCM_debug_PopStack "IGCM_check_PrintHeader"
}

#D-#==================================================
#D-function IGCM_check_PrintJob
#D-* Purpose:
#D-* Examples:
#D-
function IGCM_check_PrintJob
{
  IGCM_debug_PushStack "IGCM_check_PrintJob" $@

  printf "|"

  # Print rebuild and pack jobs
  # ---------------------------
  for JobType in ${JobType_list[*]} ; do
    eval Date=\${${JobType}_Date[${ind}]}
    eval Status=\${${JobType}_Status[${ind}]}
    eval Nb=\${${JobType}_Nb[${ind}]}

    if [ X${Status} == XOK  ] ; then
      Color=${ColCpl}
    else
      Color=${ColFat}
    fi
    printf "  ${Color}%-8s${ColNon} : %3s  |" ${Date} ${Nb}
  done

  Color=${ColExp}

  # Print monitoring jobs
  # ---------------------
  JobType=monitoring
  if [ $ind -eq 0 ] ; then
    eval Date=\${${JobType}_Date}
  else
    Date=""
  fi
  printf "     ${Color}%-4s${ColNon}     |" ${Date}

  # Print atlas jobs
  # ----------------
  JobType=atlas
  eval Date=\${${JobType}_Date[${ind}]}
  printf "  ${Color}%-12s${ColNon}  |" ${Date}

  printf "\n"

  IGCM_debug_PopStack "IGCM_check_PrintJob"
}

#D-#==================================================
#D-function IGCM_check_PrintFooter
#D-* Purpose:
#D-* Examples:
#D-
function IGCM_check_PrintFooter
{
  IGCM_debug_PushStack "IGCM_check_PrintFooter" $@

  printf "|===========================================================================================================|\n"
  #date +"${DateFormat}"

  IGCM_debug_PopStack "IGCM_check_PrintFooter"
}

#D-#==================================================
#D-function IGCM_check_CheckPendingRebuild
#D-* Purpose:
#D-* Examples:
#D-
function IGCM_check_CheckPendingRebuild
{
  IGCM_debug_PushStack "IGCM_check_PendingRebuild" $@

  NbRebuild="."
  FirstRebuild="."
  LastRebuild="."

  if [ ${RebuildExists} == 0 ] ; then
    set -A RebuildList $( find ${REBUILD_DIR}/ -name "REBUILD_*" | sort )
    if [ ${#RebuildList[*]} -gt 0 ] ; then
      NbRebuild=$( IGCM_sys_CountFileArchive ${REBUILD_DIR} )

      FirstRebuild=$( basename ${RebuildList[0]} | cut -f2 -d\_ )
      LastRebuild=$( basename ${RebuildList[ (( NbRebuild=${NbRebuild}-1 )) ]} | cut -f2 -d\_ )
    fi
  fi

  IGCM_debug_PopStack "IGCM_check_PendingRebuild"
}

#D-#==================================================
#D-function IGCM_check_CheckRebPackJobs
#D-* Purpose:
#D-* Examples:
#D-
function IGCM_check_CheckRebPackJobs
{
  IGCM_debug_PushStack "IGCM_check_CheckRebPackJobs" $@

  NbLines=0
  for JobType in ${JobType_list[*]} ; do
    eval String=\${${JobType}_String}
    eval Field=\${${JobType}_Field}
    eval Activ=\${${JobType}_Activ}

    if ( ${Activ} ) ; then

      set -A FileList $( ls ${POST_DIR}/${JobType}.*.out | tail -n ${NbHisto} )

      if [ ${#FileList[*]} -gt ${NbLines} ] ; then
        NbLines=${#FileList[*]}
      fi

      (( ind = 0 ))
      for FileName in ${FileList[*]} ; do
        LastDate=""
        LastDate=$( basename ${FileName} | gawk -F"." '{ print $(NF-1) }' )

        set -- $( gawk -v String=${String} \
                       'BEGIN { nb_ok = 0 ; nb_ko = 0 } \
                       ($1 ~ String) { \
                         if ($3 !~ /error./) { \
                           nb_ok = nb_ok + 1 \
                         } else { \
                           nb_ko = nb_ko + 1 \
                         } \
                       } \
                       END { print nb_ok " " nb_ko }' \
                       ${POST_DIR}/${JobType}.${LastDate}.out )
        Match=$1
        Error=$2

        (( Nb = ${Match} - ${Error} ))

        if ( [ ${Error} -eq 0 ] && [ ${Nb} -gt 0 ] ) ; then
          Status=OK
        else
          Status=KO
        fi

        eval ${JobType}_Date[$ind]=${LastDate}
        eval ${JobType}_Status[$ind]=${Status}
        eval ${JobType}_Nb[$ind]=${Nb}

        (( ind = ind + 1 ))
      done

    else

        eval ${JobType}_Date[0]=""
        eval ${JobType}_Status[0]=""
        eval ${JobType}_Nb[0]=""

    fi

  done

  IGCM_debug_PopStack "IGCM_check_CheckRebPackJobs"
}

#D-#==================================================
#D-function IGCM_check_CheckMonitoring
#D-* Purpose:
#D-* Examples:
#D-
function IGCM_check_CheckMonitoring
{
  IGCM_debug_PushStack "IGCM_check_CheckMonitoring" $@

    LastDate=""
    JobType=monitoring
    IGCM_sys_TestDirArchive ${CWORK_DIR}/MONITORING
    RC=$?
    if [ $RC == 0 ] ; then
      FileTmp=$( IGCM_sys_RshArchive "ls ${CWORK_DIR}/MONITORING/files/*.nc | head -n 1" )
      IGCM_sys_GetDate_Monitoring ${FileTmp} LastDate
      eval ${JobType}_Date=${LastDate}
    fi

  IGCM_debug_PopStack "IGCM_check_CheckMonitoring"
}

#D-#==================================================
#D-function IGCM_check_CheckAtlas
#D-* Purpose:
#D-* Examples:
#D-
function IGCM_check_CheckAtlas
{
  IGCM_debug_PushStack "IGCM_check_CheckAtlas" $@

  JobType=atlas
  IGCM_sys_TestDirArchive ${CWORK_DIR}/ATLAS
  RC=$?
  if [ $RC == 0 ] ; then
    set -A FileList $( IGCM_sys_RshArchive "ls ${CWORK_DIR}/ATLAS | tail -n ${NbHisto}" )

    if [ ${#FileList[*]} -gt ${NbLines} ] ; then
      NbLines=${#FileList[*]}
    fi

    (( ind = 0 ))
    for FileName in ${FileList[*]} ; do
      eval ${JobType}_Date[$ind]=${FileName}
      (( ind = ind + 1 ))
    done
  fi

  IGCM_debug_PopStack "IGCM_check_CheckAtlas"
}
