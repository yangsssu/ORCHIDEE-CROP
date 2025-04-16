#!/bin/ksh

#**************************************************************
# Author: Sebastien Denvil, Martial Mancip
# Contact: Sebastien.Denvil__at__ipsl.jussieu.fr Martial.Mancip__at__ipsl.jussieu.fr
# $Revision:: 1280                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2016-02-02 12:13:08 +0100 (Tue, 02 Feb 2016) $ Date of last commit
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************

#===================================
function IGCM_config_CommonConfiguration
{
  IGCM_debug_PushStack "IGCM_config_CommonConfiguration" $@

  # Debug Print :
  [ ${Verbosity} -gt 0 ] && echo
  IGCM_debug_Print 1 "IGCM_config_CommonConfiguration" $@

  # config.card path
  configCardPath=$1

  #==================================
  typeset option auxprint

  #==================================
  # Read UserChoices section:
  [ ${Verbosity} -gt 0 ] && echo
  IGCM_debug_Print 1 "DefineArrayFromOption  : config_UserChoices"

  IGCM_card_DefineArrayFromSection ${configCardPath} UserChoices
  for option in ${config_UserChoices[*]} ; do
    IGCM_card_DefineVariableFromOption ${configCardPath} UserChoices ${option}
    eval auxprint=\${config_UserChoices_${option}}
    IGCM_debug_Print 3 "${option} : ${auxprint}"
  done

  #==================================
  # Read Ensemble section:
  [ ${Verbosity} -gt 0 ] && echo
  IGCM_debug_Print 1 "DefineArrayFromOption  : config_Ensemble"

  IGCM_card_DefineArrayFromSection ${configCardPath} Ensemble
  for option in ${config_Ensemble[*]} ; do
    IGCM_card_DefineVariableFromOption ${configCardPath} Ensemble ${option}
    eval auxprint=\${config_Ensemble_${option}}
    IGCM_debug_Print 3 "${option} : ${auxprint}"
  done

  #==================================
  # Read Post section:
  [ ${Verbosity} -gt 0 ] && echo
  IGCM_debug_Print 1 "DefineArrayFromOption : config_Post"

  IGCM_card_DefineArrayFromSection ${configCardPath} Post
  for option in ${config_Post[*]} ; do
    IGCM_card_DefineVariableFromOption ${configCardPath} Post ${option}
    eval auxprint=\${config_Post_${option}}
    IGCM_debug_Print 3 "${option} : ${auxprint}"
  done
  [ ${Verbosity} -gt 0 ] && echo

  #==================================
  # Define default value to keep compatibility with previous card: means before changes due to TGCC
  # Apply some overrules to ensure proper usage of computing centres resources
  #
  if [ X${PackDefault} = Xtrue ] ; then
    if [ X${config_UserChoices_SpaceName} = XTEST ]; then
      # TEST simulations will not be packed and will stay on SCRATCHDIR filesystem
      IGCM_debug_Print 1 "SpaceName=TEST. OVERRULE PackFrequency to NONE"
      config_Post_PackFrequency=NONE
    else
      # Default to RebuildFrequency if nothing has been set up related to PackFrequency
      [ X${config_Post_PackFrequency} = X ] && config_Post_PackFrequency=${config_Post_RebuildFrequency}
    fi
  else
    # If we DO NOT apply pack in this computing center
    config_Post_PackFrequency=NONE
  fi

  #====================================================
  # Define ARCHIVE : Dedicated to large files
  # Define STORAGE : Dedicated to small/medium files
  # Define R_OUT   : Output tree located on ARCHIVE
  # Define R_BUF   : Output tree located on STORAGE (files waiting treatment, or file lcoation when SpaceName=!PROD)
  # Define R_FIG   : Output tree located on STORAGE hosting figures (monitoring and atlas, and/or small files)
  # Define R_TMP   : A temporary space used by IGCM_debug_send_AMQP_msg__MAILTUNNEL. Must be persistent in between jobs
  IGCM_sys_defineArchives

  #====================================================
  # R_SAVE : Job output directory
  # R_BUFR : Job output buffered directory

  if ( [ ! X${config_UserChoices_SpaceName} = X ] && [ ! X${config_UserChoices_ExperimentName} = X ] ) ; then
    FreeName=$( echo ${config_UserChoices_JobName} | sed 's/.*_//' )
    if ( [ ! X${config_Ensemble_EnsembleName} = X ] && [ ! X${config_Ensemble_EnsembleDate} = X ] ) ; then
      R_SAVE=${R_OUT}/${config_UserChoices_TagName}/${config_UserChoices_SpaceName}/${config_UserChoices_ExperimentName}/${config_Ensemble_EnsembleName}/${config_Ensemble_EnsembleDate}/${FreeName}
      R_FIGR=${R_FIG}/${config_UserChoices_TagName}/${config_UserChoices_SpaceName}/${config_UserChoices_ExperimentName}/${config_Ensemble_EnsembleName}/${config_Ensemble_EnsembleDate}/${FreeName}
      R_BUFR=${R_BUF}/${config_UserChoices_TagName}/${config_UserChoices_SpaceName}/${config_UserChoices_ExperimentName}/${config_Ensemble_EnsembleName}/${config_Ensemble_EnsembleDate}/${FreeName}
      R_DODS=${config_UserChoices_TagName}/${config_UserChoices_SpaceName}/${config_UserChoices_ExperimentName}/${config_Ensemble_EnsembleName}/${config_Ensemble_EnsembleDate}/${FreeName}
    else
      R_SAVE=${R_OUT}/${config_UserChoices_TagName}/${config_UserChoices_SpaceName}/${config_UserChoices_ExperimentName}/${FreeName}
      R_FIGR=${R_FIG}/${config_UserChoices_TagName}/${config_UserChoices_SpaceName}/${config_UserChoices_ExperimentName}/${FreeName}
      R_BUFR=${R_BUF}/${config_UserChoices_TagName}/${config_UserChoices_SpaceName}/${config_UserChoices_ExperimentName}/${FreeName}
      R_DODS=${config_UserChoices_TagName}/${config_UserChoices_SpaceName}/${config_UserChoices_ExperimentName}/${FreeName}
    fi
  else
    if ( [ ! X${config_Ensemble_EnsembleName} = X ] && [ ! X${config_Ensemble_EnsembleDate} = X ] ) ; then
      R_SAVE=${R_OUT}/${config_UserChoices_TagName}/${config_Ensemble_EnsembleName}/${config_Ensemble_EnsembleDate}/${config_UserChoices_JobName}
      R_FIGR=${R_FIG}/${config_UserChoices_TagName}/${config_Ensemble_EnsembleName}/${config_Ensemble_EnsembleDate}/${config_UserChoices_JobName}
      R_BUFR=${R_BUF}/${config_UserChoices_TagName}/${config_Ensemble_EnsembleName}/${config_Ensemble_EnsembleDate}/${config_UserChoices_JobName}
      R_DODS=${config_UserChoices_TagName}/${config_Ensemble_EnsembleName}/${config_Ensemble_EnsembleDate}/${config_UserChoices_JobName}
    else
      R_SAVE=${R_OUT}/${config_UserChoices_TagName}/${config_UserChoices_JobName}
      R_FIGR=${R_FIG}/${config_UserChoices_TagName}/${config_UserChoices_JobName}
      R_BUFR=${R_BUF}/${config_UserChoices_TagName}/${config_UserChoices_JobName}
      R_DODS=${config_UserChoices_TagName}/${config_UserChoices_JobName}
    fi
  fi

  #====================================================
  # Define R_OUT_KSH : Storage place for job output
  # Define R_OUT_EXE : Storage place for binary used during simulation
  R_OUT_KSH=${R_SAVE}/Out
  R_OUT_EXE=${R_SAVE}/Exe

  #====================================================
  # Define R_BUF_KSH : Buffer place for job output
  # Define R_BUF_EXE : Buffer place for binary used during simulation
  R_BUF_KSH=${R_BUFR}/Out
  R_BUF_EXE=${R_BUFR}/Exe

  #====================================================
  # Define REBUILD_DIR : where we store files needing rebuild process
  REBUILD_DIR=${R_BUFR}/REBUILD
  if [ ! X${TaskType} = Xchecking ] ; then
    IGCM_sys_MkdirWork ${REBUILD_DIR}
  fi

  #====================================================
  # DodsCopy : apply default value if not defined
  if ( [ X${config_Post_DodsCopy} = X${NULL_STR} ] || [ X${config_Post_DodsCopy} = X ] ) ; then
    config_Post_DodsCopy=TRUE
  fi

  #====================================================
  # IgnoreNonMonotonic : apply default value if not defined
  if ( [ X${config_Post_IgnoreNonMonotonic} = X${NULL_STR} ] || [ X${config_Post_IgnoreNonMonotonic} = X ] ) ; then
    config_Post_IgnoreNonMonotonic=FALSE
  fi

  #====================================================
  # Define StackFileLocation : directory where we store stack files
  # Define StackFileName : stack file containing call tree and instrumentation
  # Stack file containing call tree will be stored there.
  if ( $DEBUG_debug ) ; then
    StackFileLocation=${StackFileLocation:=${R_BUF_KSH}}
    [ ! -d ${StackFileLocation} ] && mkdir -p ${StackFileLocation}
    if [ X${TaskType} = Xcomputing ]; then
      StackFileName=computing.stack.$$
    elif [ X${TaskType} = Xpost-processing ]; then
      StackFileName=${Script_Post_Output}.stack.$$
    elif [ X${TaskType} = Xchecking ]; then
      StackFileName=checking.stack.$$
    else
      IGCM_debug_Exit "IGCM_config_CommonConfiguration unknown TaskType : ${TaskType}"
      IGCM_debug_Verif_Exit
    fi

    # This boolean will trigger the filling of the stack
    # Only now we know where things should be ...
    # We don't fill the stack when we perform checking task
    if [ ! X${TaskType} = Xchecking ] ; then
      ActivateStackFilling=true
    fi
  fi

  IGCM_debug_PopStack "IGCM_config_CommonConfiguration"
}

#===================================
function IGCM_config_Initialize
{
  IGCM_debug_PushStack "IGCM_config_Initialize"

  # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_config_Initialize"

  # Test modipsl tree existence.
  IGCM_sys_TestDir ${MODIPSL}
  [ $? != 0 ] && IGCM_debug_Exit "IGCM_sys_TestDir"
  IGCM_sys_TestDir ${libIGCM}
  [ $? != 0 ] && IGCM_debug_Exit "IGCM_sys_TestDir"
  IGCM_sys_TestDir ${R_EXE}
  [ $? != 0 ] && IGCM_debug_Exit "IGCM_sys_TestDir"
  IGCM_sys_TestDir ${SUBMIT_DIR}
  [ $? != 0 ] && IGCM_debug_Exit "IGCM_sys_TestDir"

  if ( $DEBUG_debug ) ; then
    echo "Keep trace of inital SUBMIT_DIR : "
    ls -lta ${SUBMIT_DIR}
  fi

  #==================================
  # Read ListOfComponents section:
  echo
  IGCM_debug_Print 1 "DefineArrayFromSection : ListOfComponents"

  IGCM_card_DefineArrayFromSection ${SUBMIT_DIR}/config.card ListOfComponents
  for comp in ${config_ListOfComponents[*]} ; do
    IGCM_card_DefineArrayFromOption ${SUBMIT_DIR}/config.card ListOfComponents ${comp}
  done
  IGCM_debug_Print 3 ${config_ListOfComponents[*]}

  #==================================
  # Read Executable section:
  IGCM_card_DefineArrayFromSection   ${SUBMIT_DIR}/config.card Executable

  #==================================
  # Read Restarts section:
  # Restarts : Gerneral rule or local for each component.
  echo
  IGCM_debug_Print 1 "DefineArrayFromOption : config_Restarts"

  IGCM_card_DefineArrayFromSection ${SUBMIT_DIR}/config.card Restarts
  for option in ${config_Restarts[*]} ; do
    IGCM_card_DefineVariableFromOption ${SUBMIT_DIR}/config.card Restarts ${option}
    eval auxprint=\${config_Restarts_${option}}
    IGCM_debug_Print 3 "${option} : ${auxprint}"
  done

  #==================================
  # Define Job Outputs Name
  echo
  IGCM_debug_Print 2 "Define Script_Output_Prefix and Exe_Output"
  Script_Output_Prefix=${config_UserChoices_Script_Output_Prefix:='Script_Output'}
  IGCM_debug_Print 3 "Script_Output_Prefix = ${Script_Output_Prefix}"
  Exe_Output=out_execution
  IGCM_debug_Print 3 "Exe_Output           = ${Exe_Output}"

  #===================================================================#
  # Prepare variables available for ${COMP}.card and ${COMP}.driver   #
  #             But available to any son functions                    #
  #===================================================================#

  # Convert yyyy-mm-dd date to gregorian yyyymmdd
  DateBegin=$( IGCM_date_ConvertFormatToGregorian ${config_UserChoices_DateBegin} )
  DateEnd=$(   IGCM_date_ConvertFormatToGregorian ${config_UserChoices_DateEnd}   )

  # Period Length In Days between DateBegin and DateEnd
  (( ExperienceLengthInDays=$( IGCM_date_DaysBetweenGregorianDate ${DateEnd} ${DateBegin} )  + 1 ))
  if [ ${ExperienceLengthInDays} -lt 0 ] ; then
    IGCM_debug_Print 1 "Problem with dates in config.card : ${DateEnd} < ${DateBegin} ! You must check that."
    IGCM_debug_Exit "IGCM_config_Initialize" " Wrong Dates."
    IGCM_debug_Verif_Exit
  fi

  # Day and Year of Initial State (Given in julian format)
  InitDay=$(( $( IGCM_date_ConvertGregorianDateToJulian $DateBegin ) % 1000 ))
  InitYear=$(( $( IGCM_date_ConvertGregorianDateToJulian $DateBegin ) / 1000 ))

  #================================================================#
  #                  Test and Prepare directories                  #
  #================================================================#

  # ==> 4 kinds of input files :
  #     1) R_INIT  : Initial State Files   (Etat0, carteveg)
  #     2) R_BC    : Boundary Conditions   (Forcages, lai)
  #     3) Parameters files (allready define through ${SUBMIT_DIR})
  #     4) Restarts files   (allready define in IGCM_config_Initialize)

  # Here we offer the possibility to redefine R_INIT, R_BC
  # and PeriodNb through config.card
  R_INIT=${config_UserChoices_R_INIT:=${R_IN}/INIT}
  echo
  IGCM_debug_Print 2 "(Re)Define R_INIT, R_BC and PeriodNb"
  IGCM_debug_Print 3 "R_INIT=${R_INIT}"
  R_BC=${config_UserChoices_R_BC:=${R_IN}/BC}
  IGCM_debug_Print 3  "R_BC=${R_BC}"
  PeriodNb=${config_UserChoices_PeriodNb:=${PeriodNb}}
  IGCM_debug_Print 3  "Loop in main Job with ${PeriodNb} period(s)"

  # SD ADA SPECIFIC #
  #      TO FIX     #
  #IGCM_sys_TestDirArchive ${R_IN}
  #[ $? != 0 ] && IGCM_debug_Exit "IGCM_sys_TestDirArchive"

  if ( ${FirstInitialize} ) ; then
    IGCM_sys_MkdirArchive   ${R_SAVE}
    [ ! ${config_Post_PackFrequency} = NONE ] && IGCM_sys_Mkdir ${R_BUFR}
  else
    IGCM_sys_TestDirArchive ${R_SAVE}
    [ $? != 0 ] && IGCM_debug_Exit "IGCM_sys_TestDirArchive ${R_SAVE}"

    if [ ! ${config_Post_PackFrequency} = NONE ] ; then
      IGCM_sys_TestDir ${R_BUFR}
      [ $? != 0 ] && IGCM_debug_Exit "IGCM_sys_TestDir ${R_BUFR}"
    fi

    # Test state of run in run.card. Will schedule an exit if another process setted it to "Fatal"
    IGCM_config_StateCheck

    # And EXIT if not OK
    IGCM_debug_Verif_Exit
  fi

  #====================================================
  # Experience type : DEB(ug), DEV(elopment), RUN
  if [ X${JobType} != XRUN ] ; then
    echo
    echo "===================================================="
    echo "libIGCM JOB is NOT in RUN type mode."
    echo "!! OUTPUT files will NOT be PROTECTED !!"
    echo "Be carefull : you can ERASE the result of this job !"

    case ${JobType} in
    DEB)
      echo "DEBUG mode : activation of 'set -vx' mode."
      echo "DEBUG mode : no protection for output files."
      echo "DEBUG mode : if active force asynchronous rebuild frequency to PeriodLength frequency."
      ;;
    DEV)
      echo "DEVelopment mode : no protection for output files."
      echo "DEVelopment mode : if active force asynchronous rebuild frequency to PeriodLength frequency."
      ;;
    esac

    if ( [ X${config_Post_RebuildFrequency} != XNONE ] && [ ${DRYRUN} -eq 0 ] ) ; then
      if [ X${config_Post_RebuildFrequency} != X${config_UserChoices_PeriodLength} ] ; then
        echo "------------"
        echo "WARNING : Job is NOT in RUN mode then we will force REBUILD Frequency"
        echo "          to PeriodLength : ${config_UserChoices_PeriodLength}"
        echo "------------"
        config_Post_RebuildFrequency=${config_UserChoices_PeriodLength}
      fi
    fi
    echo "===================================================="
    echo
  fi

  IGCM_debug_PopStack "IGCM_config_Initialize"
}

#===================================
function IGCM_config_DaysInPeriodLength
{
  IGCM_debug_PushStack "IGCM_config_DaysInPeriodLength"

  typeset i

  # Determine number of day(s) in PeriodLength :
  case ${config_UserChoices_PeriodLength} in
  *Y|*y)
    PeriodLengthInYears=$( echo ${config_UserChoices_PeriodLength} | sed -e 's/[yY]//' )
    echo
    IGCM_debug_Print 2 "Number of years for PeriodLength : ${PeriodLengthInYears}"
    PeriodLengthInDays=0
    i=0
    until [ $i -ge $PeriodLengthInYears ] ; do
      (( PeriodLengthInDays = PeriodLengthInDays + $( IGCM_date_DaysInYear $(( year + i )) ) ))
      (( i=i+1 ))
    done
    ;;
  *M|*m)
    PeriodLengthInMonths=$( echo ${config_UserChoices_PeriodLength} | sed -e 's/[mM]//' )
    echo
    IGCM_debug_Print 2 "Number of months for PeriodLength : ${PeriodLengthInMonths}"
    PeriodLengthInDays=0
    i=0
    until [ $i -ge $PeriodLengthInMonths ] ; do
      if [ $(( 10#${month} + ${i} )) -lt 13 ] ; then
        (( PeriodLengthInDays  = PeriodLengthInDays + $( IGCM_date_DaysInMonth $year $(( 10#${month} + ${i} )) ) ))
      else
        (( PeriodLengthInDays  = PeriodLengthInDays + $( IGCM_date_DaysInMonth $year $(( 10#${month} + ${i} - 12 )) ) ))
      fi
      (( i=i+1 ))
    done
    ;;
  *D|*d)
    PeriodLengthInMonths=0
    PeriodLengthInDays=$( echo ${config_UserChoices_PeriodLength} | sed -e 's/[dD]//' )
    echo
    IGCM_debug_Print 2 "Number of days for PeriodLength : ${PeriodLengthInDays}";;
  *)
    IGCM_debug_Exit "IGCM_config_DaysInPeriodLength " ${config_UserChoices_PeriodLength} " invalid period length : choose in *Y, *M, *D."
    IGCM_debug_Verif_Exit ;;
  esac

  IGCM_debug_PopStack "IGCM_config_DaysInPeriodLength"
}

#===================================
function IGCM_config_DateCoherency
{
  IGCM_debug_PushStack "IGCM_config_DateCoherency"

  echo
  IGCM_debug_Print 1 "IGCM_config_DateCoherency"
  echo

  typeset Length VerifiedPeriodDateBegin VerifiedPeriodDateEnd

  # check coherency between (PeriodDateBegin, PeriodDateEnd) and (DateBegin, CumulPeriod, PeriodLength)
  # DateBegin + CumulPeriod*PeriodLength = PeriodDateBegin
  echo

  case ${config_UserChoices_PeriodLength} in
  *Y|*y)
    Length=$( IGCM_date_DaysInCurrentPeriod ${DateBegin} $(( ${CumulPeriod} * ${PeriodLengthInYears} ))Y )
    ;;
  *M|*m)
    Length=$( IGCM_date_DaysInCurrentPeriod ${DateBegin} $(( ${CumulPeriod} * ${PeriodLengthInMonths} ))M )
    ;;
  *D|*d)
    Length=$( IGCM_date_DaysInCurrentPeriod ${DateBegin} $(( ${CumulPeriod} * ${PeriodLengthInDays} ))D )
    ;;
  esac
  VerifiedPeriodDateEnd=$( IGCM_date_AddDaysToGregorianDate ${DateBegin} ${Length}-1 )

  if [ ${VerifiedPeriodDateEnd} != ${PeriodDateEnd} ] ; then
    IGCM_debug_Print 1 "From run.card PeriodDateEnd is not consistent with DateBegin and CumulPeriod."
    IGCM_debug_Print 1 "We have DateBegin = ${DateBegin}"
    IGCM_debug_Print 1 "We have CumulPeriod = ${CumulPeriod}"
    IGCM_debug_Print 1 "We have PeriodDateEnd = ${PeriodDateEnd}"
    IGCM_debug_Print 1 "We have VerifiedPeriodDateEnd = ${VerifiedPeriodDateEnd}"
    IGCM_debug_Print 1 "You must have change run.card in an inconsistent way."

    IGCM_debug_Exit "STOP here to avoid further issues."
  fi

  # PeriodDateBegin + PeriodLength = PeriodDateEnd
  VerifiedPeriodDateBegin=$( IGCM_date_AddDaysToGregorianDate ${VerifiedPeriodDateEnd} $(( ${PeriodLengthInDays} * -1 )) )

  IGCM_debug_PopStack "IGCM_config_DateCoherency"
}

#===================================
function IGCM_config_StateCheck
{
  IGCM_debug_PushStack "IGCM_config_StateCheck"

    #Test state of run in run.card
    IGCM_card_DefineVariableFromOption ${SUBMIT_DIR}/run.card Configuration PeriodState

    if [ ${run_Configuration_PeriodState} = "Fatal" ] ; then
      echo
      IGCM_debug_Print 1 "!! Error in run.card with PeriodState : " ${run_Configuration_PeriodState} "!!"
      IGCM_debug_Print 1 "Check the overall status of your simulation by visiting this page:"
      IGCM_debug_Print 1 "http://prodiguer-test-web.ipsl.fr/static/simulation.detail.html?uid=${simuid}"
      IGCM_debug_Print 1 "Then try running ${libIGCM}/clean_month.job to rerun one period"
      IGCM_debug_Print 1 "Then try running ${libIGCM}/clean_year.job to rerun more."
      IGCM_debug_Exit
    elif [ $( echo ${run_Configuration_PeriodState} | grep Fatal | wc -l ) -eq 1 ] ; then
      echo
      IGCM_debug_Print 1 "!! Error in run.card with PeriodState : " ${run_Configuration_PeriodState} "!!"
      IGCM_debug_Print 1 "Compute jobs has been stop because at least the above mentionned post-processing jobs fails."
      IGCM_debug_Print 1 "Check post-processing jobs carefully by visiting this page:"
      IGCM_debug_Print 1 "http://prodiguer-test-web.ipsl.fr/static/simulation.detail.html?uid=${simuid}"
      IGCM_debug_Print 1 "Please visit that page to see how to fix issues:"
      IGCM_debug_Print 1 "https://forge.ipsl.jussieu.fr/igcmg_doc/wiki/DocGmonitor"
      IGCM_debug_Print 1 "Then try running ${libIGCM}/clean_month.job to rerun one period"
      IGCM_debug_Print 1 "Then try running ${libIGCM}/clean_year.job to rerun more."
      IGCM_debug_Exit
    fi

  IGCM_debug_PopStack "IGCM_config_StateCheck"
}

#===================================
function IGCM_config_Check
{
  IGCM_debug_PushStack "IGCM_config_Check"

  # If one of the following modulo is not zero :
  # we will issue an error then explain and exit in
  # AA_job IGCM_debug_Verif_Exit call before binary submission

  echo
  IGCM_debug_Print 1 "IGCM_config_Check"
  echo

  typeset i

  # Check RebuildFrequency against key frequencies : PeriodLength ; PackFrequency ; TimeSeriesFrequency ; SeasonalFrequency
  if ( [ ! X${config_Post_RebuildFrequency} = X${NULL_STR} ] && [ ! X${config_Post_RebuildFrequency} = XNONE ] ) ; then
    AsynchronousRebuild=true
    IGCM_debug_Print 1 "Asynchronous rebuild has been activated."
    echo
    # modulo (RebuildFrequency and PeriodLength/TimeSeriesFrequency/SeasonalFrequency) must be zero
    IGCM_debug_Print 1 "Check coherence between RebuildFrequency and PeriodLength"
    IGCM_post_CheckModuloFrequency config_Post_RebuildFrequency config_UserChoices_PeriodLength
    IGCM_debug_Print 1 "Check coherence between PackFrequency and RebuildFrequency"
    IGCM_post_CheckModuloFrequency config_Post_PackFrequency config_Post_RebuildFrequency
    IGCM_debug_Print 1 "Check coherence between TimeSeriesFrequency and RebuildFrequency"
    IGCM_post_CheckModuloFrequency config_Post_TimeSeriesFrequency config_Post_RebuildFrequency
    IGCM_debug_Print 1 "Check coherence between SeasonalFrequency and RebuildFrequency"
    IGCM_post_CheckModuloFrequency config_Post_SeasonalFrequency config_Post_RebuildFrequency
  else
    AsynchronousRebuild=false
    IGCM_debug_Print 1 "Asynchronous rebuild has not been activated"
    IGCM_debug_Print 1 "Proceed with standard post-treatment pathway"
    echo
    #modulo (PeriodLength and TimeSeriesFrequency/SeasonalFrequency) must be zero
    IGCM_debug_Print 1 "Check coherence between TimeSeriesFrequency and PeriodLength"
    IGCM_post_CheckModuloFrequency config_Post_TimeSeriesFrequency config_UserChoices_PeriodLength
    IGCM_debug_Print 1 "Check coherence between SeasonalFrequency and PeriodLength"
    IGCM_post_CheckModuloFrequency config_Post_SeasonalFrequency   config_UserChoices_PeriodLength
  fi

  # Check PackFrequency against other key frequencies
  # Modulo (PackFrequency and TimeSeriesFrequency/SeasonalFrequency and PeriodLenght) must be zero
  if ( [ ! X${config_Post_PackFrequency} = X${NULL_STR} ] && [ ! X${config_Post_PackFrequency} = XNONE ] ) ; then
    Pack=true
    #
    IGCM_debug_Print 1 "Check coherence between PackFrequency and PeriodLength"
    IGCM_post_CheckModuloFrequency config_Post_PackFrequency config_UserChoices_PeriodLength
    IGCM_debug_Print 1 "Check coherence between TimeSeriesFrequency and PackFrequency"
    IGCM_post_CheckModuloFrequency config_Post_TimeSeriesFrequency config_Post_PackFrequency
    IGCM_debug_Print 1 "Check coherence between SeasonalFrequency and PackFrequency"
    IGCM_post_CheckModuloFrequency config_Post_SeasonalFrequency config_Post_PackFrequency
  else
    Pack=false
  fi

  # modulo (TimeSeriesFrequency and all Chunck2D) must be zero
  NbJob=${#CHUNCK2D_SIZE[@]}
  i=0
  until [ $i -ge $NbJob ]; do
    value=${CHUNCK2D_SIZE[${i}]}
    IGCM_debug_Print 1 "Check coherence between ${CHUNCK2D_NAME[${i}]} Chunck2D frequency and TimeSeriesFrequency"
    IGCM_post_CheckModuloFrequency value config_Post_TimeSeriesFrequency
    case ${value} in
    *Y|*y) ;;
    *)
      IGCM_debug_Print 1 "All ChunckJob2D frequency must be expressed in year *Y|*y in comp.card"
      IGCM_debug_Exit "This will stop the job" ;;
    esac
    (( i=i+1 ))
  done

  # modulo (TimeSeriesFrequency and all Chunck3D) must be zero
  NbJob=${#CHUNCK3D_SIZE[@]}
  i=0
  until [ $i -ge $NbJob ]; do
    value=${CHUNCK3D_SIZE[${i}]}
    IGCM_debug_Print 1 "Check coherence between ${CHUNCK3D_NAME[${i}]} Chunck3D frequency and TimeSeriesFrequency"
    IGCM_post_CheckModuloFrequency value config_Post_TimeSeriesFrequency
    case ${value} in
    *Y|*y) ;;
    *)
      IGCM_debug_Print 1 "All ChunckJob3D frequency must be expressed in year *Y|*y in comp.card"
      IGCM_debug_Exit "This will stop the job" ;;
    esac
    (( i=i+1 ))
  done

  # check to be sure there is enough space on temporary filesystems to run
  echo
  IGCM_debug_Print 1 "Check if there is enough space on temporary filesystem"
  IGCM_sys_check_quota

  # check to be sure that RUN_DIR_PATH, that will be removed is not pointing to an important directory
  echo
  IGCM_debug_Print 1 "Check where RUN_DIR_PATH variable is pointing to"
  IGCM_sys_check_path


  IGCM_debug_PopStack "IGCM_config_Check"
}

#===================================
function IGCM_config_ConfigureExecution
{
  IGCM_debug_PushStack " IGCM_config_ConfigureExecution"

  #echo
  IGCM_debug_Print 1 " IGCM_config_ConfigureExecution"
  #echo

  typeset ExeNameIn ExeNameFirst CompNameFirst configCardPath comp i
  typeset tempvar tempvarMPI tempvarNOD NbElts NbExec

  # config.card path
  configCardPath=$1

  coreNumber=0
  mpiTasks=0
  openMPthreads=0
  NbExec=0

  OK_PARA_MPI=false
  OK_PARA_OMP=false
  OK_PARA_NOD=false
  OK_PARA_MPMD=false

  for comp in ${config_ListOfComponents[*]} ; do

    # Manage component executable
    IGCM_card_DefineArrayFromOption ${configCardPath} Executable ${comp}

    eval ExeNameIn=\${config_Executable_${comp}[0]}

    # NO order in config.card for parallelized values !
    # just use suffix : MPI , OMP and NOD (for number of NODes.)

    # NOD is the number of NODes allocated
    eval ${comp}_PROC_NOD=0

    # MPI is the number of MPI processus per nodes
    eval ${comp}_PROC_MPI=0

    # OMP is the number of OpenMP threads per MPI processus
    eval ${comp}_PROC_OMP=0

    # Only if we really have an executable for the component :
    if ( [ "X${ExeNameIn}" != X\"\" ] && [ "X${ExeNameIn}" != "Xinca.dat" ] ) ; then

      IGCM_debug_Print 1 ${comp}

      # Keep the first executable found and the first CompName
      ExeNameFirst=${ExeNameIn}
      CompNameFirst=${comp}

      # Are we a second executable?
      (( NbExec = NbExec + 1 ))

      # set 1 MPI task, 1 OpenMP thread and 1 node as default
      eval ${comp}_PROC_MPI=1
      eval ${comp}_PROC_OMP=1
      eval ${comp}_PROC_NOD=1

      eval NbElts=\${#config_Executable_${comp}[@]}

      if [ ${NbElts} -gt 2 ] ; then
        #
        # CURRENT METHOD TO SPECIFY MPI AND OMP RESSOURCES
        #
        i=2
        while [ ${i} -lt ${NbElts} ] ; do
          eval tempvar=\${config_Executable_${comp}[${i}]}
          IGCM_debug_Print 2 ${tempvar}

          if [ X${tempvar} = X ] ; then
            IGCM_debug_Print 2 "Error reading MPI/OMP parameters !!!"
            IGCM_debug_Exit "Check your config.card. Exit now"
            IGCM_debug_Verif_Exit
          fi

          case ${tempvar} in
          *[mM][pP][iI]*)
            # Read MPI parameter for composante
            eval ${comp}_PROC_MPI=$( echo ${tempvar} | tr '[a-z]' '[A-Z]' | sed -e "s/MPI//" )
            OK_PARA_MPI=true;;
          *[oO][mM][pP]*)
            # Read OMP parameter for composante
            eval ${comp}_PROC_OMP=$( echo ${tempvar} | tr '[a-z]' '[A-Z]' | sed -e "s/OMP//" )
            ;;
          *[nN][oO][dD]*)
            # Read NOD (NumBer of Nodes) parameter for composante
            eval ${comp}_PROC_NOD=$( echo ${tempvar} | tr '[a-z]' '[A-Z]' | sed -e "s/NOD//" )
            OK_PARA_NOD=true
            OK_PARA_MPI=true
            ;;
          esac
          (( i = i + 1 ))
        done
      else
        #
        # BACKWARD COMPATIBILITY NOT SUPPORTED ANYMORE
        #
        IGCM_debug_Exit "You are using a deprecated ressources specification mechanism in your config.card"
        IGCM_debug_Exit "Please check : https://forge.ipsl.jussieu.fr/igcmg_doc/wiki/DocEsetup#ThesectionExecutable"
        IGCM_debug_Exit "Please modify ${configCardPath}"
        exit
      fi
      eval tempvarMPI=\${${comp}_PROC_MPI}
      eval tempvarNOD=\${${comp}_PROC_NOD}
      eval tempvarOMP=\${${comp}_PROC_OMP}

      # set OMP mode if more than 1 OMP thread.
      [ ${tempvarOMP} -ge 2 ] && OK_PARA_OMP=true

      # Number of OMP threads
      [ ${openMPthreads} -lt ${tempvarOMP} ] && openMPthreads=${tempvarOMP}

      # SUM UP NUMBER OF CORES
      (( coreNumber = coreNumber + tempvarMPI * tempvarNOD * tempvarOMP ))

      # SUM UP NUMBER OF MPI TASKS
      (( mpiTasks = mpiTasks + tempvarMPI * tempvarNOD ))
    fi
  done

  # MANDATORY FOR THE OPA9.DRIVER. USED TO EDIT OPA NAMELIST
  # WE SHOULD PLANIFY NUM_PROC_??? DEPRECATION
  NUM_PROC_CPL=${CPL_PROC_MPI}
  NUM_PROC_OCE=${OCE_PROC_MPI}
  NUM_PROC_ATM=${ATM_PROC_MPI}

  # set MPMD mode if more than 2 executable names.
  [ ${NbExec} -ge 2 ] && OK_PARA_MPMD=true  

  # Define the execution type we are running in
  if ( ${OK_PARA_MPMD} ) ; then
    if ( ${OK_PARA_MPI} ) ; then
      # MPMD always implies MPI
      executionType=1
    fi
    if ( ${OK_PARA_OMP} ) ; then
      # MPMD + MPI/OMP
      executionType=2
    fi
  else
    if ( ( ${OK_PARA_MPI} ) && ( ${OK_PARA_OMP} ) ) ; then
      # SPMD + MPI/OMP
      executionType=3
    elif ( ( ${OK_PARA_MPI} ) && ( ! ${OK_PARA_OMP} ) ) ; then
      # SPMD + MPI only
      executionType=4
    elif ( ( ! ${OK_PARA_MPI} ) && ( ${OK_PARA_OMP} ) ) ; then
      # SPMD + OMP only
      executionType=5
    elif ( ( ! ${OK_PARA_MPI} ) && ( ! ${OK_PARA_OMP} ) ) ; then
      # SEQUENTIAL THEN
      executionType=6
      coreNumber=1
    fi
  fi

  IGCM_debug_Print 1 "MPI/OMP treatment coreNumber = ${coreNumber}"
  IGCM_debug_Print 1 "MPI/OMP treatment mpiTasks = ${mpiTasks}"
  IGCM_debug_Print 1 "MPI/OMP treatment openMPthreads = ${openMPthreads}"
  IGCM_debug_Print 1 "MPI/OMP treatment executionType = ${executionType}"

  IGCM_debug_PopStack "IGCM_config_ConfigureExecution"
}

#===================================
function IGCM_config_PeriodStart
{
  IGCM_debug_PushStack "IGCM_config_PeriodStart"

  echo
  IGCM_debug_Print 1 "IGCM_config_PeriodStart"
  echo

  if ( ${FirstInitialize} ) ; then
    #================================================#
    #         Initialize date/period information     #
    #================================================#

    IGCM_date_GetYearMonthDay ${DateBegin} year month day
    IGCM_config_DaysInPeriodLength

    PeriodDateBegin=${DateBegin}
    PeriodDateEnd=$( IGCM_date_AddDaysToGregorianDate ${DateBegin} $(( ${PeriodLengthInDays} - 1 )) )
    CumulPeriod=1

    #=================================================#
    #              Write updated run.card             #
    #=================================================#

    #Correct run.card Configuration for this period
    IGCM_card_WriteOption ${SUBMIT_DIR}/run.card Configuration PeriodDateBegin ${PeriodDateBegin}
    IGCM_card_WriteOption ${SUBMIT_DIR}/run.card Configuration PeriodDateEnd ${PeriodDateEnd}
    IGCM_card_WriteOption ${SUBMIT_DIR}/run.card Configuration CumulPeriod ${CumulPeriod}
    if [ X$( grep "SubmitPath" ${SUBMIT_DIR}/run.card ) != X ] ; then
      IGCM_card_WriteOption ${SUBMIT_DIR}/run.card Configuration SubmitPath ${SUBMIT_DIR}
    fi

    IGCM_card_WriteOption ${SUBMIT_DIR}/run.card Configuration PeriodState "Running"

  else
    #================================================#
    #         The file run.card allready exist       #
    #================================================#

    # Test state of run in run.card. Will schedule an exit if another process setted it to "Fatal"
    IGCM_config_StateCheck
    # And EXIT if not OK
    IGCM_debug_Verif_Exit

    #===================================#
    #        Read updated run.card      #
    #===================================#

    IGCM_card_DefineVariableFromOption ${SUBMIT_DIR}/run.card Configuration PeriodDateBegin
    IGCM_card_DefineVariableFromOption ${SUBMIT_DIR}/run.card Configuration PeriodDateEnd
    IGCM_card_DefineVariableFromOption ${SUBMIT_DIR}/run.card Configuration CumulPeriod

    PeriodDateBegin=$( IGCM_date_ConvertFormatToGregorian ${run_Configuration_PeriodDateBegin} )
    PeriodDateEnd=$( IGCM_date_ConvertFormatToGregorian ${run_Configuration_PeriodDateEnd} )
    CumulPeriod=${run_Configuration_CumulPeriod}

    LastPeriodDateEnd=$( IGCM_date_AddDaysToGregorianDate $( IGCM_date_ConvertFormatToGregorian ${PeriodDateBegin} ) -1 )

    if [ ${Period} = 1 ]; then
      # save last Job output and current run.card
      typeset Potential
      IGCM_sys_Cd ${SUBMIT_DIR}
      #
      IGCM_debug_Print 2 "Save previous ksh job output"
      for Potential in $( ls ${Script_Output_Prefix}_${config_UserChoices_JobName}.[0-9][0-9][0-9][0-9][0-9][0-9] ) ; do
        if [ X${Pack} = Xtrue ] ; then
          ( IGCM_sys_TestFileBuffer  ${R_BUF_KSH}/${Potential} ) || IGCM_sys_Cp ${Potential} ${R_BUF_KSH}/${Potential}.$$
        else
          ( IGCM_sys_TestFileArchive ${R_OUT_KSH}/${Potential} ) || IGCM_sys_Cp ${Potential} ${R_OUT_KSH}/${Potential}.$$
        fi
      done
      #
      IGCM_debug_Print 2 "Save current run.card"
      IGCM_card_CheckConflict run.card
      if [ X${Pack} = Xtrue ] ; then
        IGCM_sys_Cp ${SUBMIT_DIR}/run.card ${R_BUF_KSH}/run.card
      else
        IGCM_sys_Cp ${SUBMIT_DIR}/run.card ${R_OUT_KSH}/run.card
      fi
      #
      IGCM_sys_Cd ${RUN_DIR}
    else
      unset FileToBeDeleted
    fi

    # Determine number of day(s) in PeriodLength
    IGCM_date_GetYearMonthDay $PeriodDateBegin year month day
    IGCM_config_DaysInPeriodLength

    # Check coherency between (PeriodDateBegin, PeriodDateEnd) and (DateBegin, CumulPeriod, PeriodLength)
    IGCM_config_DateCoherency
    # And EXIT if not OK
    IGCM_debug_Verif_Exit

    # Test state of run in run.card. Will schedule an exit if another process setted it to "Fatal"
    IGCM_config_StateCheck
    # And EXIT if not OK
    IGCM_debug_Verif_Exit

    # We can say we are "Running" now.
    IGCM_card_WriteOption ${SUBMIT_DIR}/run.card Configuration PeriodState "Running"
  fi

  # BEGIN: SHOULD GO IN A FUNCTION FROM libIGCM_date.ksh
  # Compute year_m1 and year_p1 (year minus 1Y and year plus 1Y)
  year_m1=$(( year - 1 ))
  year_p1=$(( year + 1 ))
  # Compute month_m1 (month minus 1M)
  # Compute yyyymm_m1 (yyyymm minus 1M)
  month_m1=$(( 10#${month} - 1 ))
  if [ ${month_m1} = 0 ]; then
    month_m1=12
    yyyymm_m1=${year_m1}12
  elif [ ${month_m1} -le 9 ]; then
    month_m1=0${month_m1}
    yyyymm_m1=${year}${month_m1}
  else
    yyyymm_m1=${year}${month_m1}
  fi
  # Compute month_p1 (month plus 1M)
  # Compute yyyymm_p1 (yyyymm plus 1M)
  month_p1=$(( 10#${month} + 1 ))
  if [ ${month_p1} = 13 ]; then
    month_p1=01
    yyyymm_p1=${year_p1}01
  elif [ ${month_p1} -le 9 ]; then
    month_p1=0${month_p1}
    yyyymm_p1=${year}${month_p1}
  else
    yyyymm_p1=${year}${month_p1}
  fi
  #IGCM_debug_Print 1 "jg 1 month_m1 = ${month_m1} month_p1 = ${month_p1} "
  #IGCM_debug_Print 1 "jg 1 calculate yyyymm_m1 = ${yyyymm_m1} "
  #IGCM_debug_Print 1 "jg 1 calculate yyyymm_p1 = ${yyyymm_p1} "

  #===================================================================#
  # Calculate CyclicYear to be used for looping over a given forcing  #
  # period. Add CyclicBegin and CyclicEnd in config.card UserChoices. #
  #===================================================================#

  # To use the variable CyclicYear, one must add in config.card CyclicBegin and CyclicEnd.
  # CyclicBegin is the first year in the cycle. CyclicEnd is the last year included in the cycle.
  if ( [ ! X${config_UserChoices_CyclicBegin} = X ] && [ ! X${config_UserChoices_CyclicEnd} = X ] ) ; then
    CycleNb=$(( ${config_UserChoices_CyclicEnd} - ${config_UserChoices_CyclicBegin} + 1 ))
    CyclicYear_p1=NOTDEFINED

    # For current year
    yeartmp=$year
    diffy=$(( $yeartmp - ${config_UserChoices_CyclicBegin} ))
    while [ $diffy -lt 0 ] ; do
      yeartmp=$(( ${yeartmp} + ${CycleNb} ))
      diffy=$(( $yeartmp - ${config_UserChoices_CyclicBegin} ))
    done
    CyclicYear=$(( ( ${diffy} % ${CycleNb} ) + ${config_UserChoices_CyclicBegin} ))

    # For next coming year
    yeartmp=$(( $year + 1 ))
    diffy=$(( $yeartmp - ${config_UserChoices_CyclicBegin} ))
    while [ $diffy -lt 0 ] ; do
      yeartmp=$(( ${yeartmp} + ${CycleNb} ))
      diffy=$(( $yeartmp - ${config_UserChoices_CyclicBegin} ))
    done
    CyclicYear_p1=$(( ( ${diffy} % ${CycleNb} ) + ${config_UserChoices_CyclicBegin} ))

    IGCM_debug_Print 1 "CyclicYear   = ${CyclicYear}, CyclicYear_p1 = ${CyclicYear_p1}, current year=$year"
  else
    CyclicYear="ERROR_CyclicYear_Variable_Not_Defined"
    CyclicYear_p1="ERROR_CyclicYear_p1_Variable_Not_Defined"
    IGCM_debug_Print 1 "CyclicYear wont be use without adding CyclicBegin and CyclicEnd in config.card"
  fi

  # END: SHOULD GO IN A FUNCTION FROM libIGCM_date.ksh

  #===================================================================#
  # Prepare variables available for ${COMP}.card and ${COMP}.driver   #
  #             But available to any son functions                    #
  #===================================================================#

  # Period Length In Days between DateBegin and DateCurrent (at end of period == PeriodDateEnd !)
  (( SimulationLengthInDays = $( IGCM_date_DaysBetweenGregorianDate ${PeriodDateEnd} ${DateBegin} ) + 1 ))

  # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_config_PeriodStart : Before Execution"
  IGCM_debug_Print 1 "Year of simulation      : ${year}"
  IGCM_debug_Print 1 "Month of simulation     : ${month}"
  IGCM_debug_Print 1 "PeriodLengthInDays      : ${PeriodLengthInDays}"
  IGCM_debug_Print 1 "PeriodDateBegin         : ${PeriodDateBegin}"
  IGCM_debug_Print 1 "PeriodDateEnd           : ${PeriodDateEnd}"
  IGCM_debug_Print 1 "SimulationLengthInDays  : ${SimulationLengthInDays}"
  IGCM_debug_Print 1 "ExperienceLengthInDays  : ${ExperienceLengthInDays}"

  #================================================================#
  #         Prepare variables available for comp_finalyze          #
  #================================================================#

  # Period for save files
  DatesPeriod=${PeriodDateBegin}_${PeriodDateEnd}

  # Prefix for save files of this period
  PREFIX=${config_UserChoices_JobName}_${DatesPeriod}

  # List of files that will be deleted in RUN_DIR after run
  [ -f stack ] && FileToBeDeleted[0]="stack"

  # Test if the same run as already been saved :
  if [ X${JobType} = XRUN ] ; then
    if [ ${DRYRUN} -le 0 ] ; then
      if ( IGCM_sys_TestFileBuffer ${R_BUF_KSH}/${PREFIX}_${Exe_Output} ) ; then
        IGCM_debug_Exit "IGCM_config_PeriodStart" "You are currently RErunning an old job."
        IGCM_debug_Print 1 "Because of readonly permissions, you can't RErun a job when saved files"
        IGCM_debug_Print 1 " are still in the ARCHIVE directory. You must deleted those files, or "
        IGCM_debug_Print 1 " the whole ${R_SAVE} tree. See clean_month.job in ${libIGCM} directory."
        IGCM_debug_Print 1 " This exit has been initiated because at least ${R_BUF_KSH}/${PREFIX}_${Exe_Output} exists."
        IGCM_debug_Verif_Exit
      fi
    fi
  else
    if ( IGCM_sys_TestFileBuffer ${R_BUF_KSH}/${PREFIX}_${Exe_Output} ) ; then
      IGCM_debug_Print 1 "IGCM_config_PeriodStart" "RErun an old job. Allowed in DEBUG or DEV mode."
    fi
  fi

  #================================================================#
  #       Prepare variables available for binary execution         #
  #       Call function for creation of run script                 #
  #       Only done once per job                                   #
  #================================================================#

  if [ ${Period} -eq 1 ]; then
    # Define the execution context (MPMD, SPMD, MPI/OMP ...)
    IGCM_config_ConfigureExecution ${SUBMIT_DIR}/config.card
    # Create the execution script for the current context
    IGCM_sys_build_execution_scripts
  fi

  ExecutionFail=false

  IGCM_debug_PopStack "IGCM_config_PeriodStart"
}

#===================================
function IGCM_config_SaveSourceModifications
{
  IGCM_debug_PushStack "IGCM_config_SaveSourceModifications"

  typeset ExeOutDateMax listVarEnv
  ExeOutDateMax=$1

  listVarEnv="ExeOutDateMax,R_OUT_EXE,PREFIX,SUBMIT_DIR"
  IGCM_sys_RshMaster "\
    . ${libIGCM}/libIGCM_sys/libIGCM_sys.ksh; \
    export ExeOutDateMax=${ExeOutDateMax};\
    export R_OUT_EXE=${R_OUT_EXE};\
    export PREFIX=${PREFIX};\
    export SUBMIT_DIR=${SUBMIT_DIR};\
    export listVarEnv=${listVarEnv};\
    Script_Output=out_SaveSourceModifications;\
    IGCM_sys_Qsub ${libIGCM}/SaveSourceModifications.job ${ExeOutDateMax} ${R_OUT_EXE} ${PREFIX} ${SUBMIT_DIR}"

  IGCM_debug_PopStack "IGCM_config_SaveSourceModifications"
}

#===================================
function IGCM_config_PeriodEnd
{
  IGCM_debug_PushStack "IGCM_config_PeriodEnd"

  echo
  IGCM_debug_Print 1 "IGCM_config_PeriodEnd"
  echo

  if [ ${DRYRUN} -le 1 ] ; then

    IGCM_debug_Print 1 "Check components binary : size and creation date"

    typeset LS_comp LS_bin ExeDate ExeCpuLog NextExeSize LastCompExeSize
    typeset comp i
    typeset ExeNameIn ExeNameOut UpdateExe ExeSecDateMax

    #==================================#
    #        Get last Exe Size         #
    #==================================#

    (( i=0 ))
    if ( ${FirstInitialize} ) ; then
      run_Log_LastExeSize=""
      for comp in ${config_ListOfComponents[*]} ; do
        run_Log_LastExeSize[$i]=0
        (( i=i+1 ))
      done
    else
      IGCM_card_DefineArrayFromOption ${SUBMIT_DIR}/run.card Log LastExeSize
    fi
    #==================================#
    #         And Build ExeDate        #
    #==================================#

    # ExeDate = ATM_Jun_12_09:34-SRF_Jun_12_09:34-OCE_Jun_12_09:34-ICE_Jun_12_09:34-CPL_Jun_12_09:33
    # Would be nice to have next line but no way to format ls output (need to ls -l --time-style "+%Y-%m-%dT%H:%M")
    # ExeDate = ATM_2009-06-12T09:34+SRF_2009-06-12T09:34+OCE_2009-06-12T09:34+ICE_2009-06-12T09:34+CPL_2009-06-12T09:34
    ExeDate=""
    NextExeSize="( "
    (( i=0 ))
    UpdateExe=false
    (( ExeSecDateMax = 0 ))
    for comp in ${config_ListOfComponents[*]} ; do

      IGCM_debug_Print 3 ${comp}

      eval ExeNameIn=\${config_Executable_${comp}[0]}
      eval ExeNameOut=\${config_Executable_${comp}[1]}
      # Only if we really have an executable for the component :
      if [ X${ExeNameIn} = X\"\" ] ; then
        # If there is no exe file for this component
        (( ExeSize=0 ))
      else
        LS_bin=${R_EXE}/${ExeNameIn}
        IGCM_sys_FileSize ${LS_bin} ExeSize

        set +A LS_comp -- $( LC_TIME=en_US ls -l ${LS_bin} )
        if [ X${ExeDate} = X ] ; then
          # First component exe date
          ExeDate=${comp}_${LS_comp[5]}_${LS_comp[6]}
        else
          ExeDate=${ExeDate}-${comp}_${LS_comp[5]}_${LS_comp[6]}
        fi
        ExeDate=${ExeDate}_${LS_comp[7]}
      fi

      if [ ${i} -eq 0 ] ; then
        # First component
        NextExeSize="( "${ExeSize}
      else
        NextExeSize=${NextExeSize}", "${ExeSize}
      fi
      LastCompExeSize=${run_Log_LastExeSize[$i]}
      (( i=i+1 ))

      if [ ${ExeSize} -ne ${LastCompExeSize} ] ; then
        if ( ${FirstInitialize} ) ; then
          IGCM_debug_Print 1 "Save first ${ExeNameIn} in ${R_OUT_EXE} !"
        else
          IGCM_debug_Print 1 "${ExeNameIn} has changed in ${R_EXE} !"
          IGCM_debug_Print 1 "Save latest ${ExeNameIn} in ${R_OUT_EXE} !"
          FileToBeDeleted[${#FileToBeDeleted[@]}]=${ExeNameOut}
        fi
        IGCM_sys_Put_Out ${ExeNameOut} ${R_OUT_EXE}/${PREFIX}_${ExeNameIn} rw
        UpdateExe=true

        # SD : switch off for now
        #IGCM_sys_GetDate_FichWork ${LS_bin} ExeSecDate
        #if [ $ExeSecDateMax -lt $ExeSecDate ] ; then
        #  ExeSecDateMax=$ExeSecDate
        #fi
      fi
    done

    # SD : switch off for now
    #if ( ${UpdateExe} ) ; then
    #  echo "Launch SaveSourceModifications."
    #  IGCM_config_SaveSourceModifications ${ExeSecDateMax}
    #fi

    NextExeSize=${NextExeSize}" )"
    IGCM_card_WriteOption ${SUBMIT_DIR}/run.card Log LastExeSize "${NextExeSize}"

    if [ ${DRYRUN} -le 1 ] ; then
      tail -1500 ${Exe_Output} > ${Exe_Output}_tail.txt
      ExeCpuLog=$( gawk -f ${libIGCM}/libIGCM_sys/IGCM_add_out.awk ${Exe_Output}_tail.txt )
      RET=$?
      if [ $RET -eq 0 ] ; then
        # ExeCpuLog variable contents 5 fields
        echo "${CumulPeriod} ${PeriodDateBegin} ${PeriodDateEnd} ${ExeCpuLog} ${ExeDate}" |   \
          gawk '{printf("# %11d | %15s | %15s | %19s | %19s | %15.5f | %15.5f | %15.5f | %s\n", \
            $1,$2,$3,$4,$5,$6,$7,$8,$9)}' >> ${SUBMIT_DIR}/run.card
      fi
      FileToBeDeleted[${#FileToBeDeleted[@]}]=${Exe_Output}_tail.txt
    fi

  fi

  #==================================#
  #         Save Job output          #
  #==================================#
  if [ X${Pack} = Xtrue ] ; then
    IGCM_sys_PutBuffer_Out ${Exe_Output} ${R_BUF_KSH}/${PREFIX}_${Exe_Output}
  else
    IGCM_sys_Put_Out ${Exe_Output} ${R_OUT_KSH}/${PREFIX}_${Exe_Output}
  fi
  FileToBeDeleted[${#FileToBeDeleted[@]}]=${Exe_Output}

  # All was right ? no ? then we stop.
  IGCM_debug_Verif_Exit

  # If all was OK, we can delete all files not necessary for next Job
  echo
  IGCM_debug_Print 1 "Files that will be deleted before next period-run : "

  if [ ${DRYRUN} -le 2 ] ; then
    for f in ${FileToBeDeleted[@]} ; do [ -f ${f} ] && ls -la $f ; [ -f ${f} ] && rm -f $f ; done
  else
    echo ${FileToBeDeleted[@]}
  fi

  # Send some accounting element to the user if CumulPeriod=3
  if [ ${CumulPeriod} -eq 3 ] ; then
    echo
    IGCM_debug_Print 1 "Send email containing some accounting information : "

    RealCpuTime=$( echo ${ExeCpuLog} | gawk '{print $3}' )

    consumeHoursPerPeriod=$( echo "scale=6;${RealCpuTime}*${coreNumber}/3600" | bc )

    consumeHoursPerWholeSimulation=$( echo "scale=6;${consumeHoursPerPeriod}/${PeriodLengthInDays}*${ExperienceLengthInDays}" | bc )

    recommendedPeriodNb=$( echo "scale=6;${jobWarningDelay}/3600/${consumeHoursPerPeriod}*${coreNumber}" | bc )

    IGCM_sys_SendMail Accounting
  fi

  #=================================================#
  #         Modification of libIGCM behaviour       #
  #=================================================#

  # To use this function, one must copy libIGCM.card from ${libIGCM} directory
  # and put it in ${SUBMIT_DIR} directory. After modifications of ${SUBMIT_DIR}/libIGCM.card,
  # variables define inside [UserChanges] will be modified for next Period of libIGCM main loop.
  if [ -f ${SUBMIT_DIR}/libIGCM.card ] ; then
    echo
    echo "########################################################################"
    echo "!!!                 Modification of libIGCM behaviour                !!!"
    echo

    IGCM_debug_Print 1 "DefineArrayFromOption  : libIGCM_UserChanges in libIGCM.card"
    IGCM_card_DefineArrayFromSection ${SUBMIT_DIR}/libIGCM.card UserChanges
    IGCM_debug_Print 2 "libIGCM_UserChanges" ${libIGCM_UserChanges[*]}

    # Special treatments for libIGCM internals
    for option in ${libIGCM_UserChanges[*]} ; do
      IGCM_card_DefineVariableFromOption ${SUBMIT_DIR}/libIGCM.card UserChanges ${option}

      echo "We will change : ${option}."
      eval echo "Previous value : " \${${option}}
      eval echo "Change to : " \${libIGCM_UserChanges_${option}}

      eval ${option}=\${libIGCM_UserChanges_${option}}

      case ${option} in
      config_UserChoices_DateEnd)
        IGCM_debug_PrintVariables 1 config_UserChoices_DateEnd
        DateEnd=$( IGCM_date_ConvertFormatToGregorian ${config_UserChoices_DateEnd} )

        # Period Length In Days between DateBegin and DateEnd
        (( ExperienceLengthInDays=$( IGCM_date_DaysBetweenGregorianDate ${DateEnd} ${DateBegin} )  + 1 ))
        if [ ${ExperienceLengthInDays} -lt 0 ] ; then
          IGCM_debug_Print 1 "Problem with dates in libIGCM.card : ${DateEnd} < ${DateBegin} ! You must check that."
          IGCM_debug_Exit "IGCM_PeriodEnd have wrong dates."
          IGCM_debug_Verif_Exit
        fi
        ;;
      config_UserChoices_PeriodLength)
        IGCM_debug_Print 1  "Change config_UserChoices_PeriodLength=${config_UserChoices_PeriodLength}"
        ;;
      PeriodNb)
        IGCM_debug_Print 1  "Loop in main Job with ${PeriodNb} period(s)"
        ;;
      config_Post_RebuildFrequency)
        IGCM_debug_Print 1  "Change config_Post_RebuildFrequency=${config_Post_RebuildFrequency} : IGCM_post_Configure"
        IGCM_post_Configure
        ;;
      config_Post_TimeSeriesFrequency)
        IGCM_debug_Print 1  "Change config_Post_TimeSeriesFrequency = ${config_Post_TimeSeriesFrequency} : IGCM_post_Configure"
        IGCM_post_Configure
        ;;
      config_Post_SeasonalFrequency)
        IGCM_debug_Print 1  "Change config_Post_SeasonalFrequency = ${config_Post_SeasonalFrequency} : IGCM_post_Configure"
        IGCM_post_Configure
        ;;
      esac
    done

    echo
    echo "########################################################################"
    echo
  fi

  #=================================================#
  #         Determine next computed period          #
  #=================================================#

  PeriodDateBegin=$( IGCM_date_AddDaysToGregorianDate ${PeriodDateEnd} 1 )
  IGCM_date_GetYearMonthDay $PeriodDateBegin year month day
  year_m1=$(( year - 1 ))
  year_p1=$(( year + 1 ))
  IGCM_config_DaysInPeriodLength
  PeriodDateEnd=$( IGCM_date_AddDaysToGregorianDate ${PeriodDateBegin} $(( ${PeriodLengthInDays} - 1 )) )

  # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_config_PeriodEnd : Preparing Next Execution"
  IGCM_debug_Print 1 "PeriodDateBegin       : ${PeriodDateBegin}"
  IGCM_debug_Print 1 "PeriodDateEnd         : ${PeriodDateEnd}"
  IGCM_debug_Print 1 "PeriodLengthInDays    : ${PeriodLengthInDays}"

  PeriodDateBegin=$( IGCM_date_ConvertFormatToHuman ${PeriodDateBegin} )
  PeriodDateEnd=$( IGCM_date_ConvertFormatToHuman ${PeriodDateEnd} )

  (( CumulPeriod = CumulPeriod + 1 ))

  # Debug Print :
  echo
  IGCM_debug_Print 3 "PeriodDateBegin Human : ${PeriodDateBegin}"
  IGCM_debug_Print 3 "PeriodDateEnd Human   : ${PeriodDateEnd}"
  IGCM_debug_Print 3 "CumulPeriod           : ${CumulPeriod}"

  #=================================================#
  #             Write updated run.card              #
  #=================================================#

  IGCM_card_WriteOption ${SUBMIT_DIR}/run.card Configuration PeriodDateBegin ${PeriodDateBegin}
  IGCM_card_WriteOption ${SUBMIT_DIR}/run.card Configuration PeriodDateEnd ${PeriodDateEnd}
  IGCM_card_WriteOption ${SUBMIT_DIR}/run.card Configuration CumulPeriod ${CumulPeriod}

  if ( ${FirstInitialize} ) ; then
    # It's no more the first time
    FirstInitialize=false
  fi

  IGCM_debug_PopStack "IGCM_config_PeriodEnd"
}

#===================================
function IGCM_config_Finalize
{
  IGCM_debug_PushStack "IGCM_config_Finalize"

  echo
  IGCM_debug_Print 1 "IGCM_config_Finalize"
  echo

  # Test state of run in run.card. Will schedule an exit if another process setted it to "Fatal"
  IGCM_config_StateCheck

  # And EXIT if not OK
  IGCM_debug_Verif_Exit

  if [ ${SimulationLengthInDays} -ge ${ExperienceLengthInDays} ] ; then
    #==========================#
    # End of entire simulation #
    #==========================#
    simulationIsOver=true

    # Mail notification
    IGCM_sys_SendMail
    #
    IGCM_card_WriteOption ${SUBMIT_DIR}/run.card Configuration PeriodState "Completed"
    #
    IGCM_debug_Print 1 "Normal End of computation."

  else
    #=================#
    # Submit next job #
    #=================#
    simulationIsOver=false

    IGCM_card_WriteOption ${SUBMIT_DIR}/run.card Configuration PeriodState "OnQueue"

    # Name of next Ksh Script output :
    Script_Output=${Script_Output_Prefix}_${config_UserChoices_JobName}.$( printf "%06d" ${CumulPeriod} )

    IGCM_debug_Print 1 "Submit next job"
    # SUBMIT NEXT JOB from SUBMIT_DIR and come back in RUN_DIR
    IGCM_sys_Cd ${SUBMIT_DIR}
    # Keep only the 5 latest ${Script_Output_Prefix}_${config_UserChoices_JobName}
    ScriptTot=$( ls ${Script_Output_Prefix}_${config_UserChoices_JobName}.?????? 2>/dev/null | wc -l )
    [ ${ScriptTot} -gt 5 ] && rm -f $( ls ${Script_Output_Prefix}_${config_UserChoices_JobName}.?????? | head -$(( ${ScriptTot} - 5 )) )
    # Submit next job and come back
    IGCM_sys_Qsub ${SUBMIT_DIR}/Job_${config_UserChoices_JobName}
    IGCM_sys_Cd -
  fi

  # Clean ${RUN_DIR}=${RUN_DIR_PATH}/${config_UserChoices_JobName}.${$}
  # Only for production run (No clean up in DEV or DEB mode)
  # and command sent from .. directory.
  IGCM_sys_Cd ..
  [ X${JobType} = XRUN ] && IGCM_sys_RmRunDir -rf ${RUN_DIR_PATH}

  # Inform the rabbitMQ queue
  IGCM_debug_BigBro_Finalize

  IGCM_debug_PopStack "IGCM_config_Finalize"
}

#===================================
