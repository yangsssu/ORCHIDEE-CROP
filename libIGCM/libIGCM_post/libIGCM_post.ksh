#!/bin/ksh

#**************************************************************
# Author: Sebastien Denvil
# Contact: Sebastien.Denvil__at__ipsl.jussieu.fr
# $Revision:: 1298                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2016-03-11 12:27:15 +0100 (Fri, 11 Mar 2016) $ Date of last commit
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************

#=======================================================================

function IGCM_post_Configure
{
  IGCM_debug_PushStack "IGCM_post_Configure"

  # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_post_Configure"
  #
  # Initialize
  #
  POST=false
  RebuildFrequency=false
  PackFrequency=false
  TimeSeriesFrequency=false
  SeasonalFrequency=false
  unset list_post
  #
  # ONLY IF SOMETHING NEEDS TO BE DONE (EATHER TIME SERIES OR SEASONAL) COMPUTE THE MODULOS
  #
  if [ X${AsynchronousRebuild} = Xtrue ] ; then
    list_post="RebuildFrequency"
  fi
  #
  if [ X${Pack} = Xtrue ] ; then
    list_post="${list_post} PackFrequency"
  fi
  #
  if ( [ X${TimeSeries} = Xtrue ] || [ X${TimeSeries2D} = Xtrue ] || [ X${TimeSeries3D} = Xtrue ] || [ X${TimeSeriesChunck2D} = Xtrue ] || [ X${TimeSeriesChunck3D} = Xtrue ] ) ; then
    list_post="${list_post} TimeSeriesFrequency"
  fi
  #
  if [ X${Seasonal} = Xtrue ] ; then
    list_post="${list_post} SeasonalFrequency"
  fi

  # Overrule Time Series triggering. Special case 1.
  if ( [ ! X${config_Post_TimeSeriesFrequency} = X${NULL_STR} ] && \
    [ ! X${config_Post_TimeSeriesFrequency} = XNONE ]           && \
    [ ${SimulationLengthInDays} -ge ${ExperienceLengthInDays} ] ) ; then
    TimeSeriesFrequency=true
    POST=true
  fi
  # Overrule Rebuild triggering. Special case 2.
  if ( [ X${AsynchronousRebuild} = Xtrue ] && [ ${SimulationLengthInDays} -ge ${ExperienceLengthInDays} ] ) ; then
    RebuildFrequency=true
    POST=true
  fi
  # Overrule Pack triggering. Special case 3.
  if ( [ X${Pack} = Xtrue ] && [ ${SimulationLengthInDays} -ge ${ExperienceLengthInDays} ] ) ; then
    PackFrequency=true
    POST=true
  fi

  # READ REBUILD OR PACK OR TIME SERIES OR SEASONAL FREQUENCY
  # AND TURN ON THE SUBMISSION FLAG WHEN MODULO IS ZERO
  for post_freq in ${list_post} ; do
    # Extract frequency from previously defined variable
    config_Post_post_freq=$( eval echo \${config_Post_${post_freq}} )
    # Offset for Seasonal Average starting period
    if [ ${post_freq} = SeasonalFrequency ] ; then
      if ( [ X${config_Post_SeasonalFrequencyOffset} = X${NULL_STR} ] || [ X${config_Post_SeasonalFrequencyOffset} = XNONE ] || [ X${config_Post_SeasonalFrequencyOffset} = X ] ) ; then
        PeriodOffset=0
      else
        PeriodOffset=${config_Post_SeasonalFrequencyOffset}
      fi
    else
      PeriodOffset=0
    fi
    # Compute Modulo between frequencys (/!\second argument will be multiplied by CumuPeriod/!\)
    # RebuildFrequency needs additionnal information
    if [ ${post_freq} = RebuildFrequency ] ; then
      IGCM_post_ModuloRuntimeFrequency config_Post_post_freq config_UserChoices_PeriodLength NbPeriodPerFrequency
      NbRebuildDir=${NbPeriodPerFrequency}
    else
      IGCM_post_ModuloRuntimeFrequency config_Post_post_freq config_UserChoices_PeriodLength
    fi
    #
    eval IGCM_debug_Print 1 \" "${post_freq} flag value : \${${post_freq}}" \"
  done
  #
  IGCM_debug_Print 2 "POST-TREATEMENT flag value : ${POST}"
  #
  # Prepare headers for the shell dedicated to post-processing
  if ( [ ${PackFrequency} = true ] && ( [ ${TimeSeriesFrequency} = true ] || [ ${SeasonalFrequency} = true ] ) ) ; then
    if [ $DRYRUN -le 1 ]; then
      echo "#!/bin/ksh                                     " >  ${R_BUFR}/FlushPost_${PeriodDateEnd}.ksh
      echo "function IGCM_FlushPost                        " >> ${R_BUFR}/FlushPost_${PeriodDateEnd}.ksh
      echo "{                                              " >> ${R_BUFR}/FlushPost_${PeriodDateEnd}.ksh
      echo "IGCM_debug_PushStack \"IGCM_FlushPost\"        " >> ${R_BUFR}/FlushPost_${PeriodDateEnd}.ksh
      echo "echo                                           " >> ${R_BUFR}/FlushPost_${PeriodDateEnd}.ksh
      echo "IGCM_debug_Print 1 \"IGCM_FlushPost\"          " >> ${R_BUFR}/FlushPost_${PeriodDateEnd}.ksh
      echo "echo                                           " >> ${R_BUFR}/FlushPost_${PeriodDateEnd}.ksh
    fi
  fi
  IGCM_debug_PopStack "IGCM_post_Configure"
}

function IGCM_post_Submit
{
  IGCM_debug_PushStack "IGCM_post_Submit"

  typeset listVarEnv DaysTemp

  POST_DIR=${R_BUF_KSH}

  if [ ${POST} = true ]; then
    echo
    IGCM_debug_Print 1 "IGCM_post_Submit"
    echo
    IGCM_debug_Print 2 "POST_DIR = ${POST_DIR}"
  fi

  #============ TIME SERIES POST-PROCESSING ===========#
  if [ ${TimeSeriesFrequency} = true ] ; then

    IGCM_debug_Print 1 "TIME SERIES POST-PROCESSING ACTIVATED"
    echo



    # Get information from last execution
    IGCM_card_DefineVariableFromOption ${SUBMIT_DIR}/run.card PostProcessing TimeSeriesCompleted
    CompletedFlag=${run_PostProcessing_TimeSeriesCompleted}
    #



    listVarEnv="DEBUG_debug,BigBrother,postProcessingStopLevel,MODIPSL,libIGCM,libIGCM_SX,SUBMIT_DIR,POST_DIR,Script_Post_Output,MASTER,RebuildFrequency,DateBegin,PeriodDateEnd,StandAlone,CompletedFlag,TsTask,CompToRead,FlagToRead,RESOL_ATM,RESOL_OCE,RESOL_ICE,RESOL_MBG,RESOL_SRF,RESOL_SBG"
    #
    jLoop=${#ListDimension[*]}
    j=0
    until [ $j -ge ${jLoop} ]; do
      Dimension=${ListDimension[${j}]}
      #
      if [ X$( eval echo \${TimeSeries${Dimension}} ) = Xtrue ] ; then
      #
        IGCM_debug_Print 1 "TIME SERIES POST-PROCESSING ${Dimension} ACTIVATED"
        echo
        #
        if [ X${Dimension} = X ] ; then
          TsTask="empty"
          Script_Post_Output=create_ts.${PeriodDateEnd}
        else
          TsTask=${Dimension}
          Script_Post_Output=create_ts.${PeriodDateEnd}.${TsTask}
        fi
        #
        if ( [ ${RebuildFrequency} = true ] || [ ${PackFrequency} = true ] ) ; then
          #
          if [ ${PackFrequency} = true ] ; then
            FunctionPath=${R_BUFR}/FlushPost_${PeriodDateEnd}.ksh
          else
            FunctionPath=${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
          fi
          #
          if [ -f ${FunctionPath} ] ; then
#           if [ X${MASTER} = Xtitane ] ; then
#             echo "IGCM_sys_RshPost <<-EOF"                       >> ${FunctionPath}
#           fi
            echo "export DEBUG_debug=${DEBUG_debug}                        " >> ${FunctionPath}
            echo "export BigBrother=${BigBrother}                          " >> ${FunctionPath}
            echo "export postProcessingStopLevel=${postProcessingStopLevel}" >> ${FunctionPath}
            echo "export MODIPSL=${MODIPSL}                                " >> ${FunctionPath}
            echo "export libIGCM_SX=${libIGCM}                             " >> ${FunctionPath}
            echo "export libIGCM=${libIGCM_POST}                           " >> ${FunctionPath}
            echo "export SUBMIT_DIR=${SUBMIT_DIR}                          " >> ${FunctionPath}
            echo "export POST_DIR=${POST_DIR}                              " >> ${FunctionPath}
            echo "export MASTER=${MASTER}                                  " >> ${FunctionPath}
            echo "export RebuildFrequency=${RebuildFrequency}              " >> ${FunctionPath}
            echo "export DateBegin=${DateBegin}                            " >> ${FunctionPath}
            echo "export PeriodDateEnd=${PeriodDateEnd}                    " >> ${FunctionPath}
            echo "export StandAlone=false                                  " >> ${FunctionPath}
            echo "export CompletedFlag=${CompletedFlag}                    " >> ${FunctionPath}
            echo "export TsTask=${TsTask}                                  " >> ${FunctionPath}
            echo "unset  CompToRead                                        " >> ${FunctionPath}
            echo "unset  FlagToRead                                        " >> ${FunctionPath}
            echo "export RESOL_ATM=${RESOL_ATM}                            " >> ${FunctionPath}
            echo "export RESOL_OCE=${RESOL_OCE}                            " >> ${FunctionPath}
            echo "export RESOL_ICE=${RESOL_ICE}                            " >> ${FunctionPath}
            echo "export RESOL_MBG=${RESOL_MBG}                            " >> ${FunctionPath}
            echo "export RESOL_SRF=${RESOL_SRF}                            " >> ${FunctionPath}
            echo "export RESOL_SBG=${RESOL_SBG}                            " >> ${FunctionPath}
            echo "export listVarEnv=${listVarEnv}                          " >> ${FunctionPath}
            echo "export Script_Post_Output=${Script_Post_Output}          " >> ${FunctionPath}
#           if [ X${MASTER} = Xtitane ] ; then
#             echo ". ${libIGCM_POST}/libIGCM_debug/libIGCM_debug.ksh" >> ${FunctionPath}
#             echo ". ${libIGCM_POST}/libIGCM_sys/libIGCM_sys.ksh"     >> ${FunctionPath}
#           fi
            echo "IGCM_sys_MkdirWork ${POST_DIR}                 " >> ${FunctionPath}
            echo "IGCM_debug_Verif_Exit                          " >> ${FunctionPath}
            echo "IGCM_sys_QsubPost create_ts                    " >> ${FunctionPath}
#           if [ X${MASTER} = Xtitane ] ; then
#             echo "EOF"                                           >> ${FunctionPath}
#           fi
          fi
        else
          IGCM_sys_RshPost <<-EOF
                    export DEBUG_debug=${DEBUG_debug}
                    export BigBrother=${BigBrother}
                    export postProcessingStopLevel=${postProcessingStopLevel}
                    export MODIPSL=${MODIPSL}
                    export libIGCM_SX=${libIGCM}
                    export libIGCM=${libIGCM_POST}
                    export SUBMIT_DIR=${SUBMIT_DIR}
                    export POST_DIR=${POST_DIR}
                    export MASTER=${MASTER}
                    export RebuildFrequency=${RebuildFrequency}
                    export DateBegin=${DateBegin}
                    export PeriodDateEnd=${PeriodDateEnd}
                    export StandAlone=false
                    export CompletedFlag=${CompletedFlag}
                    export TsTask=${TsTask}
                    unset  CompToRead
                    unset  FlagToRead
                    export RESOL_ATM=${RESOL_ATM}
                    export RESOL_OCE=${RESOL_OCE}
                    export RESOL_ICE=${RESOL_ICE}
                    export RESOL_MBG=${RESOL_MBG}
                    export RESOL_SRF=${RESOL_SRF}
                    export RESOL_SBG=${RESOL_SBG}
                    export listVarEnv=${listVarEnv}
                    export Script_Post_Output=${Script_Post_Output}
                    . ${libIGCM_POST}/libIGCM_debug/libIGCM_debug.ksh
                    . ${libIGCM_POST}/libIGCM_sys/libIGCM_sys.ksh
                    IGCM_sys_MkdirWork ${POST_DIR}
                    IGCM_debug_Verif_Exit
                    IGCM_sys_QsubPost create_ts
EOF
        fi
      fi
      #
      if [ X$( eval echo \${TimeSeriesChunck${Dimension}} ) = Xtrue ] ; then
        #
        IGCM_debug_Print 1 "TIME SERIES POST-PROCESSING ${Dimension} WITH CHUNCK ACTIVATED"
        echo
        # Need to Remember This One
        SavedDateBegin=${DateBegin}
        # Kind of task create_ts will perform
        TsTask=Chunck${Dimension}
        # Number of chunck jobs to configure and submit
        eval NbJob=\${#CHUNCK${Dimension}_COMP[@]}
        typeset i
        i=0
        until [ $i -ge $NbJob ]; do
          CompToRead=$( eval echo \${CHUNCK${Dimension}_COMP[\${i}]} )
          FlagToRead=$( eval echo \${CHUNCK${Dimension}_FLAG[\${i}]} )
          NameToRead=$( eval echo \${CHUNCK${Dimension}_NAME[\${i}]} )
          ChunckSize=$( eval echo \${CHUNCK${Dimension}_SIZE[\${i}]} )
          # Chunck Length (mandatory in Year)
          YearsChunckLength=$( echo ${ChunckSize} | sed -e "s/[yY]//" )
          #
          IGCM_date_GetYearMonth ${DateBegin}     YearBegin MonthBegin
          #
          IGCM_date_GetYearMonth ${PeriodDateEnd} YearEnd   MonthEnd
          # How many chunck in total since simulation began
          NbYearsChunckLoop=$(( ( ${YearEnd} - ${YearBegin} + 1 ) / ${YearsChunckLength} ))
          #  Tweak special case
          [ $(( ( ${YearEnd} - ${YearBegin} + 1 ) % ${YearsChunckLength} )) = 0 ] && NbYearsChunckLoop=$(( ${NbYearsChunckLoop} - 1 ))
          # Starting Year of the current chunck
          ChunckTsYearBegin=$(( ${NbYearsChunckLoop} * ${YearsChunckLength} + ${YearBegin} ))
          # Starting date of the current chunck
          ChunckTsDateBegin=${ChunckTsYearBegin}${MonthBegin}01
          #
          Script_Post_Output=create_ts.${PeriodDateEnd}.${TsTask}.${CompToRead}.${NameToRead}
          #
          if ( [ ${RebuildFrequency} = true ] || [ ${PackFrequency} = true ] ) ; then
            #
            if [ ${PackFrequency} = true ] ; then
              FunctionPath=${R_BUFR}/FlushPost_${PeriodDateEnd}.ksh
            else
              FunctionPath=${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
            fi
            #
            if [ -f ${FunctionPath} ] ; then
#             if [ X${MASTER} = Xtitane ] ; then
#               echo "IGCM_sys_RshPost <<-EOF"                       >> ${FunctionPath}
#             fi
              echo "export DEBUG_debug=${DEBUG_debug}                        " >> ${FunctionPath}
              echo "export BigBrother=${BigBrother}                          " >> ${FunctionPath}
              echo "export postProcessingStopLevel=${postProcessingStopLevel}" >> ${FunctionPath}
              echo "export MODIPSL=${MODIPSL}                                " >> ${FunctionPath}
              echo "export libIGCM_SX=${libIGCM}                             " >> ${FunctionPath}
              echo "export libIGCM=${libIGCM_POST}                           " >> ${FunctionPath}
              echo "export SUBMIT_DIR=${SUBMIT_DIR}                          " >> ${FunctionPath}
              echo "export POST_DIR=${POST_DIR}                              " >> ${FunctionPath}
              echo "export MASTER=${MASTER}                                  " >> ${FunctionPath}
              echo "export RebuildFrequency=${RebuildFrequency}              " >> ${FunctionPath}
              echo "export DateBegin=${ChunckTsDateBegin}                    " >> ${FunctionPath}
              echo "export PeriodDateEnd=${PeriodDateEnd}                    " >> ${FunctionPath}
              echo "export StandAlone=false                                  " >> ${FunctionPath}
              echo "export CompletedFlag=${CompletedFlag}                    " >> ${FunctionPath}
              echo "export TsTask=${TsTask}                                  " >> ${FunctionPath}
              echo "export CompToRead=${CompToRead}                          " >> ${FunctionPath}
              echo "export FlagToRead=${FlagToRead}                          " >> ${FunctionPath}
              echo "export RESOL_ATM=${RESOL_ATM}                            " >> ${FunctionPath}
              echo "export RESOL_OCE=${RESOL_OCE}                            " >> ${FunctionPath}
              echo "export RESOL_ICE=${RESOL_ICE}                            " >> ${FunctionPath}
              echo "export RESOL_MBG=${RESOL_MBG}                            " >> ${FunctionPath}
              echo "export RESOL_SRF=${RESOL_SRF}                            " >> ${FunctionPath}
              echo "export RESOL_SBG=${RESOL_SBG}                            " >> ${FunctionPath}
              echo "export listVarEnv=${listVarEnv}                          " >> ${FunctionPath}
              echo "export Script_Post_Output=${Script_Post_Output}          " >> ${FunctionPath}
#             if [ X${MASTER} = Xtitane ] ; then
#               echo ". ${libIGCM_POST}/libIGCM_debug/libIGCM_debug.ksh" >> ${FunctionPath}
#               echo ". ${libIGCM_POST}/libIGCM_sys/libIGCM_sys.ksh"     >> ${FunctionPath}
#             fi
              echo "IGCM_sys_MkdirWork ${POST_DIR}                 " >> ${FunctionPath}
              echo "IGCM_debug_Verif_Exit                          " >> ${FunctionPath}
              echo "IGCM_sys_QsubPost create_ts                    " >> ${FunctionPath}
#             if [ X${MASTER} = Xtitane ] ; then
#               echo "EOF"                                           >> ${FunctionPath}
#             fi
            fi
          else
            IGCM_sys_RshPost <<-EOF
                        export DEBUG_debug=${DEBUG_debug}
                        export BigBrother=${BigBrother}
                        export postProcessingStopLevel=${postProcessingStopLevel}
                        export MODIPSL=${MODIPSL}
                        export libIGCM_SX=${libIGCM}
                        export libIGCM=${libIGCM_POST}
                        export SUBMIT_DIR=${SUBMIT_DIR}
                        export POST_DIR=${POST_DIR}
                        export MASTER=${MASTER}
                        export RebuildFrequency=${RebuildFrequency}
                        export DateBegin=${ChunckTsDateBegin}
                        export PeriodDateEnd=${PeriodDateEnd}
                        export StandAlone=false
                        export CompletedFlag=${CompletedFlag}
                        export TsTask=${TsTask}
                        export CompToRead=${CompToRead}
                        export FlagToRead=${FlagToRead}
                        export RESOL_ATM=${RESOL_ATM}
                        export RESOL_OCE=${RESOL_OCE}
                        export RESOL_ICE=${RESOL_ICE}
                        export RESOL_MBG=${RESOL_MBG}
                        export RESOL_SRF=${RESOL_SRF}
                        export RESOL_SBG=${RESOL_SBG}
                        export listVarEnv=${listVarEnv}
                        export Script_Post_Output=${Script_Post_Output}
                        . ${libIGCM_POST}/libIGCM_debug/libIGCM_debug.ksh
                        . ${libIGCM_POST}/libIGCM_sys/libIGCM_sys.ksh
                        IGCM_sys_MkdirWork ${POST_DIR}
                        IGCM_debug_Verif_Exit
                        IGCM_sys_QsubPost create_ts
EOF
            #
          fi
          #
          export DateBegin=${SavedDateBegin}
          #
          (( i=i+1 ))
          #
        done
      fi
      (( j=j+1 ))
    done
  fi

  #=============  SEASONAL POST-PROCESSING ============#
  if [ ${SeasonalFrequency} = true ] ; then
    #
    IGCM_debug_Print 1 "SEASONNAL POST-PROCESSING"
    echo
    #
    Script_Post_Output=create_se.${PeriodDateEnd}
    #
    listVarEnv="DEBUG_debug,BigBrother,postProcessingStopLevel,MODIPSL,libIGCM,libIGCM_SX,SUBMIT_DIR,POST_DIR,Script_Post_Output,MASTER,DateBegin,PeriodDateEnd,StandAlone,RESOL_ATM,RESOL_OCE,RESOL_ICE,RESOL_MBG,RESOL_SRF,RESOL_SBG"

    if ( [ ${RebuildFrequency} = true ] || [ ${PackFrequency} = true ] ) ; then
      #
      if [ ${PackFrequency} = true ] ; then
        FunctionPath=${R_BUFR}/FlushPost_${PeriodDateEnd}.ksh
      else
        FunctionPath=${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      fi
      #
      if [ -f ${FunctionPath} ] ; then
      #
#     if [ X${MASTER} = Xtitane ] ; then
#       echo "IGCM_sys_RshPost <<-EOF"                     >> ${FunctionPath}
#     fi
      echo "export DEBUG_debug=${DEBUG_debug}                        " >> ${FunctionPath}
      echo "export BigBrother=${BigBrother}                          " >> ${FunctionPath}
      echo "export postProcessingStopLevel=${postProcessingStopLevel}" >> ${FunctionPath}
      echo "export MODIPSL=${MODIPSL}                                " >> ${FunctionPath}
      echo "export libIGCM_SX=${libIGCM}                             " >> ${FunctionPath}
      echo "export libIGCM=${libIGCM_POST}                           " >> ${FunctionPath}
      echo "export SUBMIT_DIR=${SUBMIT_DIR}                          " >> ${FunctionPath}
      echo "export POST_DIR=${POST_DIR}                              " >> ${FunctionPath}
      echo "export MASTER=${MASTER}                                  " >> ${FunctionPath}
      echo "export DateBegin=${DateBegin}                            " >> ${FunctionPath}
      echo "export PeriodDateEnd=${PeriodDateEnd}                    " >> ${FunctionPath}
      echo "export StandAlone=false                                  " >> ${FunctionPath}
      echo "export RESOL_ATM=${RESOL_ATM}                            " >> ${FunctionPath}
      echo "export RESOL_OCE=${RESOL_OCE}                            " >> ${FunctionPath}
      echo "export RESOL_ICE=${RESOL_ICE}                            " >> ${FunctionPath}
      echo "export RESOL_MBG=${RESOL_MBG}                            " >> ${FunctionPath}
      echo "export RESOL_SRF=${RESOL_SRF}                            " >> ${FunctionPath}
      echo "export RESOL_SBG=${RESOL_SBG}                            " >> ${FunctionPath}
      echo "export listVarEnv=${listVarEnv}                          " >> ${FunctionPath}
      echo "export Script_Post_Output=${Script_Post_Output}          " >> ${FunctionPath}
#     if [ X${MASTER} = Xtitane ] ; then
#       echo ". ${libIGCM_POST}/libIGCM_debug/libIGCM_debug.ksh" >> ${FunctionPath}
#       echo ". ${libIGCM_POST}/libIGCM_sys/libIGCM_sys.ksh"     >> ${FunctionPath}
#     fi
      echo "IGCM_sys_MkdirWork ${POST_DIR}                 " >> ${FunctionPath}
      echo "IGCM_debug_Verif_Exit                          " >> ${FunctionPath}
      echo "IGCM_sys_QsubPost create_se                    " >> ${FunctionPath}
#     if [ X${MASTER} = Xtitane ] ; then
#       echo "EOF"                                         >> ${FunctionPath}
#     fi
      fi
    else
      IGCM_sys_RshPost <<-EOF
            export DEBUG_debug=${DEBUG_debug}
            export BigBrother=${BigBrother}
            export postProcessingStopLevel=${postProcessingStopLevel}
            export MODIPSL=${MODIPSL}
            export libIGCM_SX=${libIGCM}
            export libIGCM=${libIGCM_POST}
            export SUBMIT_DIR=${SUBMIT_DIR}
            export POST_DIR=${POST_DIR}
            export MASTER=${MASTER}
            export DateBegin=${DateBegin}
            export PeriodDateEnd=${PeriodDateEnd}
            export StandAlone=false
            export RESOL_ATM=${RESOL_ATM}
            export RESOL_OCE=${RESOL_OCE}
            export RESOL_ICE=${RESOL_ICE}
            export RESOL_MBG=${RESOL_MBG}
            export RESOL_SRF=${RESOL_SRF}
            export RESOL_SBG=${RESOL_SBG}
            export listVarEnv=${listVarEnv}
            export Script_Post_Output=${Script_Post_Output}
            . ${libIGCM_POST}/libIGCM_debug/libIGCM_debug.ksh
            . ${libIGCM_POST}/libIGCM_sys/libIGCM_sys.ksh
            IGCM_sys_MkdirWork ${POST_DIR}
            IGCM_debug_Verif_Exit
            IGCM_sys_QsubPost create_se
EOF
    fi
  fi

  #============== PACK PROCESSING FOR RESTART, DEBUG AND OUTPUT FILES =============#
  if [ ${PackFrequency} = true ] ; then
    IGCM_debug_Print 1 "PACK POST-PROCESSING"
    # -----------------------------------------------------------------------------------
    # Function IGCM_FlushPost called by pack_output.job has not been closed yet. Do it now
    # Will submit Time Series OR Seasonal Average if needed
    # -----------------------------------------------------------------------------------
    if [ -f ${R_BUFR}/FlushPost_${PeriodDateEnd}.ksh ] ; then
      echo "IGCM_debug_PopStack \"IGCM_FlushPost\" " >> ${R_BUFR}/FlushPost_${PeriodDateEnd}.ksh
      echo "}                                      " >> ${R_BUFR}/FlushPost_${PeriodDateEnd}.ksh
    fi
    echo
    ## Need to Remember This One
    SavedDateBegin=${DateBegin}
    ## Need to Remember This One
    SavedDateEnd=${DateEnd}
    #
    DaysTemp=$(( $( IGCM_date_DaysInPreviousPeriod ${PeriodDateEnd} ${config_Post_PackFrequency} end ) - 1 ))
    #
    PackDateBegin=$( IGCM_date_AddDaysToGregorianDate ${PeriodDateEnd} -${DaysTemp} )
    #
    script=pack_debug
    #
    Script_Post_Output=${script}.${PeriodDateEnd}
    #
    listVarEnv="DEBUG_debug,BigBrother,postProcessingStopLevel,MODIPSL,libIGCM,libIGCM_SX,SUBMIT_DIR,POST_DIR,Script_Post_Output,MASTER,DateBegin,DateEnd,PeriodPack,StandAlone"
    IGCM_sys_RshPost <<-EOF
    export DEBUG_debug=${DEBUG_debug}
    export BigBrother=${BigBrother}
    export postProcessingStopLevel=${postProcessingStopLevel}
    export MODIPSL=${MODIPSL}
    export libIGCM_SX=${libIGCM}
    export libIGCM=${libIGCM_POST}
    export SUBMIT_DIR=${SUBMIT_DIR}
    export POST_DIR=${POST_DIR}
    export MASTER=${MASTER}
    export DateBegin=${PackDateBegin}
    export DateEnd=${PeriodDateEnd}
    export PeriodPack=${config_Post_PackFrequency}
    export StandAlone=false
    export listVarEnv=${listVarEnv}
    export Script_Post_Output=${Script_Post_Output}
    . ${libIGCM_POST}/libIGCM_debug/libIGCM_debug.ksh
    . ${libIGCM_POST}/libIGCM_card/libIGCM_card.ksh
    . ${libIGCM_POST}/libIGCM_sys/libIGCM_sys.ksh
    IGCM_sys_MkdirWork ${POST_DIR}
    IGCM_debug_Verif_Exit
    IGCM_sys_QsubPost ${script}
    IGCM_debug_Verif_Exit
EOF
    #
    script=pack_restart
    #
    Script_Post_Output=${script}.${PeriodDateEnd}
    #
      IGCM_sys_RshPost <<-EOF
      export DEBUG_debug=${DEBUG_debug}
      export BigBrother=${BigBrother}
      export postProcessingStopLevel=${postProcessingStopLevel}
      export MODIPSL=${MODIPSL}
      export libIGCM_SX=${libIGCM}
      export libIGCM=${libIGCM_POST}
      export SUBMIT_DIR=${SUBMIT_DIR}
      export POST_DIR=${POST_DIR}
      export MASTER=${MASTER}
      export DateBegin=${PackDateBegin}
      export DateEnd=${PeriodDateEnd}
      export PeriodPack=${config_Post_PackFrequency}
      export StandAlone=false
      export listVarEnv=${listVarEnv}
      export Script_Post_Output=${Script_Post_Output}
      . ${libIGCM_POST}/libIGCM_debug/libIGCM_debug.ksh
      . ${libIGCM_POST}/libIGCM_card/libIGCM_card.ksh
      . ${libIGCM_POST}/libIGCM_sys/libIGCM_sys.ksh
      IGCM_sys_MkdirWork ${POST_DIR}
      IGCM_debug_Verif_Exit
      IGCM_sys_QsubPost ${script}
      IGCM_debug_Verif_Exit
EOF
    #
    script=pack_output
    #
    Script_Post_Output=${script}.${PeriodDateEnd}
    #
    if ( [ ${RebuildFrequency} = true ] && [ -f ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh ] ) ; then
#       if [ X${MASTER} = Xtitane ] ; then
#         echo "IGCM_sys_RshPost <<-EOF"                     >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
#       fi
      echo "export DEBUG_debug=${DEBUG_debug}              " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export BigBrother=${BigBrother}                " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export MODIPSL=${MODIPSL}                      " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export libIGCM_SX=${libIGCM}                   " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export libIGCM=${libIGCM_POST}                 " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export SUBMIT_DIR=${SUBMIT_DIR}                " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export POST_DIR=${POST_DIR}                    " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export MASTER=${MASTER}                        " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export DateBegin=${PackDateBegin}              " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export DateEnd=${PeriodDateEnd}                " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export PeriodPack=${config_Post_PackFrequency} " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export StandAlone=false                        " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export listVarEnv=${listVarEnv}                " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export Script_Post_Output=${Script_Post_Output}" >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export script=${script}                        " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export postProcessingStopLevel=${postProcessingStopLevel}" >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
#       if [ X${MASTER} = Xtitane ] ; then
#         echo ". ${libIGCM_POST}/libIGCM_debug/libIGCM_debug.ksh" >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
#         echo ". ${libIGCM_POST}/libIGCM_sys/libIGCM_sys.ksh"     >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
#       fi
      echo "IGCM_sys_MkdirWork ${POST_DIR}                 " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "IGCM_debug_Verif_Exit                          " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "IGCM_sys_QsubPost ${script}                    " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
#       if [ X${MASTER} = Xtitane ] ; then
#         echo "EOF"                                         >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
#       fi
      echo "IGCM_debug_Verif_Exit                          " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
    else
      IGCM_sys_RshPost <<-EOF
        export DEBUG_debug=${DEBUG_debug}
        export BigBrother=${BigBrother}
        export postProcessingStopLevel=${postProcessingStopLevel}
        export MODIPSL=${MODIPSL}
        export libIGCM_SX=${libIGCM}
        export libIGCM=${libIGCM_POST}
        export SUBMIT_DIR=${SUBMIT_DIR}
        export POST_DIR=${POST_DIR}
        export MASTER=${MASTER}
        export DateBegin=${PackDateBegin}
        export DateEnd=${PeriodDateEnd}
        export PeriodPack=${config_Post_PackFrequency}
        export StandAlone=false
        export listVarEnv=${listVarEnv}
        export Script_Post_Output=${Script_Post_Output}
        . ${libIGCM_POST}/libIGCM_debug/libIGCM_debug.ksh
        . ${libIGCM_POST}/libIGCM_card/libIGCM_card.ksh
        . ${libIGCM_POST}/libIGCM_sys/libIGCM_sys.ksh
        IGCM_sys_MkdirWork ${POST_DIR}
        IGCM_debug_Verif_Exit
        IGCM_sys_QsubPost ${script}
        IGCM_debug_Verif_Exit
EOF
      export DateBegin=${SavedDateBegin}
      export DateEnd=${SavedDateEnd}
    fi
  fi

  #============== REBUILD POST-PROCESSING =============#
  if ( [ X${AsynchronousRebuild} = Xtrue ] && [ -f ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh ] ) ; then
    # -----------------------------------------------------------------------------------
    # Function IGCM_FlushRebuild define in rebuild.ksh has not been closed yet. Do it now
    # -----------------------------------------------------------------------------------
    echo "IGCM_debug_PopStack \"IGCM_FlushRebuild\" " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
    echo "}                                         " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
    IGCM_sys_Mv REBUILD_${PeriodDateBegin} ${REBUILD_DIR}
  fi
  #
  if [ ${RebuildFrequency} = true ] ; then
    IGCM_debug_Print 1 "REBUILD POST-PROCESSING FROM WORKDIR"
    echo
    script=rebuild_fromWorkdir
    #
    Script_Post_Output=${script}.${PeriodDateEnd}
    #
    listVarEnv="DEBUG_debug,BigBrother,postProcessingStopLevel,MODIPSL,libIGCM,libIGCM_SX,SUBMIT_DIR,REBUILD_DIR,POST_DIR,Script_Post_Output,MASTER,DateBegin,PeriodDateBegin,PeriodDateEnd,NbRebuildDir,StandAlone,RESOL_ATM,RESOL_OCE,RESOL_ICE,RESOL_MBG,RESOL_SRF,RESOL_SBG"
    IGCM_sys_RshPost <<-EOF
      export DEBUG_debug=${DEBUG_debug}
      export BigBrother=${BigBrother}
      export postProcessingStopLevel=${postProcessingStopLevel}
      export MODIPSL=${MODIPSL}
      export libIGCM_SX=${libIGCM}
      export libIGCM=${libIGCM_POST}
      export SUBMIT_DIR=${SUBMIT_DIR}
      export REBUILD_DIR=${REBUILD_DIR}
      export POST_DIR=${POST_DIR}
      export MASTER=${MASTER}
      export DateBegin=${DateBegin}
      export PeriodDateBegin=${PeriodDateBegin}
      export PeriodDateEnd=${PeriodDateEnd}
      export NbRebuildDir=${NbRebuildDir}
      export StandAlone=false
      export RESOL_ATM=${RESOL_ATM}
      export RESOL_OCE=${RESOL_OCE}
      export RESOL_ICE=${RESOL_ICE}
      export RESOL_MBG=${RESOL_MBG}
      export RESOL_SRF=${RESOL_SRF}
      export RESOL_SBG=${RESOL_SBG}
      export listVarEnv=${listVarEnv}
      export Script_Post_Output=${Script_Post_Output}
      . ${libIGCM_POST}/libIGCM_debug/libIGCM_debug.ksh
      . ${libIGCM_POST}/libIGCM_card/libIGCM_card.ksh
      . ${libIGCM_POST}/libIGCM_sys/libIGCM_sys.ksh
      IGCM_sys_MkdirWork ${POST_DIR}
      IGCM_debug_Verif_Exit
      IGCM_sys_QsubPost ${script}
      IGCM_debug_Verif_Exit
EOF
  fi
  IGCM_debug_PopStack "IGCM_post_Submit"
}

#===================================
function IGCM_post_CheckModuloFrequency
{
  IGCM_debug_PushStack "IGCM_post_CheckModuloFrequency" $@

  # Used by IGCM_config_Check
  # from 2 libIGCM compatible frequency (*Y, *M, *D, *y, *m, *d)
  # Issue an exit instruction IGCM_debug_Exit if there modulo is not zero
  # Input parameter are the name of the variable, not the frequency value itself
  # example
  # IGCM_post_ModuloFrequency config_Post_RebuildFrequency config_UserChoices_PeriodLength

  typeset MasterName SlaveName MasterFrequency SlaveFrequency PeriodMasterYear PeriodMasterMonth PeriodMasterDay PeriodSlaveYear PeriodSlaveMonth PeriodSlaveDay

  # Get the name of the variable
  MasterName=$1
  SlaveName=$2
  # Get the value the above name points to
  MasterFrequency=$( eval echo \${${1}} )
  SlaveFrequency=$( eval echo \${${2}} )

  IGCM_debug_Print 2 "IGCM_post_CheckModuloFrequency : Master=${MasterFrequency} Slave=${SlaveFrequency}"

  case ${MasterFrequency} in
  *y|*Y)
    PeriodMasterYear=$( echo ${MasterFrequency} | sed -e "s/[yY]//" )
    case ${SlaveFrequency} in
    *Y|*y)
      PeriodSlaveYear=$( echo ${SlaveFrequency} | sed -e "s/[yY]//" )
      if ( [ ${PeriodSlaveYear} -gt ${PeriodMasterYear} ] || \
      [ $(( ${PeriodMasterYear} % ${PeriodSlaveYear} )) -ne 0 ] );  then
        IGCM_debug_Print 1 "${SlaveName} frequency ${SlaveFrequency} not compatbile with"
        IGCM_debug_Print 1 "${MasterName} frequency : ${MasterFrequency}"
        IGCM_debug_Exit "Check your frequency"
      else
        [ X${3} = XNbPeriodPerFrequency ] && NbPeriodPerFrequency=$(( ${PeriodMasterYear} / ${PeriodSlaveYear} ))
      fi ;;
    *M|*m)
      PeriodSlaveMonth=$( echo ${SlaveFrequency} | sed -e "s/[mM]//" )
      if ( [ ${PeriodSlaveMonth} -gt $(( ${PeriodMasterYear} * 12 )) ] || \
        [ $(( ( ${PeriodMasterYear} * 12 ) % ${PeriodSlaveMonth} )) -ne 0 ] ) ; then
        IGCM_debug_Print 1 "${SlaveName} frequency ${SlaveFrequency} not compatbile with"
        IGCM_debug_Print 1 "${MasterName} frequency : ${MasterFrequency}"
        IGCM_debug_Exit "Check your frequency"
      else
        [ X${3} = XNbPeriodPerFrequency ] && NbPeriodPerFrequency=$(( ( 12 * ${PeriodMasterYear} ) / ${PeriodSlaveMonth} ))
      fi ;;
    *D|*d)
      PeriodSlaveDay=$( echo ${SlaveFrequency} | sed -e "s/[dD]//" )
      NbDays=$( IGCM_date_DaysInYear ${year} )
      if [ ${config_UserChoices_CalendarType} = 360d ] || [ ${config_UserChoices_CalendarType} = noleap ] ; then
        if ( [ ${PeriodSlaveDay} -gt $(( ${PeriodMasterYear} * ${NbDays} )) ] || \
          [ $(( ( ${PeriodMasterYear} * ${NbDays} ) % ${PeriodSlaveDay} )) -ne 0 ] ; ) then
          IGCM_debug_Print 1 "${SlaveName} frequency ${SlaveFrequency} not compatbile with"
          IGCM_debug_Print 1 "${MasterName} frequency : ${MasterFrequency}"
          IGCM_debug_Exit "Check your frequency"
        fi
      else
        IGCM_debug_Print 1 "For ${MasterName} with leap calendar:"
        IGCM_debug_Print 1 "We have a daily ${SlaveName} frequency and ${MasterName}=${MasterFrequency}"
        IGCM_debug_Print 1 "No post-treatment. Case not properly handle at this moment by libIGCM. Sorry"
        IGCM_debug_Exit    "Check your frequency ${MasterName} and choose a daily frequency for this one too."
      fi ;;
    esac ;;
  *M|*m)
    PeriodMasterMonth=$( echo ${MasterFrequency} | sed -e "s/[mM]//" )
    case ${SlaveFrequency} in
    *Y|*y)
      PeriodSlaveYear=$( echo ${SlaveFrequency} | sed -e "s/[yY]//" )
      if ( [ ${PeriodMasterMonth} -gt $(( ${PeriodSlaveYear} * 12 )) ] || \
        [ $(( ${PeriodMasterMonth} % ( ${PeriodSlaveYear} * 12 ) )) -ne 0 ] ) ; then
        IGCM_debug_Print 1 "${SlaveName} frequency ${SlaveFrequency} not compatbile with"
        IGCM_debug_Print 1 "${MasterName} frequency : ${MasterFrequency}"
        IGCM_debug_Exit "Check your frequency"
      else
        [ X${3} = XNbPeriodPerFrequency ] && NbPeriodPerFrequency=$(( ${PeriodMasterMonth} / ( 12 * ${PeriodSlaveYear} ) ))
      fi ;;
    *M|*m)
      PeriodSlaveMonth=$( echo ${SlaveFrequency} | sed -e "s/[mM]//" )
      if ( [ ${PeriodSlaveMonth} -gt ${PeriodMasterMonth} ] || \
        [ $(( ${PeriodMasterMonth} % ${PeriodSlaveMonth} )) -ne 0 ] ) ;  then
        IGCM_debug_Print 1 "${SlaveName} frequency ${SlaveFrequency} not compatbile with"
        IGCM_debug_Print 1 "${MasterName} frequency : ${MasterFrequency}"
        IGCM_debug_Exit "Check your frequency"
      else
        [ X${3} = XNbPeriodPerFrequency ] && NbPeriodPerFrequency=$(( ${PeriodMasterMonth} / ${PeriodSlaveMonth} ))
      fi ;;
    *D|*d)
      IGCM_debug_Print 1 "${SlaveName} frequency ${SlaveFrequency} not compatbile with"
      IGCM_debug_Print 1 "${MasterName} frequency : ${MasterFrequency}"
      IGCM_debug_Exit "Check your frequency" ;;
    esac ;;
  *D|*d)
    PeriodMasterDay=$( echo ${MasterFrequency} | sed -e "s/[dD]//" )
    case ${SlaveFrequency} in
    *Y|*y)
      IGCM_debug_Print 1 "${SlaveName} frequency ${SlaveFrequency} not compatbile with"
      IGCM_debug_Print 1 "${MasterName} frequency ${MasterFrequency}"
      IGCM_debug_Exit "Check your frequency" ;;
    *M|*m)
      IGCM_debug_Print 1 "${SlaveName} frequency ${SlaveFrequency} not compatbile with"
      IGCM_debug_Print 1 "${MasterName} frequency ${MasterFrequency}"
      IGCM_debug_Exit "Check your frequency" ;;
    *D|*d)
      PeriodSlaveDay=$( echo ${SlaveFrequency} | sed -e "s/[dD]//" )
      if ( [ ${PeriodSlaveDay} -gt ${PeriodMasterDay} ] || \
        [ $(( ${PeriodMasterDay} % ${PeriodSlaveDay} )) -ne 0 ] ) ;  then
        IGCM_debug_Print 1 "${SlaveName} frequency ${SlaveFrequency} not compatbile with"
        IGCM_debug_Print 1 "${MasterName} frequency : ${MasterFrequency}"
        IGCM_debug_Exit "Check your frequency"
      else
        [ X${3} = XNbPeriodPerFrequency ] && NbPeriodPerFrequency=$(( ${PeriodMasterDay} / ${PeriodSlaveDay} ))
      fi ;;
    esac ;;
  NONE|none)
    ;;
  *)
    IGCM_debug_Print 1 "KeyWord ${MasterFrequency} not allowed for ${MasterName} in config.card"
    IGCM_debug_Exit "Check your ${MasterName} in config.card" ;;
  esac

  IGCM_debug_PopStack "IGCM_post_CheckModuloFrequency"
}

#===================================
function IGCM_post_ModuloRuntimeFrequency
{
  IGCM_debug_PushStack "IGCM_post_ModuloRuntimeFrequency" $@

  # Used by IGCM_post_Configure
  # - from libIGCM (config_UserChoices_PeriodLength frequency * CumulPeriod) and
  # - post-processing compatible frequency (*Y, *M, *D, *y, *m, *d)
  # --> turn on post-processing submission when their modulo is zero
  # Input parameter are the name of the variable, not the frequency value itself
  # example
  # IGCM_post_ModuloRuntimeFrequency config_Post_SeasonalFrequency config_UserChoices_PeriodLength

  typeset MasterName SlaveName MasterFrequency SlaveFrequency PeriodMasterYear PeriodMasterMonth PeriodMasterDay PeriodSlaveYear PeriodSlaveMonth PeriodSlaveDay

  # Get the name of the variable
  MasterName=$1
  SlaveName=$2
  # Get the value the above name points to
  eval MasterFrequency=\${${1}}
  eval SlaveFrequency=\${${2}}

  echo
  IGCM_debug_Print 2 "IGCM_post_ModuloRuntimeFrequency : Master=${MasterFrequency} Slave=${SlaveFrequency} CumulPeriod=${CumulPeriod}"

  case ${MasterFrequency} in
  *y|*Y)
    PeriodMasterYear=$( echo ${MasterFrequency} | sed -e "s/[yY]//" )
    case ${SlaveFrequency} in
    *Y|*y)
      PeriodSlaveYear=$( echo ${SlaveFrequency} | sed -e "s/[yY]//" )
      if [ $(( ( ${CumulPeriod} * ${PeriodSlaveYear} - ${PeriodOffset} ) % ${PeriodMasterYear} )) -eq 0 ] ;  then
        if [ $(( ${CumulPeriod} * ${PeriodSlaveYear} - ${PeriodOffset} )) -ne 0 ] ; then
          eval ${post_freq}=true ; POST=true
          [ X${3} = XNbPeriodPerFrequency ] && NbPeriodPerFrequency=$(( ${PeriodMasterYear} / ${PeriodSlaveYear} ))
        fi
      fi;;
    *M|*m)
      PeriodSlaveMonth=$( echo ${SlaveFrequency} | sed -e "s/[mM]//" )
      if [ $(( ( ${CumulPeriod} * ${PeriodSlaveMonth} - ${PeriodOffset} * 12 ) % ( ${PeriodMasterYear} * 12 ) )) -eq 0 ] ; then
        if [ $(( ${CumulPeriod} * ${PeriodSlaveMonth} - ${PeriodOffset} * 12 )) -ne 0 ] ; then
          eval ${post_freq}=true ; POST=true
          [ X${3} = XNbPeriodPerFrequency ] && NbPeriodPerFrequency=$(( ( 12 * ${PeriodMasterYear} ) / ${PeriodSlaveMonth} ))
        fi
      fi;;
    *D|*d)
      PeriodSlaveDay=$( echo ${SlaveFrequency} | sed -e "s/[dD]//" )
      NbDays=$( IGCM_date_DaysInYear ${year} )
      if [ $(( ( ${CumulPeriod} - ( ${PeriodOffset} * ${NbDays} / ${PeriodSlaveDay} ) ) % ( ${NbDays} * ${PeriodMasterYear} / ${PeriodSlaveDay} ) )) -eq 0 ] ; then
        if [ $(( ${CumulPeriod} - ( ${PeriodOffset} * ${NbDays} / ${PeriodSlaveDay} ) )) -ne 0 ] ; then
          eval ${post_freq}=true ; POST=true
          [ X${3} = XNbPeriodPerFrequency ] && NbPeriodPerFrequency=$(( ( ${NbDays} * ${PeriodMasterYear} ) / ${PeriodSlaveDay} ))
        fi
      fi;;
    esac ;;
  *M|*m)
    PeriodMasterMonth=$( echo ${MasterFrequency} | sed -e "s/[mM]//" )
    case ${SlaveFrequency} in
    *Y|*y)
      PeriodSlaveYear=$( echo ${SlaveFrequency} | sed -e "s/[yY]//" )
      if [ $(( ( ${CumulPeriod} * ${PeriodSlaveYear} * 12 - ${PeriodOffset} ) % ( ${PeriodMasterMonth} ) )) -eq 0 ] ; then
        if [ $(( ${CumulPeriod} * ${PeriodSlaveYear} * 12 - ${PeriodOffset} )) -ne 0 ] ; then
          eval ${post_freq}=true ; POST=true
          [ X${3} = XNbPeriodPerFrequency ] && NbPeriodPerFrequency=$(( ${PeriodMasterMonth} / ( 12 * ${PeriodSlaveYear} ) ))
        fi
      fi;;
    *M|*m)
      PeriodSlaveMonth=$( echo ${SlaveFrequency} | sed -e "s/[mM]//" )
      if [ $(( ( ${CumulPeriod} * ${PeriodSlaveMonth} - ${PeriodOffset} ) % ${PeriodMasterMonth} )) -eq 0 ] ;  then
        if [ $(( ${CumulPeriod} * ${PeriodSlaveMonth} -  ${PeriodOffset} )) -ne 0 ] ; then
          eval ${post_freq}=true ; POST=true
          [ X${3} = XNbPeriodPerFrequency ] && NbPeriodPerFrequency=$(( ${PeriodMasterMonth} / ${PeriodSlaveMonth} ))
        fi
      fi;;
    *D|*d)
      IGCM_debug_Print 1 "PeriodLength frequency ${SlaveFrequency} not compatbile with"
      IGCM_debug_Print 1 "${flag_post} frequency : ${MasterFrequency} " ;;
    esac ;;
  *D|*d)
    PeriodMasterDay=$( echo ${MasterFrequency} | sed -e "s/[dD]//" )
    case ${SlaveFrequency} in
    *Y|*y)
      IGCM_debug_Print 1 "PeriodLength frequency ${SlaveFrequency} not compatbile with"
      IGCM_debug_Print 1 "${flag_post} frequency : ${MasterFrequency} " ;;
    *M|*m)
      IGCM_debug_Print 1 "PeriodLength frequency ${SlaveFrequency} not compatbile with"
      IGCM_debug_Print 1 "${flag_post} frequency : ${MasterFrequency} " ;;
    *D|*d)
      PeriodSlaveDay=$( echo ${SlaveFrequency} | sed -e "s/[dD]//" )
      if [ $(( ( ${CumulPeriod} * ${PeriodSlaveDay} - ${PeriodOffset} ) % ${PeriodMasterDay} )) -eq 0 ] ;  then
        if [ $(( ${CumulPeriod} * ${PeriodSlaveDay} - ${PeriodOffset} )) -ne 0 ] ; then
          eval ${post_freq}=true ; POST=true
          [ X${3} = XNbPeriodPerFrequency ] && NbPeriodPerFrequency=$(( ${PeriodMasterDay} / ${PeriodSlaveDay} ))
        fi
      fi;;
    esac ;;
  NONE|none)
    ;;
  *)
    IGCM_debug_Print 1 "KeyWord not allowed for ${post_freq} in config.card"
    ;;
  esac

  IGCM_debug_PopStack "IGCM_post_ModuloRuntimeFrequency"
}
