#!/bin/ksh

#**************************************************************
# Author: Patrick Brockmann, Martial Mancip
# Contact: Patrick.Brockmann__at__cea.fr Martial.Mancip__at__ipsl.jussieu.fr
# $Revision:: 1324                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2016-04-15 12:25:08 +0200 (Fri, 15 Apr 2016) $ Date of last commit
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************

#==================================================
# The documentation of this file can be automatically generated
# if you use the prefix #D- for comments to be extracted.
# Extract with command: cat lib* | grep "^#D-" | cut -c "4-"
#==================================================

#==================================================
# Add high level verbosity
typeset -i Verbosity=${Verbosity:=3}

#==================================================
# DEBUG_debug
# Add low level verbosity
DEBUG_debug=${DEBUG_debug:=false}

#D- postProcessingStopLevel (0,1,2,3)
#D- 3 stop if any post-processing went wrong
#D- 2 dont stop if atlas, monitoring or metrics failed
#D- 1 dont stop if atlas, monitoring, metrics, create_ts or create_se failed
#D- 0 dont stop if atlas, monitoring, metrics, create_ts, create_se, rebuild or pack_* failed
postProcessingStopLevel=${postProcessingStopLevel:=0}

#==================================================
# GENERATE RANDOM ERROR ; only apply if ( ${DEBUG_debug} )
typeset -r RandomError=false

#==================================================
# NULL_STR
# Default null string
typeset -r NULL_STR="_0_"

#==================================================
# libIGCM_CurrentTag
# Current libIGCM tag, check compatibilty with *.card
typeset -r libIGCMVersion="2.8.1"

#==================================================
# Exit Flag (internal debug)
# When true, end the master loop AFTER SAVES FILES
ExitFlag=false

#==================================================
# When we start to run the simulation is not finished
simulationIsOver=false

#==================================================
# When we start to run we dont flush AMQP messages
FlushAMQP=false

#==================================================
# Declare a stack of functions calls
unset IGCM_debug_Stack
unset IGCM_debug_StackArgs
unset IGCM_debug_StackTiming
IGCM_debug_Stack[0]=${NULL_STR}
IGCM_debug_StackArgs[0]=${NULL_STR}
IGCM_debug_StackTiming[0]=${NULL_STR}
IGCM_debug_LenStack=0

#D-#==================================================================
#D-function IGCM_debug_getDate_ms
#D- * Purpose: Give number of milliseconds since 01-jan-1970
function IGCM_debug_getDate_ms
{
  typeset nanosecs ms
  # nano secondes since 01-jan-1970
  nanosecs=$( date +%s%N )

  # truncate the last 6 digits to get milliseconds since 01-jan-1970
  ms=${nanosecs:0:${#nanosecs}-6}

  echo "$ms"
}

#D-#==================================================================
#D-function IGCM_debug_sizeOfTabContent
#D- * Purpose: Give sumed size of a list of files
#D- * Usage: IGCM_debug_sizeOfTabContent entityList destination
#D- *        where entityList is a list of files or directory
#D- *        where dest is either a directory or a file name
function IGCM_debug_sizeOfTabContent
{
  typeset entityListe destination iEntity sizeKo sumSizeKo sumSizeMo

  eval set +A entityListe \${${1}}
  destination=${2}
  sumSizeKo=0

  # Here we will try to compute size (file or directory size) from local path and not from archive.
  for ((i = 0; i < ${#entityListe[*]}; i += 1)) ; do
    if [ -f ${entityListe[$i]} ] ; then
      # One file or a bunch of files has been copied without renaming from a visible filesystem
      iEntity=${entityListe[$i]}
    elif [ -f ${entityListe[$i]##/*/} ] ; then
      # One file or a bunch of files has been copied without renaming from an non visible filesystem
      # remove path /home/login/../ from entityListe elements
      iEntity=${entityListe[$i]##/*/}
    elif [ -f ${destination} ] ; then
      # a file has been copied and renamed
      iEntity=${destination}
    elif [ -f ${destination}/${entityListe[$i]##/*/} ] ; then
      # a copy in a directory but not in ${PWD}
      iEntity=${destination}/${entityListe[$i]##/*/}
    elif [ -d ${entityListe[$i]} ] ; then
      # a directory has been copied from a non remote place
      iEntity=${entityListe[$i]}
    elif [ -d ${destination}/${entityListe[$i]##/*/} ] ; then
      # a directory has been copied from a remote archive and not renamed
      iEntity=${destination}/${entityListe[$i]##/*/}
    elif [ -d ${destination} ] ; then
      # a directory has been copied from a remote archive and renamed
      iEntity=${destination}
    fi
    sizeKo=$( du --apparent-size -skL ${iEntity} | gawk '{print $1}' )
    sumSizeKo=$(( $sumSizeKo + $sizeKo ))
  done
  sumSizeMo=$( echo "scale=6;${sumSizeKo}/1024" | bc )
  echo "${sumSizeKo}|${sumSizeMo}"
}

#D-#==================================================================
#D-function IGCM_debug_send_AMQP_msg__MAILTUNNEL
#D- * Purpose: Take over AMQP C client using mail as a message recipient
#D- * One argument : base64 encoded message
#D- * Attach encoded config.card when starting the simulation

#D-#==================================================================
#D-function IGCM_debug_sendAMQP_Metrics
#D- * Purpose: Take over AMQP C client using mail as a message recipient
#D- * Two arguments : - Directory where metrics.json files can be found
#D- *                 - Metrics Group Name. metrics will be added to this group
#D- * Attach encoded metrics.json files.

#D-#==================================================================
#D-function IGCM_debug_sendAMQP_projectAccounting
#D- * Purpose: Take over AMQP C client using mail as a message recipient
#D- * One argument : - File name where project accounting details are stored
#D- * Attach encoded accounting file.


#D-#==================================================================
#D-function IGCM_debug_SendAMQP
#D- * Purpose: Send body; encoded body and config.card to rabbitMQ
#D-#==================================================================
#D-function IGCM_debug_CallStack
#D-* Purpose: Print the call stack tree from the oldest to the youngest (opposite of the display standard)
#D-
function IGCM_debug_CallStack {
  if ( $DEBUG_debug ) ; then
    # Cosmetics
    typeset i decal
    i=0
    until [ $i -eq ${IGCM_debug_LenStack} ]; do
      decal=0
      until [ $decal -eq ${i} ]; do
        printf -- ' '
        (( decal = decal + 1 ))
      done
      echo "$i - ${IGCM_debug_Stack[$(( $IGCM_debug_LenStack-$i-1 ))]}" "(${IGCM_debug_StackArgs[$(( $IGCM_debug_LenStack-$i-1 ))]})"
      ((i = i + 1))
    done
  fi
}

#D-#==================================================================
#D-function IGCM_debug_PushStack
#D-* Purpose: Push a function name in the stack
#D-
function IGCM_debug_PushStack {
  if ( $DEBUG_debug ) ; then
    typeset decal inputs startTime_ms

    # Only cosmetics : stack file
    if [ X${ActivateStackFilling} = Xtrue ] ; then
      echo >> ${StackFileLocation}/${StackFileName}
      decal=0
      while [ ${decal} -lt ${IGCM_debug_LenStack} ]; do
        printf ' ' >> ${StackFileLocation}/${StackFileName}
        (( decal = decal + 1 ))
      done

      # Fill the stack file
      echo "> ${IGCM_debug_LenStack} : ${@}" >> ${StackFileLocation}/${StackFileName}
    fi

    # Save input list in an indexed array
    INPUTS=( $@ )

    # Get timing information
    startTime_ms=$( IGCM_debug_getDate_ms )

    # We add function call name on beginning of the stack
    set +A IGCM_debug_Stack -- ${1} ${IGCM_debug_Stack[*]}

    # Save timing in milliseconds in an indexed array
    set +A IGCM_debug_StackTiming -- ${startTime_ms} ${IGCM_debug_StackTiming[*]}

    # We include the "null" Args in the beginning of the StackArgs
    set +A IGCM_debug_StackArgs ${NULL_STR} ${IGCM_debug_StackArgs[*]}

    # Then, we shift StackArgs tabular
    # Replacing blank separated list by comma separated list of quoted elements (except the first and last element)
    if [ $# -gt 1 ]; then
      IGCM_debug_StackArgs[0]=$(echo ${INPUTS[*]:1} | sed -e "s/\ /\",\"/g" )
    fi

    # Increment LenStack
    (( IGCM_debug_LenStack = IGCM_debug_LenStack + 1 ))

    #IGCM_debug_CallStack
  fi
}

#D-#==================================================================
#D-function IGCM_debug_PopStack
#D-* Purpose: Pop a function name in the stack
#D-
function IGCM_debug_PopStack {
  if ( $DEBUG_debug ) ; then
    typeset i decal command arguments startTime_ms endTime_ms
    typeset instrumentation dest prefix
    # they are not typeset because they are send "by adress" to son functions
    # we unset them to avoid "memory effect"
    unset fileList source

    # INTRODUCE SIMPLE ERROR GENERATOR TO TEST SUPERVISOR
    # PROBABILITY ERROR IS 0.0001 PER COMMAND OR FUNCTION CALL
    # THERE ARE ~500 COMMAND OR FUNCTION CALL PER PERIOD
    # ONLY WHEN TaskType is "computing".
    if [ X${ActivateBigBro} = Xtrue ] ; then
      if [ X${TaskType} = Xcomputing ]; then
        if ( ${RandomError} ) ; then
          if [ $((RANDOM%10000)) -le 10 ] ; then
            IGCM_debug_Print 1 "Random error has been triggered"
            if [ X${ActivateStackFilling} = Xtrue ] ; then
              echo "RANDOM ERROR" >> ${StackFileLocation}/${StackFileName}
            fi
            ExitFlag=true
          fi
        fi
      fi
    fi

    if [ "${IGCM_debug_Stack[0]}" = "${1}" ]; then
      # Everything is cool

      # Get timing information
      endTime_ms=$( IGCM_debug_getDate_ms )

      # Save Stack information before poping the stack
      command=${IGCM_debug_Stack[0]}

      # Go from comma separated list of quoted elements (except the first and the last element)
      # to unquoted space separated elements in an array
      set -A arguments -- $( echo ${IGCM_debug_StackArgs[0]} | sed -e "s/\",\"/\ /g" )

      # Save Stack information before poping the stack
      startTime_ms=${IGCM_debug_StackTiming[0]}

      # Pop the stack
      (( IGCM_debug_LenStack = IGCM_debug_LenStack - 1 ))
      set -A IGCM_debug_Stack -- ${IGCM_debug_Stack[*]:1}
      set -A IGCM_debug_StackArgs -- ${IGCM_debug_StackArgs[*]:1}
      set -A IGCM_debug_StackTiming -- ${IGCM_debug_StackTiming[*]:1}
    else
      echo 'IGCM_debug_Exit : stack is corrupted ! LenStack =' ${IGCM_debug_LenStack}
      IGCM_debug_Exit $@
    fi

    # Special actions depending on command to prepare IGCM_debug_PrintInfosActions call
    # We are interested in:
    #  0. Which command performs the work
    #  1. Size of entity we are working with
    #  2. Where are we reading
    #  3. Where are we writing
    #  4. How long it took

    instrumentation=false

    case ${command} in
    # Classical copy (only files are given to IGCM_sys_Cp as options)
    IGCM_sys_Cp)
      instrumentation=true
      # All but the latest
      fileList=${arguments[*]:0:${#arguments[*]}-1}
      # just need the first file to get the directory
      source=${arguments[0]}
      # Nothing but the latest
      dest=${arguments[${#arguments[*]}-1]}
      # Size of file whose name are stored in a list
      entitySize=$( IGCM_debug_sizeOfTabContent fileList ${dest} )
      ;;

    # Copy from archive machine or from buffer
    IGCM_sys_Get|IGCM_sys_GetBuffer)
      instrumentation=true
      if [ ${#arguments[*]} -eq 2 ] ; then
        source=${arguments[0]}
        dest=${arguments[1]}
        # Size of file whose name are stored in a variable
        entitySize=$( IGCM_debug_sizeOfTabContent source ${dest} )
      elif ( [ ${#arguments[*]} -eq 3 ] && [ ${arguments[0]} = '/l' ] ) ; then
        # IGCM_sys_Get /l liste_file[*] /ccc/scratch/cont003/dsm/p86denv/RUN_DIR/985998_14754/
        # Keep the array name hosting the all list
        eval set +A fileList \${${arguments[1]}}
        # just need the first file to get the directory
        source=${fileList[0]}
        dest=${arguments[2]}
        # Size of file whose name are stored in a list
        entitySize=$( IGCM_debug_sizeOfTabContent fileList[*] ${dest} )
      elif [ [ ${#arguments[*]} -ge 3 ] ; then
       # All but the latest
        fileList=${arguments[*]:0:${#arguments[*]}-1}
        # just need the first file to get the directory
        source=${arguments[0]}
        # Nothing but the latest
        dest=${arguments[${#arguments[*]}-1]}
        # Size of file whose name are stored in a list
        entitySize=$( IGCM_debug_sizeOfTabContent fileList ${dest} )
      fi
      ;;

    # Copy from compute node or copy to archive/buffer
    IGCM_sys_Get_Master|IGCM_sys_Get_Dir|IGCM_sys_Put_Out|IGCM_sys_PutBuffer_Out)
      instrumentation=true
      source=${arguments[0]}
      dest=${arguments[1]}
      # Size of file whose name are stored in a variable
      entitySize=$( IGCM_debug_sizeOfTabContent source ${dest} )
      ;;

    # Rebuild command
    IGCM_sys_rebuild|IGCM_sys_rebuild_station)
      instrumentation=true
      # All but the first
      fileList=${arguments[*]:1:${#arguments[*]}-1}
      # just need a file to get the directory
      source=${arguments[1]}
      # Nothing but the first
      dest=${arguments[0]}
      # Size of file whose name are stored in a list
      entitySize=$( IGCM_debug_sizeOfTabContent fileList ${dest} )
      ;;

    # NCO commands
    IGCM_sys_ncrcat|IGCM_sys_ncecat|IGCM_sys_ncra|IGCM_sys_ncks|IGCM_sys_cdo)
      # Example of what we want to catch : only filenames in those command lines
      # IGCM_sys_ncrcat -O -v ${list_var_final_ncrcat} ${OUT_SE[*]} ${RESULT_SE}
      # IGCM_sys_ncrcat --hst -v ${liste_coord}${var} ${file1} ${liste_file_tmp[*]} ${file_out}
      # IGCM_sys_ncrcat -p ${dir} ${liste_file_tmp} --output ${output}
      # IGCM_sys_ncrcat -x -v ${list_var} -p ${dir} ${liste_file_tmp} --output ${output}
      instrumentation=true
      keepGoing=true
      prefix=.
      i=0
      while ( ${keepGoing} ) ; do
        # the last one is not interesting
        if [ ${i} -eq ${#arguments[*]}-1 ] ; then
          keepGoing=false
        # look after "-p" option. Path prefix is the following arguments
        elif [ ${arguments[${i}]} = "-p" ] ; then
          ((i = i + 1))
          prefix=${arguments[${i}]}
          ((i = i + 1))
        elif [ ${i} -eq ${#arguments[*]}-1 ] ; then
          keepGoing=false
        # looking for files
        elif [ -f ${prefix}/${arguments[${i}]} ] ; then
          fileList="${fileList} ${prefix}/${arguments[${i}]}"
          ((i = i + 1))
        # other options are not interesting
        else
          ((i = i + 1))
        fi
      done

      # i value is at least 1
      # just need one file to get the directory
      source=$( echo ${fileList} | gawk '{print $1}' )
      # Nothing but the latest
      dest=${arguments[${#arguments[*]}-1]}
      # Size of file whose name are stored in a list
      entitySize=$( IGCM_debug_sizeOfTabContent fileList ${dest} )
      ;;
    esac

    # Print information related to instrumentation
    ( ${instrumentation} ) && IGCM_debug_PrintInfosActions ${command} ${entitySize} ${startTime_ms} ${endTime_ms} ${dest} ${source}

    # Only cosmetics : stack file
    if [ X${ActivateStackFilling} = Xtrue ] ; then
      decal=0
      while [ ${decal} -lt ${IGCM_debug_LenStack} ]; do
        printf ' ' >> ${StackFileLocation}/${StackFileName}
        (( decal = decal + 1 ))
      done
    fi

    if ( ${ExitFlag} ) ; then
      # Inform the stack file
      if [ X${ActivateStackFilling} = Xtrue ] ; then
        echo '!!! ExitFlag has been activated !!!' >> ${StackFileLocation}/${StackFileName}
      fi

      # Unplugged message 4900 handling for now. To ease downstream treatment.
      if [ X${ActivateBigBro} = Xtrue ] ; then
        if [ X${TaskType} = Xcomputing ]; then
          # RabbitMQ message code "COMPUTING JOBs COMMAND FAILURE"
          code=1900
        elif [ X${TaskType} = Xpost-processing ]; then
          # RabbitMQ message code "POST-PROCESSING JOBs COMMAND FAILURE"
          code=2900
        elif [ X${TaskType} = Xchecking ]; then
          # RabbitMQ message code "POST-PROCESSING FROM CHECKER JOBs COMMAND FAILURE"
          code=3900
        fi
        # RabbitMQ message body
        Body=$( echo "{${genericSimulationID},\"msgCode\":\"${code}\",\"msgUID\":\"$(uuidgen)\",\"command\":\"${command}\",\"msgTimestamp\":\"$( date +"%Y-%m-%dT%H:%M:%S.%N%z" )\"}" )

        # Fill the rabbitMQ queue
        IGCM_debug_sendAMQP
      fi
    else
      # Inform the stack file
      if [ X${ActivateStackFilling} = Xtrue ] ; then
        echo "< ${IGCM_debug_LenStack} : ${@}" >> ${StackFileLocation}/${StackFileName}
      fi
    fi

    # Reset array if necessary
    if [ ${IGCM_debug_LenStack} = 0 ]; then
      #echo
      #IGCM_debug_Print 3 "Clean stack array"
      #echo
      unset IGCM_debug_Stack
      unset IGCM_debug_StackArgs
      unset IGCM_debug_StackTiming
      IGCM_debug_Stack[0]=${NULL_STR}
      IGCM_debug_StackArgs[0]=${NULL_STR}
      IGCM_debug_StackTiming[0]=${NULL_STR}
    fi
  fi
  #IGCM_debug_CallStack
}

#D-#==================================================================
#D-function IGCM_debug_BigBro_Initialize
#D-* Purpose: switch rabbitMQ on
#D-
function IGCM_debug_BigBro_Initialize {
  IGCM_debug_PushStack "IGCM_debug_BigBro_Initialize"

  typeset postProcessingIDLength postProcessingName postProcessingDate postProcessingDimn postProcessingComp postProcessingFile

# Message type standard fields:
# https://github.com/Prodiguer/prodiguer-docs/wiki/MQ-Standard-Message-Fields

# Message type dictionnary and custom fields:
# https://github.com/Prodiguer/prodiguer-docs/wiki/Monitoring-Message-Dictionary

  if [ X${BigBrother} = Xtrue ] ; then
    # create a unique ID for this specific job
    jobuid=$(uuidgen)

    # get the assigned id by the scheduler for that job
    IGCM_sys_getJobSchedulerID jobSchedulerID

    if [ X${TaskType} = Xcomputing ]; then
      if ( ${FirstInitialize} ) ; then
        # RabbitMQ message code "BEGIN A SIMULATION"
        code=0000
        # create and persist a unique id for this simulation
        simuid=$(uuidgen)
        IGCM_card_WriteOption ${SUBMIT_DIR}/run.card Configuration simuid ${simuid}
        # Standard fields for the first message
        genericSimulationID=$( echo "\"msgApplication\":\"monitoring\",\"msgProducer\":\"libigcm\",\"msgProducerVersion\":\"${libIGCMVersion}\",\"activity\":\"IPSL\",\"name\":\"${config_UserChoices_JobName}\",\"experiment\":\"${config_UserChoices_ExperimentName}\",\"space\":\"${config_UserChoices_SpaceName}\",\"model\":\"${config_UserChoices_TagName}\",\"startDate\":\"${config_UserChoices_DateBegin}\",\"endDate\":\"${config_UserChoices_DateEnd}\",\"login\":\"${LOGIN}\",\"centre\":\"${CENTER}\",\"machine\":\"${MASTER}\",\"simuid\":\"${simuid}\",\"jobuid\":\"${jobuid}\"" )
        # RabbitMQ message body with specific fields associated message codes treated here
        Body=$( echo "{${genericSimulationID},\"msgCode\":\"${code}\",\"accountingProject\":\"${PROJECT}\",\"jobWarningDelay\":\"${jobWarningDelay}\",\"jobSchedulerID\":\"${jobSchedulerID}\",\"jobSubmissionPath\":\"${SUBMIT_DIR}\",\"msgUID\":\"$(uuidgen)\",\"msgTimestamp\":\"$( date +"%Y-%m-%dT%H:%M:%S.%N%z" )\"}" )
        # Fill the rabbitMQ queue (the config.card in use will be sent)
        IGCM_debug_sendAMQP activate
      else
        # RabbitMQ message code "A NEW COMPUTING JOB IS RUNNING PART OF A SIMULATION"
        code=1000
        # retrieve this simulation's unique id
        IGCM_card_DefineVariableFromOption ${SUBMIT_DIR}/run.card Configuration simuid
        simuid=${run_Configuration_simuid}
        # Using standard fields for message others than the first one. Still subject to change
        genericSimulationID=$( echo "\"msgApplication\":\"monitoring\",\"msgProducer\":\"libigcm\",\"msgProducerVersion\":\"${libIGCMVersion}\",\"simuid\":\"${simuid}\",\"jobuid\":\"${jobuid}\"" )
        # RabbitMQ message body with specific fields associated message codes treated here
        Body=$( echo "{${genericSimulationID},\"msgCode\":\"${code}\",\"accountingProject\":\"${PROJECT}\",\"jobWarningDelay\":\"${jobWarningDelay}\",\"jobSchedulerID\":\"${jobSchedulerID}\",\"jobSubmissionPath\":\"${SUBMIT_DIR}\",\"msgUID\":\"$(uuidgen)\",\"msgTimestamp\":\"$( date +"%Y-%m-%dT%H:%M:%S.%N%z" )\"}" )
        # Fill the rabbitMQ queue
        IGCM_debug_sendAMQP
      fi

      # NOT VERY NICE BUT ... IT WORKS
      # Be sure that the genericSimulationID will be small from now on
      # Using standard fields for messages others than the first one. Still subject to change
      genericSimulationID=$( echo "\"msgApplication\":\"monitoring\",\"msgProducer\":\"libigcm\",\"msgProducerVersion\":\"${libIGCMVersion}\",\"simuid\":\"${simuid}\",\"jobuid\":\"${jobuid}\"" )

    elif [ X${TaskType} = Xpost-processing ]; then
      # RabbitMQ message code "A NEW POST-PROCESSING JOB IS RUNNING PART OF A SIMULATION"
      code=2000
      # retrieve this simulation's unique id
      IGCM_card_DefineVariableFromOption ${SUBMIT_DIR}/run.card Configuration simuid
      simuid=${run_Configuration_simuid}
      # Using standard fields for message others than the first one. Still subject to change
      genericSimulationID=$( echo "\"msgApplication\":\"monitoring\",\"msgProducer\":\"libigcm\",\"msgProducerVersion\":\"${libIGCMVersion}\",\"simuid\":\"${simuid}\",\"jobuid\":\"${jobuid}\"" )
      
      # Specify the post-processing task we are dealing with
      postProcessingIDLength=$( echo "${Script_Post_Output}" | tr -d -c "\." | wc -c )
      postProcessingName=$( echo "${Script_Post_Output}" | gawk -F. '{print $1}' )
      postProcessingDate=$( echo "${Script_Post_Output}" | gawk -F. '{print $2}' )
      postProcessingDimn="null"
      postProcessingComp="null"
      postProcessingFile="null"
      if [ ${postProcessingIDLength} -eq 2 ] ; then
        postProcessingDimn=$( echo "${Script_Post_Output}" | gawk -F. '{print $3}' )
      elif [ ${postProcessingIDLength} -eq 4 ] ; then
        postProcessingComp=$( echo "${Script_Post_Output}" | gawk -F. '{print $4}' )
        postProcessingFile=$( echo "${Script_Post_Output}" | gawk -F. '{print $5}' )
      fi

      # RabbitMQ message body with specific fields associated message codes treated here
      Body=$( echo "{${genericSimulationID},\"msgCode\":\"${code}\",\"accountingProject\":\"${PROJECT}\",\"jobWarningDelay\":\"${jobWarningDelay}\",\"jobSchedulerID\":\"${jobSchedulerID}\",\"jobSubmissionPath\":\"${SUBMIT_DIR}\",\"msgUID\":\"$(uuidgen)\",\"msgTimestamp\":\"$( date +"%Y-%m-%dT%H:%M:%S.%N%z" )\",\"postProcessingName\":\"${postProcessingName}\",\"postProcessingDate\":\"${postProcessingDate}\",\"postProcessingDimn\":\"${postProcessingDimn}\",\"postProcessingComp\":\"${postProcessingComp}\",\"postProcessingFile\":\"${postProcessingFile}\"}" )
      # Fill the rabbitMQ queue
      IGCM_debug_sendAMQP
    fi

    # Turn the flag on
    ActivateBigBro=true

    # Save project accounting details in a file
    IGCM_sys_projectAccounting cpt_${CENTER}_${PROJECT}_$( date +"%Y%m%d_%H%M" ).dat

    # And send it
    IGCM_debug_sendAMQP_projectAccounting cpt_${CENTER}_${PROJECT}_$( date +"%Y%m%d_%H%M" ).dat

  fi
  IGCM_debug_PopStack "IGCM_debug_BigBro_Initialize"
}

#D-#==================================================================
#D-function IGCM_debug_BigBro_Finalize
#D-* Purpose: Finalize rabbitMQ messages exchanges
#D-
function IGCM_debug_BigBro_Finalize {
  IGCM_debug_PushStack "IGCM_debug_BigBro_Finalize"

  # Message type standard fields:
  # https://github.com/Prodiguer/prodiguer-docs/wiki/MQ-Standard-Message-Fields

  # Message type dictionnary and custom fields:
  # https://github.com/Prodiguer/prodiguer-docs/wiki/Monitoring-Message-Dictionary

  if ( $DEBUG_debug ) ; then
    if [ X${ActivateBigBro} = Xtrue ] ; then
      if [ X${TaskType} = Xcomputing ]; then
        if ( ${simulationIsOver} ) ; then
          # RabbitMQ message code "SIMULATION ENDS"
          code=0100
          FlushAMQP=true
        elif ( ${ExitFlag} ) ; then
          # RabbitMQ message code "EXIT THE JOBS BECAUSE ERROR(S) HAS BEEN TRIGGERED"
          code=1999
          FlushAMQP=true
        else
          # RabbitMQ message code "COMPUTING JOB ENDS"
          code=1100
        fi
      elif [ X${TaskType} = Xpost-processing ]; then
        if ( ${ExitFlag} ) ; then
          # RabbitMQ message code "POST-PROCESSING JOB FAILS"
          code=2999
          FlushAMQP=true
        else
          # RabbitMQ message code "POST-PROCESSING JOB ENDS"
          code=2100
          FlushAMQP=true
        fi
      elif [ X${TaskType} = Xchecking ]; then
        if ( ${ExitFlag} ) ; then
          # RabbitMQ message code "POST-PROCESSING JOB FAILS"
          code=3999
          FlushAMQP=true
        else
          # RabbitMQ message code "POST-PROCESSING JOB ENDS"
          code=3100
          FlushAMQP=true
        fi
      fi
      # RabbitMQ message body
      Body=$( echo "{${genericSimulationID},\"msgCode\":\"${code}\",\"msgUID\":\"$(uuidgen)\",\"msgTimestamp\":\"$( date +"%Y-%m-%dT%H:%M:%S.%N%z" )\"}" )
      # Fill the rabbitMQ queue
      IGCM_debug_sendAMQP
    fi
  fi
  
  IGCM_debug_PopStack "IGCM_debug_BigBro_Finalize"
}

#D-#==================================================================
#D-function IGCM_debug_Exit
#D-* Purpose: Print Call Stack and set ExitFlag to true
#D-
function IGCM_debug_Exit {
  IGCM_debug_PushStack "IGCM_debug_Exit"
  echo "IGCM_debug_Exit : " "${@}"
  echo
  echo "!!!!!!!!!!!!!!!!!!!!!!!!!!"
  echo "!!   ERROR TRIGGERED    !!"
  echo "!!   EXIT FLAG SET      !!"
  echo "!------------------------!"
  echo
  IGCM_debug_CallStack
  ExitFlag=true
  IGCM_debug_PopStack "IGCM_debug_Exit"
}

#D-#==================================================
#D-function IGCM_debug_Verif_Exit
#D-* Purpose: exit with number 1 if ExitFlag is true
#D-

#D-#==================================================================
#D-function IGCM_debug_Print
#D-* Purpose: Print arguments according to a level of verbosity.
#D-
function IGCM_debug_Print
{
  typeset level=$1
  shift

  if [ X"${1}" = X"-e" ]; then
    typeset cmd_echo="echo -e"
    shift
  else
    typeset cmd_echo="echo"
  fi

  if [ ${level} -le ${Verbosity} ] ; then
    typeset i
    case "${level}" in
    1) for i in "$@" ; do
      ${cmd_echo} $(date +"%Y-%m-%d %T") "--Debug1-->" ${i}
      done ;;
    2) for i in "$@" ; do
      ${cmd_echo} $(date +"%Y-%m-%d %T") "--------Debug2-->" ${i}
      done ;;
    3) for i in "$@" ; do
      ${cmd_echo} $(date +"%Y-%m-%d %T") "--------------Debug3-->" ${i}
      done ;;
    esac
  fi
}

#D-#==================================================================
#D-function IGCM_debug_PrintVariables
#D-* Purpose: Print arguments when match a pattern
#D-           according to a level of verbosity.
function IGCM_debug_PrintVariables
{
  typeset level=$1
  shift

  list=$( set | grep ^$1 | sed -e "s/'//g" )

  if [ "X${list}" != X ]  ; then
    IGCM_debug_Print ${level} ${list}
  fi
}

#D-#==================================================================
#D-function IGCM_debug_PrintInfosActions
#D-* Purpose: Print information related to instrumentation
function IGCM_debug_PrintInfosActions
{
  typeset actionType=$1
  typeset entitySize=$2
  typeset start_ms=$3
  typeset end_ms=$4

  typeset dest=$5
  typeset source=$6

  typeset diff_ms entitySizeKo entitySizeMo flux_Ko_ms flux_Ko_s flux_Mo_s
  typeset dirFrom dirTo

  diff_ms=$(( $end_ms - $start_ms ))
  # echo "diff_ms=$diff_ms"

  entitySizeKo=$( echo ${entitySize} | gawk -F"|" '{print $1}' )
  # echo "entitySizeKo=$entitySizeKo"
  entitySizeMo=$( echo ${entitySize} | gawk -F"|" '{print $2}' )

  # flux en Ko / ms
  flux_Ko_ms=$( echo "scale=6;${entitySizeKo}/${diff_ms}" | bc )
  # echo "flux_Ko_ms=$flux_Ko_ms"

  # flux en Ko / s
  flux_Ko_s=$(( $flux_Ko_ms * 1000 ))
  # echo "flux_Ko_s=$flux_Ko_s"

  # flux en Mo / s
  flux_Mo_s=$( echo "scale=6;${flux_Ko_s}/1024" | bc )
  # echo "flux_Mo_s=$flux_Mo_s"

  if [ -d $dest ] ; then
    dirTo=$( readlink -f ${dest} )
  else
    dirTo=$( readlink -f $( dirname ${dest} ) )
  fi

  if [ -d $source ] ; then
    dirFrom=$( readlink -f ${source} )
  else
    dirFrom=$( readlink -f $( dirname ${source} ) )
  fi

  instrumentationContent=$( echo "\"actionName\":\"${actionType}\",\"size_Mo\":\"${entitySizeMo}\",\"duration_ms\":\"${diff_ms}\",\"throughput_Mo_s\":\"${flux_Mo_s}\",\"dirFrom\":\"${dirFrom}\",\"dirTo\":\"${dirTo}\"" )

  if [ X${ActivateStackFilling} = Xtrue ] ; then
    echo "{${instrumentationContent}}" >> ${StackFileLocation}/${StackFileName}
  fi

  # Inform the rabbitMQ queue
  if [ X${ActivateBigBro} = Xtrue ] ; then
    # RabbitMQ message body
    Body=$( echo "{${genericSimulationID},\"msgCode\":\"7000\",\"msgUID\":\"$(uuidgen)\",${instrumentationContent},\"msgTimestamp\":\"$( date +"%Y-%m-%dT%H:%M:%S.%N%z" )\"}" )
    # Fill the rabbitMQ queue
    IGCM_debug_sendAMQP
  fi
}

#D-#==================================================================
#D-function IGCM_debug_Check
#D- * Purpose: Check the present file by comparison with a reference file
function IGCM_debug_Check
{
  #---------------------
  if [ ! -n "${libIGCM}" ] ; then
    echo "Check libIGCM_debug ..........................................[ FAILED ]"
    echo "--Error--> libIGCM variable is not defined"
    exit 2
  fi

  #---------------------
  if [ ! -n "${Verbosity}" ] ; then
    echo "Check libIGCM_debug ..........................................[ FAILED ]"
    echo "--Error--> Verbosity variable is not defined"
    exit 3
  fi

  #---------------------
  # Need to remove timestamps here
  diff ${libIGCM}/libIGCM_debug/IGCM_debug_Test.ref <(${libIGCM}/libIGCM_debug/IGCM_debug_Test.ksh | sed -e "s:[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]\:[0-9][0-9]\:[0-9][0-9] ::g") > /dev/null 2>&1
  status=$?

  if [ ${status} -eq 0 ] ; then
    echo "Check libIGCM_debug ..............................................[ OK ]"
  else
    echo "Check libIGCM_debug ..........................................[ FAILED ]"
    echo "--Error--> Execution of ${libIGCM}/libIGCM_debug/IGCM_debug_Test.ksh"
    echo "           has produced the file IGCM_debug_Test.ref.failed"
    echo "           Please analyse differences with the reference file by typing:"
    echo "           diff IGCM_debug_Test.ref.failed ${libIGCM}/libIGCM_debug/IGCM_debug_Test.ref"
    echo "           Report errors to the author: Patrick.Brockmann@cea.fr"
    diff ${libIGCM}/libIGCM_debug/IGCM_debug_Test.ref <(${libIGCM}/libIGCM_debug/IGCM_debug_Test.ksh | sed -e "s:[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]\:[0-9][0-9]\:[0-9][0-9] ::g")
    exit 4
  fi
  #---------------------
}
