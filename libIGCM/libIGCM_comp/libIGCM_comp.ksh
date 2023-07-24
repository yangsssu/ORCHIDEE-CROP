#!/bin/ksh

#**************************************************************
# Author: Sebastien Denvil, Martial Mancip
# Contact: Sebastien.Denvil__at__ipsl.jussieu.fr Martial.Mancip__at__ipsl.jussieu.fr
# $Revision:: 1289                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2016-03-07 16:49:45 +0100 (Mon, 07 Mar 2016) $ Date of last commit
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************

#========================================================================
function IGCM_comp_Initialize
{
  IGCM_debug_PushStack "IGCM_comp_Initialize"

  # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_comp_Initialize"
  echo

  typeset comp compname comptagname auxprint card_UserChoices first_option option i j
  for comp in ${config_ListOfComponents[*]} ; do

    # Define component
    IGCM_card_DefineArrayFromOption ${SUBMIT_DIR}/config.card ListOfComponents ${comp}
    eval compname=\${config_ListOfComponents_${comp}[0]} > /dev/null 2>&1
    eval comptagname=\${config_ListOfComponents_${comp}[1]} > /dev/null 2>&1

    # Debug Print
    IGCM_debug_Print 1 "Initialize ${comp} : ${compname} component."

    # ${compname}.card PATH
    card=${SUBMIT_DIR}/COMP/${compname}.card

    # Manage component executable
    IGCM_card_DefineArrayFromOption ${SUBMIT_DIR}/config.card Executable ${comp}

    # Define all options in section [comp]
    IGCM_debug_Print 3 " DefineArrayFromSection : ${comp}"
    IGCM_card_DefineArrayFromSection ${SUBMIT_DIR}/config.card ${comp}
    eval config_comp=\${config_${comp}[*]} > /dev/null 2>&1
    for option in ${config_comp[*]} ; do
        IGCM_card_DefineVariableFromOption ${SUBMIT_DIR}/config.card ${comp} ${option}
    done
    IGCM_debug_Print 3 " Found in section config_${comp} :  ${config_comp[*]} "

    # Debug Print :
    eval auxprint=\${config_${comp}_WriteFrequency}
    IGCM_debug_Print 1 "Write frequency for ${compname} : ${auxprint} "
    #2> /dev/null

    # Debug Print :
    IGCM_debug_Print 2 "Initialize following component library"

    # Source drivers in directory DRIVER if it exist
    # else source them from directory COMP
    if [ -d ${SUBMIT_DIR}/DRIVER ] ; then
      IGCM_debug_Print 2 ${SUBMIT_DIR}/DRIVER/${compname}.driver
      # Source component library
      . ${SUBMIT_DIR}/DRIVER/${compname}.driver
    else
      IGCM_debug_Print 2 ${SUBMIT_DIR}/COMP/${compname}.driver
      # Source component library
      . ${SUBMIT_DIR}/COMP/${compname}.driver
    fi
    IGCM_debug_Print 3 "With tag : ${comptagname}"

    # Debug Print
    IGCM_debug_Print 3 "Initialize ${comp} output directory."

    # Define ARCHIVED Dirs
    eval R_OUT_${comp}=${R_SAVE}/${comp}
    eval IGCM_sys_MkdirArchive \${R_OUT_${comp}}

    eval R_OUT_${comp}_O=\${R_OUT_${comp}}/Output
    eval IGCM_sys_MkdirArchive \${R_OUT_${comp}_O}

    eval R_OUT_${comp}_R=\${R_OUT_${comp}}/Restart
    [ ${config_Post_PackFrequency} = NONE ] && eval IGCM_sys_MkdirArchive \${R_OUT_${comp}_R}

    eval R_OUT_${comp}_D=\${R_OUT_${comp}}/Debug
    [ ${config_Post_PackFrequency} = NONE ] && eval IGCM_sys_MkdirArchive \${R_OUT_${comp}_D}

    eval R_OUT_${comp}_O_I=\${R_OUT_${comp}_O}/INS
    eval R_OUT_${comp}_O_H=\${R_OUT_${comp}_O}/HF
    eval R_OUT_${comp}_O_D=\${R_OUT_${comp}_O}/DA
    eval R_OUT_${comp}_O_M=\${R_OUT_${comp}_O}/MO
    eval R_OUT_${comp}_O_Y=\${R_OUT_${comp}_O}/YE

    # Define BUFFERED Dirs
    if ( [ ! ${config_Post_PackFrequency} = NONE ] || [ X${config_UserChoices_SpaceName} = XTEST ] ) ; then
      eval R_BUF_${comp}=${R_BUFR}/${comp}
      eval IGCM_sys_Mkdir \${R_BUF_${comp}}

      eval R_BUF_${comp}_O=\${R_BUF_${comp}}/Output
      eval IGCM_sys_Mkdir \${R_BUF_${comp}_O}

      eval R_BUF_${comp}_R=\${R_BUF_${comp}}/Restart
      eval IGCM_sys_Mkdir \${R_BUF_${comp}_R}

      eval R_BUF_${comp}_D=\${R_BUF_${comp}}/Debug
      eval IGCM_sys_Mkdir \${R_BUF_${comp}_D}

      eval R_BUF_${comp}_O_I=\${R_BUF_${comp}_O}/INS
      eval R_BUF_${comp}_O_H=\${R_BUF_${comp}_O}/HF
      eval R_BUF_${comp}_O_D=\${R_BUF_${comp}_O}/DA
      eval R_BUF_${comp}_O_M=\${R_BUF_${comp}_O}/MO
      eval R_BUF_${comp}_O_Y=\${R_BUF_${comp}_O}/YE
    fi

    # Read UserChoices section of component card
    IGCM_debug_Print 2 "DefineArrayFromSection : ${compname}_UserChoices ${card}"
    IGCM_card_DefineArrayFromSection ${card} UserChoices
    eval first_option=\${${compname}_UserChoices[0]} > /dev/null 2>&1

    # If section is not empty we define corresponding variables
    if [ X${first_option} != X"Error:" ] ; then
      if [ X${card_UserChoices[0]} != X ] ; then
        unset card_UserChoices
      fi
      eval set +A card_UserChoices -- \${${compname}_UserChoices[*]} > /dev/null 2>&1
      IGCM_debug_Print 3 "${compname}_UserChoices_values:"
      for option in ${card_UserChoices[*]} ; do
        IGCM_card_DefineVariableFromOption ${card} UserChoices ${option}
        eval IGCM_debug_Print 3 "${option}=\${${compname}_UserChoices_${option}}"
      done
    fi

    # Read and Build Output File stuff
    IGCM_debug_Print 2 "DefineArrayFromOption  : ${compname}_OutputFiles ${card}"
    IGCM_card_DefineArrayFromOption ${card} OutputFiles List
    ListFilesName=${compname}_OutputFiles_List
    eval FileName0=\${${ListFilesName}[0]} > /dev/null 2>&1
    #
    if [ X${FileName0} != X${NULL_STR} ] ; then
      #
      #IGCM_debug_Print 1 "Component      : ${compname}"
      #
      # INITIALISATION
      #
      eval NbFiles=\${#${ListFilesName}[@]} > /dev/null 2>&1
      #
      i=2
      #
      until [ $i -ge $NbFiles ]; do
        #
        eval flag_post=\${${ListFilesName}[$i]} > /dev/null 2>&1
        #
        if [ X${flag_post} != XNONE ] ; then
          #
          # First of all
          #
          IGCM_card_DefineArrayFromSection ${card} ${flag_post}
          # This section is mandatory
          if [ "X$( eval echo \${${compname}_${flag_post}[@]} )" = "X" ] ; then
            IGCM_debug_Print 1 "IGCM_card_DefineArrayFromSection ${card} ${flag_post}"
            IGCM_debug_Exit "${flag_post} section do not exist in ${card}. Please check your card."
            IGCM_debug_Verif_Exit
          fi
          #
          # Seasonal case : If option Seasonal is not found (old cards) put SEASONAL ON by default
          #
          # variable option allready typeset above
          for option in $( eval echo \${${compname}_${flag_post}[*]} ) ; do
            if [ ${option} = Seasonal ] ; then
              FoundSeasonal=true
              IGCM_card_DefineVariableFromOption ${card} ${flag_post} Seasonal
            fi
          done
          #
          if [ ! X${FoundSeasonal} = Xtrue ] ; then
            eval ${compname}_${flag_post}_Seasonal=ON
          fi
          #
          if [ $( eval echo \${${compname}_${flag_post}_Seasonal} ) = ON ] ; then
            Seasonal=true
          fi

          # Dimension = vide si vieille card.
          IGCM_card_DefineArrayFromOption ${card} ${flag_post} TimeSeriesVars
          IGCM_card_DefineArrayFromOption ${card} ${flag_post} Patches
          if [ X"$( eval echo \${${compname}_${flag_post}_TimeSeriesVars[*]} )" = X"Option not" ] ; then
            # New TimeSeriesVar description, with 2D, 3D and associate ChunckJob.
            ListDimension[0]=2D
            ListDimension[1]=3D
            TimeSeries=false
            iLoop=${#ListDimension[*]}
            j=0
            until [ $j -ge ${iLoop} ]; do
              Dimension=${ListDimension[${j}]}
              IGCM_card_DefineArrayFromOption ${card} ${flag_post} TimeSeriesVars${Dimension}
              IGCM_card_DefineVariableFromOption ${card} ${flag_post} ChunckJob${Dimension}
              #
              # Time series WITHOUT chunk
              #
              if [ ! $( eval echo \${${compname}_${flag_post}_TimeSeriesVars${Dimension}} ) = ${NULL_STR} ] ; then
                if [ $( eval echo \${${compname}_${flag_post}_ChunckJob${Dimension}} ) = NONE ] ; then
                  IGCM_debug_Print 3 "${Dimension} time series activated for ${flag_post}"
                  eval TimeSeries${Dimension}=true
                fi
              fi
              #
              # Time series WITH chunk
              #
              if [ ! $( eval echo \${${compname}_${flag_post}_TimeSeriesVars${Dimension}} ) = ${NULL_STR} ] ; then
                chunck_size=$( eval echo \${${compname}_${flag_post}_ChunckJob${Dimension}} )
                if [ ! ${chunck_size} = NONE ] &&  [ ! ${chunck_size} = OFF ] ; then
                  IGCM_debug_Print 3 "${Dimension} time series activated with chunck for ${flag_post}"
                  eval TimeSeriesChunck${Dimension}=true
                  eval CHUNCK${Dimension}_COMP[\${#CHUNCK${Dimension}_COMP[*]}]=${comp}
                  eval CHUNCK${Dimension}_FLAG[\${#CHUNCK${Dimension}_FLAG[*]}]=${i}
                  eval CHUNCK${Dimension}_NAME[\${#CHUNCK${Dimension}_NAME[*]}]=${flag_post}
                  eval CHUNCK${Dimension}_SIZE[\${#CHUNCK${Dimension}_SIZE[*]}]=${chunck_size}
                fi
              fi
              (( j=j+1 ))
            done
          else
            ListDimension[0]=""
            TimeSeries=true
            TimeSeries2D=false
            TimeSeries3D=false
            TimeSeriesChunck2D=false
            TimeSeriesChunck3D=false
          fi
        fi
        (( i=i+3 ))
      done
    fi
    # Debug Print
    IGCM_debug_Print 3 "Initialize ${compname} with driver."
    # INIT component
    ${comp}_Initialize
    echo
  done

  IGCM_debug_PopStack "IGCM_comp_Initialize"
}

#=======================================================================
function IGCM_comp_PrepareDeletedFiles
{
  IGCM_debug_PushStack "IGCM_comp_PrepareDeletedFiles" $@

  if [ X${2} != X. ] ; then
    eval FileToBeDeleted[${#FileToBeDeleted[@]}]=$( basename ${2} ) > /dev/null 2>&1
  else
    eval FileToBeDeleted[${#FileToBeDeleted[@]}]=$( basename ${1} ) > /dev/null 2>&1
  fi

  IGCM_debug_PopStack "IGCM_comp_PrepareDeletedFiles"
}

#=======================================================================
function IGCM_comp_GetInputInitialStateFiles
{
  IGCM_debug_PushStack "IGCM_comp_GetInputInitialStateFiles"

  # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_comp_GetInputInitialStateFiles"
  echo

  # Only the first time step need InitialStateFiles
  # otherwise it's BoundaryConditions
  if ( ${FirstInitialize} ) ; then
    typeset comp compname comptagname card ListFilesName FileName0 NbFiles i i_
    typeset file_in_ file_in file_out_ file_out do_init
    for comp in ${config_ListOfComponents[*]} ; do
      # Initialize
      do_init="y"
      # Do we need to bring initial state file for this component
      if [ "${config_Restarts_OverRule}" = "y" ] ; then
        eval do_init="n"
      else
        # Read component Restarts parameters
        IGCM_card_DefineVariableFromOption ${SUBMIT_DIR}/config.card ${comp} Restart
        eval do_start=\${config_${comp}_Restart} > /dev/null 2>&1
        if [ "${do_start}" = "y" ] ; then
          do_init="n"
        else
          do_init="y"
        fi
      fi

      if [ "${do_init}" = "y" ] ; then
        # Define component
        eval compname=\${config_ListOfComponents_${comp}[0]} > /dev/null 2>&1
        eval comptagname=\${config_ListOfComponents_${comp}[1]} > /dev/null 2>&1

        # Debug Print :
        IGCM_debug_Print 3 "Initialisation files ${compname}"

        card=${SUBMIT_DIR}/COMP/${compname}.card

        IGCM_card_DefineArrayFromOption ${card} InitialStateFiles List
        ListFilesName=${compname}_InitialStateFiles_List

        eval FileName0=\${${ListFilesName}[0]} > /dev/null 2>&1
        if [ X${FileName0} != X${NULL_STR} ] ; then
          eval NbFiles=\${#${ListFilesName}[@]} > /dev/null 2>&1
          (( i=0 ))
          until [ $i -ge $NbFiles ]; do
            eval file_in_=\${${ListFilesName}[$i]} > /dev/null 2>&1
            eval file_in=${file_in_}
            (( i_ = i+1 ))
            eval file_out_=\${${ListFilesName}[$i_]} > /dev/null 2>&1
            eval file_out=${file_out_}

            IGCM_sys_IsFileArchived ${file_in}
            if [ $? = 0 ] ; then
              IGCM_sys_Get ${file_in} ${file_out}
              #IGCM_comp_PrepareDeletedFiles ${file_in} ${file_out}
            else
              IGCM_sys_Cp ${file_in} ${file_out}
            fi
            (( i=i+2 ))
          done
        fi
      fi
    done
  fi
  IGCM_debug_PopStack "IGCM_comp_GetInputInitialStateFiles"
}

#=======================================================================
# Definition of Smooth modulo function
# usage :
# IGCM_SmoothModulo StringModulo value
#
# StringModulo : A string of min max and modulo like definition of Scilab vectors.
# [min]:[modulo:][max]
# where :
# [] value are optionnals;
# empty min equal 1
# empty max equal infinity
# modulo not given or empty equal 1
# empty string or just ':' equal always.
#
# value : the value to test with the definition
#
# return : true(1)/false(0)
function IGCM_SmoothModulo
{
  IGCM_debug_PushStack "IGCM_SmoothModulo"
  typeset defVector ModValue

  eval set +A defVector -- $( echo "${1}" | \
    gawk -F ':' '{print ($1 == "" ? 1 : $1) " " (NF==3 ? ($2 == "" ? 1 : $2) : 1) " " (NF==3 ? ($3 == "" ? -1 : $3) : ($2 == "" ? -1 : $2))}' )

  # Save Smooth Min and Max. Needed to call IGCM_sys_Get when appropriate
  arr[1]=${defVector[0]}
  arr[2]=${defVector[2]}

  # Test limits :
  # ${defVector[0]} <= ${2} <= ${defVector[2]}
  #          or ${defVector[2]} == -1
  if ( [ ${2} -ge ${defVector[0]} ] && ( [ ${2} -le ${defVector[2]} ] || [ ${defVector[2]} -lt 0 ] ) ) ; then
    # Test modulo
    ModValue=$( expr \( ${2} - ${defVector[0]} \) % ${defVector[1]} )
    if [ ${ModValue} -eq 0 ] ;  then
      arr[3]=true
      echo ${arr[@]}
      IGCM_debug_PopStack "IGCM_SmoothModulo"
      return 1
    else
      arr[3]=false
      echo ${arr[@]}
      IGCM_debug_PopStack "IGCM_SmoothModulo"
      return 0
    fi
  else
    arr[3]=false
    echo ${arr[@]}
    IGCM_debug_PopStack "IGCM_SmoothModulo"
    return 0
  fi
}

#=======================================================================
function IGCM_comp_GetInputSmoothFiles
{
  IGCM_debug_PushStack "IGCM_comp_GetInputSmoothFiles"

  # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_comp_GetInputSmoothFiles"
  echo

  typeset comp compname comptagname card ListFilesName FileName0 NbFiles j i i_ i__
  typeset file_in_ file_in file_out_ file_out ret SmoothDef aux val

  for comp in ${config_ListOfComponents[*]} ; do
    # Define component
    eval compname=\${config_ListOfComponents_${comp}[0]} > /dev/null 2>&1
    eval comptagname=\${config_ListOfComponents_${comp}[1]} > /dev/null 2>&1

    # Debug Print :
    IGCM_debug_Print 3 "Smooth files ${compname}"

    card=${SUBMIT_DIR}/COMP/${compname}.card

    IGCM_card_DefineArrayFromOption ${card} SmoothFiles List
    ListFilesName=${compname}_SmoothFiles_List
    eval FileName0=\${${ListFilesName}[0]} > /dev/null 2>&1

    if ( [ X${FileName0} != X${NULL_STR} ] && [ X${FileName0} != X"Section" ] ) ; then
      eval NbFiles=\${#${ListFilesName}[@]} > /dev/null 2>&1

      (( i=0 ))
      until [ $i -ge $NbFiles ]; do
        eval file_in_=\${${ListFilesName}[$i]} > /dev/null 2>&1
        eval file_in=${file_in_}
        (( i_ = i+1 ))
        eval file_out_=\${${ListFilesName}[$i_]} > /dev/null 2>&1
        eval file_out=${file_out_}

        # define CumulPeriod definition for this file
        (( i__ = i+2 ))
        eval SmoothDef=\${${ListFilesName}[$i__]}
        IGCM_debug_Print 3 "  ${file_in} ${SmoothDef}"
        aux=$( IGCM_SmoothModulo ${SmoothDef} ${CumulPeriod} )
        j=1
        for val in ${aux} ; do
          [ ${j} -eq 1 ] && SmoothMin=${val}
          [ ${j} -eq 2 ] && SmoothMax=${val}
          [ ${j} -eq 3 ] && ret=${val}
          (( j=j+1 ))
        done
        [ ${SmoothMax} -eq -1 ] && SmoothMax=${CumulPeriod}
        if ( [ X${ret} = Xtrue ] || ( [ ${Period} -eq 1 ] && [ ${CumulPeriod} -ge ${SmoothMin} ] && [ ${CumulPeriod} -le ${SmoothMax} ] ) ) ; then

          IGCM_sys_IsFileArchived ${file_in}
          if [ $? = 0 ] ; then
            IGCM_sys_Get ${file_in} ${file_out}
            #IGCM_comp_PrepareDeletedFiles ${file_in} ${file_out}
          else
            IGCM_sys_Cp ${file_in} ${file_out}
          fi
        fi
        (( i=i+3 ))
      done
    fi
  done

  IGCM_debug_PopStack "IGCM_comp_GetInputSmoothFiles"
}

#=======================================================================
function IGCM_comp_GetInputBoundaryFiles
{
  IGCM_debug_PushStack "IGCM_comp_GetInputBoundaryFiles"

  # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_comp_GetInputBoundaryFiles"
  echo

  typeset comp compname comptagname card ListFilesName FileName0 NbFiles i i_
  typeset file_in_ file_in file_out_ file_out

  if [ ${Period} = 1 ]; then
    ListFixBoundary=" "
  fi

  for comp in ${config_ListOfComponents[*]} ; do

    # Define component
    eval compname=\${config_ListOfComponents_${comp}[0]} > /dev/null 2>&1
    eval comptagname=\${config_ListOfComponents_${comp}[1]} > /dev/null 2>&1

    # Debug Print :
    IGCM_debug_Print 3 "Boundary files ${compname}"

    card=${SUBMIT_DIR}/COMP/${compname}.card

    IGCM_card_DefineArrayFromOption ${card} BoundaryFiles List
    ListFilesName=${compname}_BoundaryFiles_List
    eval FileName0=\${${ListFilesName}[0]} > /dev/null 2>&1

    if [ X${FileName0} != X${NULL_STR} ] ; then
      eval NbFiles=\${#${ListFilesName}[@]} > /dev/null 2>&1

      (( i=0 ))
      until [ $i -ge $NbFiles ]; do
        eval file_in_=\${${ListFilesName}[$i]} > /dev/null 2>&1
        eval file_in=${file_in_}
        (( i_ = i+1 ))
        eval file_out_=\${${ListFilesName}[$i_]} > /dev/null 2>&1
        eval file_out=${file_out_}

        IGCM_sys_IsFileArchived ${file_in}
        if [ $? = 0 ] ; then
          IGCM_sys_Get ${file_in} ${file_out}
          IGCM_comp_PrepareDeletedFiles ${file_in} ${file_out}
        else
          IGCM_sys_Cp ${file_in} ${file_out}
        fi

        (( i=i+2 ))
      done
    fi

    # Get non deleted files
    if [ ${Period} = 1 ]; then

      IGCM_card_DefineArrayFromOption ${card} BoundaryFiles ListNonDel
      ListFilesName=${compname}_BoundaryFiles_ListNonDel
      eval FileName0=\${${ListFilesName}[0]} > /dev/null 2>&1

      if [ X${FileName0} != X${NULL_STR} ] ; then
        eval NbFiles=\${#${ListFilesName}[@]} > /dev/null 2>&1

        (( i=0 ))
        until [ $i -ge $NbFiles ]; do
          eval file_in_=\${${ListFilesName}[$i]} > /dev/null 2>&1
          eval file_in=${file_in_}
          (( i_ = i+1 ))
          eval file_out_=\${${ListFilesName}[$i_]} > /dev/null 2>&1
          eval file_out=${file_out_}

          IGCM_sys_IsFileArchived ${file_in}
          if [ $? = 0 ] ; then
            IGCM_sys_Get ${file_in} ${file_out}
            #IGCM_comp_PrepareDeletedFiles ${file_in} ${file_out}
          else
            IGCM_sys_Cp ${file_in} ${file_out}
          fi

          if [ X${file_out} != X. ] ; then
            ListFixBoundary=${ListFixBoundary}" "${file_out}
          else
            ListFixBoundary=${ListFixBoundary}" "$( basename ${file_in} )
          fi

          (( i=i+2 ))
        done
      fi
    fi
  done

  IGCM_debug_PopStack "IGCM_comp_GetInputBoundaryFiles"
}

#=======================================================================
function IGCM_comp_DelFixeBoundaryFiles
{
  IGCM_debug_PushStack "IGCM_comp_DelFixeBoundaryFiles"

  # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_comp_DelFixeBoundaryFiles"
  echo

  ls -l ${ListFixBoundary}
  rm -f ${ListFixBoundary}

  IGCM_debug_PopStack "IGCM_comp_DelFixeBoundaryFiles"
}

#=======================================================================
function IGCM_comp_GetInputParametersFiles
{
  IGCM_debug_PushStack "IGCM_comp_GetInputParametersFiles"

  # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_comp_GetInputParametersFiles"
  echo

  typeset comp compname comptagname card ListFilesName FileName0 NbFiles i i_ file_in file_out
  for comp in ${config_ListOfComponents[*]} ; do
    # Define component
    eval compname=\${config_ListOfComponents_${comp}[0]} > /dev/null 2>&1
    eval comptagname=\${config_ListOfComponents_${comp}[1]} > /dev/null 2>&1

    # Debug Print :
    IGCM_debug_Print 3 "Parameters ${compname}"

    card=${SUBMIT_DIR}/COMP/${compname}.card

    IGCM_card_DefineArrayFromOption ${card} ParametersFiles List
    ListFilesName=${compname}_ParametersFiles_List
    eval FileName0=\${${ListFilesName}[0]} > /dev/null 2>&1

    if [ X${FileName0} != X${NULL_STR} ] ; then
      eval NbFiles=\${#${ListFilesName}[@]} > /dev/null 2>&1

      (( i=0 ))
      until [ $i -ge $NbFiles ]; do
        eval file_in_=\${${ListFilesName}[$i]} > /dev/null 2>&1
        eval file_in=${file_in_}
        (( i_ = i+1 ))
        eval file_out_=\${${ListFilesName}[$i_]} > /dev/null 2>&1
        eval file_out=${file_out_}

        IGCM_sys_Cp ${file_in} ${file_out}
        IGCM_comp_PrepareDeletedFiles ${file_in} ${file_out}

        (( i=i+2 ))
      done
    fi
  done

  IGCM_debug_PopStack "IGCM_comp_GetInputParametersFiles"
}

#=======================================================================
function IGCM_comp_GetInputRestartFiles
{
  IGCM_debug_PushStack "IGCM_comp_GetInputRestartFiles"

  # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_comp_GetInputRestartFiles"
  echo

  typeset Date_tmp Date_r Path_r do_start CompOldName Path_OUT Path_BUF
  typeset Buffered Archived Tared PotentialTarFile IsMatching TarFileFound
  typeset comp compname comptagname card ListFilesName FileName0 NbFiles i i_
  typeset file_in file_out file_in_ file_out_ file_in_Name
  typeset -Z4 j4
  #BASH declare j4

  IsMatching=""

  for comp in ${config_ListOfComponents[*]} ; do
    # Define component
    eval compname=\${config_ListOfComponents_${comp}[0]} > /dev/null 2>&1
    eval comptagname=\${config_ListOfComponents_${comp}[1]} > /dev/null 2>&1
    #
    card=${SUBMIT_DIR}/COMP/${compname}.card
    #
    IGCM_card_DefineArrayFromOption ${card} RestartFiles List
    ListFilesName=${compname}_RestartFiles_List
    eval FileName0=\${${ListFilesName}[0]} > /dev/null 2>&1

    # Debug Print :
    IGCM_debug_Print 3 "restart ${compname}"

    if ( ${FirstInitialize} ) ; then

      if [ "${config_Restarts_OverRule}" = "y" ] ; then
        eval config_${comp}_Restart="y"
        eval config_${comp}_RestartDate=${config_Restarts_RestartDate}
        eval config_${comp}_RestartJobName=${config_Restarts_RestartJobName}
        eval config_${comp}_RestartPath=${config_Restarts_RestartPath}
        eval do_start=\${config_${comp}_Restart} > /dev/null 2>&1
        eval CompOldName=${comp}
      else
        # Read component Restarts parameters
        IGCM_card_DefineVariableFromOption ${SUBMIT_DIR}/config.card ${comp} Restart
        eval do_start=\${config_${comp}_Restart} > /dev/null 2>&1

        if [ "${do_start}" = "y" ] ; then
          IGCM_card_DefineVariableFromOption ${SUBMIT_DIR}/config.card ${comp} RestartDate
          IGCM_card_DefineVariableFromOption ${SUBMIT_DIR}/config.card ${comp} RestartJobName
          IGCM_card_DefineVariableFromOption ${SUBMIT_DIR}/config.card ${comp} RestartPath
        else
          eval config_${comp}_RestartDate=-1
          eval config_${comp}_RestartJobName=${NULL_STR}
          eval config_${comp}_RestartPath=${NULL_STR}
        fi

        eval CompOldName=\${config_${comp}_OldName}
        if [ X${CompOldName} = X ] ; then
          eval CompOldName=${comp}
        fi

        # Reinitialize IsMatching to allow searching for a different tar file for each component.
        IsMatching=""
        TarFileFound=""
      fi

      if [ "${do_start}" = "y" ] ; then

        # Restore Restarts files
        #-----------------------
        if ( [ X${FileName0} != X${NULL_STR} ] && [ X${FileName0} != XNONE ] ) ; then
          eval NbFiles=\${#${ListFilesName}[@]} > /dev/null 2>&1

          (( i=1 ))
          until [ $i -gt $NbFiles ]; do
            eval file_in_=\${${ListFilesName}[$i]} > /dev/null 2>&1
            eval file_in=${file_in_}
            (( i_ = i+1 ))
            eval file_out_=\${${ListFilesName}[$i_]} > /dev/null 2>&1
            eval file_out=${file_out_}

            eval Date_tmp=\${config_${comp}_RestartDate}
            Date_r=$( IGCM_date_ConvertFormatToGregorian ${Date_tmp} )
            # will be re-use
            eval RestartPath=\${config_${comp}_RestartPath}
            eval RestartJobName=\${config_${comp}_RestartJobName}
            #
            Path_r=${RestartPath}/${RestartJobName}/${CompOldName}/Restart
            file_in_Name=${RestartJobName}_${Date_r}_${file_in}

            extension_in=$( echo ${file_in_Name##*.} )
            extension_out=$( echo ${file_out##*.} )

            generic_restart_file_name_in=$( basename ${file_in_Name} .${extension_in} )
            generic_restart_file_name_out=$( basename ${file_out} .${extension_out} )

            Path_OUT=${Path_r}/${generic_restart_file_name_in}

            if [ $( IGCM_sys_TestFileBuffer ${Path_OUT}*.${extension_in} ; echo $? ) = 0 ] ; then
              IGCM_debug_Print 3 "Buffered restart ${Path_OUT}*.${extension_in} "
              Buffered=true
              Archived=false
              Tared=false
              nb_restart_file=$(IGCM_sys_CountFileBuffer ${Path_OUT}_????.${extension_in})
            elif [ $( IGCM_sys_TestFileArchive ${Path_OUT}*.${extension_in} ; echo $? ) = 0 ] ; then
              IGCM_debug_Print 3 "Archived restart ${Path_OUT}*.${extension_in}"
              Buffered=false
              Archived=true
              Tared=false
              nb_restart_file=$(IGCM_sys_CountFileArchive ${Path_OUT}_????.${extension_in})
            else
              IGCM_debug_Print 3 "No restart file in the buffer nor in the archive directory"
              IGCM_debug_Print 3 "${Path_OUT}*.${extension_in} do not exist"
              IGCM_debug_Print 3 "Restart files will now be searched for in : ${RestartPath}/${RestartJobName}/RESTART"
              Buffered=false
              Archived=false
              Tared=true
              # Look after the tar file we want if we did not found it already
              if [ X${IsMatching} = X ] ; then
                IGCM_sys_TestDirArchive ${RestartPath}/${RestartJobName}/RESTART
                if [ $? ] ; then
                  for PotentialTarFile in $( IGCM_sys_RshArchive "find ${RestartPath}/${RestartJobName}/RESTART -name "${RestartJobName}_*restart*.tar" -print" ) ; do
                    IsMatching=$( echo ${PotentialTarFile##*/} | \
                      sed "s:_restart::" | \
                      sed "s:^${RestartJobName}_::" | \
                      sed "s:\.tar$::" | \
                      gawk -F_ -v restartdate=${Date_r} \
                      '{if (($1 <= restartdate) && ($2 >= restartdate)) {print $1"_"$2}}' )
                    if [ ! X${IsMatching} = X ] ; then
                      TarFileFound=${PotentialTarFile}
                      break
                    fi
                  done
                fi
                # Stop here if nothing has been found
                if [ X${TarFileFound} = X ] ; then
                  IGCM_debug_Print 3 "Restart files were not found!"
                  IGCM_debug_Print 3 "Restart files have been searched for in buffer and archive directory."
                  IGCM_debug_Print 3 "They have been searched for in packed and unpacked format."
                  IGCM_debug_Exit "Please double check restart settings in config.card"
                  IGCM_debug_Verif_Exit
                fi
              fi
              IGCM_sys_PrepareTaredRestart ${TarFileFound}
              TarFileLocation=$( basename ${TarFileFound} )
              IGCM_debug_Print 1 "tar xvf ${TarFileLocation} ${comp}_${generic_restart_file_name_in}*.${extension_in}"
              tar xvf ${TarFileLocation} ${comp}_${generic_restart_file_name_in}*.${extension_in}
              nb_restart_file=$( IGCM_sys_CountFileBuffer ${comp}_${generic_restart_file_name_in}_????.${extension_in} )
            fi

            if [ ${nb_restart_file} -gt 1 ] ; then
              j=0                                      # BASH LINE NOT NEEDED
              # BASH for j4 in in $( eval echo {0000..$(( nb_restart_file - 1 ))} ) ; do
              until [ $j -ge ${nb_restart_file} ]; do  # BASH LINE NOT NEEDED
                j4=${j}                                # BASH LINE NOT NEEDED
                if [ X${Buffered} = Xtrue ] ; then
                  IGCM_sys_GetBuffer ${Path_OUT}_${j4}.${extension_in} ${generic_restart_file_name_out}_${j4}.${extension_out}
                elif [ X${Archived} = Xtrue ] ; then
                  IGCM_sys_Get ${Path_OUT}_${j4}.${extension_in} ${generic_restart_file_name_out}_${j4}.${extension_out}
                elif [ X${Tared} = Xtrue ] ; then
                  IGCM_sys_Mv ${comp}_${generic_restart_file_name_in}_${j4}.${extension_in} ${generic_restart_file_name_out}_${j4}.${extension_out}
                fi
                (( j=j+1 ))                            #BASH LINE NOT NEEDED
              done

              # OCE SPECIFIC TO REBUILD RESTART WHEN NUMBER OF RESTART FILES DONT MATCH MPI PROCESS
              if [ X${OCE_PROC_MPI} != X ] ; then
                if [ ${OCE_PROC_MPI} -ne ${nb_restart_file} ] ; then
                  IGCM_sys_rebuild ${generic_restart_file_name_out}.${extension_out} ${generic_restart_file_name_out}_????.${extension_out}
                  IGCM_sys_Rm ${generic_restart_file_name_out}_????.${extension_out}
                fi
              fi
            else
              if [ X${Buffered} = Xtrue ] ; then
                IGCM_sys_GetBuffer ${Path_r}/${file_in_Name} ${file_out}
              elif [ X${Archived} = Xtrue ] ; then
                IGCM_sys_Get ${Path_r}/${file_in_Name} ${file_out}
              elif [ X${Tared} = Xtrue ] ; then
                IGCM_sys_Mv ${comp}_${file_in_Name} ${file_out}
              fi
            fi
            (( i=i+3 ))
          done
        else
          if [ X${FileName0} != XNONE ] ; then
            IGCM_debug_Exit "IGCM_comp_GetInputRestartFiles : No file in list for ${compname}."
          else
            IGCM_debug_Print 1 "IGCM_comp_GetInputRestartFiles : NONE specified in Restart List ${compname}."
          fi
        fi
      fi
    elif ( [ ${Period} -eq 1 ] && [ ${DRYRUN} -eq 0 ] ) ; then
      # if not FirstInitialize and first loop of this job

      # Restore Restarts files
      #-----------------------
      if ( [ X${FileName0} != X${NULL_STR} ] && [ X${FileName0} != XNONE ] ) ; then
        eval NbFiles=\${#${ListFilesName}[@]} > /dev/null 2>&1

        (( i=1 ))
        until [ $i -gt $NbFiles ]; do
          eval file_in_=\${${ListFilesName}[$i]} > /dev/null 2>&1
          eval file_in=${file_in_}
          (( i_ = i+1 ))
          eval file_out_=\${${ListFilesName}[$i_]} > /dev/null 2>&1
          eval file_out=${file_out_}

          file_in_Name=${config_UserChoices_JobName}_${LastPeriodDateEnd}_${file_in}

          extension_in=$( echo ${file_in_Name##*.} )
          extension_out=$( echo ${file_out##*.} )

          generic_restart_file_name_in=$( basename ${file_in_Name} .${extension_in} )
          generic_restart_file_name_out=$( basename ${file_out} .${extension_out} )

          eval Path_BUF=\${R_BUF_${comp}_R}/${generic_restart_file_name_in}
          eval Path_OUT=\${R_OUT_${comp}_R}/${generic_restart_file_name_in}

          if [ $( IGCM_sys_TestFileBuffer ${Path_BUF}*.${extension_in} ; echo $? ) = 0 ] ; then
            IGCM_debug_Print 3 "Buffered restart ${Path_BUF}*.${extension_in}"
            Buffered=true
            Archived=false
            Tared=false
            nb_restart_file=$(IGCM_sys_CountFileBuffer ${Path_BUF}_????.${extension_in})
          elif [ $( IGCM_sys_TestFileArchive ${Path_OUT}*.${extension_in} ; echo $? ) = 0 ] ; then
            IGCM_debug_Print 3 "Archived restart ${Path_OUT}*.${extension_in}"
            Buffered=false
            Archived=true
            Tared=false
            nb_restart_file=$(IGCM_sys_CountFileArchive ${Path_OUT}_????.${extension_in})
          else
            IGCM_debug_Print 3 "No restart file in the buffer nor in the archive directory"
            IGCM_debug_Print 3 "Restart files will now be searched for in : ${R_SAVE}/RESTART"
            Buffered=false
            Archived=false
            Tared=true
            # Look after the tar file we want if we did not found it already
            if [ X${IsMatching} = X ] ; then
              for PotentialTarFile in $( IGCM_sys_RshArchive "find ${R_SAVE}/RESTART -name "${config_UserChoices_JobName}_*_restart.tar" -print" ) ; do
                IsMatching=$( echo ${PotentialTarFile##*/} | sed "s:^${config_UserChoices_JobName}_::" | sed "s:\.restart\.tar$::" | gawk -F_ -v restartdate=${LastPeriodDateEnd} '{if (($1 < restartdate) && ($2 >= restartdate)) {print $1"_"$2}}' )
                if [ ! X${IsMatching} = X ] ; then
                  TarFileFound=${PotentialTarFile}
                  break
                fi
              done
            fi
            # Stop here if nothing has been found
            if [ X${TarFileFound} = X ] ; then
              IGCM_debug_Print 3 "Restart files were not found!"
              IGCM_debug_Print 3 "Restart files have been searched for in buffer and archive directory."
              IGCM_debug_Print 3 "They have been searched for in packed and unpacked format."
              IGCM_debug_Exit "Please double check restart settings in config.card"
              IGCM_debug_Verif_Exit
            fi
            IGCM_sys_PrepareTaredRestart ${TarFileFound}
            TarFileLocation=$( basename ${TarFileFound} )
            IGCM_debug_Print 1 "tar xvf ${TarFileLocation} ${comp}_${generic_restart_file_name_in}*.${extension_in}"
            tar xvf ${TarFileLocation} ${comp}_${generic_restart_file_name_in}*.${extension_in}
            nb_restart_file=$( IGCM_sys_CountFileBuffer ${comp}_${generic_restart_file_name_in}_????.${extension_in} )
          fi

          if [ ${nb_restart_file} -gt 1 ] ; then
            j=0                                     # BASH LINE NOT NEEDED
            #BASH for j4 in in $( eval echo {0000..$(( nb_restart_file - 1 ))} ) ; do
            until [ $j -ge ${nb_restart_file} ]; do # BASH LINE NOT NEEDED
              j4=${j}                               # BASH LINE NOT NEEDED
              if [ X${Buffered} = Xtrue ] ; then
                IGCM_sys_GetBuffer ${Path_BUF}_${j4}.${extension_in} ${generic_restart_file_name_out}_${j4}.${extension_out}
              elif [ X${Archived} = Xtrue ] ; then
                IGCM_sys_Get ${Path_OUT}_${j4}.${extension_in} ${generic_restart_file_name_out}_${j4}.${extension_out}
              elif [ X${Tared} = Xtrue ] ; then
                IGCM_sys_Mv ${comp}_${generic_restart_file_name_in}_${j4}.${extension_in} ${generic_restart_file_name_out}_${j4}.${extension_out}
              fi
              (( j=j+1 ))                           # BASH LINE NOT NEEDED
            done
          else
            if [ X${Buffered} = Xtrue ] ; then
              eval IGCM_sys_GetBuffer \${R_BUF_${comp}_R}/${file_in_Name} ${file_out}
            elif [ X${Archived} = Xtrue ] ; then
              eval IGCM_sys_Get \${R_OUT_${comp}_R}/${file_in_Name} ${file_out}
            elif [ X${Tared} = Xtrue ] ; then
              IGCM_sys_Mv ${comp}_${file_in_Name} ${file_out}
            fi
          fi
          (( i=i+3 ))
        done
      else
        if [ X${FileName0} != XNONE ] ; then
          IGCM_debug_Exit "IGCM_comp_GetInputRestartFiles : No file in list for ${compname}."
        else
          IGCM_debug_Print 1 "IGCM_comp_GetInputRestartFiles : NONE specified in Restart List ${compname}."
        fi
      fi
    fi
  done

  NbFiles=$( ls * 2> /dev/null | wc -l )
  if [ ${NbFiles} -gt 0 ] ; then
    IGCM_sys_Chmod u+rw *
  fi

  IGCM_debug_PopStack "IGCM_comp_GetInputRestartFiles"
}

#=======================================================================
function IGCM_comp_PeriodStart
{
  IGCM_debug_PushStack "IGCM_comp_PeriodStart"

  # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_comp_PeriodStart"
  echo

  typeset ExeNameIn ExeNameOut
  typeset comp compname comptagname
  for comp in ${config_ListOfComponents[*]} ; do
    # Define component
    eval compname=\${config_ListOfComponents_${comp}[0]} > /dev/null 2>&1
    eval comptagname=\${config_ListOfComponents_${comp}[1]} > /dev/null 2>&1

    # Copy executable for this component
    eval ExeNameIn=\${config_Executable_${comp}[0]}
    eval ExeNameOut=\${config_Executable_${comp}[1]}

    # Debug Print
    IGCM_debug_Print 3 "PeriodStart ${compname} Driver Function (if any)."
    # UPDATE component
    ${comp}_PeriodStart 2> /dev/null

  done

  IGCM_debug_PopStack "IGCM_comp_PeriodStart"
}



#=======================================================================
function IGCM_comp_modifyFile
{
#
# syntax:     IGCM_comp_modifyFile  filein  key  [value]
#
# For example : IGCM_comp_modifyFile metrics_template.py case_id \'SE_${YEARS}\'
#
# This function is used to replace a pattern in a file for a specific variable.
#
# Arguments:
# - filein : the file in run directory in which the variable should be set
# - key    : the variable to modify
# - value  : the value to set the key equal to
#
  IGCM_debug_PushStack "IGCM_comp_modifyFile"

  typeset filein key value pattern

  # Set local variables and test the arguments
  if [ $# = 3 ] ; then
    # Normal case with 3 arguments
    filein=$1 ; key=$2 ; value=$3
  else
    IGCM_debug_Exit "IGCM_comp_modifyFile: Bad number of arguments."
    IGCM_debug_PopStack "IGCM_comp_modifyFile"
    return
  fi
  IGCM_debug_Print 1 "Entering IGCM_comp_modifyFile with arguments: ${filein} ${key} ${value}"

  # Test if the file exist
  if [ ! -f ${filein} ] ; then
    IGCM_debug_Exit "IGCM_comp_modifyFile: ${filein} does not exist."
    IGCM_debug_PopStack "IGCM_comp_modifyFile"
    return
  fi

  # Read the line with key in the file without the comments
  pattern=$( grep "^ *${key} *=" ${filein} | sed -e "s% *\!.*%%" )

  # Verify the existance of the pattern
  if [ X"${pattern}" = X ] ; then
    # Variable key is not set in filein, stop.
    IGCM_debug_Exit "IGCM_comp_modifyFile : Variable ${key} is not set in correct file. It should be set in ${filein}."
    IGCM_debug_PopStack "IGCM_comp_modifyFile"
    return
  fi

  # Now change key in filein
  #sed -e "s:${pattern}:${key}=${value}:" ${filein} > ${filein}.tmp
  sed -e "s:^ *${key} *=.*:${key}=${value}:" ${filein} > ${filein}.tmp
  IGCM_debug_Print 1 "IGCM_comp_modifyFile: In ${filein} set ${key}=${value}"
  \mv ${filein}.tmp ${filein}
  
  IGCM_debug_PopStack "IGCM_comp_modifyFile"
}

#=======================================================================
function IGCM_comp_modifyDefFile
{
#
# syntax:     IGCM_comp_modifyDefFile  type  filein  key  [value]
#
# For example : IGCM_comp_modifyDefFile blocker run.def day_step 1200
#
# This function is used to modify a parameter file for a specific variable.
# The file must be a ".def" file, i.e. with IOIPSL parameter file syntax.
# This function can be used in the comp.driver files for the components.
#
# Arguments:
# - type   : first argument must be blocker, nonblocker or force
#            For "blocker" case, the variable must be attributed the keyworld AUTO
#            otherwise this function will exit.
#            For "nonblocker" case, the user can remove or modify the variable. For
#            this case, as long as AUTO is not set, no modification will be done.
#            For "force" case, the variable will be modified even if it is not set to AUTO
# - filein : the file in run directory of .def type in which the variable should be set
# - key    : the variable to modify
# - value  : the value to set the key equal to, optional. If value is not set or if
#            value=DEFAULT, then a default value must be given in filein using syntax :
#            key= AUTO : DEFAULT=def_value
#
  IGCM_debug_PushStack "IGCM_comp_modifyDefFile"

  typeset type filein key value
  typeset filelist nb_occ modify

  # Set local variables and test the arguments
  if [ $# = 4 ] ; then
    # Normal case with 4 arguments
    type=$1 ; filein=$2 ; key=$3 ; value=$4
  elif [ $# = 3 ] ; then
    # Normal case with 3 arguments
    type=$1 ; filein=$2 ;	key=$3; value="DEFAULT"
  else
    IGCM_debug_Exit "IGCM_comp_modifyDefFile: Bad number of arguments."
    IGCM_debug_PopStack "IGCM_comp_modifyDefFile"
    return
  fi
  IGCM_debug_Print 1 "Entering IGCM_comp_modifyDefFile with arguments: ${type} ${filein} ${key} ${value}"

  # Test if first argument is correct
  if [ ${type} != blocker ] && [ ${type} != nonblocker ] && [ ${type} != force ] ; then
    IGCM_debug_Exit "IGCM_comp_modifyDefFile: Error in first argument must be blocker, nonblocker or force"
    IGCM_debug_PopStack "IGCM_comp_modifyDefFile"
    return
  fi

  # Test if the file exist. 
  # Exit with error if the file does not exist for the case blocker or force. 
  # Only return for the case nonblocker. 
  if [ ! -f ${filein} ] ; then
    if [ ${type} = blocker ] || [ ${type} = force ] ; then
      IGCM_debug_Exit "IGCM_comp_modifyDefFile: ${filein} does not exist."
    else
      IGCM_debug_Print 1 "IGCM_comp_modifyDefFile: ${filein} does not exist. Nothing will be done for this file."
    fi
    IGCM_debug_PopStack "IGCM_comp_modifyDefFile"
    return
  fi

  # Define list of files to test using all files with suffix .def (except used*.def)
  filelist=$( ls *def | grep -v used )

  # Count number of occurances for the key in all files
  nb_occ=$( grep -w ${key} ${filelist} | grep -v "#"  | wc -l )

  # Test if key is set several times
  if [ ${nb_occ} -gt 1 ] ; then
    IGCM_debug_Exit "IGCM_comp_modifyDefFile : Error in ${filein}: Variable=${key} is set ${nb_occ} times"
    IGCM_debug_PopStack "IGCM_comp_modifyDefFile"
    return
  fi

  # Treatement according to different cases
  if [ ${nb_occ} -eq 0 ] && [ ${type} = blocker ] ; then
    # Stop if the key is never set and the function is blocker
    IGCM_debug_Exit "IGCM_comp_modifyDefFile : Error in ${filein}: Variable=${key} has been removed but this function is blocker. "
    IGCM_debug_Print 1 "IGCM_comp_modifyDefFile: restore ${filein} for variable ${key}."
  elif [ ${nb_occ} -eq 0 ] && [ ${type} = nonblocker ] ; then
    # The key is not set but it is a nonblocker call so nothing is done.
    IGCM_debug_Print 1 "IGCM_comp_modifyDefFile: ${key} is not set in ${filein}. This is a nonblocker call so nothing is done."
    IGCM_debug_Print 1 "IGCM_comp_modifyDefFile: Default value for ${key} from the model will be used."
    modify=no
  elif [ $( grep ${key} ${filein} | grep -v "\#"  |wc -l ) = 0 ] ; then
    # Variable key is not set in filein, stop.
    IGCM_debug_Exit "IGCM_comp_modifyDefFile : Variable ${key} is not set in correct file. It should be set in ${filein}."
  fi

  # Check if AUTO is set in the filein on the same line as the key variable
  if [ $( grep -w ${key} ${filein} | grep -v "\#" | grep AUTO | wc -l ) = 1 ] ; then
    # Modification will be done for all cases
    modify=yes
  else
    # The variable was not set to AUTO
    if [ ${type} = blocker ] ; then
      # Exit because this is a blocker call
      IGCM_debug_Exit "IGCM_comp_modifyDefFile : The variable ${key} cannot be modified. It should be set to AUTO."
      IGCM_debug_PopStack "IGCM_comp_modifyDefFile"
      return
    elif [ ${type} = nonblocker ] ; then
      # Do nothing. Suppose that the user did set the variable correct
      IGCM_debug_Print 1 "IGCM_comp_modifyDefFile: ${key} is set by the user. Nothing done."
      modify=no
    elif [ ${type} = force ] ; then
      # Force modification
      IGCM_debug_Print 1 "IGCM_comp_modifyDefFile : Variabl=${key} was not set to AUTO. Modification will be forced."
      modify=yes
    fi
  fi

  # Do the modifcation now
  if [ ${modify} = yes ] ; then

    # For option DEFAULT, read default value from file.
    if [ X"${value}" = XDEFAULT ] || [ X"${value}" = X ] ; then
      # Case to set DEFAULT value
      # Read default value from filein
      value=$( grep ${key} ${filein} | grep -v "\#" | awk  -F"DEFAULT *=" '{print $2}')

      if [ X"${value}" = X ] ; then
        IGCM_debug_Exit "IGCM_comp_modifyDefFile : The variable ${key} needs a DEFAULT value in ${filein}."
        IGCM_debug_Print 1 "IGCM_comp_modifyDefFile: The syntax in ${filein} should be:"
        IGCM_debug_Print 1 "IGCM_comp_modifyDefFile: ${key}=_AUTO_:DEFAULT=def_value"
        IGCM_debug_PopStack "IGCM_comp_modifyDefFile"
        return
      fi
    fi

    # Now change key in filein
    sed -e "s/^${key}\ *=.*/${key}= ${value}/" ${filein} > ${filein}.tmp
    IGCM_debug_Print 1 "IGCM_comp_modifyDefFile: In ${filein} set ${key}=${value}"
    \mv ${filein}.tmp ${filein}
  fi
  IGCM_debug_PopStack "IGCM_comp_modifyDefFile"
}

#=======================================================================
function IGCM_comp_modifyNamelist
{
#
# syntax:     IGCM_comp_modifyNamelist  type  filein  key  [value]
#
# For example : IGCM_comp_modifyNamelist blocker run.def day_step 1200
#
# This function is used to modify a parameter file for a specific variable.
# The file must be a "namelist" file, i.e. with fortran namelist syntax.
# This function can be used in the comp.driver files for the components.
#
# Arguments:
# - type   : first argument must be blocker, nonblocker or force
#            For "blocker" case, the variable must be attributed the keyworld AUTO
#            otherwise this function will exit.
#            For "nonblocker" case, the user can remove or modify the variable. For
#            this case, as long as AUTO is not set, no modification will be done.
#            For "force" case, the variable will be modified even if it is not set to AUTO
# - filein : the file in run directory of .def type in which the variable should be set
# - key    : the variable to modify
# - value  : the value to set the key equal to, optional. If value is not set or if
#            value=DEFAULT, then a default value must be given in filein using syntax :
#            key= AUTO : DEFAULT=def_value
#
  IGCM_debug_PushStack "IGCM_comp_modifyNamelist"

  typeset type filein key value pattern modify

  # Set local variables and test the arguments
  if [ $# = 4 ] ; then
    # Normal case with 4 arguments
    type=$1 ; filein=$2 ; key=$3 ; value=$4
  elif [ $# = 3 ] ; then
    # Normal case with 3 arguments
    type=$1 ; filein=$2 ;	key=$3; value="DEFAULT"
  else
    IGCM_debug_Exit "IGCM_comp_modifyNamelist: Bad number of arguments."
    IGCM_debug_PopStack "IGCM_comp_modifyNamelist"
    return
  fi
  IGCM_debug_Print 1 "Entering IGCM_comp_modifyNamelist with arguments: ${type} ${filein} ${key} ${value}"

  # Test if first argument is correct
  if [ ${type} != blocker ] && [ ${type} != nonblocker ] && [ ${type} != force ] ; then
    IGCM_debug_Exit "IGCM_comp_modifyNamelist: Error in first argument must be blocker, nonblocker or force"
    IGCM_debug_PopStack "IGCM_comp_modifyNamelist"
    return
  fi

  # Test if the file exist. 
  # Exit with error if the file does not exist for the case blocker or force. 
  # Only return for the case nonblocker. 
  if [ ! -f ${filein} ] ; then
    if [ ${type} = blocker ] || [ ${type} = force ] ; then
      IGCM_debug_Exit "IGCM_comp_modifyNamelist: ${filein} does not exist."
    else
      IGCM_debug_Print 1 "IGCM_comp_modifyNamelist: ${filein} does not exist. Nothing will be done for this file."
    fi
    IGCM_debug_PopStack "IGCM_comp_modifyNamelist"
    return
  fi

  # Read the line with key in the file without the comments
  pattern=$( grep "^ *${key} *=" ${filein} | sed -e "s% *\!.*%%" )

  # Verify the existance of the pattern
  if [ X"$pattern" = X ] ; then
    # Variable key is not set in filein, stop.
    IGCM_debug_Exit "IGCM_comp_modifyNamelist : Variable ${key} is not set in correct file. It should be set in ${filein}."
    IGCM_debug_PopStack "IGCM_comp_modifyNamelist"
    return
  fi

  # Check if the variable is set to AUTO in the filein
  if [ $( echo $pattern | grep AUTO | wc -l ) = 1 ] ; then
    # Modification will be done for all cases
    modify=yes
  else
    # The variable was not set to AUTO
    if [ ${type} = blocker ] ; then
      # Exit because this is a blocker call
      IGCM_debug_Exit "IGCM_comp_modifyNamelist : The variable ${key} cannot be modified. It should be set to AUTO."
      IGCM_debug_PopStack "IGCM_comp_modifyNamelist"
      return
    elif [ ${type} = nonblocker ] ; then
      # Do nothing. Suppose that the user did set the variable correct
      IGCM_debug_Print 1 "IGCM_comp_modifyNamelist: ${key} is set by the user. Nothing done."
      modify=no
    elif [ ${type} = force ] ; then
      # Force modification
      IGCM_debug_Print 1 "IGCM_comp_modifyNamelist : Variabl=${key} was not set to AUTO. Modification will be forced."
      modify=yes
    fi
  fi

  # Do the modifcation now
  if [ ${modify} = yes ] ; then

    # For option DEFAULT, read default value from file.
    if [ X"${value}" = XDEFAULT ] || [ X"${value}" = X ] ; then
      # Case to set DEFAULT value
      # Read default value from filein
      value=$( echo $pattern | awk  -F"DEFAULT *=" '{print $2}')

      if [ X"${value}" = X ] ; then
        IGCM_debug_Exit "IGCM_comp_modifyNamelist : The variable ${key} needs a DEFAULT value in ${filein}."
        IGCM_debug_Print 1 "IGCM_comp_modifyNamelist: The syntax in ${filein} should be:"
        IGCM_debug_Print 1 "IGCM_comp_modifyNamelist: ${key}=_AUTO_:DEFAULT=def_value"
        IGCM_debug_PopStack "IGCM_comp_modifyNamelist"
        return
      fi
    fi

    # Now change key in filein
    sed -e "s/${pattern}/       ${key}=${value}/" ${filein} > ${filein}.tmp
    IGCM_debug_Print 1 "IGCM_comp_modifyNamelist: In ${filein} set ${key}=${value}"
    \mv ${filein}.tmp ${filein}
  fi
  IGCM_debug_PopStack "IGCM_comp_modifyNamelist"
}

#=======================================================================
function IGCM_comp_modifyXmlFile
{
#
# syntax:     IGCM_comp_modifyXmlFile  type  filein  keyid  keyattrib  value
#
# For example : IGCM_comp_modifyXmlFile force file_def_orchidee.xml sechiba2 enabled .TRUE.
#          or   IGCM_comp_modifyXmlFile blocker iodef.xml using_server NONE false
#
# This function is used to modify the value for a specific attribute and variable id.
# The file must be a valid xml file.
# This function can be used in the comp.driver files for the components.
#
# Arguments:
# - type      : first argument must be blocker, nonblocker or force.
#               For "blocker" case, the variable must be attributed the keyworld AUTO
#               otherwise this function will exit.
#               For "nonblocker" case, the user can remove or modify the variable. For
#               this case, as long as AUTO is not set, no modification will be done.
#               For "force" case, the variable will be modified even if it is not set to AUTO
# - filein    : the file in run directory of .xml type in which the variable should be set
# - keyid     : the variable to modify
# - keyattrib : the attribute name to modify. If NONE, then the variable itself will be modified
# - value     : the value to set in the filein
#
  IGCM_debug_PushStack "IGCM_comp_modifyXmlFile"

  typeset type filein keyid keyattrib value modify

  # Set local variables and test the arguments
  if [ $# = 5 ] ; then
    # Normal case with 4 arguments
    type=$1 ; filein=$2 ; keyid=$3 ; keyattrib=$4 ; value=$5
  else
    IGCM_debug_Exit "IGCM_comp_modifyXmlFile: Bad number of arguments."
    IGCM_debug_PopStack "IGCM_comp_modifyXmlFile"
    return
  fi
  IGCM_debug_Print 1 "Entering IGCM_comp_modifyXmlFile with arguments: type=${type} file=${filein}, id=${keyid} attribute=${keyattrib}, value=${value}"

  # Test if first argument is correct
  if [ ${type} != blocker ] && [ ${type} != nonblocker ] && [ ${type} != force ] ; then
    IGCM_debug_Exit "IGCM_comp_modifyXmlFile: Error in first argument must be blocker, nonblocker or force"
    IGCM_debug_PopStack "IGCM_comp_modifyXmlFile"
    return
  fi

  # Test if the file exist. 
  # Exit with error if the file does not exist for the case blocker or force. 
  # Only return for the case nonblocker. 
  if [ ! -f ${filein} ] ; then
    if [ ${type} = blocker ] || [ ${type} = force ] ; then
      IGCM_debug_Exit "IGCM_comp_modifyXmlFile: ${filein} does not exist."
    else
      IGCM_debug_Print 1 "IGCM_comp_modifyXmlFile: ${filein} does not exist. Nothing will be done for this file."
    fi
    IGCM_debug_PopStack "IGCM_comp_modifyXmlFile"
    return
  fi

  # Test if keyid is set in filein, otherwise exit
  if [ $( grep -w ${keyid} ${filein} | wc -l ) = 0 ] ; then
    # Variable key is not set in filein, stop.
    IGCM_debug_Exit "IGCM_comp_modifyXmlFile : ${keyid} is not set in the file. Bad syntax of ${filein} file."
    IGCM_debug_PopStack "IGCM_comp_modifyXmlFile"
    return
  fi

  # Check if AUTO is set on the same line as keyid and keyattrib
  if [  $( grep -w ${keyid} ${filein} | grep AUTO | wc -l ) = 1 ] ; then
    # Modifification will be done
    modify=yes
  else
    if [ ${type} = blocker ] ; then
      # Exit, the variable must be set to AUTO
      IGCM_debug_Exit "IGCM_comp_modifyXmlFile : blocker function. The ${keyattrib} for ${keyid} must be set to AUTO in ${filein}."
      IGCM_debug_PopStack "IGCM_comp_modifyXmlFile"
      return
    elif [ ${type} = nonblocker ] ; then
      # Nothing will be done
      IGCM_debug_Print 1 "Nonblocker nothing is done for ${filein}, id=${keyid} and attribute ${keyattrib}"
      modify=no
    elif [ ${type} = force ] ; then
      # Force modification
      IGCM_debug_Print 1 "IGCM_comp_modifyXmlFile : Attribute=${keyattrib} for id=${keyid} was not set to AUTO. Modification will be forced."
      modify=yes
    fi
  fi

  # Do the modifcation now
  if [ ${modify} = yes ] ; then
    if [ ${keyattrib} = NONE ] ; then
      # Case to modify the variable itself
      IGCM_debug_Print 1 "Now modify ${filein} for id=${keyid} by setting the variable=${value}"
      sed -e "s/\(<[^\"]*\"${keyid}\".*>\)\([^<]*\)\(<[^>]*\)/\1${value}\3/" ${filein} > ${filein}.tmp
    else
      # Check if keyattrib is set on the same line as keyid
      if [  $( grep -w ${keyid} ${filein} | grep ${keyattrib} | wc -l ) = 1 ] ; then
        # Case to modify the attribute value
        IGCM_debug_Print 1 "Now modify ${filein} for id=${keyid} by setting attribute to ${keyattrib}=${value}"
        sed -e "/id=\"${keyid}\"/s/\(${keyattrib}=\"\)[^\"]*\(\"\)/\1${value}\2/" ${filein} > ${filein}.tmp
      else
        # Case to add the attribute and its value
        IGCM_debug_Print 1 "Now add in ${filein} for id=${keyid} the attribute ${keyattrib} to the value ${value}"
        sed -e "/id=\"${keyid}\"/s/\/>/ ${keyattrib}=\"${value}\"\/>/" ${filein} > ${filein}.tmp
      fi
    fi
    \mv ${filein}.tmp ${filein}
  fi
  IGCM_debug_PopStack "IGCM_comp_modifyXmlFile"
}

#=======================================================================
function IGCM_comp_Update
{
  IGCM_debug_PushStack "IGCM_comp_Update"

    # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_comp_Update"
  echo

  typeset ExeNameIn ExeNameOut
  typeset comp compname comptagname
  for comp in ${config_ListOfComponents[*]} ; do
    # Define component
    eval compname=\${config_ListOfComponents_${comp}[0]} > /dev/null 2>&1
    eval comptagname=\${config_ListOfComponents_${comp}[1]} > /dev/null 2>&1

    # Copy executable for this component
    eval ExeNameIn=\${config_Executable_${comp}[0]}
    eval ExeNameOut=\${config_Executable_${comp}[1]}

    # If missing executable and DRYRUN is set to 0 or 1  then stop!
    if [ ${DRYRUN} -le 1 ] && [ X${ExeNameIn} != X\"\" ] &&  [ ! -f ${R_EXE}/${ExeNameIn} ] ; then
      IGCM_debug_Exit "IGCM_comp_Update missing executable ${ExeNameIn}"
    fi

    if [ ${Period} -eq 1 ] && [ -f ${R_EXE}/${ExeNameIn} ] ; then
      eval IGCM_sys_Cp ${R_EXE}/${ExeNameIn} ${ExeNameOut}
      if [ -f ${RUN_DIR}/${ExeNameOut} ] ; then
        eval IGCM_sys_Chmod +rx ${RUN_DIR}/${ExeNameOut}
      fi
    elif [ -f ${R_EXE}/${ExeNameIn} ] && [ ! -f ${RUN_DIR}/${ExeNameOut} ] ; then
      eval IGCM_sys_Cp ${R_EXE}/${ExeNameIn} ${ExeNameOut}
      if [ -f ${RUN_DIR}/${ExeNameOut} ] ; then
        eval IGCM_sys_Chmod +rx ${RUN_DIR}/${ExeNameOut}
      fi
    fi

    # Debug Print
    IGCM_debug_Print 1 "Update ${compname} Parameter Files."
    # UPDATE component
    ${comp}_Update

  done

  IGCM_debug_PopStack "IGCM_comp_Update"
}

#=======================================================================
function IGCM_comp_Finalize
{
  IGCM_debug_PushStack "IGCM_comp_Finalize"

  # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_comp_Finalize"
  echo

  typeset ListTextName TextName0
  typeset comp compname comptagname card ListFilesName FileName0 NbFiles SaveOnArchive
  typeset i i_ file_in file_in_ file_out file_out_ file_outin file_outin_ generic_file_name nb_rebuild_file
  typeset -Z4 j4 #BASH declare j4
  typeset list_file nlist_file
  typeset compactoutputs

  # Initialize array hosting list of rebuilded files to copy
  unset rebuildedActionsList

  # Text compacting options
  compactoutputs=false
  if [ X${JobType} != XRUN ] ; then
    compactoutputs=true
  elif [ X${config_UserChoices_CompactText} != Xn ] ; then
    compactoutputs=true
  fi

  # Prepare headers for the shell dedicated to offline rebuild
  if [ X${AsynchronousRebuild} = Xtrue ] ; then
    [ ! -d ${RUN_DIR}/REBUILD_${PeriodDateBegin} ] && IGCM_sys_Mkdir ${RUN_DIR}/REBUILD_${PeriodDateBegin}
    if [ ${DRYRUN} -le 1 ] ; then
      echo "#!/bin/ksh                                        " >  ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "function IGCM_FlushRebuild                        " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "{                                                 " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "IGCM_debug_PushStack \"IGCM_FlushRebuild\"        " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "echo                                              " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "IGCM_debug_Print 1 \"IGCM_FlushRebuild\"          " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "echo                                              " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export R_SAVE=${R_SAVE}                           " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export R_BUFR=${R_BUFR}                           " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export R_OUT_KSH=${R_OUT_KSH}                     " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export R_BUF_KSH=${R_BUF_KSH}                     " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export config_UserChoices_JobName=${config_UserChoices_JobName}     " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      echo "export config_UserChoices_SpaceName=${config_UserChoices_SpaceName} " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
    fi
  fi

  for comp in ${config_ListOfComponents[*]} ; do
    # Define component
    eval compname=\${config_ListOfComponents_${comp}[0]} > /dev/null 2>&1
    eval comptagname=\${config_ListOfComponents_${comp}[1]} > /dev/null 2>&1

    # Debug Print
    IGCM_debug_Print 1 "Finalize ${comp} : ${compname} component."
    # FINALIZE component
    ${comp}_Finalize

    card=${SUBMIT_DIR}/COMP/${compname}.card

    # Save Output Text files of models
    #---------------------------------
    IGCM_debug_Print 2 "Save Output Text files for ${comp} : ${compname} component."
    IGCM_card_DefineArrayFromOption ${card} OutputText List
    ListTextName=${compname}_OutputText_List

    eval TextName0=\${${ListTextName}[0]} > /dev/null 2>&1
    if [ X${TextName0} != X${NULL_STR} ] ; then
      eval NbFiles=\${#${ListTextName}[@]} > /dev/null 2>&1

      (( i=0 ))
      until [ $i -eq $NbFiles ]; do
        eval file_in=\${${ListTextName}[$i]} > /dev/null 2>&1
        eval file_out=${PREFIX}_${file_in}

        (( i=i+1 ))

        unset list_file
        #set +A list_file -- $( ls ${file_in}* | sort 2>/dev/null )
        # result for a a1 a10 a2 with file_in=a a a1 a2 a10
        set +A list_file -- $( [ -f ${file_in} ] && ls ${file_in} ; for i in $(ls ${file_in}* 2>/dev/null | sed "s/${file_in}//" | sort -n) ; do ls ${file_in}$i ; done )
        nlist_file=${#list_file[@]}
        if [ ${nlist_file} -gt 1 ] ; then
          if ( ${compactoutputs} ) ; then
            IGCM_debug_Print 2 "Parallelism of Text Output with ${nlist_file} files."
            IGCM_debug_Print 2 "Compact files in ${file_out} : " ${list_file[*]}
            echo ${list_file[*]} > ${file_out}
            echo "" >> ${file_out}

            (( i_ = 0 ))
            for file in ${list_file[@]} ; do
              echo "_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ " >> ${file_out}
              echo "| " ${i_} " " ${file} >> ${file_out}
              echo "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - " >> ${file_out}
              cat ${file} | sed "s/\(.*\)/${i_}\1/" >> ${file_out}
              echo "" >> ${file_out}
              eval FileToBeDeleted[${#FileToBeDeleted[@]}]="${file}" > /dev/null 2>&1
                  (( i_ = i_ + 1 ))
            done
            if ( ${ExecutionFail} ) ; then
              IGCM_sys_Cp ${file_out} ${SUBMIT_DIR}/Debug
            fi

            if [ X${Pack} = Xtrue ] ; then
              eval IGCM_sys_PutBuffer_Out ${file_out} \${R_BUF_${comp}_D}/${file_out}
            else
              eval IGCM_sys_Put_Out ${file_out} \${R_OUT_${comp}_D}/${file_out}
            fi

            eval FileToBeDeleted[${#FileToBeDeleted[@]}]="${file_out}" > /dev/null 2>&1
          else
            for file in ${list_file[@]} ; do
              if ( ${ExecutionFail} ) ; then
                IGCM_sys_Cp ${file} ${SUBMIT_DIR}/Debug/${PREFIX}_${file}
              fi

              if [ X${Pack} = Xtrue ] ; then
                eval IGCM_sys_PutBuffer_Out ${file} \${R_BUF_${comp}_D}/${PREFIX}_${file}
              else
                eval IGCM_sys_Put_Out ${file} \${R_OUT_${comp}_D}/${PREFIX}_${file}
              fi

              eval FileToBeDeleted[${#FileToBeDeleted[@]}]="${file}" > /dev/null 2>&1
            done
          fi
        else
          if ( [ -f ${file_in}_0000 ] || [ -f ${file_in}0 ] ) ; then
            eval IGCM_sys_Mv ${file_in}* ${file_in}
          fi

          if ( ${ExecutionFail} ) ; then
            IGCM_sys_Cp ${file_in} ${SUBMIT_DIR}/Debug/${file_out}
          fi

          if [ X${Pack} = Xtrue ] ; then
            eval IGCM_sys_PutBuffer_Out ${file_in} \${R_BUF_${comp}_D}/${file_out}
          else
            eval IGCM_sys_Put_Out ${file_in} \${R_OUT_${comp}_D}/${file_out}
          fi
          eval FileToBeDeleted[${#FileToBeDeleted[@]}]="${file_in}" > /dev/null 2>&1
        fi
      done
    fi

    # Save Restarts files
    #--------------------
    IGCM_debug_Print 2 "Save Restart files for ${comp} : ${compname} component."
    IGCM_card_DefineArrayFromOption ${card} RestartFiles List
    ListFilesName=${compname}_RestartFiles_List
    eval FileName0=\${${ListFilesName}[0]} > /dev/null 2>&1

    if ( [ X${FileName0} != X${NULL_STR} ] && [ X${FileName0} != XNONE ] ) ; then
      eval NbFiles=\${#${ListFilesName}[@]} > /dev/null 2>&1

      (( i=0 ))
      until [ $i -ge $NbFiles ]; do
        eval file_in_=\${${ListFilesName}[$i]} > /dev/null 2>&1
        eval file_in=${file_in_}

        (( i_ = i+1 ))
        eval file_out_=\${${ListFilesName}[$i_]} > /dev/null 2>&1
        eval file_out=${file_out_}

        (( i_ = i+2 ))
        eval file_outin_=\${${ListFilesName}[$i_]} > /dev/null 2>&1
        eval file_outin=${file_outin_}

        generic_restart_file_name_in=$(    basename ${file_in} .nc )
        generic_restart_file_name_out=$(   basename ${config_UserChoices_JobName}_${PeriodDateEnd}_${file_out} .nc )
        generic_restart_file_name_outin=$( basename ${file_outin} .nc )

        nb_restart_file=$( ls ${generic_restart_file_name_in}_????.nc 2>/dev/null | wc -l )
        if [ ${nb_restart_file} -gt 1 ] ; then
          j=0                                     # BASH LINE NOT NEEDED
          # BASH for j4 in in $( eval echo {0000..$(( nb_restart_file - 1 ))} ) ; do
          until [ $j -ge ${nb_restart_file} ]; do # BASH LINE NOT NEEDED
            j4=${j}                               # BASH LINE NOT NEEDED
            if [ X${Pack} = Xtrue ] ; then
              eval IGCM_sys_PutBuffer_Rest ${generic_restart_file_name_in}_${j4}.nc \${R_BUF_${comp}_R}/${generic_restart_file_name_out}_${j4}.nc
            else
              eval IGCM_sys_Put_Rest ${generic_restart_file_name_in}_${j4}.nc \${R_OUT_${comp}_R}/${generic_restart_file_name_out}_${j4}.nc
            fi
            if [ ! ${file_in} = ${file_outin} ] ; then
              if ( ${ExitFlag} ) ; then
                echo "IGCM_sys_Mv ${generic_restart_file_name_in}_${j4}.nc ${generic_restart_file_name_outin}_${j4}.nc not executed."
              else
                IGCM_sys_Mv ${generic_restart_file_name_in}_${j4}.nc ${generic_restart_file_name_outin}_${j4}.nc
              fi
            fi
            (( j=j+1 ))                           # BASH LINE NOT NEEDED
          done
        else
          if [ X${Pack} = Xtrue ] ; then
            eval IGCM_sys_PutBuffer_Rest ${file_in} \${R_BUF_${comp}_R}/${config_UserChoices_JobName}_${PeriodDateEnd}_${file_out}
          else
            eval IGCM_sys_Put_Rest ${file_in} \${R_OUT_${comp}_R}/${config_UserChoices_JobName}_${PeriodDateEnd}_${file_out}
          fi
          if [ ! ${file_in} = ${file_outin} ] ; then
            if ( ${ExitFlag} ) ; then
              echo "IGCM_sys_Mv ${file_in} ${file_outin} not executed."
            else
              IGCM_sys_Mv ${file_in} ${file_outin}
            fi
          fi
        fi

        (( i=i+3 ))
      done
    else
      if [ X${FileName0} != XNONE ] ; then
        IGCM_debug_Exit "IGCM_comp_Finalize : No file in restart list for ${compname}."
      else
        IGCM_debug_Print 1 "IGCM_comp_Finalize : NONE specified in Restart List ${compname}."
      fi
    fi

    # Save Output files
    #------------------
    IGCM_debug_Print 2 "Save Output files for ${comp} : ${compname} component."
    IGCM_card_DefineArrayFromOption ${card} OutputFiles List
    ListFilesName=${compname}_OutputFiles_List
    eval FileName0=\${${ListFilesName}[0]} > /dev/null 2>&1

    if [ X${FileName0} != X${NULL_STR} ] ; then
      eval NbFiles=\${#${ListFilesName}[@]} > /dev/null 2>&1

      (( i=0 ))
      until [ $i -ge $NbFiles ]; do
        SaveOnArchive=true
        eval file_in_=\${${ListFilesName}[$i]} > /dev/null 2>&1
        eval file_in=${file_in_}
        (( i_ = i+1 ))
        eval file_out_=\${${ListFilesName}[$i_]} > /dev/null 2>&1
        eval file_out=${file_out_}
        #
        # Override file_out path remplacing R_SAVE by R_BUFR
        #
        if [ X${Pack} = Xtrue ] ; then
          file_out=$( echo $file_out | sed "s:^$R_SAVE:$R_BUFR:" )
        fi
        #
        # Not necessarily the best option. /!\ Potential side effects /!\
        #
        (( i_ = i+2 ))
        eval flag_post=\${${ListFilesName}[$i_]} > /dev/null 2>&1
        #
        generic_file_name=$( basename ${file_in} .nc )
        nb_rebuild_file=$( ls | grep "^${generic_file_name}_[0-9]*.nc" | wc -l )
        #
        if ( [ ${nb_rebuild_file} -eq 1 ] && [ -f ${generic_file_name}_0000.nc ] ) ; then
          IGCM_debug_Print 2 "Parallelism with 1 file. Rebuilding ${file_in} not needed"
          IGCM_sys_Mv ${generic_file_name}_0000.nc ${file_in}
        elif [ ${nb_rebuild_file} -gt 1 ] ; then
          IGCM_debug_Print 2 "Parallelism detected and rebuilding ${file_in} is needed"
          if [ X${AsynchronousRebuild} = Xfalse ] ; then
            IGCM_debug_Print 2 "Rebuilding ${file_in} online"
            IGCM_sys_rebuild ${file_in} ${generic_file_name}_[0-9]*.nc
          else
            IGCM_debug_Print 2 "Preparing offline rebuild for ${file_in}"
            IGCM_sys_Mv ${generic_file_name}_[0-9]*.nc ${RUN_DIR}/REBUILD_${PeriodDateBegin}

            # Prepare the shell dedicated to offline rebuild
            if [ $DRYRUN -le 1 ]; then
              if [ ${file_in} = histstn.nc ] ; then
                echo "IGCM_sys_rebuild_station ${file_in} ${generic_file_name}_*.nc" >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
              else
                echo "IGCM_sys_rebuild ${file_in} ${generic_file_name}_[0-9]*.nc" >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
              fi
              echo "IGCM_debug_Verif_Exit" >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
            fi
            #
            # Load Patch we need to apply and apply
            if [ $DRYRUN -le 1 ]; then
              if [ X$( eval echo \${${compname}_${flag_post}_Patches[0]} ) !=  X${NULL_STR} ]; then
                for Patch in $( eval echo \${${compname}_${flag_post}_Patches[*]} ) ; do
                  echo ". ${libIGCM_POST}/libIGCM_post/IGCM_${Patch}.ksh" >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
                  echo "IGCM_${Patch} ${file_in}                        " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
                  echo "IGCM_debug_Verif_Exit                           " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
                done
              fi
            fi
            #
            if [ $DRYRUN -le 1 ]; then
              if [ X${Pack} = Xtrue ] ; then
                rebuildedActionsList[${#rebuildedActionsList[*]}]="IGCM_sys_PutBuffer_Out ${file_in} ${file_out}"
              else
                rebuildedActionsList[${#rebuildedActionsList[*]}]="IGCM_sys_Put_Out ${file_in} ${file_out}"
              fi
              rebuildedActionsList[${#rebuildedActionsList[*]}]="IGCM_debug_Verif_Exit"
              rebuildedActionsList[${#rebuildedActionsList[*]}]="IGCM_sys_Rm ${generic_file_name}_[0-9]*.nc"
            fi
            SaveOnArchive=false
          fi
        fi
        #
        if [ ${SaveOnArchive} = true ] ; then
          #
          # Rebuild has been done online or it was not needed
          #
          # If we need to apply a patch we use TMP DIRECTORY before ARCHIVING if asynchronous rebuild is on
          #
          thereisapatch=$( eval echo \${${compname}_${flag_post}_Patches[0]} )
          if ( [ ! X${thereisapatch} = X${NULL_STR} ] && [ ! X${thereisapatch} = X ] && [ X${AsynchronousRebuild} = Xtrue ] && [ -f ${file_in} ] ) ; then
            IGCM_sys_Mv ${file_in} ${RUN_DIR}/REBUILD_${PeriodDateBegin}
            eval FileToBeDeleted[${#FileToBeDeleted[@]}]=REBUILD_${PeriodDateBegin}/${file_in} > /dev/null 2>&1
            #
            if [ $DRYRUN -le 1 ]; then
              for Patch in $( eval echo \${${compname}_${flag_post}_Patches[*]} ) ; do
                echo ". ${libIGCM_POST}/libIGCM_post/IGCM_${Patch}.ksh" >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
                echo "IGCM_${Patch} ${file_in}                        " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
                echo "IGCM_debug_Verif_Exit                           " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
              done
              #
              if [ X${Pack} = Xtrue ] ; then
                echo "IGCM_sys_PutBuffer_Out ${file_in} ${file_out}   " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
              else
                echo "IGCM_sys_Put_Out ${file_in} ${file_out}         " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
              fi
              echo "IGCM_debug_Verif_Exit                             " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
              #
            fi
          else
            #
            # No Patch, No Asynchronous rebuild, online rebuild has been done or was not needed
            #
            if [ X${Pack} = Xtrue ] ; then
              IGCM_sys_PutBuffer_Out ${file_in} ${file_out}
            else
              IGCM_sys_Put_Out ${file_in} ${file_out}
            fi
            eval FileToBeDeleted[${#FileToBeDeleted[@]}]="${file_in}" > /dev/null 2>&1
            if [ ${nb_rebuild_file} -gt 1 ] ; then
              for DelFile in $( ls | grep "${generic_file_name}[_0-9]*.nc" ) ; do
                eval FileToBeDeleted[${#FileToBeDeleted[@]}]=${DelFile} > /dev/null 2>&1
              done
            fi
          fi
        fi
        (( i=i+3 ))
      done
    fi
    echo
  done
  # Append the sync call and the copy sequence to the IGCM_FlushRebuild function if needed
  if [ ${#rebuildedActionsList[*]} -ne 0 ] ; then
    echo "IGCM_sys_sync              " >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
    i=0
    until [ ${i} -ge ${#rebuildedActionsList[*]} ]; do
      echo ${rebuildedActionsList[$i]} >> ${RUN_DIR}/REBUILD_${PeriodDateBegin}/rebuild.ksh
      (( i=i+1 ))
    done
  fi
  IGCM_debug_PopStack "IGCM_comp_Finalize"
}
