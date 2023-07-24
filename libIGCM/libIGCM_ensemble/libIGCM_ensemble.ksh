#!/bin/ksh

#**************************************************************
# Author: Sebastien Denvil, Sonia Labetoulle, Nicolas Lebas, Sebastien Nguyen
# Contact: Nicolas.Lebas__at__locean-ipsl.upmc.fr
# $Revision:: 1284                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2016-03-03 16:33:25 +0100 (Thu, 03 Mar 2016) $ Date of last commit
# IPSL (2012)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
# >>> Date ensemble <<<
# Author: Nicolas Lebas (adapted from Sonia Labetoulle)
# Contact: Nicolas.Lebas__at__locean-ipsl.upmc.fr
# IPSL (2014)
#
# >>> Add 3D perturbation maps to oceanic restart <<<
# Author: Sebastien Nguyen
# Contact: Sebastien.Nguyen__at__locean-ipsl.upmc.fr
# IPSL (2014)
#
#**************************************************************

# Read which ensemble type are active
function IGCM_ensemble_Init
{
  IGCM_debug_PushStack "IGCM_ensemble_Init"

  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_PERTURB active
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_DATE active
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_PARAMETRIC active

  IGCM_debug_Print 1 "Ens_PERTURB ACTIVE     = ${ensemble_Ens_PERTURB_active}"
  IGCM_debug_Print 1 "Ens_DATE ACTIVE        = ${ensemble_Ens_DATE_active}"
  IGCM_debug_Print 1 "Ens_PARAMETRIC ACTIVE  = ${ensemble_Ens_PARAMETRIC_active}"
  echo ""

  #====================================================
  # Define ARCHIVE : Dedicated to large files
  # Define STORAGE : Dedicated to small/medium files
  # Define R_OUT   : Output tree located on ARCHIVE
  # Define R_FIG   : Output tree located on STORAGE hosting figures (monitoring and atlas, and/or small files)
  # Define R_BUF   : USELESS and DEPRECATED output tree.
  IGCM_sys_defineArchives

  IGCM_debug_PopStack "IGCM_ensemble_Init"
}

# Set Alphanumerical variables ajust to member nb
function IGCM_ensemble_SetAlpha
{
  IGCM_debug_PushStack "IGCM_ensemble_SetAlpha"

  set -A Alpha      A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
  set -A AlphaMonth a b c d e f g h i j k l

  IGCM_debug_PopStack "IGCM_ensemble_SetAlpha"
}

############### Perturb ENSEMBLE #################
function IGCM_ensemble_CastInit
{
  IGCM_debug_PushStack "IGCM_ensemble_CastInit"

  IGCM_sys_Mkdir ${RUN_DIR}

  IGCM_sys_Cp ${SUBMIT_DIR}/config.card   ${RUN_DIR}
  IGCM_sys_Cp ${SUBMIT_DIR}/ensemble.card ${RUN_DIR}
  IGCM_sys_Cp ${SUBMIT_DIR}/Job_*         ${RUN_DIR}
  IGCM_sys_Cp ${SUBMIT_DIR}/run.card.init ${RUN_DIR}
  if [ -f ${SUBMIT_DIR}/Qsub.* ]; then
    IGCM_sys_Cp ${SUBMIT_DIR}/Qsub.*        ${RUN_DIR}
  fi
  if [ -f ${SUBMIT_DIR}/Qclean.* ]; then
    IGCM_sys_Cp ${SUBMIT_DIR}/Qclean.*      ${RUN_DIR}
  fi

  # Useful?
  #if [ -f  ${SUBMIT_DIR}/CreatedDir.txt ] ; then
  #  IGCM_sys_Cp ${SUBMIT_DIR}/CreatedDir.txt ${RUN_DIR}
  #fi
  # Useful?
  #if [ -f  ${SUBMIT_DIR}/Qsub.sh ] ; then
  #  IGCM_sys_Cp ${SUBMIT_DIR}/Qsub.sh ${RUN_DIR}
  #fi
  echo ${PWD}

  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_PERTURB active
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_PERTURB NAME
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_PERTURB BEGIN_INIT
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_PERTURB END_INIT
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_PERTURB PERIODICITY
  IGCM_card_DefineArrayFromOption    ${F_CFG_ENS} Ens_PERTURB NONPERIODIC
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_PERTURB LENGTH
  IGCM_card_DefineArrayFromOption    ${F_CFG_ENS} Ens_PERTURB LENGTH_NONPERIODIC
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_PERTURB MEMBER
  IGCM_card_DefineArrayFromOption    ${F_CFG_ENS} Ens_PERTURB MEMBER_LIST
  IGCM_card_DefineArrayFromOption    ${F_CFG_ENS} Ens_PERTURB MEMBER_NAMESLIST
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_PERTURB MEMBER_INITFROM
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_PERTURB MEMBER_INITPATH
  IGCM_card_DefineArrayFromOption    ${F_CFG_ENS} Ens_PERTURB PERTURB_BIN
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_PERTURB INITFROM
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_PERTURB INITPATH
  IGCM_card_DefineVariableFromOption config.card UserChoices JobName
  IGCM_card_DefineVariableFromOption config.card UserChoices TagName
  IGCM_card_DefineVariableFromOption config.card UserChoices CalendarType
  IGCM_card_DefineArrayFromSection   config.card ListOfComponents

  echo
  IGCM_debug_Print 1 "[Ens_PERTURB]"
  IGCM_debug_Print 1 "ACTIVE            = ${ensemble_Ens_PERTURB_active}"
  IGCM_debug_Print 1 "NAME              = ${ensemble_Ens_PERTURB_NAME}"
  IGCM_debug_Print 1 "BEGIN_INIT        = ${ensemble_Ens_PERTURB_BEGIN_INIT}"
  IGCM_debug_Print 1 "END_INIT          = ${ensemble_Ens_PERTURB_END_INIT}"
  IGCM_debug_Print 1 "PERIODICITY       = ${ensemble_Ens_PERTURB_PERIODICITY}"
  IGCM_debug_Print 1 "NONPERIODIC       = ${ensemble_Ens_PERTURB_NONPERIODIC[*]}"
  IGCM_debug_Print 1 "LENGTH             = ${ensemble_Ens_PERTURB_LENGTH}"
  IGCM_debug_Print 1 "LENGTH_NONPERIODIC = ${ensemble_Ens_PERTURB_LENGTH_NONPERIODIC[*]}"
  IGCM_debug_Print 1 "MEMBER            = ${ensemble_Ens_PERTURB_MEMBER}"
  IGCM_debug_Print 1 "MEMBER_LIST       = ${ensemble_Ens_PERTURB_MEMBER_LIST[*]}"
  IGCM_debug_Print 1 "MEMBER_NAMESLIST  = ${ensemble_Ens_PERTURB_MEMBER_NAMESLIST[*]}"
  IGCM_debug_Print 1 "MEMBER_INITFROM   = ${ensemble_Ens_PERTURB_MEMBER_INITFROM}"
  IGCM_debug_Print 1 "MEMBER_INITPATH   = ${ensemble_Ens_PERTURB_MEMBER_INITPATH}"
  IGCM_debug_Print 1 "PERTURB_BIN       = ${ensemble_Ens_PERTURB_PERTURB_BIN[*]}"
  IGCM_debug_Print 1 "INITFROM          = ${ensemble_Ens_PERTURB_INITFROM}"
  IGCM_debug_Print 1 "INITPATH          = ${ensemble_Ens_PERTURB_INITPATH}"
  IGCM_debug_Print 1 "JobName           = ${config_UserChoices_JobName}"
  IGCM_debug_Print 1 "TagName           = ${config_UserChoices_TagName}"
  IGCM_debug_Print 1 "CalendarType      = ${config_UserChoices_CalendarType}"
  IGCM_debug_Print 1 "ListOfComponents  = ${config_ListOfComponents[*]}"

  PerturbExe=${ensemble_Ens_PERTURB_PERTURB_BIN[0]}

  case ${PerturbExe} in
  AddNoise)
    PerturbComp=${ensemble_Ens_PERTURB_PERTURB_BIN[1]}
    PerturbFile=${ensemble_Ens_PERTURB_PERTURB_BIN[2]}
    PerturbVar=${ensemble_Ens_PERTURB_PERTURB_BIN[3]}
    PerturbAmp=${ensemble_Ens_PERTURB_PERTURB_BIN[4]}

    IGCM_debug_Print 1 "PerturbExe  = ${PerturbExe}"
    IGCM_debug_Print 1 "PerturbFile = ${PerturbFile}"
    IGCM_debug_Print 1 "PerturbComp = ${PerturbComp}"
    IGCM_debug_Print 1 "PerturbVar  = ${PerturbVar}"
    IGCM_debug_Print 1 "PerturbAmp  = ${PerturbAmp}"
    ;;
  AddPertu3DOCE)
    PerturbComp=${ensemble_Ens_PERTURB_PERTURB_BIN[1]}
    PerturbFile=${ensemble_Ens_PERTURB_PERTURB_BIN[2]}
    PerturbVar=${ensemble_Ens_PERTURB_PERTURB_BIN[3]}
    PerturbMask=${ensemble_Ens_PERTURB_PERTURB_BIN[4]}

    IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_PERTURB MASKPATH

    IGCM_debug_Print 1 "PerturbExe  = ${PerturbExe}"
    IGCM_debug_Print 1 "PerturbFile = ${PerturbFile}"
    IGCM_debug_Print 1 "PerturbComp = ${PerturbComp}"
    IGCM_debug_Print 1 "PerturbVar  = ${PerturbVar}"
    IGCM_debug_Print 1 "PerturbMask = ${PerturbMask}"
    IGCM_debug_Print 1 "MASK PATH   = ${ensemble_Ens_PERTURB_MASKPATH}"
    ;;
  esac

  IGCM_ensemble_SetAlpha ${ensemble_Ens_PERTURB_MEMBER}

  # A few checks Period case:

  # ... Check PERIODICITY ...
  case ${ensemble_Ens_PERTURB_PERIODICITY} in
  NONE)
    IGCM_debug_Print 1 "periodic start not active"
    CastPeriodicStart=false
    ;;
  *[Yy]|*[Mm])
    CastPeriodicStart=true
    IGCM_debug_Print 1 "Periodic length : ${ensemble_Ens_PERTURB_PERIODICITY}" ;;
  *)
    IGCM_debug_Exit "IGCM_ensemble_CastInit ${ensemble_Ens_PERTURB_PERIODICITY} : invalid PERIODICITY"
    IGCM_debug_Exit "Choose a value in *Y or *M"
    IGCM_debug_Verif_Exit ;;
  esac
  # ... Check LENGTH ...
  case ${ensemble_Ens_PERTURB_LENGTH} in
  *[Yy]|*[Mm])
    IGCM_debug_Print 1 "Periodic duration : ${ensemble_Ens_PERTURB_LENGTH}" ;;
  *)
    IGCM_debug_Exit "IGCM_ensemble_CastInit ${ensemble_Ens_PERTURB_LENGTH} invalid LENGTH"
    IGCM_debug_Exit "Choose a value in choose in *Y or *M"
    IGCM_debug_Verif_Exit ;;
  esac

  # A few checks for the Non-Periodic case:
  DateNum=0
  while [ ${DateNum} -lt ${#ensemble_Ens_PERTURB_NONPERIODIC[*]} ] ; do

    # - Check LENGTH_NONPERIODIC
    case ${ensemble_Ens_PERTURB_LENGTH_NONPERIODIC[${DateNum}]} in
    _0_)
      IGCM_debug_Print 1 "non-periodic start not active"
      CastNonPeriodicStart=false
      ;;
    *[Yy]|*[Mm])
      IGCM_debug_Print 1 "Non-periodic duration : ${ensemble_Ens_PERTURB_LENGTH_NONPERIODIC[${DateNum}]}"
      CastNonPeriodicStart=true
      ;;
    *)
      IGCM_debug_Exit "IGCM_ensemble_CastInit ${ensemble_Ens_PERTURB_LENGTH_NONPERIODIC[${DateNum}]} : invalid LENGTH"
      IGCM_debug_Exit "choose in *Y or *M"
      IGCM_debug_Verif_Exit ;;
    esac
    (( DateNum = DateNum + 1 ))
  done

  # A few checks for the MEMBER_LIST case:
  case ${ensemble_Ens_PERTURB_MEMBER_LIST[0]} in
  _0_)
    IGCM_debug_Print 1 "list of perturbation maps not active"
    CastMemberList=false
    ;;
  *)
    if [ ${CastPeriodicStart} = "true" ] ; then
      IGCM_debug_Exit "list of perturbation maps for periodic start not implemented, will stop execution"
      IGCM_debug_Verif_Exit
    elif [ ${CastNonPeriodicStart} = "true" ] ; then
      IGCM_debug_Exit "list of perturbation maps for non periodic start not implemented, will stop execution"
      IGCM_debug_Verif_Exit
    fi

    # test that MEMBER_NAMESLIST and MEMBER_LIST have the same size

    if [ ${#ensemble_Ens_PERTURB_MEMBER_LIST[*]} -ne ${#ensemble_Ens_PERTURB_MEMBER_NAMESLIST[*]} ] ; then
      IGCM_debug_Exit "number of elements in MEMBER_LIST and MEMBER_NAMESLIST differ"
      IGCM_debug_Verif_Exit
    fi

    IGCM_debug_Print 1 "list of perturbation maps : ${ensemble_Ens_PERTURB_MEMBER_LIST[*]}"
    IGCM_debug_Print 1 "list of members names : ${ensemble_Ens_PERTURB_MEMBER_NAMESLIST[*]}"
    CastMemberList=true
    ;;
  esac

#  IGCM_debug_Exit "fin du test MEMBER_LIST"
#  IGCM_debug_Verif_Exit

  # Need to know all the restart filename of the component we will apply the noise to
  IGCM_card_DefineArrayFromOption config.card ListOfComponents ${PerturbComp}
  eval compname=\${config_ListOfComponents_${PerturbComp}[0]} > /dev/null 2>&1

  # Target the component's card we apply the noise to
  card=${SUBMIT_DIR}/COMP/${compname}.card

  # Read the restart file list. To be used later
  IGCM_card_DefineArrayFromOption ${card} RestartFiles List
  ListFilesName=${compname}_RestartFiles_List
  eval FileName0=\${${ListFilesName}[0]} > /dev/null 2>&1
  eval NbFiles=\${#${ListFilesName}[@]} > /dev/null 2>&1

  # Check
  IGCM_debug_Print 1 "Nb Restart Files  = ${NbFiles}"

  IGCM_debug_PopStack "IGCM_ensemble_CastInit"
}

function IGCM_ensemble_CastPeriodicStarts
{
  IGCM_debug_PushStack "IGCM_ensemble_CastPeriodicStarts"

  [ ${CastPeriodicStart} = false ] && return

  echo
  IGCM_debug_Print 1 "Manage periodic starts"

#.. Manage periodic starts ..
#   ======================

# ... Loop over DateBegin ...
  eval DateBegin=\${ensemble_Ens_PERTURB_BEGIN_INIT}

  while [ ${DateBegin} -le ${ensemble_Ens_PERTURB_END_INIT} ] ; do
    IGCM_date_GetYearMonth ${DateBegin} year month

  # - Determine number of day(s) in PERIODICITY
    PeriodLengthInDays=$( IGCM_date_DaysInCurrentPeriod ${DateBegin} ${ensemble_Ens_PERTURB_PERIODICITY} )

  # - Determine number of day(s) in LENGTH
    DureeLengthInDays=$(( $( IGCM_date_DaysInCurrentPeriod ${DateBegin} ${ensemble_Ens_PERTURB_LENGTH} ) - 1 ))

  # - Determine DateEnd
    (( DateEnd = $( IGCM_date_AddDaysToGregorianDate ${DateBegin} ${DureeLengthInDays} ) ))

  # - Build directory name
    IGCM_ensemble_CastDirectoryName ${ensemble_Ens_PERTURB_NAME} ${ensemble_Ens_PERTURB_PERIODICITY} $year $month $StartDir

  # - Determine RestartDate
    (( Offset = -1 ))
    (( RestartDate = $( IGCM_date_AddDaysToGregorianDate ${DateBegin} ${Offset} ) ))

    IGCM_debug_Print 2 "${DateBegin} => ${DateEnd} : ${StartDir}"
    echo "${DateBegin} ${DateEnd} ${StartDir}" >> ${RUN_DIR}/CreatedDir.txt

  # - Create directory for current DateBegin
    if [ ! -d  ${StartDir} ] ; then
      IGCM_sys_Mkdir ${SUBMIT_DIR}/${StartDir}
      IGCM_sys_Cd ${SUBMIT_DIR}/${StartDir}
      ln -s ../../.resol .
      ln -s ../../.libmpi .
      IGCM_sys_Cd ${RUN_DIR}
    fi

  # - Create directory to store modified restart files
    RestartDir=${STORAGE}/IGCM_IN/${config_UserChoices_TagName}/${StartDir}
    IGCM_sys_MkdirArchive ${RestartDir}

  # - Loop over members
    i=0
    while [ $i -lt ${ensemble_Ens_PERTURB_MEMBER} ] ; do
      MemberDir="${StartDir}${Alpha[$i]}"
      echo
      IGCM_debug_Print 3 "${MemberDir}"

      JobName="Job_${MemberDir}"

    # * Create directory if it doesn't exist and copy/link files
      if [ ! -d  ${SUBMIT_DIR}/${StartDir}/${MemberDir} ] ; then
        IGCM_sys_Mkdir ${SUBMIT_DIR}/${StartDir}/${MemberDir}
        IGCM_sys_Cd ${SUBMIT_DIR}/${StartDir}/${MemberDir}
        ln -s ../../COMP
        ln -s ../../PARAM
        ln -s ../../POST
        ln -s ../../DRIVER
        IGCM_sys_Cd ${RUN_DIR}
        IGCM_sys_Cp config.card run.card.init ${SUBMIT_DIR}/${StartDir}/${MemberDir}
        IGCM_sys_Cp Job_${config_UserChoices_JobName} ${SUBMIT_DIR}/${StartDir}/${MemberDir}/${JobName}

        # Dump command to be lauched
        echo "cd ${StartDir}/${MemberDir}/ ;"  >> ${RUN_DIR}/Qsub.${StartDir}.sh
        echo "${SUBMIT} ${JobName} ; cd -"     >> ${RUN_DIR}/Qsub.${StartDir}.sh

        echo "cd ${StartDir}/${MemberDir}/ ;"  >> ${RUN_DIR}/Qclean.month.${StartDir}.sh
        echo "${libIGCM}/clean_month.job ; cd -"     >> ${RUN_DIR}/Qclean.month.${StartDir}.sh

        echo "cd ${StartDir}/${MemberDir}/ ;"  >> ${RUN_DIR}/Qclean.year.${StartDir}.sh
        echo "${libIGCM}/clean_year.job ; cd -"     >> ${RUN_DIR}/Qclean.year.${StartDir}.sh

        # * Update files : config.card, Job_, COMP/comp.card
        IGCM_ensemble_CastFilesUpdate ${DateBegin} ${DateEnd} ${RestartDate}

        # * Apply noise on restart file
        IGCM_ensemble_CastPerturbFile
      fi

      (( i = i + 1 ))
    done

    # Done. Save ${StartDir} submission text file
    IGCM_sys_Cp ${RUN_DIR}/Qsub.${StartDir}.sh ${SUBMIT_DIR}
    IGCM_sys_Cp ${RUN_DIR}/Qclean.month.${StartDir}.sh ${SUBMIT_DIR}
    IGCM_sys_Cp ${RUN_DIR}/Qclean.year.${StartDir}.sh ${SUBMIT_DIR}

  # - Next DateBegin
    echo "$DateBegin  $PeriodLengthInDays"
    case ${ensemble_Ens_PERTURB_PERIODICITY} in
    *[Yy]|*[Mm])
      (( DateBegin = $( IGCM_date_AddDaysToGregorianDate ${DateBegin} ${PeriodLengthInDays} ) ))
      ;;
    esac
    echo "New DateBegin = $DateBegin"
    echo "========================================================================"
  done
  IGCM_debug_PopStack "IGCM_ensemble_CastPeriodicStarts"
}

function IGCM_ensemble_CastNonPeriodicStarts
{
  IGCM_debug_PushStack "IGCM_ensemble_CastNonPeriodicStarts"

  #.. Manage non periodic starts => Loop over DateBegin ..
  #   ==========================

  [ ${CastNonPeriodicStart} = false ] && return

  echo
  IGCM_debug_Print 1 "Manage non periodic starts"

  DateNum=0
# ... Loop over ensemble_Ens_PERTURB_NONPERIODIC ...
  echo ">${DateNum}<"
  echo ">${#ensemble_Ens_PERTURB_NONPERIODIC[*]}<"
  while [ ${DateNum} -lt ${#ensemble_Ens_PERTURB_NONPERIODIC[*]} ] ; do
    DateBegin=${ensemble_Ens_PERTURB_NONPERIODIC[${DateNum}]}
    Duree=${ensemble_Ens_PERTURB_LENGTH_NONPERIODIC[${DateNum}]}
    echo ">${DateBegin}<"
    echo ">${Duree}<"

  # - Determine number of day(s) in LENGTH_NONPERIODIC
    IGCM_date_GetYearMonth ${DateBegin} year month
    DureeLengthInDays=$(( $( IGCM_date_DaysInCurrentPeriod ${DateBegin} ${Duree} ) - 1 ))

  # - Build directory name
    echo "========================================================================"
    echo "ensemble_Ens_PERTURB_NAME = ${ensemble_Ens_PERTURB_NAME}"
    IGCM_ensemble_CastDirectoryName ${ensemble_Ens_PERTURB_NAME} ${Duree} $year $month $StartDir

  # - Determine DateEnd
    (( DateEnd = $( IGCM_date_AddDaysToGregorianDate ${DateBegin} ${DureeLengthInDays} ) ))
    echo ">${DateEnd}<"
    echo "tout va bien 1"

  # - Determine RestartDate
    (( Offset = -1 ))
    (( RestartDate = $( IGCM_date_AddDaysToGregorianDate ${DateBegin} ${Offset} ) ))

    IGCM_debug_Print 2 "${DateBegin} => ${DateEnd} : ${StartDir}"

  # -  Does $StartDir already exist ?
    #echo "tout va bien 2" ${StartDir}
    if [ ! -d ${SUBMIT_DIR}/${StartDir} ] ; then
      echo "create dir"
      IGCM_sys_Mkdir ${SUBMIT_DIR}/${StartDir}
      IGCM_sys_Cd ${SUBMIT_DIR}/${StartDir}
      ln -s ../../.resol .
      ln -s ../../.libmpi .
      IGCM_sys_Cd ${RUN_DIR}
      echo "${DateBegin} ${DateEnd} ${StartDir}" >> ${RUN_DIR}/CreatedDir.txt
    fi
    PeriodDateEnd=$( grep -m1 ${StartDir} ${RUN_DIR}/CreatedDir.txt | cut -f2 -d\  )

  # - Create directory in which to store new restart files if it does'nt already exist
    RestartDir=${STORAGE}/IGCM_IN/${config_UserChoices_TagName}/${StartDir}
    IGCM_sys_MkdirArchive ${RestartDir}

  # - Loop over members
    i=0
    while [ $i -lt ${ensemble_Ens_PERTURB_MEMBER} ] ; do
      MemberDir="${StartDir}${Alpha[$i]}"
      IGCM_debug_Print 3 "${MemberDir}"

      JobName="Job_${MemberDir}"

    # * Create directory if it doesn't exist and copy files
      if [ ! -d  ${SUBMIT_DIR}/${StartDir}/${MemberDir} ] ; then
        IGCM_sys_Mkdir ${SUBMIT_DIR}/${StartDir}/${MemberDir}
        #IGCM_sys_Cp -r COMP/ PARAM/ ${StartDir}/${MemberDir}
        IGCM_sys_Cd ${SUBMIT_DIR}/${StartDir}/${MemberDir}
        ln -s ../../COMP
        ln -s ../../PARAM
        ln -s ../../POST
        ln -s ../../DRIVER
        IGCM_sys_Cd ${RUN_DIR}
        IGCM_sys_Cp config.card run.card.init ${SUBMIT_DIR}/${StartDir}/${MemberDir}
        IGCM_sys_Cp Job_${config_UserChoices_JobName} ${SUBMIT_DIR}/${StartDir}/${MemberDir}/Job_${MemberDir}

        # Dump command to be lauched
        echo "cd ${StartDir}/${MemberDir}/ ;"  >> Qsub.${StartDir}.sh
        echo "${SUBMIT} ${JobName} ; cd -"     >> Qsub.${StartDir}.sh

        echo "cd ${StartDir}/${MemberDir}/ ;"  >> ${RUN_DIR}/Qclean.month.${StartDir}.sh
        echo "${libIGCM}/clean_month.job ; cd -"     >> ${RUN_DIR}/Qclean.month.${StartDir}.sh

        echo "cd ${StartDir}/${MemberDir}/ ;"  >> ${RUN_DIR}/Qclean.year.${StartDir}.sh
        echo "${libIGCM}/clean_year.job ; cd -"     >> ${RUN_DIR}/Qclean.year.${StartDir}.sh

        # * Update files : config.card, Job_, COMP/comp.card
        echo "${PeriodDateEnd} ? ${DateEnd}"
        if [ ${PeriodDateEnd} -gt ${DateEnd} ] ; then
          DateEnd=${PeriodDateEnd}
        fi
        IGCM_ensemble_CastFilesUpdate ${DateBegin} ${DateEnd} ${RestartDate}

        # * Apply noise on restart file
        IGCM_ensemble_CastPerturbFile
      fi

      (( i = i + 1 ))
    done

    # Done. Save ${StartDir} submission text file
    IGCM_sys_Cp ${RUN_DIR}/Qsub.${StartDir}.sh ${SUBMIT_DIR}
    IGCM_sys_Cp ${RUN_DIR}/Qclean.month.${StartDir}.sh ${SUBMIT_DIR}
    IGCM_sys_Cp ${RUN_DIR}/Qclean.year.${StartDir}.sh ${SUBMIT_DIR}

    (( DateNum = DateNum + 1 ))
  done
  IGCM_debug_PopStack "IGCM_ensemble_CastNonPeriodicStarts"
}

function IGCM_ensemble_CastMemberList
{
  IGCM_debug_PushStack "IGCM_ensemble_CastMemberList"

  if [ ${CastMemberList} = false ] ; then
    IGCM_debug_PopStack "IGCM_ensemble_CastMemberList"
    return
  fi

  echo
  IGCM_debug_Print 1 "Manage members list"

#.. Manage members list ..
#   ======================

  # DateBegin
  eval DateBegin=\${ensemble_Ens_PERTURB_BEGIN_INIT}

  IGCM_date_GetYearMonth ${DateBegin} year month

  # - Determine number of day(s) in LENGTH
  DureeLengthInDays=$(( $( IGCM_date_DaysInCurrentPeriod ${DateBegin} ${ensemble_Ens_PERTURB_LENGTH} ) - 1 ))

  # - Determine DateEnd
  DateEnd=$( IGCM_date_AddDaysToGregorianDate ${DateBegin} ${DureeLengthInDays} )

  # bad hack enforce yearly for parent directory name
  # - Build directory name
  IGCM_ensemble_CastDirectoryName ${ensemble_Ens_PERTURB_NAME} 1Y $year $month $StartDir

  # - Determine RestartDate
  (( Offset = -1 ))
  RestartDate=$( IGCM_date_AddDaysToGregorianDate ${DateBegin} ${Offset} )

  IGCM_debug_Print 2 "${DateBegin} => ${DateEnd} : ${StartDir}"
  echo "${DateBegin} ${DateEnd} ${StartDir}" >> ${RUN_DIR}/CreatedDir.txt

  # - Create directory for current DateBegin
  if [ ! -d  ${StartDir} ] ; then
    IGCM_sys_Mkdir ${SUBMIT_DIR}/${StartDir}
    IGCM_sys_Cd ${SUBMIT_DIR}/${StartDir}
    ln -s ../../.resol .
    ln -s ../../.libmpi .
    IGCM_sys_Cd ${RUN_DIR}
  fi

  # - Create directory to store modified restart files
  RestartDir=${STORAGE}/IGCM_IN/${config_UserChoices_TagName}/${StartDir}
  IGCM_sys_MkdirArchive ${RestartDir}

  # - Loop over members
  i=0
  nbmember=${#ensemble_Ens_PERTURB_MEMBER_LIST[*]}
  while [ $i -lt $nbmember ] ; do
    MemberDir=${ensemble_Ens_PERTURB_MEMBER_NAMESLIST[${i}]}
    MemberVec=${ensemble_Ens_PERTURB_MEMBER_LIST[${i}]}

    JobName="Job_${MemberDir}"
    echo
    IGCM_debug_Print 3 "${MemberDir}"

    # * Create directory if it doesn't exist and copy/link files
    if [ ! -d  ${SUBMIT_DIR}/${StartDir}/${MemberDir} ] ; then
      IGCM_sys_Mkdir ${SUBMIT_DIR}/${StartDir}/${MemberDir}
      IGCM_sys_Cd ${SUBMIT_DIR}/${StartDir}/${MemberDir}
      ln -s ../../COMP
      ln -s ../../PARAM
      ln -s ../../POST
      ln -s ../../DRIVER
      IGCM_sys_Cd ${RUN_DIR}
      IGCM_sys_Cp config.card run.card.init ${SUBMIT_DIR}/${StartDir}/${MemberDir}
      IGCM_sys_Cp Job_${config_UserChoices_JobName} ${SUBMIT_DIR}/${StartDir}/${MemberDir}/${JobName}

      # Dump command to be lauched
      echo "cd ${StartDir}/${MemberDir}/ ;"     >> ${RUN_DIR}/Qsub.${StartDir}.sh
      echo "${SUBMIT} ${JobName} ; cd -"        >> ${RUN_DIR}/Qsub.${StartDir}.sh

      echo "cd ${StartDir}/${MemberDir}/ ;"     >> ${RUN_DIR}/Qclean.month.${StartDir}.sh
      echo "${libIGCM}/clean_month.job ; cd -"  >> ${RUN_DIR}/Qclean.month.${StartDir}.sh

      echo "cd ${StartDir}/${MemberDir}/ ;"     >> ${RUN_DIR}/Qclean.year.${StartDir}.sh
      echo "${libIGCM}/clean_year.job ; cd -"   >> ${RUN_DIR}/Qclean.year.${StartDir}.sh

      # * Update files : config.card, Job_, COMP/comp.card
      IGCM_ensemble_CastFilesUpdate ${DateBegin} ${DateEnd} ${RestartDate}

      # * Apply noise on restart file
      IGCM_ensemble_CastPerturbFile
    fi

    (( i = i + 1 ))
  done

  # Done. Save ${StartDir} submission text file
  IGCM_sys_Cp ${RUN_DIR}/Qsub.${StartDir}.sh ${SUBMIT_DIR}
  IGCM_sys_Cp ${RUN_DIR}/Qclean.month.${StartDir}.sh ${SUBMIT_DIR}
  IGCM_sys_Cp ${RUN_DIR}/Qclean.year.${StartDir}.sh ${SUBMIT_DIR}

  IGCM_debug_PopStack "IGCM_ensemble_CastMemberList"
}

function IGCM_ensemble_CastFilesUpdate
{
  IGCM_debug_PushStack "IGCM_ensemble_CastFilesUpdate"

  # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_ensemble_CastFilesUpdate :"

  HumanDateBegin=$(   IGCM_date_ConvertFormatToHuman ${1} )
  HumanDateEnd=$(     IGCM_date_ConvertFormatToHuman ${2} )
  HumanRestartDate=$( IGCM_date_ConvertFormatToHuman ${3} )
  # ==> config.card
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card Ensemble EnsembleRun 'y'
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card Ensemble EnsembleName ${ensemble_Ens_PERTURB_NAME}
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card Ensemble EnsembleDate ${StartDir}

  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card UserChoices JobName   ${MemberDir}
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card UserChoices DateBegin ${HumanDateBegin}
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card UserChoices DateEnd   ${HumanDateEnd}
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card Restarts OverRule "n"

  for comp in ${config_ListOfComponents[*]} ; do
    IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card ${comp} Restart "y"
    IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card ${comp} RestartDate ${HumanRestartDate}
    IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card ${comp} RestartJobName ${ensemble_Ens_PERTURB_INITFROM}
    IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card ${comp} RestartPath ${ensemble_Ens_PERTURB_INITPATH}
  done

  # ==> Job
  sed -e "s/\(#.*Script_Output_\)${config_UserChoices_JobName}\(\.*\)/\1${MemberDir}\2/" \
      -e "s/\(#.*\)${config_UserChoices_JobName}\(\.*\)/\1${MemberDir} \2/"            \
      -e "s/^PeriodNb=.*/PeriodNb=60/"                                                   \
      ${SUBMIT_DIR}/${StartDir}/${MemberDir}/Job_${MemberDir} > Job_${MemberDir}.tmp
  IGCM_sys_Mv Job_${MemberDir}.tmp ${SUBMIT_DIR}/${StartDir}/${MemberDir}/Job_${MemberDir}

  IGCM_debug_PopStack "IGCM_ensemble_CastFilesUpdate"
}

function IGCM_ensemble_CastDirectoryName
{
  IGCM_debug_PushStack "IGCM_ensemble_CastDirectoryName"

  #.. Debug Print ..
  echo
  IGCM_debug_Print 1 "IGCM_ensemble_CastDirectoryName :"
  echo

  Name=$1
  Duree=$2
  year=$3
  month=$4

  # - Build directory name
  case ${Duree} in
  *Y|*y)
    siecle="$( echo $year | cut -c1-2 )"
    siecle=$( (( $siecle - 18 )) )
    StartYear="${siecle}$( echo $year | cut -c3-4 )"
    StartDir="${Name}${StartYear}"
    ;;
  *M|*m)
    echo $month
    siecle="$( echo $year | cut -c1-2 )"
    siecle=$( (( $siecle - 18 )) )
    StartYear="${siecle}$( echo $year | cut -c3-4 )"
    StartMonth="${AlphaMonth[ (( 10#${month} - 1 )) ]}"
    StartDir="${Name}${StartYear}${StartMonth}"
    ;;
  esac

  IGCM_debug_PopStack "IGCM_ensemble_CastDirectoryName"
}

function IGCM_ensemble_CastPerturbFile
{
  IGCM_debug_PushStack "IGCM_ensemble_CastPerturbFile"

  typeset i i_ j
  typeset -Z4 j4
  typeset file_out file_out_

  #.. Debug Print ..
  echo
  IGCM_debug_Print 1 "IGCM_ensemble_CastPerturbFile :"

  #.. FileIn ? => RestartDate ..
  DirIn="${ensemble_Ens_PERTURB_INITPATH}/${ensemble_Ens_PERTURB_INITFROM}/${PerturbComp}/Restart"
  DirInTar="${ensemble_Ens_PERTURB_INITPATH}/${ensemble_Ens_PERTURB_INITFROM}/RESTART"
  FileIn="${ensemble_Ens_PERTURB_INITFROM}_${RestartDate}_${PerturbFile}"
  DirOut="${RestartDir}/${MemberDir}/${PerturbComp}/Restart"

  # * Create member restart directory
  IGCM_sys_TestDirArchive ${DirOut}
  RET=$?
  if [ $RET -gt 0 ] ; then
    IGCM_sys_MkdirArchive ${DirOut}
  fi

  FileOut="${MemberDir}_${RestartDate}_${PerturbFile}"
  IGCM_debug_Print 1 "FileIn  = ${DirIn}/${FileIn}"
  IGCM_debug_Print 1 "FileOut = ${DirOut}/${FileOut}.nc"

  IGCM_sys_TestFileArchive ${DirOut}/${FileOut}.nc
  RET=$?
  if [ $RET -gt 0 ] ; then

    # * Look for the restart file we apply the noise to

    # restart file list pertaining to the component we apply the noise to
    # but not being the precise restart file we will apply the noise to
    unset OtherFileInList
    # generic restart filename list (like flxat, sstoc, restart, ...)
    unset OtherGenericList

    if ( [ X${FileName0} != X${NULL_STR} ] && [ X${FileName0} != XNONE ] ) ; then
      (( i=0 ))
      until [ $i -ge ${NbFiles} ]; do

        (( i_ = i+1 ))
        eval file_out_=\${${ListFilesName}[$i_]} > /dev/null 2>&1
        eval file_out=${file_out_}

        generic_restart_file_name_out=$( basename ${file_out} .nc )

        if [ ! ${generic_restart_file_name_out} = ${PerturbFile} ] ; then
          set +A OtherFileInList ${OtherFileInList[*]} ${PerturbComp}_${ensemble_Ens_PERTURB_INITFROM}_${RestartDate}_${generic_restart_file_name_out}\*.nc
          set +A OtherGenericList ${OtherGenericList[*]} ${generic_restart_file_name_out}
        fi

        (( i=i+3 ))
      done
    fi
    # How many restart files other than the one we will apply the noise to
    NbOtherFiles=${#OtherGenericList[*]}

    ##########################
    # TO BE A FUNCTION BEGIN #
    ##########################

    if [ $( IGCM_sys_TestFileBuffer ${DirIn}/${FileIn}*.nc ; echo $? ) = 0 ] ; then
      IGCM_debug_Print 3 "Buffered restart"
      Buffered=true
      Archived=false
      Tared=false
      nb_restart_file=$(IGCM_sys_CountFileBuffer ${DirIn}/${FileIn}_????.nc)
    elif [ $( IGCM_sys_TestFileArchive ${DirIn}/${FileIn}*.nc ; echo $? ) = 0 ] ; then
      IGCM_debug_Print 3 "Archived restart"
      Buffered=false
      Archived=true
      Tared=false
      nb_restart_file=$(IGCM_sys_CountFileArchive ${DirIn}/${FileIn}_????.nc)
    else
      IGCM_debug_Print 3 "Tared restart"
      Buffered=false
      Archived=false
      Tared=true

      # Look for the tar file we want if we did not found it already
      for PotentialTarFile in $( find ${ensemble_Ens_PERTURB_INITPATH}/${ensemble_Ens_PERTURB_INITFROM}/RESTART -name "${ensemble_Ens_PERTURB_INITFROM}_*restart*.tar" -print ) ; do
        IsMatching=$( echo ${PotentialTarFile##*/} | sed "s:_restart::" | sed "s:^${ensemble_Ens_PERTURB_INITFROM}_::" | sed "s:\.tar$::" | gawk -F_ -v restartdate=${RestartDate} '{if (($1 <= restartdate) && ($2 >= restartdate)) {print $1"_"$2}}' )
        if [ ! X${IsMatching} = X ] ; then
          TarFileFound=${PotentialTarFile}
          break
        fi
      done

      # Extract relevant restart files
      IGCM_debug_Print 1 "tar xvf ${TarFileFound} ${PerturbComp}_${FileIn}*.nc ${OtherFileInList[*]}"
      tar xvf ${TarFileFound} ${PerturbComp}_${FileIn}*.nc ${OtherFileInList[*]}
      nb_restart_file=$( IGCM_sys_CountFileBuffer ${PerturbComp}_${FileIn}_????.nc )
    fi

    # Move around and perturb restart files so as to be able to start hindcast/forecast simulation members
    if [ ${nb_restart_file} -gt 1 ] ; then
      j=0
      until [ $j -ge ${nb_restart_file} ]; do
        j4=${j}
        if [ X${Buffered} = Xtrue ] ; then
          IGCM_sys_GetBuffer ${DirIn}/${FileIn}_${j4}.nc ${RUN_DIR}/${FileOut}_${j4}.nc

          cd ${DirOut}
          for generic in ${OtherGenericList[*]} ; do
            ln -s ${DirIn}/${ensemble_Ens_PERTURB_INITFROM}_${RestartDate}_${generic}.nc ${MemberDir}_${RestartDate}_${generic}_${j4}.nc
          done
          cd -

        elif [ X${Archived} = Xtrue ] ; then
          IGCM_sys_Get ${DirIn}/${FileIn}_${j4}.nc ${RUN_DIR}/${FileOut}_${j4}.nc

          for generic in ${OtherGenericList[*]} ; do
            IGCM_sys_RshArchive "cd ${DirOut} ; ln -s ${DirIn}/${ensemble_Ens_PERTURB_INITFROM}_${RestartDate}_${generic}.nc ${MemberDir}_${RestartDate}_${generic}_${j4}.nc"
          done

        elif [ X${Tared} = Xtrue ] ; then
          IGCM_debug_Print 2 "IGCM_sys_Mv ${PerturbComp}_${FileIn}_${j4}.nc ${RUN_DIR}/${FileOut}_${j4}.nc"
          IGCM_sys_Mv ${PerturbComp}_${FileIn}_${j4}.nc ${RUN_DIR}/${FileOut}_${j4}.nc

          for generic in ${OtherGenericList[*]} ; do
            IGCM_sys_Mv ${PerturbComp}_${ensemble_Ens_PERTURB_INITFROM}_${RestartDate}_${generic}.nc ${DirOut}/${MemberDir}_${RestartDate}_${generic}_${j4}.nc
          done

        fi
        (( j=j+1 ))
      done
    else
      if [ X${Buffered} = Xtrue ] ; then
        IGCM_sys_GetBuffer ${DirIn}/${FileIn}.nc ${RUN_DIR}/${FileOut}.nc

        cd ${DirOut}
        for generic in ${OtherGenericList[*]} ; do
          ln -s ${DirIn}/${ensemble_Ens_PERTURB_INITFROM}_${RestartDate}_${generic}.nc ${MemberDir}_${RestartDate}_${generic}.nc
        done
        cd -

      elif [ X${Archived} = Xtrue ] ; then
        IGCM_sys_Get ${DirIn}/${FileIn}.nc ${RUN_DIR}/${FileOut}.nc

        for generic in ${OtherGenericList[*]} ; do
          IGCM_sys_RshArchive "cd ${DirOut} ; ln -s ${DirIn}/${ensemble_Ens_PERTURB_INITFROM}_${RestartDate}_${generic}.nc ${MemberDir}_${RestartDate}_${generic}.nc"
        done

      elif [ X${Tared} = Xtrue ] ; then
        IGCM_debug_Print 2 "IGCM_sys_Mv ${PerturbComp}_${FileIn}.nc ${RUN_DIR}/${FileOut}.nc"
        IGCM_sys_Mv ${PerturbComp}_${FileIn}.nc ${RUN_DIR}/${FileOut}.nc

        for generic in ${OtherGenericList[*]} ; do
          IGCM_debug_Print 2 "IGCM_sys_Mv ${PerturbComp}_${ensemble_Ens_PERTURB_INITFROM}_${RestartDate}_${generic}.nc ${DirOut}/${MemberDir}_${RestartDate}_${generic}.nc"
          IGCM_sys_Mv ${PerturbComp}_${ensemble_Ens_PERTURB_INITFROM}_${RestartDate}_${generic}.nc ${DirOut}/${MemberDir}_${RestartDate}_${generic}.nc
        done

      fi
    fi

    ########################
    # TO BE A FUNCTION END #
    ########################

# treat the perturbation on different components by looking at the executable name

    case ${PerturbExe} in
    (AddNoise)
      IGCM_sys_Chmod 644 ${RUN_DIR}/${FileOut}.nc

      IGCM_debug_Print 1 "${PerturbExe} ${RUN_DIR}/${FileOut}.nc ${PerturbVar} ${PerturbAmp}"
      echo

      ${PerturbExe} ${RUN_DIR}/${FileOut}.nc ${PerturbVar} ${PerturbAmp}  > /dev/null 2>&1
      if [ $? -ne 0 ] ; then
        IGCM_debug_Exit "Abend $( basename ${PerturbExe} )"
        IGCM_debug_Verif_Exit
      fi
      IGCM_sys_Put_Out ${RUN_DIR}/${FileOut}.nc ${DirOut}/ 644
      ;;
    (AddPertu3DOCE)
      # where to find the pattern we apply to the restart
      PatternFile=${ensemble_Ens_PERTURB_MEMBER_INITPATH}/${ensemble_Ens_PERTURB_MEMBER_INITFROM}/${MemberVec}.nc

      # where to find the land mask for the grid
      MaskFile=${ensemble_Ens_PERTURB_MASKPATH}/${PerturbMask}

      # if there is multiple restart files rebuild restart file
      if [ ${nb_restart_file} -gt 1 ] ; then
        IGCM_debug_Print 1 "rebuild files ${FileOut}_????.nc"
        IGCM_sys_rebuild ${RUN_DIR}/${FileOut}.nc ${RUN_DIR}/${FileOut}_????.nc
      fi

      # there is now a single restart file
      IGCM_sys_Chmod 644 ${RUN_DIR}/${FileOut}.nc

      IGCM_debug_Print 1 "${PerturbExe} ${RUN_DIR}/${FileOut}.nc ${PerturbVar} ${PatternFile} ${MaskFile}"
      echo

      # add pattern to restart file on variable PerturbVar
      ${PerturbExe} ${RUN_DIR}/${FileOut}.nc ${PerturbVar} ${PatternFile} ${MaskFile}  > /dev/null 2>&1
      if [ $? -ne 0 ] ; then
        IGCM_debug_Exit "Abend $( basename ${PerturbExe} )"
        IGCM_debug_Verif_Exit
      fi
      IGCM_sys_Put_Out ${RUN_DIR}/${FileOut}.nc ${DirOut}/ 644
     ;;
    esac

  fi

  #.. Update config.card..
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card ${PerturbComp} Restart "y"
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card ${PerturbComp} RestartDate    ${HumanRestartDate}
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card ${PerturbComp} RestartJobName ${MemberDir}
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card ${PerturbComp} RestartPath    ${RestartDir}/
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card Ensemble EnsembleType "Ens_PERTURB"

  IGCM_debug_PopStack "IGCM_ensemble_CastPerturbFile"
}

############### Date ENSEMBLE #################
function IGCM_ensemble_DateInit
{
  IGCM_debug_PushStack "IGCM_ensemble_DateInit"

  IGCM_sys_Mkdir ${RUN_DIR}

  IGCM_sys_Cp ${SUBMIT_DIR}/config.card   ${RUN_DIR}
  IGCM_sys_Cp ${SUBMIT_DIR}/ensemble.card ${RUN_DIR}
  IGCM_sys_Cp ${SUBMIT_DIR}/Job_*         ${RUN_DIR}
  IGCM_sys_Cp ${SUBMIT_DIR}/run.card.init ${RUN_DIR}
  if [ -f ${SUBMIT_DIR}/Qsub.* ]; then
    IGCM_sys_Cp ${SUBMIT_DIR}/Qsub.*        ${RUN_DIR}
  fi
  if [ -f ${SUBMIT_DIR}/Qclean.* ]; then
    IGCM_sys_Cp ${SUBMIT_DIR}/Qclean.*      ${RUN_DIR}
  fi

  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_DATE active
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_DATE NAME
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_DATE BEGIN_INIT
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_DATE BEGIN_RESTART
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_DATE END_INIT
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_DATE PERIODICITY
  IGCM_card_DefineArrayFromOption    ${F_CFG_ENS} Ens_DATE NONPERIODIC
  IGCM_card_DefineArrayFromOption    ${F_CFG_ENS} Ens_DATE RESTART_NONPERIODIC
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_DATE LENGTH
  IGCM_card_DefineArrayFromOption    ${F_CFG_ENS} Ens_DATE LENGTH_NONPERIODIC
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_DATE INITFROM
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_DATE INITFROM_NONPERIODIC
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_DATE INITPATH
  IGCM_card_DefineVariableFromOption ${F_CFG_ENS} Ens_DATE INITPATH_NONPERIODIC
  IGCM_card_DefineVariableFromOption config.card UserChoices JobName
  IGCM_card_DefineVariableFromOption config.card UserChoices TagName
  IGCM_card_DefineVariableFromOption config.card UserChoices CalendarType
  IGCM_card_DefineArrayFromSection   config.card ListOfComponents

  echo
  IGCM_debug_Print 1 "[Ens_DATE]"
  IGCM_debug_Print 1 "ACTIVE               = ${ensemble_Ens_DATE_active}"
  IGCM_debug_Print 1 "NAME                 = ${ensemble_Ens_DATE_NAME}"
  IGCM_debug_Print 1 "BEGIN_INIT           = ${ensemble_Ens_DATE_BEGIN_INIT}"
  IGCM_debug_Print 1 "END_INIT             = ${ensemble_Ens_DATE_END_INIT}"
  IGCM_debug_Print 1 "PERIODICITY          = ${ensemble_Ens_DATE_PERIODICITY}"
  IGCM_debug_Print 1 "BEGIN_RESTART        = ${ensemble_Ens_DATE_BEGIN_RESTART}"
  IGCM_debug_Print 1 "NONPERIODIC          = ${ensemble_Ens_DATE_NONPERIODIC[*]}"
  IGCM_debug_Print 1 "RESTART_NONPERIODIC  = ${ensemble_Ens_DATE_RESTART_NONPERIODIC[*]}"
  IGCM_debug_Print 1 "LENGTH               = ${ensemble_Ens_DATE_LENGTH}"
  IGCM_debug_Print 1 "LENGTH_NONPERIODIC   = ${ensemble_Ens_DATE_LENGTH_NONPERIODIC[*]}"
  IGCM_debug_Print 1 "INITFROM             = ${ensemble_Ens_DATE_INITFROM}"
  IGCM_debug_Print 1 "INITFROM_NONPERIODIC = ${ensemble_Ens_DATE_INITFROM_NONPERIODIC[*]}"
  IGCM_debug_Print 1 "INITPATH             = ${ensemble_Ens_DATE_INITPATH}"
  IGCM_debug_Print 1 "INITPATH_NONPERIODIC = ${ensemble_Ens_DATE_INITPATH_NONPERIODIC[*]}"
  IGCM_debug_Print 1 "JobName              = ${config_UserChoices_JobName}"
  IGCM_debug_Print 1 "TagName              = ${config_UserChoices_TagName}"
  IGCM_debug_Print 1 "CalendarType         = ${config_UserChoices_CalendarType}"
  IGCM_debug_Print 1 "ListOfComponents     = ${config_ListOfComponents[*]}"
  echo ""

  ensemble_Ens_DATE_MEMBER=1 # actually use only 1 member
  IGCM_ensemble_SetAlpha ${ensemble_Ens_DATE_MEMBER}

  IGCM_debug_Print 1 "Check args..."
  DatePeriodicStart=false
  DateNonPeriodicStart=false

  # ... Check LENGTH ...
  case ${ensemble_Ens_DATE_LENGTH} in
  *[Yy]|*[Mm])
    IGCM_debug_Print 1 "Default simulation duration : ${ensemble_Ens_DATE_LENGTH}" ;;
  *)
    IGCM_debug_Exit "IGCM_ensemble_DateInit ${ensemble_Ens_DATE_LENGTH} invalid LENGTH"
    IGCM_debug_Exit "Choose a value in choose in *Y or *M"
    IGCM_debug_Verif_Exit ;;
  esac

  # ***************************************
  # A few checks Period case:
  # ***************************************
  # if all Periodic params are not filled: desactivate Periodic mode
  totalPeriodArgs=4
  periodFillArgs=0

  if [[ X${ensemble_Ens_DATE_BEGIN_RESTART} != "X" ]]; then
    (( periodFillArgs = periodFillArgs + 1 ))
  fi

  if [[ X${ensemble_Ens_DATE_BEGIN_INIT} != "X" ]]; then
    (( periodFillArgs = periodFillArgs + 1 ))
  fi

  if [[ X${ensemble_Ens_DATE_END_INIT} != "X" ]]; then
    (( periodFillArgs = periodFillArgs + 1 ))
  fi

  if [[ X${ensemble_Ens_DATE_PERIODICITY} != "X" ]]; then
    (( periodFillArgs = periodFillArgs + 1 ))

    # ... Check PERIODICITY ...
    case ${ensemble_Ens_DATE_PERIODICITY} in
    *[Yy]|*[Mm])
      IGCM_debug_Print 1 "Periodic length : ${ensemble_Ens_DATE_PERIODICITY}" ;;
    *)
      IGCM_debug_Exit "IGCM_ensemble_DateInit ${ensemble_Ens_DATE_PERIODICITY} : invalid PERIODICITY"
      IGCM_debug_Exit "Choose a value in *Y or *M"
      IGCM_debug_Verif_Exit ;;
    esac
  fi # if periodicity

  if [[ ${periodFillArgs} = ${totalPeriodArgs} ]]; then
    DatePeriodicStart=true
  else
    if [[ ${periodFillArgs} = 0 ]]; then
      IGCM_debug_Print 1 "Periodic start NOT ACTIVE"
      DatePeriodicStart=false
    else
      IGCM_debug_Exit "IGCM_ensemble_DateInit missing arguments for Periodic mode!"
      IGCM_debug_Exit "Get only ${periodFillArgs} on ${totalPeriodArgs} args. Check ${F_CFG_ENS} file."
      IGCM_debug_Verif_Exit
    fi
  fi

  # ***************************************
  # A few checks for the Non-Periodic case:
  # ***************************************
  if [[ ${#ensemble_Ens_DATE_NONPERIODIC[*]} != ${#ensemble_Ens_DATE_RESTART_NONPERIODIC[*]} ]] ; then
    IGCM_debug_Exit "IGCM_ensemble_DateInit: NONPERIODIC and RESTART_NONPERIODIC lists have different sizes"
    IGCM_debug_Verif_Exit
  fi

  if [[ ${#ensemble_Ens_DATE_NONPERIODIC[*]} > 0 ]] && [[ ${ensemble_Ens_DATE_NONPERIODIC[*]} != _0_ ]]; then
    DateNonPeriodicStart=true

    # Use LENGTH if no NONPERIODIC_LENGTH given
    if [[ ${#ensemble_Ens_DATE_LENGTH_NONPERIODIC[*]} < ${#ensemble_Ens_DATE_NONPERIODIC[*]} ]] ; then
      IGCM_debug_Print 1 "WARNING: LENGTH_NONPERIODIC is not fill (or not correctly). Use LENGTH value '${ensemble_Ens_DATE_LENGTH}' for all NONPERIODIC runs"
      DateNum=0
      while [ ${DateNum} -lt ${#ensemble_Ens_DATE_NONPERIODIC[*]} ] ; do
        ensemble_Ens_DATE_LENGTH_NONPERIODIC[${DateNum}]=${ensemble_Ens_DATE_LENGTH}
        (( DateNum = DateNum + 1 ))
      done
    fi

    # Use INITFROM if no INITFROM_NONPERIODIC given
    if [ ${#ensemble_Ens_DATE_INITFROM_NONPERIODIC[*]} -lt ${#ensemble_Ens_DATE_NONPERIODIC[*]} ] ; then
      IGCM_debug_Print 1 "WARNING: INITFROM_NONPERIODIC is not fill (or not correctly). Use INITFROM value '${ensemble_Ens_DATE_INITFROM}' for all NONPERIODIC runs"
      DateNum=0
      while [ ${DateNum} -lt ${#ensemble_Ens_DATE_NONPERIODIC[*]} ] ; do
        ensemble_Ens_DATE_INITFROM_NONPERIODIC[${DateNum}]=${ensemble_Ens_DATE_INITFROM}
        (( DateNum = DateNum + 1 ))
      done
    fi
    
    # Use INITPATH if no INITPATH_NONPERIODIC given
    if [ ${#ensemble_Ens_DATE_INITPATH_NONPERIODIC[*]} -lt ${#ensemble_Ens_DATE_NONPERIODIC[*]} ] ; then
      IGCM_debug_Print 1 "WARNING: INITPATH_NONPERIODIC is not fill (or not correctly). Use INITPATH value '${ensemble_Ens_DATE_INITPATH}' for all NONPERIODIC runs"
      DateNum=0
      while [ ${DateNum} -lt ${#ensemble_Ens_DATE_NONPERIODIC[*]} ] ; do
        ensemble_Ens_DATE_INITPATH_NONPERIODIC[${DateNum}]=${ensemble_Ens_DATE_INITPATH}
        (( DateNum = DateNum + 1 ))
      done
    fi
  else
    IGCM_debug_Print 1 "Non-Periodic start NOT ACTIVE"
    DateNonPeriodicStart=false
  fi

  if [[ ${DateNonPeriodicStart} = true ]]; then
    DateNum=0
    while [ ${DateNum} -lt ${#ensemble_Ens_DATE_NONPERIODIC[*]} ] ; do
      # - Check LENGTH_NONPERIODIC
      case ${ensemble_Ens_DATE_LENGTH_NONPERIODIC[${DateNum}]} in
      *[Yy]|*[Mm])
        IGCM_debug_Print 1 "Non-periodic duration ${DateNum}: ${ensemble_Ens_DATE_LENGTH_NONPERIODIC[${DateNum}]}"
        ;;
      *)
        IGCM_debug_Exit "IGCM_ensemble_DateInit ${ensemble_Ens_DATE_LENGTH_NONPERIODIC[${DateNum}]} : invalid NON PERIODIC LENGTH"
        IGCM_debug_Exit "choose in *Y or *M"
        IGCM_debug_Verif_Exit ;;
      esac

      # - Check RESTART_NONPERIODIC
      case ${ensemble_Ens_DATE_RESTART_NONPERIODIC[${DateNum}]} in
      _0_)
        IGCM_debug_Exit "IGCM_ensemble_DateInit ${ensemble_Ens_DATE_RESTART_NONPERIODIC[${DateNum}]} : invalid NON PERIODIC RESTART"
        IGCM_debug_Verif_Exit ;;
      esac

      (( DateNum = DateNum + 1 ))
    done
  fi # DateNonPeriodicStart = true

  IGCM_debug_PopStack "IGCM_ensemble_DateInit"
}

function IGCM_ensemble_DatePeriodicStarts
{
  IGCM_debug_PushStack "IGCM_ensemble_DatePeriodicStarts"

  [ ${DatePeriodicStart} = false ] && return

  echo
  IGCM_debug_Print 1 ">>>  MANAGE PERIODIC STARTS  <<<"

#.. Manage periodic starts ..
#   ======================

  # - Build directory name
  StartDir="${ensemble_Ens_DATE_NAME}"

  # - Create directory for current DateBegin
  if [ ! -d  ${StartDir} ] ; then
    IGCM_sys_Mkdir ${SUBMIT_DIR}/${StartDir}
    IGCM_sys_Cd ${SUBMIT_DIR}/${StartDir}
    ln -s ../../.resol .
    ln -s ../../.libmpi .
    IGCM_sys_Cd ${RUN_DIR}
  fi

  # ... Loop over DateBegin ...
  eval DateBegin=\${ensemble_Ens_DATE_BEGIN_INIT}
  eval RestartDate=\${ensemble_Ens_DATE_BEGIN_RESTART}

  DateNum=0
  while [ ${DateBegin} -le ${ensemble_Ens_DATE_END_INIT} ] ; do
    IGCM_date_GetYearMonth ${DateBegin} year month

    echo "========================================================================"
    echo "New DateBegin = $DateBegin"

  # - Determine number of day(s) in PERIODICITY
    PeriodLengthInDays=$( IGCM_date_DaysInCurrentPeriod ${DateBegin} ${ensemble_Ens_DATE_PERIODICITY} )

  # - Determine number of day(s) in LENGTH
    DureeLengthInDays=$(( $( IGCM_date_DaysInCurrentPeriod ${DateBegin} ${ensemble_Ens_DATE_LENGTH} ) - 1 ))

  # - Determine DateEnd
    (( DateEnd = $( IGCM_date_AddDaysToGregorianDate ${DateBegin} ${DureeLengthInDays} ) ))

    IGCM_debug_Print 2 "${DateBegin} => ${DateEnd} : ${StartDir}"
    echo "${DateBegin} ${DateEnd} ${StartDir}" >> ${RUN_DIR}/CreatedDir.txt

  # - Loop over members (default =1 no member)
    i=0
    while [ $i -lt ${ensemble_Ens_DATE_MEMBER} ] ; do
      MemberDir="${ensemble_Ens_DATE_NAME}${DateNum}${Alpha[$i]}_per"
      echo
      IGCM_debug_Print 3 "${MemberDir}"

      JobName="Job_${MemberDir}"

    # * Create directory if it doesn't exist and copy/link files
      if [ ! -d  ${SUBMIT_DIR}/${StartDir}/${MemberDir} ] ; then
        IGCM_sys_Mkdir ${SUBMIT_DIR}/${StartDir}/${MemberDir}
        IGCM_sys_Cd ${SUBMIT_DIR}/${StartDir}/${MemberDir}
        ln -s ../../COMP
        ln -s ../../PARAM
        ln -s ../../POST
        ln -s ../../DRIVER
        IGCM_sys_Cd ${RUN_DIR}
        IGCM_sys_Cp config.card run.card.init ${SUBMIT_DIR}/${StartDir}/${MemberDir}
        IGCM_sys_Cp Job_${config_UserChoices_JobName} ${SUBMIT_DIR}/${StartDir}/${MemberDir}/${JobName}

        # Dump command to be lauched
        echo "cd ${StartDir}/${MemberDir}/ ;"  >> ${RUN_DIR}/Qsub.${StartDir}.sh
        echo "${SUBMIT} ${JobName} ; cd -"     >> ${RUN_DIR}/Qsub.${StartDir}.sh

        echo "cd ${StartDir}/${MemberDir}/ ;"  >> ${RUN_DIR}/Qclean.month.${StartDir}.sh
        echo "${libIGCM}/clean_month.job ; cd -"     >> ${RUN_DIR}/Qclean.month.${StartDir}.sh

        echo "cd ${StartDir}/${MemberDir}/ ;"  >> ${RUN_DIR}/Qclean.year.${StartDir}.sh
        echo "${libIGCM}/clean_year.job ; cd -"     >> ${RUN_DIR}/Qclean.year.${StartDir}.sh

        # * Update files : config.card, Job_, COMP/comp.card
        IGCM_ensemble_DateFilesUpdate ${DateBegin} ${DateEnd} ${RestartDate}
      fi

      (( i = i + 1 ))
    done

  # - Next DateBegin & RestartDate
    echo "$DateBegin  $PeriodLengthInDays"
    case ${ensemble_Ens_DATE_PERIODICITY} in
    *[Yy]|*[Mm])
      (( DateBegin =   $( IGCM_date_AddDaysToGregorianDate ${DateBegin}   ${PeriodLengthInDays} ) ))
      (( RestartDate = $( IGCM_date_AddDaysToGregorianDate ${RestartDate} ${PeriodLengthInDays} ) ))
      ;;
    esac

    (( DateNum = DateNum + 1 )) # increment number of restart date
  done

  # Done. Save ${StartDir} submission text file
  IGCM_sys_Cp ${RUN_DIR}/Qsub.${StartDir}.sh ${SUBMIT_DIR}
  IGCM_sys_Cp ${RUN_DIR}/Qclean.month.${StartDir}.sh ${SUBMIT_DIR}
  IGCM_sys_Cp ${RUN_DIR}/Qclean.year.${StartDir}.sh ${SUBMIT_DIR}

  IGCM_debug_PopStack "IGCM_ensemble_DatePeriodicStarts"
}

function IGCM_ensemble_DateNonPeriodicStarts
{
  IGCM_debug_PushStack "IGCM_ensemble_DateNonPeriodicStarts"

  #.. Manage non periodic starts => Loop over DateBegin ..
  #   ==========================

  [ ${DateNonPeriodicStart} = false ] && return

  echo
  IGCM_debug_Print 1 ">>>  MANAGE NON PERIODIC STARTS  <<<"

  # - Build directory name
  echo ""
  echo "========================================================================"
  echo "ensemble_Ens_DATE_NAME = ${ensemble_Ens_DATE_NAME}"
  StartDir="${ensemble_Ens_DATE_NAME}"

  # -  Does $StartDir already exist ?
  if [ ! -d ${SUBMIT_DIR}/${StartDir} ] ; then
    IGCM_sys_Mkdir ${SUBMIT_DIR}/${StartDir}
    IGCM_sys_Cd ${SUBMIT_DIR}/${StartDir}
    ln -s ../../.resol .
    ln -s ../../.libmpi .
    IGCM_sys_Cd ${RUN_DIR}
  fi

  DateNum=0
  # ... Loop over ensemble_Ens_DATE_NONPERIODIC ...
  while [ ${DateNum} -lt ${#ensemble_Ens_DATE_NONPERIODIC[*]} ] ; do
    DateBegin=${ensemble_Ens_DATE_NONPERIODIC[${DateNum}]}
    Duree=${ensemble_Ens_DATE_LENGTH_NONPERIODIC[${DateNum}]}
    RestartDate=${ensemble_Ens_DATE_RESTART_NONPERIODIC[${DateNum}]}
    InitFrom=${ensemble_Ens_DATE_INITFROM_NONPERIODIC[${DateNum}]}
    InitPath=${ensemble_Ens_DATE_INITPATH_NONPERIODIC[${DateNum}]}

  # - Determine number of day(s) in LENGTH_NONPERIODIC
    IGCM_date_GetYearMonth ${DateBegin} year month
    DureeLengthInDays=$(( $( IGCM_date_DaysInCurrentPeriod ${DateBegin} ${Duree} ) - 1 ))

  # - Determine DateEnd
    (( DateEnd = $( IGCM_date_AddDaysToGregorianDate ${DateBegin} ${DureeLengthInDays} ) ))

    IGCM_debug_Print 2 "${DateBegin} => ${DateEnd} : ${StartDir}"
    echo "${DateBegin} ${DateEnd} ${StartDir}" >> ${RUN_DIR}/CreatedDir.txt

    PeriodDateEnd=$( grep -m1 ${StartDir} ${RUN_DIR}/CreatedDir.txt | cut -f2 -d\  )

  # - Loop over members
    i=0
    while [ $i -lt ${ensemble_Ens_DATE_MEMBER} ] ; do
      MemberDir="${ensemble_Ens_DATE_NAME}${DateNum}${Alpha[$i]}"
      IGCM_debug_Print 3 "${MemberDir}"

      JobName="Job_${MemberDir}"

    # * Create directory if it doesn't exist and copy files
      if [ ! -d  ${SUBMIT_DIR}/${StartDir}/${MemberDir} ] ; then
        IGCM_sys_Mkdir ${SUBMIT_DIR}/${StartDir}/${MemberDir}
        IGCM_sys_Cd ${SUBMIT_DIR}/${StartDir}/${MemberDir}
        ln -s ../../COMP
        ln -s ../../PARAM
        ln -s ../../POST
        ln -s ../../DRIVER
        IGCM_sys_Cd ${RUN_DIR}
        IGCM_sys_Cp config.card run.card.init ${SUBMIT_DIR}/${StartDir}/${MemberDir}
        IGCM_sys_Cp Job_${config_UserChoices_JobName} ${SUBMIT_DIR}/${StartDir}/${MemberDir}/Job_${MemberDir}

        # Dump command to be lauched
        echo "cd ${StartDir}/${MemberDir}/ ;"  >> ${RUN_DIR}/Qsub.${StartDir}.sh
        echo "${SUBMIT} ${JobName} ; cd -"     >> ${RUN_DIR}/Qsub.${StartDir}.sh

        echo "cd ${StartDir}/${MemberDir}/ ;"  >> ${RUN_DIR}/Qclean.month.${StartDir}.sh
        echo "${libIGCM}/clean_month.job ; cd -"     >> ${RUN_DIR}/Qclean.month.${StartDir}.sh

        echo "cd ${StartDir}/${MemberDir}/ ;"  >> ${RUN_DIR}/Qclean.year.${StartDir}.sh
        echo "${libIGCM}/clean_year.job ; cd -"     >> ${RUN_DIR}/Qclean.year.${StartDir}.sh

        # * Update files : config.card, Job_, COMP/comp.card
        IGCM_ensemble_DateFilesUpdate ${DateBegin} ${DateEnd} ${RestartDate} ${InitFrom} ${InitPath}
      fi

      (( i = i + 1 ))
    done

    # Done. Save ${StartDir} submission text file
    IGCM_sys_Cp ${RUN_DIR}/Qsub.${StartDir}.sh ${SUBMIT_DIR}
    IGCM_sys_Cp ${RUN_DIR}/Qclean.month.${StartDir}.sh ${SUBMIT_DIR}
    IGCM_sys_Cp ${RUN_DIR}/Qclean.year.${StartDir}.sh ${SUBMIT_DIR}

    (( DateNum = DateNum + 1 ))
  done

  # Done. Save ${StartDir} submission text file
  IGCM_sys_Cp ${RUN_DIR}/Qsub.${StartDir}.sh ${SUBMIT_DIR}
  IGCM_sys_Cp ${RUN_DIR}/Qclean.month.${StartDir}.sh ${SUBMIT_DIR}
  IGCM_sys_Cp ${RUN_DIR}/Qclean.year.${StartDir}.sh ${SUBMIT_DIR}

  IGCM_debug_PopStack "IGCM_ensemble_DateNonPeriodicStarts"
}

function IGCM_ensemble_DateFilesUpdate
{
  IGCM_debug_PushStack "IGCM_ensemble_DateFilesUpdate"

  # Debug Print :
  echo
  IGCM_debug_Print 1 "IGCM_ensemble_DateFilesUpdate :"

  HumanDateBegin=$(   IGCM_date_ConvertFormatToHuman ${1} )
  HumanDateEnd=$(     IGCM_date_ConvertFormatToHuman ${2} )
  HumanRestartDate=$( IGCM_date_ConvertFormatToHuman ${3} )
  if [[ X${4} != "X" ]]; then
    initFrom=${4} # non periodic config (INITFROM could be different between members)
  else
    initFrom=${ensemble_Ens_DATE_INITFROM} # periodic (same INITFROM value)
  fi

  if [[ X${5} != "X" ]]; then
    initPath=${5} # non periodic config (INITPATH could be different between members)
  else
    initPath=${ensemble_Ens_DATE_INITPATH} # periodic (same INITPATH value)
  fi
  
  # ==> config.card
  # [ENSEMBLE]
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card Ensemble EnsembleRun 'y'
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card Ensemble EnsembleName ${ensemble_Ens_DATE_NAME}
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card Ensemble EnsembleDate ${HumanDateBegin}
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card Ensemble EnsembleType "Ens_DATE"

  # [UserChoices]
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card UserChoices JobName   ${MemberDir}
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card UserChoices DateBegin ${HumanDateBegin}
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card UserChoices DateEnd   ${HumanDateEnd}

  # [Restarts]
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card Restarts OverRule "y"
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card Restarts RestartDate ${HumanRestartDate}
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card Restarts RestartJobName ${initFrom}
  IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card Restarts RestartPath ${initPath}

    # [ATM/OCE/...]
  for comp in ${config_ListOfComponents[*]} ; do
    IGCM_card_WriteOption ${SUBMIT_DIR}/${StartDir}/${MemberDir}/config.card ${comp} Restart "n"
  done
  unset initFrom

  # ==> Job
  sed -e "s/\(#.*Script_Output_\)${config_UserChoices_JobName}\(\.*\)/\1${MemberDir}\2/" \
      -e "s/\(#.*\)${config_UserChoices_JobName}\(\.*\)/\1${MemberDir} \2/"            \
      -e "s/^PeriodNb=.*/PeriodNb=60/"                                                   \
      ${SUBMIT_DIR}/${StartDir}/${MemberDir}/Job_${MemberDir} > Job_${MemberDir}.tmp
  IGCM_sys_Mv Job_${MemberDir}.tmp ${SUBMIT_DIR}/${StartDir}/${MemberDir}/Job_${MemberDir}

  IGCM_debug_PopStack "IGCM_ensemble_DateFilesUpdate"
}

############### Parametric ENSEMBLE #################
