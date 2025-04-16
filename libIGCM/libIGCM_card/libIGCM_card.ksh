#!/bin/ksh

#**************************************************************
# Author: Patrick Brockmann
# Contact: Patrick.Brockmann__at__cea.fr
# $Revision:: 1152                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2015-01-19 15:45:38 +0100 (Mon, 19 Jan 2015) $ Date of last commit
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************

#==================================================
# The documentation of this file can be automatically generated
# if you use the prefix #D- for comments to be extracted.
# Extract with command: cat lib* | grep "^#D-" | cut -c "4-"
#==================================================

#D-#==================================================================
#D-libIGCM_card
#D-This ksh library handles extraction of information from configuration file
#D-called "card" file (en français fichier "carte").
#D-All function described bellow must be prefixed by IGCM_card.
#D-A card file is organized as follows :
#D- ---------------------
#D-[Messages]
#D-Option1= "Hello Earth"
#D-Option2= "Hello Mars"
#D-
#D-# My comments
#D-[Recipes]
#D-Cake= "file1.doc"
#D-Starter= "file2.doc"
#D-
#D-[ColorValues]
#D-Red= 120
#D-Blue= 230
#D-Green= 178
#D-
#D-[Couples]
#D-List1=   (up, down), \
#D-         (humid, dry), \
#D-         (hot, cold), \
#D-         (far, close)
#D-List2=   (ice, fire, air, water)
#D- ---------------------
#D-

#D-#==================================================================
#D-function IGCM_card_PrintOption
#D-* Purpose: Print an option from a given file.card and section
#D-* Usage: IGCM_card_PrintOption file.card section option
#D-* Only used by IGCM_card_Test.ksh
#D-
function IGCM_card_PrintOption
{
  IGCM_debug_PushStack "IGCM_card_PrintOption" $@
  if ( [ -r "$1" ] && [ -f "$1" ] ) ; then
    gawk -f ${libIGCM}/libIGCM_card/IGCM_card_PrintOption.awk "$@"
  else
    echo
    IGCM_debug_Print 1 "--Error--> IGCM_card_PrintOption $@"
    IGCM_debug_Print 1 "           $1 is not readable"
    IGCM_debug_Exit "IGCM_card_PrintOption"
  fi
  IGCM_debug_PopStack "IGCM_card_PrintOption"
}

#D-#==================================================================
#D-function IGCM_card_PrintSection
#D-* Purpose: Print all options from a given file.card and section
#D-* Usage: IGCM_card_PrintSection file.card section
#D-* Only used by IGCM_card_Test.ksh
#D-
function IGCM_card_PrintSection
{
  IGCM_debug_PushStack "IGCM_card_PrintSection" $@
  if ( [ -r "$1" ] && [ -f "$1" ] ) ; then
    gawk -f ${libIGCM}/libIGCM_card/IGCM_card_PrintSection.awk -- "$@"
  else
    IGCM_debug_Print 1 "--Error--> IGCM_card_PrintSection $@"
    IGCM_debug_Print 1 "           $1 is not readable"
    IGCM_debug_Exit "IGCM_card_PrintSection"
  fi
  IGCM_debug_PopStack "IGCM_card_PrintSection"
}

#D-#==================================================================
#D-function IGCM_card_DefineVariableFromOption
#D-* Purpose: Define a variable from a given file.card, section and option
#D-*          Variable name is automatically defined as file_section_option
#D-* Usage: IGCM_card_DefineVariableFromOption file.card section option
#D-
function IGCM_card_DefineVariableFromOption
{
  IGCM_debug_PushStack "IGCM_card_DefineVariableFromOption" $@
  if ( [ -r "$1" ] && [ -f "$1" ] ) ; then
    # Get basename of card file ($1)
    typeset name1=${1##*/}
      # Build name of variable as $1_$2_$3 (cardname_Section_Option)
      typeset name=${name1%%.*}_${2}_${3}
      typeset value=$( gawk -f ${libIGCM}/libIGCM_card/IGCM_card_PrintOption.awk -- "$@" )

      # Only if a Section is missing we exit the job.
      # We must allow missing Option to keep backward compatibilty.
      if [ "${value}" = "Error: Section not found" ] ; then
        echo
        IGCM_debug_Print 1 "Error with readding of ${name} variable in ${1}."
        IGCM_debug_Print 1 "Error: Section ${2} not found"
        IGCM_debug_Exit
        IGCM_debug_Verif_Exit
      elif [ "${value}" = "Error: Option not found" ] ; then
         eval ${name}=${NULL_STR}
      else
        eval ${name}=${value}
      fi
  else
    echo
    IGCM_debug_Print 1 "--Error--> IGCM_card_DefineVariableFromOption"
    IGCM_debug_Print 1 "--Error--> $1 is not readable"
    IGCM_debug_Exit "IGCM_card_DefineVariableFromOption"
    IGCM_debug_Verif_Exit
  fi
  IGCM_debug_PopStack "IGCM_card_DefineVariableFromOption"
}

#D-#==================================================================
#D-function IGCM_card_DefineArrayFromOption
#D-* Purpose: Define an array variable from a given file.card, section and option
#D-*          Array variable is automatically defined as file_section_option
#D-* Usage: IGCM_card_DefineArrayFromOption file.card section option
#D-
function IGCM_card_DefineArrayFromOption
{
  IGCM_debug_PushStack "IGCM_card_DefineArrayFromOption" $@
  if ( [ -r "$1" ] && [ -f "$1" ] ) ; then
    # Get basename of card file ($1)
    typeset name1=${1##*/}
    # Build name of array as $1_$2_$3 (cardname_Section_Option)
    typeset name=${name1%%.*}_${2}_${3}
    eval unset ${name}
    eval ${name}[0]=${NULL_STR}
    set +A ${name} -- $( gawk -f ${libIGCM}/libIGCM_card/IGCM_card_PrintOption.awk -- "$@" | gawk -- 'BEGIN {FS="[() ,]+"} {for (i=2; i <= NF-1; i++) printf("%s ",$i)}' )
  else
    echo
    IGCM_debug_Print 1 "--Error--> IGCM_card_DefineArrayFromOption $@"
    IGCM_debug_Print 1 "           $1 is not readable"
    IGCM_debug_Exit "IGCM_card_DefineArrayFromOption"
  fi
  IGCM_debug_PopStack "IGCM_card_DefineArrayFromOption"
}

#D-#==================================================================
#D-function IGCM_card_DefineArrayFromSection
#D-* Purpose: Define an array variable from a given file.card and section
#D-*          Array variable is automatically defined as file_section
#D-* Usage: IGCM_card_DefineArrayFromSection file.card section
#D-
function IGCM_card_DefineArrayFromSection
{
  IGCM_debug_PushStack "IGCM_card_DefineArrayFromSection" $@
  if ( [ -r "$1" ] && [ -f "$1" ] ) ; then
    # Get basename of card file ($1)
    typeset name1=${1##*/}
    # Build name of array as $1_$2 (cardname_Section)
    typeset name=${name1%%.*}_${2}
    eval unset ${name}
    eval ${name}[0]=${NULL_STR}
    set +A ${name} -- $( gawk -f ${libIGCM}/libIGCM_card/IGCM_card_PrintSection.awk -- "$@" )
    #if [ "$( eval echo \${${name}[@]} )" = "Error: Section not found" ] ; then
    #    echo
    #    IGCM_debug_Print 1 "Error with readding of ${name} variable in ${1}."
    #    IGCM_debug_Print 1 "Error: Section ${2} not found"
    #    IGCM_debug_Exit
    #    IGCM_debug_Verif_Exit
    #fi
    if [ "$( eval echo \${${name}[@]} )" = "Error: Section not found" ] ; then
      echo
      IGCM_debug_Print 1 "Warning with readding of ${name} variable in ${1}."
      IGCM_debug_Print 1 "Warning: Section ${2} not found"
      eval unset ${name}
    fi
  else
    IGCM_debug_Print 1 "--Error--> IGCM_card_DefineArrayFromSection $@"
    IGCM_debug_Print 1 "           $1 is not readable"
    IGCM_debug_Exit "IGCM_card_DefineArrayFromSection"
  fi
  IGCM_debug_PopStack "IGCM_card_DefineArrayFromSection"
}

#D-#==================================================================
#D-function IGCM_card_WriteOption
#D-* Purpose: Write an option in a given file.card and section
#D-* Usage: IGCM_card_WriteOption file.card section newvalue
#D-* Examples: IGCM_card_WriteOption file.card Recipes Red 150
#D-            IGCM_card_WriteOption file.card Messages Option2 '"Hello Mercure"'
#D-            IGCM_card_WriteOption file.card Messages ListVal1 '( 1, 2, 3 )'
#D-            listname="(Sebastien, Martial, Patrick)"
#D-            IGCM_card_WriteOption NewTestFile.card Messages ListVal2 "${listname}"
#D-
function IGCM_card_WriteOption
{
  IGCM_debug_PushStack "IGCM_card_WriteOption" $@
  if ( [ -r "$1" ] && [ -w "$1" ]  && [ -f "$1" ] ) ; then
    if [ $( IGCM_card_PrintOption "$1" "$2" "$3" | grep "not found" | wc -l ) -gt 0 ] ; then
      IGCM_debug_Print 1 "!!! Issue with IGCM_card_WriteOption !!!"
      IGCM_debug_Print 1 "We tried to write : $@"
      IGCM_debug_Exit "Must check that option $3 in section $2 exist in this file $1"
      IGCM_debug_Verif_Exit
    fi

    # The tmpfile uses now the real path of the card to be modified,
    # not just a local tmpfile with PID.
    tmpfile=$1_mutex_$$

    IGCM_card_CheckConflict $1

    # Do the job
    ( gawk -f ${libIGCM}/libIGCM_card/IGCM_card_WriteOption.awk -- "$@" 2> /dev/null ) > ${tmpfile}

    cp $1 $1.bak
    mv ${tmpfile} $1

  else
    echo
    IGCM_debug_Print 1 "--Error--> IGCM_card_WriteOption $@"
    IGCM_debug_Print 1 "           $1 is not readable or not writable"
    IGCM_debug_Exit "IGCM_card_WriteOption"
  fi
  IGCM_debug_PopStack "IGCM_card_WriteOption"
}

#D-#==================================================================
#D-function IGCM_card_CheckConflict
#D-* Purpose: Check that a card is not in use by another process. If it is the case wait until it is not.
#D-* Usage: IGCM_card_CheckConflict run.card
#D-* Examples:
#D-
function IGCM_card_CheckConflict
{
  IGCM_debug_PushStack "IGCM_card_CheckConflict" $@
  typeset isleep tmpfiles

  # Watch for possible conflics : Check for other tmpfiles.
  set +A tmpfiles -- $( ls $1_mutex_[0-9]* 2>/dev/null )
  ((isleep=0))
  while [ ${#tmpfiles[@]} -gt 0 ] ; do
    echo "Conflict between two processes working on " $1 "!!!" ${tmpfiles[@]}
    sleep 1
    ((isleep=isleep+1))
    if [ ${isleep} -gt 20 ] ; then
      echo "Too many loops waiting for other process working on " $1 ". We continue."
      echo "You should see if one process of your run or post-treatment may have terminated suddenly."
      echo "Afer, you should erase this(those) file(s) : " ${tmpfiles[@]}
      # Send a mail to USER ??
      break ;
    fi
    unset tmpfiles
    set +A tmpfiles -- $( ls $1_mutex_[0-9]* 2>/dev/null )
  done

  IGCM_debug_PopStack "IGCM_card_CheckConflict"
}

#D-#==================================================================
#D-function IGCM_card_WriteArrayOption
#D-* Purpose: Write an array option a given file.card and section
#D-* Usage: IGCM_card_WriteArrayOption file.card section option newarray
#D-* Examples: set -A MyArray -- 1 2 3
#D-            IGCM_card_WriteArrayOption file.card Recipes List MyArray
#D-
function IGCM_card_WriteArrayOption
{
  IGCM_debug_PushStack "IGCM_card_WriteArrayOption" $@

  if ( [ -r "$1" ] && [ -w "$1" ]  && [ -f "$1" ] ) ; then
    if [ X"${4}" != X"" ]; then
      tab=$4
      IGCM_card_WriteOption $1 $2 $3 '('$( eval echo \${${tab}[@]} | sed -e 's/ /,/g' )')'
    else
      IGCM_card_WriteOption $1 $2 $3 '()'
    fi
  else
    echo
    IGCM_debug_Print 1 "--Error--> IGCM_card_WriteArrayOption $@"
    IGCM_debug_Print 1 "           $1 is not readable or not writable"
    IGCM_debug_Exit "IGCM_card_WriteArrayOption"
  fi
  IGCM_debug_PopStack "IGCM_card_WriteArrayOption"
}

#D-#==================================================================
#D-function IGCM_card_Check
#D-* Purpose: Check the present file by comparison with a reference file
#D-* Usage: IGCM_card_Check
#D-
function IGCM_card_Check
{
  #---------------------
  if [ ! -n "${libIGCM}" ] ; then
    echo "Check libIGCM_card ...........................................[ FAILED ]"
    echo "--Error--> libIGCM variable is not defined"
    IGCM_debug_Exit "IGCM_card_Check"
  fi

  #---------------------
  whence -v gawk > /dev/null 2>&1
  if [ ! $? -eq 0 ] ; then
    echo "Check libIGCM_card ...........................................[ FAILED ]"
    echo "--Error--> gawk command is not defined"
    IGCM_debug_Exit "IGCM_card_Check"
  fi

  #---------------------
  # No need to remove timestamps here
  diff ${libIGCM}/libIGCM_card/IGCM_card_Test.ref <(${libIGCM}/libIGCM_card/IGCM_card_Test.ksh) > /dev/null 2>&1
  status=$?

  if [ ${status} -eq 0 ] ; then
    echo "Check libIGCM_card ...............................................[ OK ]"
  else
    echo "Check libIGCM_card ...........................................[ FAILED ]"
    echo "--Error--> Execution of ${libIGCM}/libIGCM_card/IGCM_card_Test.ksh"
    echo "           has produced the file IGCM_card_Test.ref.failed"
    echo "           Please analyse differences with the reference file by typing:"
    echo "           diff IGCM_card_Test.ref.failed ${libIGCM}/libIGCM_card/IGCM_card_Test.ref"
    echo "           Report errors to the author: Patrick.Brockmann@cea.fr"
    diff ${libIGCM}/libIGCM_card/IGCM_card_Test.ref <(${libIGCM}/libIGCM_card/IGCM_card_Test.ksh)
    IGCM_debug_Exit "IGCM_card_Check"
  fi
}
