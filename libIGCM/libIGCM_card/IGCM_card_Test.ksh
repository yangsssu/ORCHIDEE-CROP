#!/bin/ksh

#**************************************************************
# Author: Patrick Brockmann
# Contact: Patrick.Brockmann__at__cea.fr
# $Revision:: 1119                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2014-10-24 12:41:17 +0200 (Fri, 24 Oct 2014) $ Date of last commit
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************

# This script is used to generate a reference file with command :
# IGCM_card_Test.ksh > IGCM_card_Test.ref 2>&1
# The reference file produced is used by the function IGCM_card_Check
# to verify that the libIGCM_card produce the same results

#=================================================
libIGCM=`dirname $0`/..
typeset DEBUG_sys=false
typeset TaskType=checking
. ${libIGCM}/libIGCM_debug/libIGCM_debug.ksh
. ${libIGCM}/libIGCM_sys/libIGCM_sys.ksh
. ${libIGCM}/libIGCM_card/libIGCM_card.ksh

#=================================================
cat ${libIGCM}/libIGCM_card/TestFile.card

#=================================================
# function IGCM_card_PrintOption

IGCM_card_PrintOption ${libIGCM}/libIGCM_card/TestFile.card Messages Option1
IGCM_card_PrintOption ${libIGCM}/libIGCM_card/TestFile.card Messages Option2
IGCM_card_PrintOption ${libIGCM}/libIGCM_card/TestFile.card ColorValues Red
IGCM_card_PrintOption ${libIGCM}/libIGCM_card/TestFile.card ColorValues Blue
IGCM_card_PrintOption ${libIGCM}/libIGCM_card/TestFile.card ColorValues Green

#=================================================
# function IGCM_card_PrintSection

IGCM_card_PrintSection ${libIGCM}/libIGCM_card/TestFile.card Recipes
IGCM_card_PrintSection ${libIGCM}/libIGCM_card/TestFile.card Couples

#==================================================
# function IGCM_card_DefineVariableFromOption

IGCM_card_DefineVariableFromOption ${libIGCM}/libIGCM_card/TestFile.card Messages Option2
echo ${TestFile_Messages_Option2}
IGCM_card_DefineVariableFromOption ${libIGCM}/libIGCM_card/TestFile.card ColorValues Red
echo ${TestFile_ColorValues_Red}

#==================================================
# function IGCM_card_DefineArrayFromOption

IGCM_card_DefineArrayFromOption ${libIGCM}/libIGCM_card/TestFile.card Couples List1
echo ${TestFile_Couples_List1[*]}
IGCM_card_DefineArrayFromOption ${libIGCM}/libIGCM_card/TestFile.card Couples List2
echo ${TestFile_Couples_List2[*]}

#=================================================
# function IGCM_card_DefineArrayFromSection

IGCM_card_DefineArrayFromSection ${libIGCM}/libIGCM_card/TestFile.card ColorValues
echo ${TestFile_ColorValues[*]}

#==================================================
# function IGCM_card_WriteOption

cp ${libIGCM}/libIGCM_card/TestFile.card  ${libIGCM}/libIGCM_card/NewTestFile.$$.card
IGCM_card_WriteOption ${libIGCM}/libIGCM_card/NewTestFile.$$.card Messages Option2 '"Hello Mercure"'

IGCM_card_WriteOption ${libIGCM}/libIGCM_card/NewTestFile.$$.card Messages ListVal1 '( 1, 2, 3 )'
listname="(Sebastien, Martial, Patrick)"
IGCM_card_WriteOption ${libIGCM}/libIGCM_card/NewTestFile.$$.card Messages ListVal2 "${listname}"

set -A tableau one, two, three, four
echo ${tableau[*]}
IGCM_card_WriteOption ${libIGCM}/libIGCM_card/NewTestFile.$$.card Messages ListVal3 "( ${tableau[*]} )"

IGCM_card_WriteOption ${libIGCM}/libIGCM_card/NewTestFile.$$.card ColorValues Red 888

diff ${libIGCM}/libIGCM_card/TestFile.card ${libIGCM}/libIGCM_card/NewTestFile.$$.card
rm -f ${libIGCM}/libIGCM_card/NewTestFile.$$.card ${libIGCM}/libIGCM_card/NewTestFile.$$.card.bak
