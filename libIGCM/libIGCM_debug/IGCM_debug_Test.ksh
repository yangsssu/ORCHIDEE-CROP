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
# IGCM_debug_Test.ksh > IGCM_debug_Test.ref 2>&1
# The reference file produced is used by the function IGCM_debug_Check
# to verify that the libIGCM_debug produce the same results

#=================================================
libIGCM=`dirname $0`/..
typeset TaskType=checking
. ${libIGCM}/libIGCM_debug/libIGCM_debug.ksh

#=================================================
Verbosity=3

Option1="Hello Earth"
Option2="Hello Mars"

List="${R_ENTREE}/flux_iceberg . ${R_ENTREE}/Albedo.nc ."

#=================================================
# function IGCM_debug_Print

IGCM_debug_Print 1 ${Option1}
IGCM_debug_Print 1 ${Option2}

IGCM_debug_Print 2 "A comment at level 2"
IGCM_debug_Print 3 "A comment at level 3" "Another comment at level 3"
IGCM_debug_Print 1 "A comment at level 1"
IGCM_debug_Print 3 "Another comment at level 3"
IGCM_debug_Print 2 "Another comment at level 2"

IGCM_debug_Print 1 ${List}

#=================================================
# function IGCM_debug_PrintVariables

IGCM_debug_PrintVariables 1 Option
