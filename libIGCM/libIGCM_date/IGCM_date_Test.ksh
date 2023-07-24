#!/bin/ksh

#**************************************************************
# Author: Sebastien Denvil
# Contact: Sebastien.Denvil__at__ipsl.jussieu.fr
# $Revision:: 1119                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2014-10-24 12:41:17 +0200 (Fri, 24 Oct 2014) $ Date of last commit
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************

# This script is used to generate a reference file with command :
# IGCM_date_Test.ksh > IGCM_date_Test.ref 2>&1
# The reference file produced is used by the function IGCM_date_Check
# to verify that the libIGCM_date produce the same results

#=================================================
libIGCM=`dirname $0`/..
typeset TaskType=checking
. ${libIGCM}/libIGCM_debug/libIGCM_debug.ksh
. ${libIGCM}/libIGCM_date/libIGCM_date.ksh

if [ X${config_UserChoices_CalendarType} = X ] ; then
    config_UserChoices_CalendarType="leap"
fi

echo "#=================================#"
echo "IGCM_date_DaysInYear test"
echo "#=================================#"

echo ""
echo "#"
echo "IGCM_date_DaysInYears 1998"
echo "#"
IGCM_date_DaysInYear 1998

echo ""
echo "#"
echo "IGCM_date_DaysInYears 2000"
echo "#"
IGCM_date_DaysInYear 2000

echo "#=================================#"
echo "IGCM_date_DaysInMonth test"
echo "#=================================#"

echo ""
echo "#"
echo "IGCM_date_DaysInMonth 1998 02"
echo "#"
IGCM_date_DaysInMonth 1998 02

echo ""
echo "#"
echo "IGCM_date_DaysInMonth 20000206"
echo "#"
IGCM_date_DaysInMonth 20000206

echo "#==========================================#"
echo "IGCM_date_ConvertGregorianDateToJulian test"
echo "#==========================================#"

echo ""
echo "#"
echo "IGCM_date_ConvertGregorianDateToJulian 19980312"
echo "#"
IGCM_date_ConvertGregorianDateToJulian 19980312

echo ""
echo "#"
echo "IGCM_date_ConvertGregorianDateToJulian 19980830"
echo "#"
IGCM_date_ConvertGregorianDateToJulian 19980830

echo "#==========================================#"
echo "IGCM_date_ConvertJulianDateToGregorian test"
echo "#==========================================#"

echo ""
echo "#"
echo "IGCM_date_ConvertJulianDateToGregorian 1998326"
echo "#"
IGCM_date_ConvertJulianDateToGregorian 1998326

echo ""
echo "#"
echo "IGCM_date_ConvertJulianDateToGregorian 2000298"
echo "#"
IGCM_date_ConvertJulianDateToGregorian 2000298

echo "#==================================#"
echo "IGCM_date_AddDaysToJulianDate test"
echo "#==================================#"

echo ""
echo "#"
echo "IGCM_date_AddDaysToJulianDate 1998312 14"
echo "#"
IGCM_date_AddDaysToJulianDate 1998312 14

echo ""
echo "#"
echo "IGCM_date_AddDaysToJulianDate 2000312 -14"
echo "#"
IGCM_date_AddDaysToJulianDate 2000312 -14

echo "#====================================#"
echo "IGCM_date_AddDaysToGregorianDate test"
echo "#====================================#"

echo ""
echo "#"
echo "IGCM_date_AddDaysToGregorianDate 19980312 44"
echo "#"
IGCM_date_AddDaysToGregorianDate 19980312 44

echo ""
echo "#"
echo "IGCM_date_AddDaysToGregorianDate 20000228 -44"
echo "#"
IGCM_date_AddDaysToGregorianDate 20000228 -44

echo "#====================================#"
echo "IGCM_date_DaysBetweenJulianDate test"
echo "#====================================#"

echo ""
echo "#"
echo "IGCM_date_DaysBetweenJulianDate 1860001 1865365"
echo "#"
IGCM_date_DaysBetweenJulianDate 1860001 1865365

echo "#======================================#"
echo "IGCM_date_DaysBetweenGregorianDate test"
echo "#======================================#"

echo ""
echo "#"
echo "IGCM_date_DaysBetweenGregorianDate 18801231 18750101"
echo "#"
IGCM_date_DaysBetweenGregorianDate 18801231 18750101
