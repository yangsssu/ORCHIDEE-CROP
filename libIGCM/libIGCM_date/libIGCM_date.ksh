#!/bin/ksh

#**************************************************************
# Author: Sebastien Denvil
# Contact: Sebastien.Denvil__at__ipsl.jussieu.fr
# $Revision:: 1206                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2015-06-05 17:28:35 +0200 (Fri, 05 Jun 2015) $ Date of last commit
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
#D-libIGCM_date
#D-This ksh library handles date calculs and convertions in different calendars.
#D-  types of calendars are possible :
#D-
#D-  - leap|gregorian|standard (other name leap) :
#D-      The normal calendar. The time origin for the
#D-      julian day in this case is 24 Nov -4713.
#D-  - noleap|365_day :
#D-      A 365 day year without leap years.
#D-  - all_leap|366_day :
#D-      A 366 day year with only leap years.
#D-  - 360d|360_day :
#D-      Year of 360 days with month of equal length.

# Number of digit in the year
typeset -r dY=${dY:=4}
#typeset -r MaxpY=$( echo "10^"$((dY+1)) | bc -l )
# Number of digit in non-human date representation
typeset -r pY=$(( dY+4 ))

#==================================================================
function IGCM_date_YearDigit
{
  IGCM_debug_PushStack "IGCM_date_YearDigit" $@

  typeset NUM

  NUM=$(( 10#${1} ))
  echo $( gawk "BEGIN { printf \"%0${dY}d\",${NUM} }" )

  IGCM_debug_PopStack "IGCM_date_YearDigit"
}

#==================================================================
function IGCM_date_GregorianDigit
{
  IGCM_debug_PushStack "IGCM_date_GregorianDigit" $@

  typeset NUM

  NUM=$(( 10#${1} ))

  echo $( gawk "BEGIN { printf \"%0${pY}d\",${NUM} }" )

  IGCM_debug_PopStack "IGCM_date_GregorianDigit"
}

#==================================================================
function IGCM_date_HumanDigit
{
  IGCM_debug_PushStack "IGCM_date_HumanDigit" $@

  echo $( IGCM_date_GregorianDigit $( print ${1} | sed 's/-//g' ) ) \
    | sed -e "s/\([0-9]\{${dY}\}\)\([0-9]\{2\}\)\([0-9]\{2\}\)/\1-\2-\3/"

  IGCM_debug_PopStack "IGCM_date_HumanDigit"
}

#==================================================================
function IGCM_date_SupressZeros
{
  IGCM_debug_PushStack "IGCM_date_SupressZeros" $@
  echo $( print ${1} | sed -e "s/0*//" )
  IGCM_debug_PopStack "IGCM_date_SupressZeros"
}

#==================================================================
function IGCM_date_ConvertFormatToGregorian
{
  IGCM_debug_PushStack "IGCM_date_ConvertFormatToGregorian" $@

  # from a yyyy-mm-dd date format return
  # a yyymmdd date format
  # usage IGCM_date_ConvertFormat yyyy-mm-dd

  # if there is no argument on the command line,
  # then assume that a y-m-d formated date is being
  # piped in
  typeset ymd
  if [ $# = 0 ]
  then
    read ymd
  else
    ymd=$1
  fi

  IGCM_date_GregorianDigit $( print ${ymd} | sed 's/-//g' )

  IGCM_debug_PopStack "IGCM_date_ConvertFormatToGregorian"
}

#==================================================================
function IGCM_date_ConvertFormatToHuman
{
  IGCM_debug_PushStack "IGCM_date_ConvertFormatToHuman" $@

  # from a yyyymmdd date format return
  # a yyyy-mm-dd date format
  # usage IGCM_date_ConvertFormat yyyymmdd

  # if there is no argument on the command line,
  # then assume that a yyyymmdd formated date is being
  # piped in
  typeset dt
  if [ $# = 0 ]
  then
    read dt
  else
    dt=$1
  fi

  # break the yyyymmdd into separate parts for year, month and day
  echo $( IGCM_date_GregorianDigit ${dt} ) | sed -e "s/\([0-9]\{${dY}\}\)\([0-9]\{2\}\)\([0-9]\{2\}\)/\1-\2-\3/"

  IGCM_debug_PopStack "IGCM_date_ConvertFormatToHuman"
}

#==================================================================
function IGCM_date_GetYearMonth
{
  IGCM_debug_PushStack "IGCM_date_GetYearMonth" $@

  # from a yyyymmdd date format return
  # a yyyy year and mm month
  # usage IGCM_date_GetYearMonth yyyymmdd year_var month_var

  # if there is no argument on the command line,
  # then assume that a yyyymmdd formated date is being
  # piped in
  typeset dt
  if [ $# = 0 ]
  then
    read dt
  else
    dt=$1
  fi

  # break the yyyymmdd into separate parts for year, month and day
  eval $2=$( IGCM_date_GregorianDigit ${dt} | sed -e "s/\([0-9]\{${dY}\}\)\([0-9]\{2\}\)\([0-9]\{2\}\)/\1/" )
  eval $3=$( IGCM_date_GregorianDigit ${dt} | sed -e "s/\([0-9]\{${dY}\}\)\([0-9]\{2\}\)\([0-9]\{2\}\)/\2/" )

  IGCM_debug_PopStack "IGCM_date_GetYearMonth"
}

#==================================================================
function IGCM_date_GetYearMonthDay
{
  IGCM_debug_PushStack "IGCM_date_GetYearMonthDay" $@

  # from a yyyymmdd date format return
  # a yyyy year, mm month and dd day
  # usage IGCM_date_GetYearMonthDay yyyymmdd year_var month_var day_var

  # if there is no argument on the command line,
  # then assume that a yyyymmdd formated date is being
  # piped in
  typeset dt
  if [ $# = 0 ]
  then
    read dt
  else
    dt=$1
  fi

  # break the yyyymmdd into separate parts for year, month and day
  eval $2=$( IGCM_date_GregorianDigit ${dt} | sed -e "s/\([0-9]\{${dY}\}\)\([0-9]\{2\}\)\([0-9]\{2\}\)/\1/" )
  eval $3=$( IGCM_date_GregorianDigit ${dt} | sed -e "s/\([0-9]\{${dY}\}\)\([0-9]\{2\}\)\([0-9]\{2\}\)/\2/" )
  eval $4=$( IGCM_date_GregorianDigit ${dt} | sed -e "s/\([0-9]\{${dY}\}\)\([0-9]\{2\}\)\([0-9]\{2\}\)/\3/" )

  IGCM_debug_PopStack "IGCM_date_GetYearMonthDay"
}

#D-#==================================================================
#D-function IGCM_date_DaysInYear
#D-* Purpose: Return the number of days in a year
#D-* Usage: IGCM_date_DaysInYear yyyy
#D-         if there is no argument on the command line,
#D-         then assume that a yyyy is being piped in
#D-
function IGCM_date_DaysInYear
{
#    IGCM_debug_PushStack "IGCM_date_DaysInYear" $@
  # return the number of days in a year
  # usage IGCM_date_DaysInYear yyyy

  # What is the calendar :
  case ${config_UserChoices_CalendarType} in
  360d|360_day)
    if [ X$2 = X ] ; then
      echo 360
    else
      eval $2=360 > /dev/null 2>&1
    fi
#    IGCM_debug_PopStack "IGCM_date_DaysInYear"
    return;;
  noleap|365_day)
    if [ X$2 = X ] ; then
      echo 365
    else
      eval $2=365 > /dev/null 2>&1
    fi

#    IGCM_debug_PopStack "IGCM_date_DaysInYear"
    return;;
  all_leap|366_day)
    if [ X$2 = X ] ; then
      echo 366
    else
      eval $2=366 > /dev/null 2>&1
    fi

#    IGCM_debug_PopStack "IGCM_date_DaysInYear"
    return;;
  esac

  typeset y a

  # if there is no argument on the command line,
  # then assume that a yyyy is being piped in
  if [ $# = 0 ]
  then
    read y
  else
    y=$(( 10#${1} ))
  fi

  # a year is a leap year if it is even divisible by 4
  # but not evenly divisible by 100
  # unless it is evenly divisible by 400

  # if it is evenly divisible by 400 it must be a leap year
  a=$(( $y % 400 ))
  if [ $a = 0 ]
  then
    if [ X$2 = X ] ; then
      echo 366
    else
      eval $2=366 > /dev/null 2>&1
    fi

#    IGCM_debug_PopStack "IGCM_date_DaysInYear"
    return
  fi

  # if it is evenly divisible by 100 it must not be a leap year
  a=$(( $y % 100 ))
  if [ $a = 0 ]
  then
    if [ X$2 = X ] ; then
      echo 365
    else
      eval $2=365 > /dev/null 2>&1
    fi

#    IGCM_debug_PopStack "IGCM_date_DaysInYear"
    return
  fi

  # if it is evenly divisible by 4 it must be a leap year
  a=$(( $y % 4 ))
  if [ $a = 0 ]
  then
    if [ X$2 = X ] ; then
      echo 366
    else
      eval $2=366 > /dev/null 2>&1
    fi

#    IGCM_debug_PopStack "IGCM_date_DaysInYear"
    return
  fi

  # otherwise it is not a leap year
  if [ X$2 = X ] ; then
    echo 365
  else
    eval $2=365 > /dev/null 2>&1
  fi

#    IGCM_debug_PopStack "IGCM_date_DaysInYear"
}

#D-#==================================================================
#D-function IGCM_date_DaysInMonth
#D-* Purpose: Calculate the number of days in a month
#D-* Usage: IGCM_date_DaysInMonth yyyy mm
#D-         or IGCM_date_DaysInMonth yyyymmdd
#D-         if there are no command line arguments then
#D-         assume that a yyyymmdd is being piped in and read the value.
#D-         if there is only one argument assume it is a yyyymmdd on the command line
#D-         other wise it is a yyyy and mm on the command line
function IGCM_date_DaysInMonth
{
#    IGCM_debug_PushStack "IGCM_date_DaysInMonth" $@

  # calculates the number of days in a month
  # usage IGCM_date_DaysInMonth yyyy mm
  # or IGCM_date_DaysInMonth yyyymmdd

  # What is the calendar :
  if ( [ "${config_UserChoices_CalendarType}" = "360d" ] || [ "${config_UserChoices_CalendarType}" = "360_day" ] ) ; then
    if [ X$3 = X ] ; then
      echo 30
    else
      eval $3=30 > /dev/null 2>&1
    fi

#   IGCM_debug_PopStack "IGCM_date_DaysInMonth"
    return
  fi

  typeset ymd y m

  # if there are no command line arguments then assume that a yyyymmdd is being
  # piped in and read the value.
  # if there is only one argument assume it is a yyyymmdd on the command line
  # other wise it is a yyyy and mm on the command line
  if [ $# = 0 ]
  then
    read ymd
  elif [ $# = 1 ]
  then
    ymd=$1
  else
    ymd=$(( ( $1 * 10000 ) + ( $2 * 100 ) + 1 ))
  fi

  # extract the year and the month
  y=$(( $ymd / 10000 )) ;
  m=$(( ( $ymd % 10000 ) / 100 )) ;

  # 30 days hath september etc.
  case $m in
  1|3|5|7|8|10|12)
    if [ X$3 = X ] ; then
      echo 31
    else
      eval $3=31 > /dev/null 2>&1
    fi

#           IGCM_debug_PopStack "IGCM_date_DaysInMonth"
    return ;;
  4|6|9|11)
    if [ X$3 = X ] ; then
      echo 30
    else
      eval $3=30 > /dev/null 2>&1
    fi

#           IGCM_debug_PopStack "IGCM_date_DaysInMonth"
    return ;;
  *) ;;
  esac

  # except for month 2 which depends on whether the year is a leap year
  # Use IGCM_date_DaysInYear to get the number of days in the year and return a value
  # accordingly.

  IGCM_date_DaysInYear $y diy
  case $diy in
  365)
    if [ X$3 = X ] ; then
      echo 28
    else
      eval $3=28 > /dev/null 2>&1
    fi

#    IGCM_debug_PopStack "IGCM_date_DaysInMonth"
    return ;;
  366)
    if [ X$3 = X ] ; then
      echo 29
    else
      eval $3=29 > /dev/null 2>&1
    fi

#    IGCM_debug_PopStack "IGCM_date_DaysInMonth"
    return ;;
  esac

#    IGCM_debug_PopStack "IGCM_date_DaysInMonth"
}

#D-#==================================================================
#D-function IGCM_date_ConvertGregorianDateToJulian
#D-* Purpose: Convert yyyymmdd to yyyyddd
#D-* Usage: IGCM_date_ConvertGregorianDateToJulian 19980429
#D-         if there is no command line argument, then assume that the date
#D-         is coming in on a pipe and use read to collect it
#D-
function IGCM_date_ConvertGregorianDateToJulian
{
  IGCM_debug_PushStack "IGCM_date_ConvertGregorianDateToJulian" $@

  # IGCM_date_ConvertGregorianDateToJulian converts yyyymmdd to yyyyddd
  # usage IGCM_date_ConvertGregorianDateToJulian 19980429

  typeset dt y m d x jul

  # if there is no command line argument, then assume that the date
  # is coming in on a pipe and use read to collect it
  if [ $# = 0 ]
  then
    read dt
  else
    dt=$( IGCM_date_SupressZeros $1 )
  fi

  # break the yyyymmdd into separate parts for year, month and day
  y=$(( $dt / 10000 ))
  m=$(( ( $dt % 10000 ) / 100 ))
  d=$(( ( $dt % 100 ) ))

  # add the days in each month up to (but not including the month itself)
  # into the days. For example if the date is 19980203 then extract the
  # number of days in January and add it to 03. If the date is June 14, 1998
  # then extract the number of days in January, February, March, April and May
  # and add them to 14.
  x=1
  while [ $x -lt $m ]
  do
    IGCM_date_DaysInMonth $y $x md
    d=$(( $d + $md ))
    x=$(( $x + 1 ))
  done

  # combine the year and day back together again and you have the julian date.
  jul=$(( ( $y * 1000 ) + $d ))
  echo $jul

  IGCM_debug_PopStack "IGCM_date_ConvertGregorianDateToJulian"
}

#D-#==================================================================
#D-function IGCM_date_ConvertJulianDateToGregorian()
#D-* Purpose: Convert yyyyddd to yyyymmdd
#D-* Usage: IGCM_date_ConvertJulianDateToGregorian 1998213
#D-         if there is no command line argument, assume one is being
#D-         piped in and read it
#D-
function IGCM_date_ConvertJulianDateToGregorian
{
  IGCM_debug_PushStack "IGCM_date_ConvertJulianDateToGregorian" $@

  # IGCM_date_ConvertJulianDateToGregorian converts yyyyddd to yyyymmdd
  # usage IGCM_date_ConvertJulianDateToGregorian 1998213

  typeset dt y m d grg

  # if there is no command line argument, assume one is being
  # piped in and read it
  if [ X$1 = X ]
  then
    read dt
  else
    dt=$1
  fi

  # break apart the year and the days
  y=$(( $dt / 1000 ))
  d=$(( $dt % 1000 ))

  # subtract the number of days in each month starting from 1
  # from the days in the date. When the day goes below 1, you
  # have the current month. Add back the number of days in the
  # month to get the correct day of the month
  m=1
  while [ $d -gt 0 ]
  do
    IGCM_date_DaysInMonth $y $m md
    d=$(( $d - $md ))
    m=$(( $m + 1 ))
  done

  d=$(( $d + $md ))

  # the loop steps one past the correct month, so back up the month
  m=$(( $m - 1 ))

  # assemble the results into a gregorian date
  grg=$(( ( $y * 10000 ) + ( $m * 100 ) + $d ))
  echo $( IGCM_date_GregorianDigit $grg )

  IGCM_debug_PopStack "IGCM_date_ConvertJulianDateToGregorian"
}

#D-#==================================================================
#D-function IGCM_date_AddDaysToJulianDate
#D-* Purpose: Add days to a yyyyddd formatted date
#D-* Usage: IGCM_date_AddDaysToJulianDate 1998312 { ,-}14
#D-         Read the difference from the command lines
#D-         and the date from the command line, or standard input
#D-
function IGCM_date_AddDaysToJulianDate
{
  IGCM_debug_PushStack "IGCM_date_AddDaysToJulianDate" $@

  # IGCM_date_AddDaysToJulianDate adds days to a yyyyddd formatted date
  # usage IGCM_date_AddDaysToJulianDate 1998312 { ,-}14

  typeset dif yd d y

  # Read the difference from the command lines
  # and the date from the command line, or standard input
  if [ X$2 = X ]
  then
    dif=$1
    read yd
  else
    yd=$1
    dif=$2
  fi

  # Break it into pieces
  d=$(( $yd % 1000 ))
  y=$(( $yd / 1000 ))

  # Add the number of days (if days is negative this results is
  # a subtraction)
  d=$(( $d + $dif ))

  # Extract the days in the year
  IGCM_date_DaysInYear $y diy

  # If the calculated day exceeds the days in the year,
  # add one year to the year and subtract the days in the year from the
  # calculated days. Extract the days in the new year and repeat
  # test until you end up with a day number that falls within the
  # days of the year
  while [ $d -gt $diy ]
  do
    d=$(( $d - $diy ))
    y=$(( $y + 1 ))
    IGCM_date_DaysInYear $y diy
  done

  # This is the reverse process. If the calculated number of days
  # is less than 1, move back one year. Extract
  # the days in this year and add the days in the year
  # loop on this test until you end up with a number that
  # falls within the days of the year
  while [ $d -lt 1 ]
  do
    y=$(( $y - 1 ))
    IGCM_date_DaysInYear $y diy
    d=$(( $d + $diy ))
  done

  # put the year and day back together and echo the result
  yd=$(( ( $y * 1000 ) + $d ))

  echo $yd

  IGCM_debug_PopStack "IGCM_date_AddDaysToJulianDate"
}

#D-#==================================================================
#D-function IGCM_date_AddDaysToGregorianDate
#D-* Purpose: Add days to a yyyymmdd formatted date
#D-* Usage: IGCM_date_AddDaysToGregorianDate 19980312 { ,-}14
#D-         Read the difference from the command lines
#D-         and the date from the command line, or standard input
#D-
function IGCM_date_AddDaysToGregorianDate
{
  IGCM_debug_PushStack "IGCM_date_AddDaysToGregorianDate" $@

  # IGCM_date_AddDaysToGregorianDate adds days to a yyyymmdd formatted date
  # usage IGCM_date_AddDaysToGregorianDate 19980312 { ,-}14

  # Read the difference from the command lines
  # and the date from the command line, or standard input
  typeset dif yd tmp res
  if [ X$2 = X ]
  then
    dif=$1
    read yd
  else
    yd=$1
    dif=$2
  fi

  tmp=$( IGCM_date_ConvertGregorianDateToJulian $yd )
  tmp=$( IGCM_date_AddDaysToJulianDate $tmp $dif )
  res=$( IGCM_date_ConvertJulianDateToGregorian $tmp )

  echo $res

  IGCM_debug_PopStack "IGCM_date_AddDaysToGregorianDate"
}

#D-#==================================================================
#D-function IGCM_date_DaysBetweenJulianDate
#D-* Purpose: Calculate the days difference between two dates and reports
#D-           the number days as jul1 - jul2
#D-* Usage: IGCM_date_DaysBetweenJulianDate jul1 jul2
#D-         where julian date is in the form yyyyddd
#D-
function IGCM_date_DaysBetweenJulianDate
{
  IGCM_debug_PushStack "IGCM_date_DaysBetweenJulianDate" $@

  # calculates the days difference between two dates and reports
  # the number days as jul1 - jul2
  # usage IGCM_date_DaysBetweenJulianDate jul1 jul2
  # where julian date is in the form yyyyddd

  usage () {
    echo "Usage:"
    echo " IGCM_date_DaysBetweenJulianDate jul1 jul2"
    echo ""
    echo " Calculates the day difference between"
    echo " two julian dates (jul1 -jul2)"
    echo " where a julian date is in the form of yyyyddd."
  }

  if [ $# -lt 2 ]; then
    usage
    IGCM_debug_Exit "IGCM_date_DaysBetweenJulianDate"
  fi

  typeset jul1 jul2 yyyy1 yyyy2 ddd1 ddd2 res

  # This process subtracts arg2 from arg1. If arg2 is larger
  # then reverse the arguments. The calculations are done, and
  # then the sign is reversed
  if [ $1 -lt $2 ]
  then
    jul1=$2
    jul2=$1
  elif [ $1 -gt $2 ]
  then
    jul1=$1
    jul2=$2
  else
    echo 0
    IGCM_debug_PopStack "IGCM_date_DaysBetweenJulianDate"
    return
  fi

  # Break the dates in to year and day portions
  yyyy1=$(( $jul1 / 1000 ))
  yyyy2=$(( $jul2 / 1000 ))
  ddd1=$(( $jul1 % 1000 ))
  ddd2=$(( $jul2 % 1000 ))

  # Subtract days
  res=$(( $ddd1 - $ddd2 ))

  # Then add days in year until year2 matches year1
  case ${config_UserChoices_CalendarType} in
  360d|360_day)
    res=$(( ( ( $yyyy1 - $yyyy2 ) * 360 ) + $res )) ;;
  noleap|365_day)
    res=$(( ( ( $yyyy1 - $yyyy2 ) * 365 ) + $res )) ;;
  all_leap|366_day)
    res=$(( ( ( $yyyy1 - $yyyy2 ) * 366 ) + $res )) ;;
  leap|gregorian|standard)
    while [ $yyyy2 -lt $yyyy1 ]
    do
      IGCM_date_DaysInYear $yyyy2 diy
      res=$(( $res + $diy ))
      yyyy2=$(( $yyyy2 + 1 ))
    done
    ;;
  esac

  # if argument 2 was larger than argument 1 then
  # the arguments were reversed before calculating
  # adjust by reversing the sign
  if [ $1 -lt $2 ]
  then
    res=$(( $res * -1 ))
  fi

  # and output the results
  echo $res

  IGCM_debug_PopStack "IGCM_date_DaysBetweenJulianDate"
}

#D-#==================================================================
#D-function IGCM_date_DaysBetweenGregorianDate ()
#D-* Purpose: Calculate the days difference between two dates and reports
#D-           the number days as grg1 - grg2
#D-* Usage: IGCM_date_DaysBetweenGregorianDate grg1 grg2
#D-         where gregorian date is in the form yyyymmdd
#D-
function IGCM_date_DaysBetweenGregorianDate
{
  IGCM_debug_PushStack "IGCM_date_DaysBetweenGregorianDate" $@

  # calculates the days difference between two dates and reports
  # the number days as grg1 - grg2
  # usage IGCM_date_DaysBetweenGregorianDate grg1 grg2
  # where gregorian date is in the form yyyymmdd

  usage () {
    echo "Usage:"
    echo " IGCM_date_DaysBetweenGregorianDate grg1 grg2"
    echo ""
    echo " Calculate day difference between"
    echo " two gregorian dates (grg1 - grg2)"
    echo " where a gregorian date is in the form of yyyymmdd."
  }

  if [ $# -lt 2 ]; then
    usage
    IGCM_debug_Exit "IGCM_date_DaysBetweenGregorianDate"
  fi

  typeset grg1 grg2 jul1 jul2 res

  # convert each date to julian
  grg1=$1
  grg2=$2

  jul1=$( IGCM_date_ConvertGregorianDateToJulian $grg1 )
  jul2=$( IGCM_date_ConvertGregorianDateToJulian $grg2 )

  if [ $jul1 -ne $jul2 ]; then
    # calculate the answer using IGCM_date_DaysBetweenJulianDate
    res=$( IGCM_date_DaysBetweenJulianDate $jul1 $jul2 )
    # and output the results
    echo $res
  else
    echo 0
  fi

  IGCM_debug_PopStack "IGCM_date_DaysBetweenGregorianDate"
}

#D-#==================================================================
#D-function IGCM_date_DaysSinceJC ()
#D-* Purpose: Calculate the days difference between a date and 00010101
#D-* Usage: IGCM_date_DaysSinceJC grg1
#D-         where gregorian date is in the form yyyymmdd
#D-
function IGCM_date_DaysSinceJC
{
  IGCM_debug_PushStack "IGCM_date_DaysSinceJC" $@

  # calculates the days difference between a date and 00010101
  # usage IGCM_date_DaysSinceJC grg1
  # where gregorian date is in the form yyyymmdd

  usage () {
    echo "Usage:"
    echo " IGCM_date_DaysSinceJC grg1"
    echo ""
    echo " Calculate day difference between"
    echo " a gregorian date and 00010101"
    echo " where a gregorian date is in the form of yyyymmdd."
  }

  if [ $# -lt 1 ]; then
    usage
    IGCM_debug_Exit "IGCM_date_DaysSinceJC"
  fi

  typeset aux num

  if   [ ${1} -lt  5000000 ]; then
    case ${config_UserChoices_CalendarType} in
    360d|360_day)
      aux=-360;;
    noleap|365_day)
      aux=-365;;
    all_leap|366_day)
      aux=-366;;
    leap|gregorian|standard)
      aux=-366;;
    esac
    num=101
  elif [ ${1} -lt 15000000 ]; then
    # To save CPU type we use auxiliary value
    # which is number of days since JC and 10000101
    case ${config_UserChoices_CalendarType} in
    360d|360_day)
      aux=359640;;
    noleap|365_day)
      aux=364635;;
    all_leap|366_day)
      aux=365634;;
    leap|gregorian|standard)
      aux=364877;;
    esac
    num=10000101
  else
    # To save CPU type we use auxiliary value
    # which is number of days since JC and 19000101
    case ${config_UserChoices_CalendarType} in
    360d|360_day)
      aux=683640;;
    noleap|365_day)
      aux=693135;;
    all_leap|366_day)
      aux=695034;;
    leap|gregorian|standard)
      aux=693595;;
    esac
    num=19000101
  fi
  echo $(( $( IGCM_date_DaysBetweenGregorianDate $1 ${num} ) + $aux ))

  IGCM_debug_PopStack "IGCM_date_DaysSinceJC"
}

#D-#==================================================================
#D-function IGCM_date_DaysInPreviousPeriod ()
#D-* Purpose: Give the numbers of days during the previous prd1 period from grg1 date # OLD create_ts_begin_date
#D-* Usage: IGCM_date_DaysInPreviousPeriod grg1 prd1 [end]
#D-         where grg1 gregorian date is in the form yyyymmdd
#D-         where prd1 period is in the form N[Yy], N[Mm], N[Dd]. N integer
#D-         where [end] is an optionnal keyword to specify grg1 is the end of prd1
#D-
function IGCM_date_DaysInPreviousPeriod {
  IGCM_debug_PushStack "IGCM_date_DaysInPreviousPeriod" $@

  typeset Length Period treatedYear PeriodLengthInYears PeriodLengthInMonths year0 i

  Period=${2}
  case ${Period} in
  *Y|*y)
    PeriodLengthInYears=$( echo ${Period} | awk -F '[yY]' '{print $1}' )
    IGCM_date_GetYearMonth ${1} year month
    year=$( IGCM_date_SupressZeros ${year} )
    if [ X${3} = Xend ] ; then
      (( year = year - PeriodLengthInYears + 1))
    fi
    Length=0
    i=0
    until [ $i -ge $PeriodLengthInYears ] ; do
      (( Length = Length + $( IGCM_date_DaysInYear $(( year + i )) ) ))
      (( i = i + 1 ))
    done
    ;;
  *M|*m)
    PeriodLengthInMonths=$( echo ${Period} | awk -F '[mM]' '{print $1}' )
    IGCM_date_GetYearMonth ${1} year month
    year=$( IGCM_date_SupressZeros ${year} )
    if [ X${3} = Xend ] ; then
      (( month = month - PeriodLengthInMonths + 1 ))
    fi
    year0=year
    if [ $month -le 0 ] ; then
      (( month = month + 12 ))
      year=$( printf "%04i\n" $(( year - 1 )) )
    fi
    month=$( printf "%02i\n" ${month} )
    treatedYear=0
    Length=0
    i=0
    for ((i = 0; i < ${PeriodLengthInMonths}; i += 1)) ; do

      (( Length = Length + $( IGCM_date_DaysInMonth $year $(( month + i - 12 * treatedYear )) ) ))

      if [ $(( month + i )) -ge $(( 12 * (treatedYear + 1) )) ] ; then
        (( year = year0 + 1 ))
        (( treatedYear = treatedYear + 1 ))
      fi
    done
    ;;
  *D|*d)
    Length=$( echo ${Period} | sed -e "s/[dD]//" ) ;;
  *)
    IGCM_debug_Exit "IGCM_date_DaysInPreviousPeriod " ${Period} " invalid PeriodLength : choose in *Y, *M, *D."
    IGCM_debug_Verif_Exit ;;
  esac
  echo ${Length}

  IGCM_debug_PopStack "IGCM_date_DaysInPreviousPeriod"
}

#D-#==================================================================
#D-function IGCM_date_DaysInNextPeriod ()
#D-* Purpose: Give the numbers of days during the next prd1 period from grg1 date # OLD create_ts_next_date
#D-* Usage: IGCM_date_DaysInNextPeriod grg1 prd1
#D-         where grg1 gregorian date is in the form yyyymmdd
#D-         where prd1 period is in the form N[Yy], N[Mm], N[Dd]. N integer
#D-
function IGCM_date_DaysInNextPeriod {
  IGCM_debug_PushStack "IGCM_date_DaysInNextPeriod" $@

  typeset Length Period treatedYear PeriodLengthInYears PeriodLengthInMonths year0 month0 i

  Period=${2}
  case ${Period} in
  *Y|*y)
    PeriodLengthInYears=$( echo ${Period} | awk -F '[yY]' '{print $1}' )
    IGCM_date_GetYearMonth ${1} year month
    year=$( IGCM_date_SupressZeros ${year} )
    Length=0
    i=0
    until [ $i -ge $PeriodLengthInYears ] ; do
      (( Length = Length + $( IGCM_date_DaysInYear $(( year + i + 1 )) ) ))
      (( i = i + 1 ))
    done
    ;;
  *M|*m)
    PeriodLengthInMonths=$( echo ${Period} | awk -F '[mM]' '{print $1}' )
    IGCM_date_GetYearMonth ${1} year month
    year=$( IGCM_date_SupressZeros ${year} )
    year0=year
    if [ $(( month + 1 )) -lt 13 ] ; then
      month0=$(( month + 1 ))
    else
      month0=$(( month + 1 - 12 ))
      (( year = year0 + 1 ))
    fi
    treatedYear=0
    Length=0
    i=0
    for ((i = 0; i < ${PeriodLengthInMonths}; i += 1)) ; do

      (( Length = Length + $( IGCM_date_DaysInMonth $year $(( month0 + i - 12 * treatedYear )) ) ))

      if [ $(( month0 + i )) -ge $(( 12 * (treatedYear + 1) )) ] ; then
        (( year = year0 + 1 ))
        (( treatedYear = treatedYear + 1 ))
      fi
    done
    ;;
  *D|*d)
    Length=$( echo ${Period} | sed -e "s/[dD]//" ) ;;
  *)
    IGCM_debug_Exit "IGCM_date_DaysInNextPeriod " ${Period} " invalid PeriodLength : choose in *Y, *M, *D."
    IGCM_debug_Verif_Exit ;;
  esac
  echo ${Length}

  IGCM_debug_PopStack "IGCM_date_DaysInNextPeriod"
}

#D-#==================================================================
#D-function IGCM_date_DaysInCurrentPeriod ()
#D-* Purpose: Give the numbers of days during the Current prd1 period from grg1 date
#D-* Usage: IGCM_date_DaysInCurrentPeriod grg1 prd1
#D-         where grg1 gregorian date is in the form yyyymmdd
#D-         where prd1 period is in the form N[Yy], N[Mm], N[Dd]. N integer
#D-
function IGCM_date_DaysInCurrentPeriod {
  IGCM_debug_PushStack "IGCM_date_DaysInCurrentPeriod" $@

  typeset Length Period treatedYear PeriodLengthInYears PeriodLengthInMonths year0 i

  Period=${2}
  case ${Period} in
  *Y|*y)
    PeriodLengthInYears=$( echo ${Period} | awk -F '[yY]' '{print $1}' )
    IGCM_date_GetYearMonth ${1} year month
    year=$( IGCM_date_SupressZeros ${year} )
    month=$( IGCM_date_SupressZeros ${month} )
    Length=0
    # if starting after february and using leap calendar
    # we need to add the number of days of the next year (potentially a leap year)
    if [ ${month} -gt 2 ] ; then
      (( year = year + 1 ))
    fi
    i=0
    until [ $i -ge $PeriodLengthInYears ] ; do
      (( Length = Length + $( IGCM_date_DaysInYear $(( year + i )) ) ))
      (( i = i + 1 ))
    done
    ;;
  *M|*m)
    PeriodLengthInMonths=$( echo ${Period} | awk -F '[mM]' '{print $1}' )
    IGCM_date_GetYearMonth ${1} year month
    year=$( IGCM_date_SupressZeros ${year} )
    year0=year
    treatedYear=0
    Length=0
    i=0
    for ((i = 0; i < ${PeriodLengthInMonths}; i += 1)) ; do

      (( Length = Length + $( IGCM_date_DaysInMonth $year $(( month + i - 12 * treatedYear )) ) ))

      if [ $(( month + i )) -ge $(( 12 * (treatedYear + 1) )) ] ; then
        (( year = year0 + 1 ))
        (( treatedYear = treatedYear + 1 ))
      fi
    done
    ;;
  *D|*d)
    Length=$( echo ${Period} | sed -e "s/[dD]//" ) ;;
  *)
    IGCM_debug_Exit "IGCM_date_DaysInCurrentPeriod " ${Period} " invalid PeriodLength : choose in *Y, *M, *D."
    IGCM_debug_Verif_Exit ;;
  esac
  echo ${Length}

  IGCM_debug_PopStack "IGCM_date_DaysInCurrentPeriod"
}

#D-#==================================================================
#D-function IGCM_date_Check
#D- * Purpose: Check the present file by comparison with a reference file
function IGCM_date_Check
{
  #---------------------
  if [ ! -n "${libIGCM}" ] ; then
    echo "Check libIGCM_date ...........................................[ FAILED ]"
    echo "--Error--> libIGCM variable is not defined"
    IGCM_debug_Exit "IGCM_date_Check"
  fi

  #---------------------
  whence -v gawk > /dev/null 2>&1
  if [ ! $? -eq 0 ] ; then
    echo "Check libIGCM_date ...........................................[ FAILED ]"
    echo "--Error--> gawk command is not defined"
    IGCM_debug_Exit "IGCM_date_Check"
  fi

  #---------------------
  # No need to remove timestamps here
  diff ${libIGCM}/libIGCM_date/IGCM_date_Test${dY}.ref <(${libIGCM}/libIGCM_date/IGCM_date_Test.ksh) > /dev/null 2>&1
  status=$?

  if [ ${status} -eq 0 ] ; then
    echo "Check libIGCM_date ...............................................[ OK ]"
  else
    echo "Check libIGCM_date ...........................................[ FAILED ]"
    echo "--Error--> Execution of ${libIGCM}/libIGCM_date/IGCM_date_Test.ksh"
    echo "           has produced the file IGCM_date_Test.ref.failed"
    echo "           Please analyse differences with the reference file by typing:"
    echo "           diff IGCM_date_Test.ref.failed ${libIGCM}/libIGCM_date/IGCM_date_Test${dY}.ref"
    echo "           Report errors to the author: Sebastien.Denvil@ipsl.jussieu.fr"
    diff ${libIGCM}/libIGCM_date/IGCM_date_Test${dY}.ref <(${libIGCM}/libIGCM_date/IGCM_date_Test.ksh)
    IGCM_debug_Exit "IGCM_date_Check"
  fi
}
