#!/bin/ksh

#**************************************************************
# Author: Josefine Ghattas
# Contact: Josefine.Ghattas__at__ipsl.jussieu.fr
# $Revision:: 1059                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2014-09-23 12:05:29 +0200 (Tue, 23 Sep 2014) $ Date of last commit
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************
#
# Interpolation of monthly input ERAI files to the actual model grid.
#
# The patch should be applied once for each variable to interpolate. The file start.nc must be present in lmdz.car OutputFiles.
# In lmdz.card all monthly ERAI files for the actual year to be interpolated must be present at format var_month_year.nc, where var=[u,v], 
# month=[1,13]. The file var_13_year.nc correspond to january the following year.
# Partial year can be interpolated.
# 
#
function IGCM_Patch_20101208_nudge_interp {
    IGCM_debug_PushStack "IGCM_Patch_nudge_interp" $@

###################################################################################################
# 1 - Define variables
# Filein is the first month to interpolate for a variable.
# Filein name should be at format X_Y_ZZZZ.nc, where X=varaiable name [u,v,ta], Y=[1,13] and ZZZZ=year.
# To do one year complete for one variable X, 13 month must be present.
###################################################################################################
    filein=${1}

# Define startfile containg the destination grid. This file must exist in the OutputFiles list in lmdz.card. 
    startfile=start.nc

# Determine which variable and year concerned by this patch
    var=$( echo ${filein} | awk '-F_' '{print $1}' )
    year=$( echo ${filein} | awk '-F_' '{print $3}' | awk '-F.nc' '{print $1}' )
    echo var=$var year=$year

###################################################################################################
# 2 - Interpolate all files for the actual varaiable.
#     Original files will be overritten.
###################################################################################################
    filelist=${var}_*.nc
    echo 'filelist 1 = ' ${filelist}
    for file in ${filelist}; do
	month=$( echo ${file} | awk '-F_' '{print $2}' | awk '-F_' '{print $1}' )
	echo 'era2gcm for file = ' ${file} 
	IGCM_era2gcm ${file} tmp_${file} ${var} ${year}
	IGCM_sys_Mv tmp_${file} ${file}
    done

###################################################################################################
# 3 - Add a final time step in each file, coming from the first time step in following month
###################################################################################################
    filelist=${var}_*.nc
    echo 'filelist 2 = ' ${filelist}
    for file in ${filelist}; do
	month=$( echo ${file} | awk '-F_' '{print $2}' | awk '-F_' '{print $1}' )
	month_next=`expr $month + 1`
	file_next=${var}_${month_next}_${year}.nc
	if [ -f ${file_next} ] ; then
	    echo First time step from file ${file_next} will be added last in file ${file}
	    ncks -O -d THOUR,0,0 ${file_next} tmp.nc
	    ncrcat -O $file tmp.nc tmptot.nc
	    IGCM_sys_Mv tmptot.nc $file
	else
	    echo Next file ${file_next} does not exist. Nothing will be done for ${file}
	fi
    done

###################################################################################################
# 4 - Remove 13'th month
###################################################################################################
    if [ -f ${var}_13_${year}.nc ] ; then
	echo Remove file ${var}_13_${year}.nc
	IGCM_sys_Rm ${var}_13_${year}.nc
    fi

    IGCM_debug_PopStack "IGCM_Patch_nudge_interp"
}


function IGCM_era2gcm {
#set -xv
# Function that does a linear interpolation of the variable in 
# filein towards the destionation grid in gridfile. Results in are 
# stored in fileout.
    IGCM_debug_PushStack "IGCM_era2gcm" $@

###################################################################
# 1 - Define variables
###################################################################
    filein=$1
    fileout=$2
    varin=$3
    year=$4

# Define file with destination grid
    gridfile=start.nc
    
# Find time boundaries
    tmin=1
    tmax=`ncdump -h ${filein} | grep time | head -1 | awk ' { print $6 } '`
    tmax=$( echo ${tmax} | awk '-F(' '{print $2}' )
    
# Choose grid and output variable name 
    if [ ${varin} = 'u' ] ; then 
	varout=uwnd
	grille='grille_u'
    elif [ ${varin} = 'v' ] ; then 
	varout=vwnd
	grille='grille_v'
    elif [ ${varin} = 'ta' ] ; then 
	varout=air
	grille='grille_t'
    elif [ ${varin} = 'r' ] ; then 
	varout=rh
	grille='grille_t'
    elif [ ${varin} = 'msl' ] ; then 
	varout=sp
	grille='grille_t'
    else 
	echo Error : ${varin} unknown!!!!
	exit
    fi 

###################################################################
# 2 - Create ferret script for interpolation
###################################################################
    cat << eod >  ${varin}.jnl
set memory/size=50

use ${gridfile}
use ${filein}
let ${varout}=${varin}

define axis/t=01-JAN-${year}:31-DEC-${year}:6/units=hours thour

define grid/like=${varout}[d=2]/x=cu[d=1]/y=cu[d=1] grille_u
define grid/like=${varout}[d=2]/x=cv[d=1]/y=cv[d=1] grille_v
define grid/like=${varout}[d=2]/x=cv[d=1]/y=cu[d=1] grille_t

save/clobber/file=${fileout} ${varout}[d=2,g=${grille},l=1,gt=thour@asn]
repeat/l=1:${tmax} save/file="${fileout}"/append ${varout}[d=2,g=${grille},gt=thour@asn]

exit
eod
    
###################################################################
# 3 - Launch interpolation with ferret
###################################################################
    ferret -nojnl <<eod >  /dev/null
go ${varin}.jnl
quit
eod

    IGCM_debug_PopStack "IGCM_era2gcm"    
}
