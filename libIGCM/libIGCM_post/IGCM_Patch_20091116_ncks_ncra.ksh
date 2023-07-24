#!/bin/ksh

#**************************************************************
# Author: Marie-Alice Foujols
# Contact: Marie-Alice.Foujols__at__ipsl.jussieu.fr
# $Revision:: 1206                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2015-06-05 17:28:35 +0200 (Fri, 05 Jun 2015) $ Date of last commit
# IPSL (2009)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************

#------------------------------------------------------------
# ncks CPL files for 1D frequency, ncra for 1M averaged files
#------------------------------------------------------------

function IGCM_Patch_20091116_ncks_ncra {
    IGCM_debug_PushStack "IGCM_Patch_ncks_ncra" $@

    CPL_OkDay=y
    CPL_OkMonth=y

    PREFIX=TEMPO
    if [ -f FIRST_TIME_CPL ] ; then
       echo FIRST_TIME_CPL exists
    else
       # create FIRST_TIME_CPL file
       date > FIRST_TIME_CPL

       #ls -l S*_out.*.nc C*_out.*.nc

       rm -f ${PREFIX}_*_cpl_*.nc 

       for file in S*_out.*.nc C*_out.*.nc
       do
           [[ ${CPL_OkDay}   = "y" ]] && IGCM_sys_ncks -A ${file} ${PREFIX}_1D_cpl_atm.nc  > /dev/null 2>&1
           [[ ${CPL_OkMonth} = "y" ]] && IGCM_sys_ncra -A ${file} ${PREFIX}_1M_cpl_atm.nc  > /dev/null 2>&1
       done
       rm -f S*_out.*.nc C*_out.*.nc

       #ls -l O*_out.*.nc

       for file in O*_out.*.nc
       do
           [[ ${CPL_OkDay}   = "y" ]] && IGCM_sys_ncks -A ${file} ${PREFIX}_1D_cpl_oce.nc   > /dev/null 2>&1
           [[ ${CPL_OkMonth} = "y" ]] && IGCM_sys_ncra -A ${file} ${PREFIX}_1M_cpl_oce.nc   > /dev/null 2>&1
       done
       rm -f O*_out.*.nc

       # ------------------------------------------------------------------
       # Test if all was right before proceeding further
       # ------------------------------------------------------------------
       IGCM_debug_Verif_Exit

       #ls -s ${PREFIX}_*_cpl_*.nc 

     fi
    
    IGCM_debug_PopStack "IGCM_Patch_ncks_ncra"
}
