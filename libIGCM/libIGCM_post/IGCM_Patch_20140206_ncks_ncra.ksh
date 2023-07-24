#!/bin/ksh

#**************************************************************
# Author: Marie-Alice Foujols
# Contact: Marie-Alice.Foujols__at__ipsl.jussieu.fr
# $Revision:: 1107                                     $ Revision of last commit
# $Author:: aclsce                                     $ Author of last commit
# $Date:: 2014-10-22 16:53:47 +0200 (Wed, 22 Oct 2014) $ Date of last commit
# IPSL (2009)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************

#------------------------------------------------------------
# ncks CPL files for 1D frequency, ncra for 1M averaged files
#------------------------------------------------------------

function IGCM_Patch_20140206_ncks_ncra {
    IGCM_debug_PushStack "IGCM_Patch_ncks_ncra" $@

    CPL_OkDay=y
    CPL_OkMonth=y

    PREFIX=TEMPO
    if [ -f FIRST_TIME_CPL ] ; then
       echo FIRST_TIME_CPL exists
    else
       # create FIRST_TIME_CPL file
       date > FIRST_TIME_CPL

       rm -f ${PREFIX}_*_cpl_*.nc 

       list_file_atm=$( find . \( -name "S*lmdz.x*.nc" -o -name "C*lmdz.x*.nc" -o -name "S*LMDZ*.nc" -o -name "C*LMDZ*.nc" \) )

       for file in ${list_file_atm} 
       do
           [[ ${CPL_OkDay}   = "y" ]] && IGCM_sys_ncks -A ${file} ${PREFIX}_1D_cpl_atm.nc  > /dev/null 2>&1
           [[ ${CPL_OkMonth} = "y" ]] && IGCM_sys_ncra -A ${file} ${PREFIX}_1M_cpl_atm.nc  > /dev/null 2>&1
       done
       rm -f S*lmdz.x*.nc C*lmdz.x*.nc S*LMDZ*.nc C*LMDZ*.nc

       for file in O*oceanx*.nc
       do
           [[ ${CPL_OkDay}   = "y" ]] && IGCM_sys_ncks -A ${file} ${PREFIX}_1D_cpl_oce.nc   > /dev/null 2>&1
           [[ ${CPL_OkMonth} = "y" ]] && IGCM_sys_ncra -A ${file} ${PREFIX}_1M_cpl_oce.nc   > /dev/null 2>&1
       done
       rm -f O*oceanx*.nc

     fi
    
    IGCM_debug_PopStack "IGCM_Patch_ncks_ncra"
}
