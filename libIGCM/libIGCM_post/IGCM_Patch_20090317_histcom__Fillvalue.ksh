#!/bin/ksh

#**************************************************************
# Author: Martial Mancip
# Contact: Martial.Mancip__at__ipsl.jussieu.fr
# $Revision:: 1206                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2015-06-05 17:28:35 +0200 (Fri, 05 Jun 2015) $ Date of last commit
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# We change _Fillvalue to old missing_value    !!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function IGCM_Patch_20090317_histcom__Fillvalue {
    IGCM_debug_PushStack "IGCM_Patch_histcom__Fillvalue" $@
 
    typeset file

    file=t_$1
    IGCM_sys_Mv ${1} ${file}
    IGCM_sys_Chmod 644 ${file}

    #IGCM_sys_ncrename -O --hst -a _Fillvalue,missing_value ${file}

    miss=$( ncdump -h ${file} | grep _Fillvalue | head -1 | awk "-F=" '{print $2}' |  awk "-Ff" '{print $1}' )

    [ ! X${miss} = X ] && IGCM_sys_ncatted -a missing_value,,c,f,${miss} ${file} 

    # ------------------------------------------------------------------
    # Test if all was right before proceeding further
    # ------------------------------------------------------------------
    IGCM_debug_Verif_Exit

    IGCM_sys_Mv ${file} ${1}
    
    IGCM_debug_PopStack "IGCM_Patch_histcom__Fillvalue"
}
