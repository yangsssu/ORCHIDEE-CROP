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
# We keep only first time axis as time_counter !!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function IGCM_Patch_20070220_histcom_time_axis {
    IGCM_debug_PushStack "IGCM_Patch_histcom_time_axis" $@

    typeset file AXISt AXISt_i VARName var ATTN ATTNV VAR_AXISt VAR_AXISt_i

    ncdump -h ${1} > ${1}_ncdump-h
    set +A AXISt $(cat ${1}_ncdump-h | grep 't_ave_.*(time_counter)' | sed -e 's/[[:space:]]float t_ave_\([0-9]*\).*/\1/')
    # liste => tous les axes de temps vont s'appeler time_counter !!
    if [ $? -eq 0 ] ; then

	file=t_$1
	IGCM_sys_Mv ${1} ${file}
	IGCM_sys_Chmod 644 ${file}

	if [ ${#AXISt[*]} -gt 1 ] ; then
	    echo "WARNING : for file ${1} we have more than one time axis !"
	    echo "We choose first one ! = t_ave_${AXISt[0]}."
	fi
        #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        # We keep only first time axis as time_counter !!!!
        #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	AXISt_i=${AXISt[0]}
	# Rename time_counter variable
	IGCM_sys_ncrename -O --hst -v t_ave_${AXISt_i},time_counter ${file}
	ncdump -h ${file} > ${file}_ncdump-h

	# We have also to rename variables indexed by this time value
        # liste => pb avec deux variables indéxées par deux valeurs de temps différentes => même nom au final!
	set +A VAR_AXISt $( cat ${file}_ncdump-h | grep "[[:space:]]*[a-z]* [a-zA-Z_]*${AXISt_i}(.*) ;" | sed -e "s/[[:space:]]*[a-z]* \([a-zA-Z_]*\)_${AXISt_i}(.*) ;/\1/" )
	for VAR_AXISt_i in ${VAR_AXISt[*]} ; do
	    IGCM_sys_ncrename -O --hst -v ${VAR_AXISt_i}_${AXISt_i},${VAR_AXISt_i} ${file}
	done

        # A-t-on besoin de renommer les attributs des autres variables ??
	# Rename relative associate string in other variables

	set +A VARName $( cat ${file}_ncdump-h | grep "[0-9a-zA-Z_]*.*:associate.*" | grep "t_ave_${AXISt_i}" | sed -e "s/[[:space:]]*\(.*\):associate.*/\1/" )
	for var in ${VARName[*]} ; do 
	    # Old attribute value
	    ATTN=$( cat ${file}_ncdump-h | grep "${var}:associate.*" | grep "t_ave_${AXISt_i}" | sed -e "s/.*:associate = \"\(.*\)\" ;/\1/" )
	    # New attribute value
	    ATTNV=$( echo ${ATTN} | sed -e "s/t_ave_${AXISt_i}/time_counter/" ) #| sed -e 's/[[:space:]]/\\ /g')
	    # change it !
	    IGCM_sys_ncatted -O --hst -a associate,${var},m,c,"${ATTNV}" ${file}
	done
        # ------------------------------------------------------------------
        # Test if all was right before proceeding further
        # ------------------------------------------------------------------
	IGCM_debug_Verif_Exit

	IGCM_sys_Mv ${file} ${1}
	IGCM_sys_Rm ${file}_ncdump-h
    fi
    IGCM_sys_Rm ${1}_ncdump-h
    
    IGCM_debug_PopStack "IGCM_Patch_histcom_time_axis"
}
