#!/bin/ksh

#**************************************************************
# Author: Arnaud Caubel
# Contact: Arnaud.Caubel__at__lsce.ipsl.fr
# $Revision:: 1206                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2015-06-05 17:28:35 +0200 (Fri, 05 Jun 2015) $ Date of last commit
# IPSL (2009)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************

#------------------------------------------------------------
# Add mask from meshmask file to oceanic variables
#------------------------------------------------------------

function IGCM_Patch_20091118_mask {

    IGCM_debug_PushStack "IGCM_Patch_mask" $@

    if [ ! -f ${config_UserChoices_JobName}_mesh_mask.nc ] ; then
      if [ -f mesh_mask.nc ] ; then
    IGCM_sys_Cp mesh_mask.nc ${config_UserChoices_JobName}_mesh_mask.nc
      elif [ -f ${R_BUFR}/OCE/Output/${config_UserChoices_JobName}_mesh_mask.nc ] ; then
	IGCM_sys_GetBuffer ${R_BUFR}/OCE/Output/${config_UserChoices_JobName}_mesh_mask.nc .
      elif [ -f ${R_SAVE}/OCE/Output/${config_UserChoices_JobName}_mesh_mask.nc ] ; then
	IGCM_sys_Get ${R_SAVE}/OCE/Output/${config_UserChoices_JobName}_mesh_mask.nc .
      else
	IGCM_debug_Exit "mesh_mask can not be found. Stop."
	IGCM_debug_Exit "neither here ${R_SAVE}/OCE/Output/${config_UserChoices_JobName}_mesh_mask.nc"
	IGCM_debug_Exit "nor here ${R_BUFR}/OCE/Output/${config_UserChoices_JobName}_mesh_mask.nc"
	IGCM_debug_Verif_Exit
      fi
    fi

    filename=${1}

    chaineT=${filename%%'grid_T'*}
    chaineU=${filename%%'grid_U'*}
    chaineV=${filename%%'grid_V'*}
    chaineW=${filename%%'grid_W'*}
    chaineIce=${filename%%'icemod'*}
    chaineTrc=${filename%%'ptrc_T'*}
    chaineDia=${filename%%'diad_T'*}
    chaineBio=${filename%%'dbio_T'*}

    if [ ${filename} != ${chaineT} ]; then
	mask='tmask' ; depth='deptht'
    elif [ ${filename} != ${chaineU} ]; then
	mask='umask' ; depth='depthu'
    elif [ ${filename} != ${chaineV} ]; then
	mask='vmask' ; depth='depthv'
    elif [ ${filename} != ${chaineW} ]; then
	mask='tmask' ; depth='depthw'
    elif [ ${filename} != ${chaineIce} ]; then
	mask='tmask' ; depth='deptht'
    elif [ ${filename} != ${chaineTrc} ]; then
	mask='tmask' ; depth='deptht'
    elif [ ${filename} != ${chaineDia} ]; then
	mask='tmask' ; depth='deptht'
    elif [ ${filename} != ${chaineBio} ]; then
	mask='tmask' ; depth='deptht'
    fi

    #============================================
    # Remove nav_lon, nav_lat file
    IGCM_sys_ncks -Oh -x -v nav_lon,nav_lat ${filename} file1.nc

    #============================================
    # Extract mask variable
    IGCM_sys_ncks -Oh -v ${mask} ${config_UserChoices_JobName}_mesh_mask.nc mask3D.nc

    #============================================
    # Rename deptht dimension
    IGCM_sys_ncrename -Oh -d z,${depth} mask3D.nc

    #============================================
    # Remove single dimension t
    IGCM_sys_ncwa -Oh -a t mask3D.nc mask3D.nc

    #============================================
    # Create mask2D 
    IGCM_sys_ncks -Oh -d ${depth},0,0 mask3D.nc mask2D.nc
    IGCM_sys_ncwa -Oh -a ${depth} mask2D.nc mask2D.nc
    IGCM_sys_ncrename -h -v ${mask},mask2D mask2D.nc

    #============================================
    # Append mask (2D) to file1.nc
    IGCM_sys_ncks -Ah -c -v mask2D mask2D.nc file1.nc

    if [ ${filename} = ${chaineIce} ]; then

        #============================================
        # Append mask (3D) only for oceanic files
	IGCM_sys_ncks -Ah -v ${mask} mask3D.nc file1.nc
	IGCM_sys_ncrename -h -v ${mask},mask3D file1.nc

        #============================================
        # Add record dimension only for oceanic files
	IGCM_sys_ncecat -Oh file1.nc file1.nc

        #============================================
        # Apply mask 3D only for oceanic files
	IGCM_sys_ncwa -Oh -a record -B 'mask3D==1' file1.nc file2.nc
	IGCM_sys_Rm file1.nc
	IGCM_sys_Mv file2.nc file1.nc


    fi

    #============================================
    # Add record dimension
    IGCM_sys_ncecat -Oh file1.nc file1.nc

    #============================================
    # Apply mask 2D
    IGCM_sys_ncwa -Oh -b -a record -B 'mask2D==1' file1.nc file1.nc

    #============================================
    # Permute record dimension in time_counter dimension
    IGCM_sys_ncpdq -Oh -a time_counter,record file1.nc file1.nc
    IGCM_sys_ncwa -Oh -a record file1.nc file1.nc

    #============================================
    # Remove mask 2D and 3D for oceanic files, and mask 2D only for ice files
    
    if [ ${filename} = ${chaineIce} ]; then
        #============================================
        # Remove mask 2D and mask 3D for oceanic files
        IGCM_sys_ncks -Oh -x -v mask2D,mask3D file1.nc file_mask.nc
    else
        #============================================
        # Remove mask 2D for ice files
        IGCM_sys_ncks -Oh -x -v mask2D file1.nc file_mask.nc
    fi

    #============================================
    # Add nav_lon nav_lat
    IGCM_sys_ncks -Ah -v nav_lon,nav_lat ${filename} file_mask.nc

    # ------------------------------------------------------------------
    # Test if all was right before proceeding further
    # ------------------------------------------------------------------
    IGCM_debug_Verif_Exit

    #============================================
    # Cleaning
    IGCM_sys_Mv file_mask.nc ${filename}
    IGCM_sys_Rm mask3D.nc mask2D.nc file1.nc
    
    IGCM_debug_PopStack "IGCM_Patch_mask"
}
