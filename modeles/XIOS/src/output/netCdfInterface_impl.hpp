/*!
   \file netCdfInterface_impl.hpp
   \author Ha NGUYEN
   \date 08 Oct 2014
   \since 06 Oct 2014

   \brief Implemention of some templated functions in netCdfInterface
 */

#ifndef __NETCDF_INTERFACE_IMPL_HPP__
#define __NETCDF_INTERFACE_IMPL_HPP__

#include "netCdfInterface.hpp"
#include "netCdfException.hpp"

namespace xios
{
   /*!
   This function makes a request to netcdf with its id, to add or change a variable attribute or gloabl attribute,
   given its name, type, number of values provided for attribute
   \param [in] ncid Id of groupd(or File Id)
   \param [in] varId Id of the variable
   \param [in] attrName Name of the attribute
   \param [in] numVal Number of values
   \param [in] op Array of values provided for attribute
   \return Error code
   */
  template<typename T>
  int CNetCdfInterface::putAttType(int ncid, int varId, const StdString& attrName,
                                   StdSize numVal, const T* op)
  {
    int status = ncPutAttType(ncid, varId, attrName.c_str(), numVal, op);
    if (NC_NOERR != status)
     {
       StdString errormsg(nc_strerror(status));
       StdStringStream sstr;
       sstr << "Error in calling function " << "ncPutAttType(ncid, varId, attrName.c_str(), numVal, op)" << std::endl;
       sstr << errormsg << std::endl;
       sstr << "Unable to set attribute " << attrName << " for a variable with id : " << varId
         << " with number of attribute  " << numVal << std::endl;
       StdString e = sstr.str();
       throw CNetCdfException(e);
     }

     return status;
  }

   /*!
   This function makes a request to netcdf with its id, to write variable values into netcdf file,
   \param [in] ncid Id of groupd(or File Id)
   \param [in] varId Id of the variable
   \param [in] start Array specifying the index in the variable where the first data value will be written
   \param [in] count Array specifying the edge lengths along each dimension of block data
   \param [in] op Array of values provided for attribute
   \return Error code
   */
  template<typename T>
  int CNetCdfInterface::putVaraType(int ncid, int varId, const StdSize* start, const StdSize* count, const T* op)
  {
    int status = ncPutVaraType(ncid, varId, start, count, op);
    if (NC_NOERR != status)
     {
       StdString errormsg(nc_strerror(status));
       StdStringStream sstr;
       sstr << "Error in calling function " << "ncPutVaraType(ncid, varId, start, count, op)" << std::endl;
       sstr << errormsg << std::endl;
       sstr << "Unable to write value of a variable with id : " << varId << std::endl;
       StdString e = sstr.str();
       throw CNetCdfException(e);
     }

     return status;
  }

}

#endif // __NETCDF_INTERFACE_IMPL_HPP__
