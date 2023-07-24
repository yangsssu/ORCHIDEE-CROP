/*!
   \file netCdfInterface_impl.hpp
   \author Ha NGUYEN
   \date 06 Oct 2014
   \since 06 Oct 2014

   \brief Implemention of some templated functions in netCdfInterface
 */

#include "netCdfInterface_impl.hpp"

namespace xios
{
#define  macroPutAtt(type) \
  template int CNetCdfInterface::putAttType(int ncid, int varid, const StdString& attrName, \
                                            StdSize numVal, const type* op);

  macroPutAtt(double);
  macroPutAtt(float);
  macroPutAtt(int);
  macroPutAtt(long);
  macroPutAtt(short);

#define  macroPutVar(type) \
 template int CNetCdfInterface::putVaraType(int ncid, int varId, const StdSize* start, \
                                            const StdSize* count, const type* op);
  macroPutVar(double);
  macroPutVar(float);
  macroPutVar(int);
}
