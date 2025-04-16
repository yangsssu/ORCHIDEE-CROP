/*!
   \file netCdfInterface.hpp
   \author Ha NGUYEN
   \date 08 Oct 2014
   \since 03 Oct 2014

   \brief Wrapper of netcdf functions.
 */
#ifndef __NETCDF_INTERFACE_HPP_
#define __NETCDF_INTERFACE_HPP_

#include "xmlioserver_spl.hpp"

#if !defined(USING_NETCDF_PAR)
#include "exception.hpp"
#endif

#include "mpi.hpp"
#include "netcdf.hpp"

namespace xios
{
  /*!
  \class CNetCdfInterface
   This class is  wrapper of some common used functions of netCdf in Xios
  */
  class CNetCdfInterface
  {
  public:
    //! Create a netcdf file
    static int create(const StdString& path, int cmode, int& ncId);

    //! Create a netcdf file on a parallel file system
    static int createPar(const StdString& path, int cmode, MPI_Comm comm, MPI_Info info, int& ncId);

    //! Open a netcdf file
    static int open(const StdString& path, int oMode, int& ncId);

    //! Open a netcdf file
    static int openPar(const StdString& path, int cmode, MPI_Comm comm, MPI_Info info, int& ncId);

    //! Close a netcdf file
    static int close(int ncId);

    //! Put netcdf file into define mode
    static int reDef(int ncId);

    //! End define mode of a netcdf file
    static int endDef(int ncId);

    //! Query identity of a named group
    static int inqNcId(int ncid, const StdString& grpName, int& grpId);

    //! Query identity of a named variable
    static int inqVarId(int ncid, const StdString& varName, int& varId);

    //! Query identity of a named dimension
    static int inqDimId(int ncid,const StdString& dimName, int& dimId);

    //! Query identity of unlimited dimension
    static int inqUnLimDim(int ncid, int& dimId);

    //! Query name of a dimension with its id
    static int inqDimName(int ncid, int dimId, StdString& dimName);

    //! Query length of dimension with its id
    static int inqDimLen(int ncid, int dimId, StdSize& dimLen);

    //! Query number of dimension of a variable with its id
    static int inqVarNDims(int ncid, int varId, int& nDims);

    //! Query list of dimension of a variable with its id
    static int inqVarDimId(int, int, int*);

    //! Query dimensions of a group
    static int inqDimIds(int ncid, int& nDims, int* dimIds, int includeParents);


    //! Define a group
    static int defGrp(int parentNcid,const StdString& grpName, int& grpId);

    //! Define a dimension
    static int defDim(int ncid,const StdString& dimName, StdSize dimLen, int& dimId);

    //! Define a variable
    static int defVar(int ncid,const StdString& varName, nc_type xtype,
                      int nDims, const int dimIds[], int& varId);

    //! Define variable chunking size
    static int defVarChunking(int ncid, int varId, int storage, StdSize chunkSize[]);

    //! Define variable fill parameters
    static int defVarFill(int ncid, int varId, int noFill, void* fillValue);


    //! Change access type of a variable
    static int varParAccess(int ncid, int varid, int access);

    //! Syn
    static int sync(int ncId);

    //! Put attribute into variable
    static int putAtt(int ncid, int varid, const StdString& attrName, nc_type xtype,
                      StdSize numVal, const void* op);


    //! Put attribute into variable with specific type
    template<typename T>
    static int putAttType(int ncid, int varid, const StdString& attrName, StdSize numVal, const T* op);

    //! Put value into a variable with a specific type
    template<typename T>
    static int putVaraType(int ncid, int varid, const StdSize* start, const StdSize* count, const T* op);

  private:
    template<typename T>
    static int ncPutAttType(int ncid, int varid, const char* attrName, StdSize numVal, const T* op);

    template<typename T>
    static int ncPutVaraType(int ncid, int varid, const StdSize* start, const StdSize* count, const T* op);

  private:
    static StdString openMode2String(int oMode);

    static StdString creationMode2String(int cMode);


  public:
    // Some functions dedude from several special cases
    //! Query the existence of a variable
    static bool isVarExisted(int ncId, const StdString& varName);

  };
}


#endif // NETCDFINTERFACE_HPP_
