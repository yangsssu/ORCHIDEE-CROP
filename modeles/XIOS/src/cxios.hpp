#ifndef __XIOS_HPP__
#define __XIOS_HPP__

#include "xmlioserver_spl.hpp"
#include "mpi.hpp"

namespace xios
{
  class CXios
  {
     public:

     static string rootFile ;
     static string xiosCodeId ;
     static string clientFile;
     static string serverFile;

     static void initialize(void) ;


     static void initClientSide(const string & codeId, MPI_Comm& localComm, MPI_Comm& returnComm) ;
     static void initServerSide(void) ;
     static void clientFinalize(void) ;
     static void parseFile(const string& filename) ;

     template <typename T>
     static T getin(const string& id,const T& defaultValue) ;

     template <typename T>
     static T getin(const string& id) ;

     static bool isClient ;
     static bool isServer ;

     static MPI_Comm globalComm ;

     static bool printLogs2Files; //!< Printing out logs into files
     static bool usingOasis ;
     static bool usingServer ;
     static size_t bufferSize ;
     static size_t defaultBufferSize ;
     static double bufferServerFactorSize ;
     static double defaultBufferServerFactorSize ;

     public:
     //! Setting xios to use server mode
     static void setUsingServer();

     //! Setting xios NOT to use server mode
     static void setNotUsingServer();

  } ;

}

//#include "cxios_impl.hpp"









#endif
