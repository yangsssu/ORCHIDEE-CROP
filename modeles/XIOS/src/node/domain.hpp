#ifndef __XMLIO_CDomain__
#define __XMLIO_CDomain__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "group_factory.hpp"

#include "declare_group.hpp"
#include "event_client.hpp"
#include "event_server.hpp"
#include "buffer_in.hpp"
#include "array_new.hpp"
#include "attribute_array.hpp"
#include "attribute_enum.hpp"

namespace xios {
   
   /// ////////////////////// Déclarations ////////////////////// ///

   class CDomainGroup;
   class CDomainAttributes;
   class CDomain;

   ///--------------------------------------------------------------

   // Declare/Define CDomainAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CDomain)
#  include "domain_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CDomain)

   ///--------------------------------------------------------------

   class CDomain
      : public CObjectTemplate<CDomain>
      , public CDomainAttributes
   {
         enum EEventId
         {
           EVENT_ID_SERVER_ATTRIBUT, EVENT_ID_LON_LAT
         } ;
         
         /// typedef ///
         typedef CObjectTemplate<CDomain>   SuperClass;
         typedef CDomainAttributes SuperClassAttribute;

      public :

         typedef CDomainAttributes RelAttributes;
         typedef CDomainGroup      RelGroup;

         /// Constructeurs ///
         CDomain(void);
         explicit CDomain(const StdString & id);
         CDomain(const CDomain & domain);       // Not implemented yet.
         CDomain(const CDomain * const domain); // Not implemented yet.

         /// Vérifications ///
         void checkAttributes(void);

      private :

         void checkDomain(void);

         void checkLocalIDomain(void);
         void checkLocalJDomain(void);

         void checkMask(void);
         void checkDomainData(void);
         void checkCompression(void);
         
         void checkZoom(void);
         void checkBounds(void);


      public :
      
         /// Autres ///

         const std::set<StdString> & getRelFiles(void) const;


         /// Test ///
         bool IsWritten(const StdString & filename) const;
         bool hasZoom(void) const;
         bool isEmpty(void) const;
         
         
         int ni_client,ibegin_client,iend_client ;
         int zoom_ni_client,zoom_ibegin_client,zoom_iend_client ;

         int nj_client,jbegin_client,jend_client ;
         int zoom_nj_client,zoom_jbegin_client,zoom_jend_client ;

         int ni_srv,ibegin_srv,iend_srv ;
         int zoom_ni_srv,zoom_ibegin_srv,zoom_iend_srv ;

         int nj_srv,jbegin_srv,jend_srv ;
         int zoom_nj_srv,zoom_jbegin_srv,zoom_jend_srv ;

         CArray<double, 1> lonvalue_srv, latvalue_srv ;
         CArray<double, 2> bounds_lon_srv, bounds_lat_srv ;
         
         
        vector<int> connectedServer ; // list of connected server 
        vector<int> nbSenders ; // for each communication with a server, number of communicating client
        vector<int> nbDataSrv ; // size of data to send to each server 
        vector< vector<int> > i_indSrv ; // for each server, i global index to send
        vector< vector<int> > j_indSrv ; // for each server, j global index to send
       
        CArray<int,2> mapConnectedServer ;  // (ni,nj) => mapped to connected server number, -1 if no server is target
               
//        vector<int> ib_srv, ie_srv, in_srv ;
//        vector<int> jb_srv, je_srv, jn_srv ;
         
      public :
      
         /// Mutateur ///
         void addRelFile(const StdString & filename);
         void completeLonLatClient(void);
         void sendServerAttribut(void) ;
         void sendLonLat(void) ;
         void computeConnectedServer(void) ;
         static bool dispatchEvent(CEventServer& event) ;
         static void recvLonLat(CEventServer& event) ;
         static void recvServerAttribut(CEventServer& event) ;
         void recvLonLat(CBufferIn& buffer) ;
         void recvServerAttribut(CBufferIn& buffer) ;
         
         /// Destructeur ///
         virtual ~CDomain(void);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);
         
         static ENodeType GetType(void);

         CArray<int, 2> local_mask;
         bool isCurvilinear ;
         bool hasBounds ;
       private :

         /// Proriétés protégées ///
         bool isChecked;
         std::set<StdString> relFiles;

   }; // class CDomain

   ///--------------------------------------------------------------

   // Declare/Define CDomainGroup and CDomainDefinition
   DECLARE_GROUP(CDomain);

   ///--------------------------------------------------------------

} // namespace xios

#endif // __XMLIO_CDomain__
