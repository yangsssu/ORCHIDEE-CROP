#include "domain.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "xmlioserver_spl.hpp"
#include "event_client.hpp"
#include "event_server.hpp"
#include "buffer_in.hpp"
#include "message.hpp"
#include "type.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "array_new.hpp"

namespace xios {
   
   /// ////////////////////// Définitions ////////////////////// ///

   CDomain::CDomain(void)
      : CObjectTemplate<CDomain>(), CDomainAttributes()
      , isChecked(false),  relFiles()
   { /* Ne rien faire de plus */ }

   CDomain::CDomain(const StdString & id)
      : CObjectTemplate<CDomain>(id), CDomainAttributes()
      , isChecked(false), relFiles()
         { /* Ne rien faire de plus */ }

   CDomain::~CDomain(void)
   { 
   }

   ///---------------------------------------------------------------

   const std::set<StdString> & CDomain::getRelFiles(void) const
   {
      return (this->relFiles); 
   }

   //----------------------------------------------------------------
   
   bool CDomain::hasZoom(void) const
   {
      return ((this->zoom_ni.getValue() != this->ni_glo.getValue()) && 
              (this->zoom_nj.getValue() != this->nj_glo.getValue()));
   }
   
   //----------------------------------------------------------------
   
   bool CDomain::isEmpty(void) const
   {
      return ((this->zoom_ni_srv == 0) || 
              (this->zoom_nj_srv == 0));
   }

   //----------------------------------------------------------------

   bool CDomain::IsWritten(const StdString & filename) const
   {
      return (this->relFiles.find(filename) != this->relFiles.end());
   }

   //----------------------------------------------------------------

   void CDomain::addRelFile(const StdString & filename)
   {
      this->relFiles.insert(filename);
   }


   StdString CDomain::GetName(void)   { return (StdString("domain")); }
   StdString CDomain::GetDefName(void){ return (CDomain::GetName()); }
   ENodeType CDomain::GetType(void)   { return (eDomain); }

   //----------------------------------------------------------------

   void CDomain::checkDomain(void)
   {
      if (!type.isEmpty() && type==type_attr::unstructured)
      {
         if (ni_glo.isEmpty() || ni_glo <= 0 )
         {
            ERROR("CDomain::checkAttributes(void)",
               << "[ Id = " << this->getId() << " ] "
               << "The global domain is badly defined,"
               << " check the \'ni_glo\'  value !") 
         }
         nj_glo=ni_glo ;
         ni_glo=1 ;
         if (!ni.isEmpty()) nj=ni ;
         if (!ibegin.isEmpty()) jbegin=ibegin ;
         if (!iend.isEmpty()) jend=iend ;
         if (!i_index.isEmpty())
         {
          j_index.resize(1,nj) ;
          for(int i=0;i<ni;i++) j_index(0,i)=i_index(i,0) ;
          i_index.resize(1,nj) ;
          for(int j=0;j<nj;j++) i_index(0,j)=0 ;
         }

         if (!mask.isEmpty())
         {
          CArray<int,2> mask_tmp(nj,1) ;
          mask_tmp = mask ;
          mask.resize(1,nj) ;
          for(int j=0;j<nj;j++) mask(0,j)=mask_tmp(j,0) ;
         }
         ni=1 ;
         ibegin=1 ;
         iend=1 ;
         
      }
      else if ((ni_glo.isEmpty() || ni_glo.getValue() <= 0 ) ||
          (nj_glo.isEmpty() || nj_glo.getValue() <= 0 ))
      {
         ERROR("CDomain::checkAttributes(void)",
               << "[ Id = " << this->getId() << " ] "
               << "The global domain is badly defined,"
               << " check the \'ni_glo\' et \'nj_glo\' values !") 
      }
      checkLocalIDomain() ;
      checkLocalJDomain() ;
      
     if (i_index.isEmpty())
     {
       i_index.resize(ni,nj) ;
       for(int j=0;j<nj;j++)
         for(int i=0;i<ni;i++) i_index(i,j)=i ;   
     }
     
     if (j_index.isEmpty())
     {
        j_index.resize(ni,nj) ;
        for(int j=0;j<nj;j++)
         for(int i=0;i<ni;i++) j_index(i,j)=j ;   
     }
 
   }

   //----------------------------------------------------------------

   void CDomain::checkLocalIDomain(void)
   {
      if (!ni.isEmpty() && !ibegin.isEmpty() && iend.isEmpty())
         iend.setValue(ibegin.getValue() + ni.getValue() - 1) ;

      else if (!ni.isEmpty() && !iend.isEmpty()   && ibegin.isEmpty())
         ibegin.setValue( - ni.getValue() + iend.getValue() + 1) ;

      else if (!ibegin.isEmpty() && !iend.isEmpty() && ni.isEmpty())
         ni.setValue(iend.getValue() - ibegin.getValue() + 1) ;

      else if (!ibegin.isEmpty() && !iend.isEmpty() && !ni.isEmpty() )
      {
         if (iend.getValue() != ibegin.getValue() + ni.getValue() - 1)
           ERROR("CDomain::checkAttributes(void)",
                 << "The domain si wrong defined,"
                 << " iend is different of (ibegin + ni - 1) !") ;
      }
      else
      {
         ERROR("CDomain::checkAttributes(void)",
               << "The domain is wrong defined,"
               << " 2 value from \'iend\', \'ibegin\', \'ni\'  must be defined !") ;
      }


      if (ni.getValue() < 0 || ibegin.getValue() > iend.getValue() ||
          ibegin.getValue() < 1 || iend.getValue() > ni_glo.getValue())
          {

         ERROR("CDomain::checkAttributes(void)",
               << "[ Id = " << this->getId() << " ] "
               << "Local domain is wrong defined,"
               << " Check the value : ni, ni_glo, ibegin, iend") ;
        }
   }

   //----------------------------------------------------------------

   void CDomain::checkLocalJDomain(void)
   {
      if (!nj.isEmpty() && !jbegin.isEmpty() && jend.isEmpty())
         jend.setValue(jbegin.getValue() + nj.getValue() - 1) ;

      else if (!nj.isEmpty() && !jend.isEmpty() && jbegin.isEmpty())
         jbegin.setValue( - nj.getValue() + jend.getValue() + 1) ;

      else if (!jbegin.isEmpty() && !jend.isEmpty() && nj.isEmpty())
         nj.setValue(jend.getValue() - jbegin.getValue() + 1) ;

      else if (!jbegin.isEmpty() && !jend.isEmpty() && !nj.isEmpty() )
      {
          if  (jend.getValue() != jbegin.getValue() + nj.getValue() - 1)
             ERROR("CDomain::checkAttributes(void)",
                 << "The domain is wrong defined,"
                 << " iend is different of (jbegin + nj - 1) !") ;
      }
      else
      {
         ERROR("CDomain::checkAttributes(void)",
               << "The domain is wrong defined,"
               << " 2 values from  jend, jbegin, nj  must be defined !") ;
      }

      if (nj.getValue() < 0 || jbegin.getValue() > jend.getValue() ||
          jbegin.getValue() < 1 || jend.getValue() > nj_glo.getValue())
         ERROR("CDomain::checkAttributes(void)",
               << "Domain is wrong defined,"
               << " Check the values : nj, nj_glo, jbegin, jend") ;
               
     ibegin_client=ibegin ; iend_client=iend ; ni_client=ni ;
     jbegin_client=jbegin ; jend_client=jend ; nj_client=nj ;
   }

   //----------------------------------------------------------------

   void CDomain::checkMask(void)
   {
      using namespace std;
      
      int ibegin_mask = 0,
          jbegin_mask = 0,
          iend_mask = iend.getValue() - ibegin.getValue(),
          jend_mask = jend.getValue() - jbegin.getValue();
      
      if (!zoom_ibegin.isEmpty())
      {
         int zoom_iend = zoom_ibegin.getValue() + zoom_ni.getValue() - 1;
         int zoom_jend = zoom_jbegin.getValue() + zoom_nj.getValue() - 1;
         
         ibegin_mask = max (ibegin.getValue(), zoom_ibegin.getValue());
         jbegin_mask = max (jbegin.getValue(), zoom_jbegin.getValue());
         iend_mask   = min (iend.getValue(), zoom_iend);
         jend_mask   = min (jend.getValue(), zoom_jend);
                 
         ibegin_mask -= ibegin.getValue();
         jbegin_mask -= jbegin.getValue();
         iend_mask   -= ibegin.getValue();
         jend_mask   -= jbegin.getValue();
      }
      

      if (!mask.isEmpty())
      {
         if ((mask.extent(0) != ni) ||
             (mask.extent(1) != nj))
            ERROR("CDomain::checkAttributes(void)",
                  <<"the mask has not the same size than the local domain"<<endl
                   <<"Local size is "<<ni<<"x"<<nj<<endl
                  <<"Mask size is "<<mask.extent(0)<<"x"<<mask.extent(1));                 
         for (int i = 0; i < ni; i++)
         {
            for (int j = 0; j < nj; j++)
            {
               if (i < ibegin_mask && i > iend_mask &&
                   j < jbegin_mask && j > jend_mask )
                     mask(i,j) = false;
            }
         }
      }
      else // (!mask.hasValue())
      { // Si aucun masque n'est défini,
        // on en crée un nouveau qui valide l'intégralité du domaine.
         mask.resize(ni,nj) ;
         for (int i = 0; i < ni.getValue(); i++)
         {
            for (int j = 0; j < nj.getValue(); j++)
            {
               if (i >= ibegin_mask && i <= iend_mask &&
                   j >= jbegin_mask && j <= jend_mask )
                     mask(i,j) = true;
               else  mask(i,j) = false;
            }
         }
      }
   }


   //----------------------------------------------------------------

   void CDomain::checkDomainData(void)
   {     
      if (!data_dim.isEmpty() &&
         !(data_dim.getValue() == 1 || data_dim.getValue() == 2))
      {
         ERROR("CDomain::checkAttributes(void)",
               << "Data dimension incompatible (must be 1 or 2) !") ;
      }
      else if (data_dim.isEmpty())
      {
         ERROR("CDomain::checkAttributes(void)",
               << "Data dimension undefined !") ;
      }

      if (data_ibegin.isEmpty())
         data_ibegin.setValue(0) ;
      if (data_jbegin.isEmpty() && (data_dim.getValue() == 2))
         data_jbegin.setValue(0) ;

      if (!data_ni.isEmpty() && (data_ni.getValue() <= 0))
      {
         ERROR("CDomain::checkAttributes(void)",
               << "Data dimension is negative (data_ni).") ;
      }
      else if (data_ni.isEmpty())
      {
         data_ni.setValue((data_dim.getValue() == 1)
                           ? (ni.getValue() * nj.getValue())
                           : ni.getValue());
      }

      if (data_dim.getValue() == 2)
      {
         if (!data_nj.isEmpty() && (data_nj.getValue() <= 0) )
         {
            ERROR("CDomain::checkAttributes(void)",
                  << "Data dimension is negative (data_nj).") ;
         }
         else if (data_nj.isEmpty())
            data_nj.setValue(nj.getValue()) ;
      }

   }

   //----------------------------------------------------------------

   void CDomain::checkCompression(void)
   {
      if (!data_i_index.isEmpty())
      {
         int ssize = data_i_index.numElements();
         if (!data_n_index.isEmpty() &&
            (data_n_index.getValue() != ssize))
         {
            ERROR("CDomain::checkAttributes(void)",
                  <<"Dimension data_i_index incompatible with data_n_index.") ;
         }
         else if (data_n_index.isEmpty())
            data_n_index.setValue(ssize) ;

         if (data_dim.getValue() == 2)
         {
            if (!data_j_index.isEmpty() &&
               (data_j_index.numElements() != data_i_index.numElements()))
            {
               ERROR("CDomain::checkAttributes(void)",
                     <<"Dimension data_j_index incompatible with data_i_index.") ;
            }
            else if (data_j_index.isEmpty())
            {
               ERROR("CDomain::checkAttributes(void)",
                     <<"data_j_index must be defined !") ;
            }
         }
      }
      else
      {
         if (!data_n_index.isEmpty() ||
            ((data_dim.getValue() == 2) && (!data_j_index.isEmpty())))
            ERROR("CDomain::checkAttributes(void)", << "data_i_index undefined") ;
      }

      if (data_n_index.isEmpty())
      { // -> bloc re-vérifié OK
         if (data_dim.getValue() == 1)
         {
            const int dni = data_ni.getValue();
            data_i_index.resize(dni) ;
            data_n_index.setValue(dni);
            for (int i = 0; i < dni; i++) data_i_index(i) = i+1 ;
         }
         else   // (data_dim == 2)
         {
            const int dni = data_ni.getValue() * data_nj.getValue();
            data_i_index.resize(dni) ;
            data_j_index.resize(dni) ;
            
            data_n_index.setValue(dni);
            
            for(int count = 0, j = 0; j  < data_nj.getValue(); j++)
            {
               for(int i = 0; i < data_ni.getValue(); i++, count++)
               {
                  data_i_index(count) = i+1 ;
                  data_j_index(count) = j+1 ;
               }
            }
         }
      }
   }

   //----------------------------------------------------------------
   
   void CDomain::completeLonLatClient(void)
   {
      int i,j,k ;
      CArray<double,1> lonvalue_temp(ni*nj) ;
      CArray<double,1> latvalue_temp(ni*nj) ;
      CArray<double,2> bounds_lon_temp(nvertex,ni*nj) ;
      CArray<double,2> bounds_lat_temp(nvertex,ni*nj) ;
      
      if (type.isEmpty())
      {
        if ( lonvalue.numElements() == ni*nj && latvalue.numElements() == ni*nj ) 
        {
          type.setValue(type_attr::curvilinear) ;
          isCurvilinear=true ;
        }
        else if ( lonvalue.numElements() == ni && latvalue.numElements() == nj ) 
        {
          type.setValue(type_attr::regular) ;
          isCurvilinear=false ;
        }
        else ERROR("void CDomain::completeLonLatClient(void)",<<"the grid is nor curvilinear, nor cartesian, because the size of longitude and latitude array is not coherent with the domain size"<<endl
                                                              <<"lonvalue size = " << lonvalue.numElements() << "different of ni or ni*nj"<<endl
                                                              <<"latvalue size = " << latvalue.numElements() << "different of nj or ni*nj" ) ;
      }
      if (type==type_attr::curvilinear || type==type_attr::unstructured)
      {
        lonvalue_temp=lonvalue ;
        latvalue_temp=latvalue ;
        if (hasBounds) bounds_lon_temp=bounds_lon ;
        if (hasBounds) bounds_lat_temp=bounds_lat ;
      }
      else
      {
        for(j=0;j<nj;j++)
          for(i=0;i<ni;i++) 
          {
            k=j*ni+i ;
            lonvalue_temp(k)=lonvalue(i) ;
            latvalue_temp(k)=latvalue(j) ;
            if (hasBounds)
            {
              for(int n=0;n<nvertex;n++) 
              {
                bounds_lon_temp(n,k)=bounds_lon(n,i) ;
                bounds_lat_temp(n,k)=bounds_lat(n,j) ;
              }
            }
          }
      }
         
      StdSize dm = zoom_ni_client * zoom_nj_client;

      lonvalue.resize(dm);
      latvalue.resize(dm);
         
      for (int i = 0; i < zoom_ni_client; i++)
      {
        for (int j = 0; j < zoom_nj_client; j++)
        {
          lonvalue(i + j * zoom_ni_client) = lonvalue_temp( (i + zoom_ibegin_client-ibegin) + (j + zoom_jbegin_client -jbegin)*ni ); 
          latvalue(i + j * zoom_ni_client) = latvalue_temp( (i + zoom_ibegin_client - ibegin)+(j + zoom_jbegin_client - jbegin)*ni );
          if (hasBounds)
          {
            for(int n=0;n<nvertex;n++) 
            {
              bounds_lon(n,i + j * zoom_ni_client) = bounds_lon_temp( n, (i + zoom_ibegin_client - ibegin) + (j + zoom_jbegin_client -jbegin)*ni ); 
              bounds_lat(n,i + j * zoom_ni_client) = bounds_lat_temp( n, (i + zoom_ibegin_client - ibegin)+(j + zoom_jbegin_client -jbegin)*ni );
            }
          }
        }
      }
    }
 

   //----------------------------------------------------------------

   void CDomain::checkZoom(void)
   {
      // Résolution et vérification des données globales de zoom.
      if (!this->zoom_ni.isEmpty() || !this->zoom_nj.isEmpty() ||
          !this->zoom_ibegin.isEmpty() || !this->zoom_jbegin.isEmpty())
      {
         if (this->zoom_ni.isEmpty()     || this->zoom_nj.isEmpty() ||
             this->zoom_ibegin.isEmpty() || this->zoom_jbegin.isEmpty())
         {
            ERROR("CDomain::checkZoom(void)",
                  <<"if one of zoom attributes is defined then all zoom attributes must be defined") ;
         }
         else
         {
            int zoom_iend = zoom_ibegin + zoom_ni - 1;
            int zoom_jend = zoom_jbegin + zoom_nj - 1;
                
            if (zoom_ibegin < 1  || zoom_jbegin < 1 || zoom_iend > ni_glo || zoom_jend > nj_glo)
               ERROR("CDomain::checkZoom(void)",
                     << "Zoom is wrong defined,"
                     << " Check the values : zoom_ni, zoom_nj, zoom_ibegin, zoom_jbegin") ;
         }
      }
      else
      {
         zoom_ni = ni_glo; 
         zoom_nj = nj_glo;
         zoom_ibegin = 1;
         zoom_jbegin = 1;
      }
      
      // compute client zoom indices

      int zoom_iend=zoom_ibegin+zoom_ni-1 ;
      zoom_ibegin_client = ibegin_client > zoom_ibegin ? ibegin_client : zoom_ibegin ;
      zoom_iend_client = iend_client < zoom_iend ? iend_client : zoom_iend ;
      zoom_ni_client=zoom_iend_client-zoom_ibegin_client+1 ;
      if (zoom_ni_client<0) zoom_ni_client=0 ;

    
      int zoom_jend=zoom_jbegin+zoom_nj-1 ;
      zoom_jbegin_client = jbegin_client > zoom_jbegin ? jbegin_client : zoom_jbegin ;
      zoom_jend_client = jend_client < zoom_jend ? jend_client : zoom_jend ;
      zoom_nj_client=zoom_jend_client-zoom_jbegin_client+1 ;
      if (zoom_nj_client<0) zoom_nj_client=0 ;

   }
   
   void CDomain::checkBounds(void)
   {
     if (!nvertex.isEmpty() && !bounds_lon.isEmpty() && !bounds_lat.isEmpty())
     {
       hasBounds=true ;
       
     }
     else 
     {
       hasBounds=false;
       nvertex=0 ;
     }
   }
            
   //----------------------------------------------------------------

   void CDomain::checkAttributes(void)
   {
      if (this->isChecked) return;
      CContext* context=CContext::getCurrent() ;

      this->checkDomain();
      this->checkZoom();
      this->checkBounds();
      
      if (context->hasClient)
      { // Côté client uniquement
         this->checkMask();
         this->checkDomainData();
         this->checkCompression();
         this->completeLonLatClient();
      }
      else
      { // Côté serveur uniquement
//         if (!this->isEmpty())
// ne sert plus //   this->completeLonLatServer();
      }
    
      if (context->hasClient)
      {
        computeConnectedServer() ;
        sendServerAttribut() ;
        sendLonLat() ;
      }
      
      this->isChecked = true;
   }
   
  void CDomain::sendServerAttribut(void)
  {
    int ni_srv=ni_glo.getValue() ;
    int ibegin_srv=1 ;
    int iend_srv=ni_glo.getValue() ;
     
    int nj_srv ;
    int jbegin_srv ;
    int jend_srv ;
    
    CContext* context=CContext::getCurrent() ;
    CContextClient* client=context->client ;
    int nbServer=client->serverSize ;
    int serverRank=client->getServerLeader() ;
    
    jend_srv=0 ;
    for(int i=0;i<=serverRank;i++)
    {
      jbegin_srv=jend_srv+1 ;
      nj_srv=nj_glo.getValue()/nbServer ;
      if (i<nj_glo.getValue()%nbServer) nj_srv++ ;
      jend_srv=jbegin_srv+nj_srv-1 ;
    }
    
     CEventClient event(getType(),EVENT_ID_SERVER_ATTRIBUT) ;   
     if (client->isServerLeader())
     {
       CMessage msg ;
       msg<<this->getId() ;
       msg<<ni_srv<<ibegin_srv<<iend_srv<<nj_srv<<jbegin_srv<<jend_srv;
       event.push(client->getServerLeader(),1,msg) ;
       client->sendEvent(event) ;
     }
     else client->sendEvent(event) ;
  }

  void CDomain::computeConnectedServer(void)
  {
    int i,j,i_ind,j_ind ;
    
    ibegin_client=ibegin ; iend_client=iend ; ni_client=ni ;
    jbegin_client=jbegin ; jend_client=jend ; nj_client=nj ;
     
    CContext* context = CContext::getCurrent() ;
    CContextClient* client=context->client ;
    int nbServer=client->serverSize ;

    // find how much client are connected to a server
    int zoom_iend=zoom_ibegin+zoom_ni-1 ;
    int zoom_jend=zoom_jbegin+zoom_nj-1 ;
    
    int blockSize=nj_glo/nbServer ;
    int ns=nj_glo%nbServer ;
    int pos=ns*(blockSize+1) ;
    int serverNum ;
    
    mapConnectedServer.resize(ni,nj) ;
    vector<int> nbData(nbServer,0) ;
    vector<int> indServer(nbServer,-1) ;
    vector<bool> IsConnected(nbServer,false) ;
    
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++)
      {
        i_ind=ibegin+i_index(i,j)-1 ;
        j_ind=jbegin+j_index(i,j)-1 ;
        
        if (j_ind<pos) serverNum=j_ind/(blockSize+1) ;
        else serverNum=ns+(j_ind-pos)/blockSize ;
        IsConnected[serverNum]=true ;
                
        if (i_ind >= zoom_ibegin-1 && i_ind <= zoom_iend-1 && j_ind >= zoom_jbegin-1 && j_ind <= zoom_jend-1)
        {
          mapConnectedServer(i,j)=serverNum ;
          nbData[serverNum]++ ;
        }
        else mapConnectedServer(i,j)=-1 ;
      }


    for(serverNum=0 ; serverNum<nbServer ; serverNum++) 
      if (IsConnected[serverNum])
      {
        ns=connectedServer.size() ;
        indServer[serverNum]=ns;
        connectedServer.push_back(serverNum) ;
        nbDataSrv.push_back(nbData[serverNum]) ;
      }
     
     i_indSrv.resize(connectedServer.size()) ;
     j_indSrv.resize(connectedServer.size()) ;

     for(j=0;j<nj;j++)
      for(i=0;i<ni;i++) 
      {
        if (mapConnectedServer(i,j)>=0)
        {
          ns=indServer[mapConnectedServer(i,j)] ;
          mapConnectedServer(i,j)= ns ;
          i_indSrv[ns].push_back(i+ibegin-1) ;
          j_indSrv[ns].push_back(j+jbegin-1) ;
        }
      }
          
    int nbConnectedServer=connectedServer.size() ;

    int* recvCount=new int[client->clientSize] ;
    int* displ=new int[client->clientSize] ;
    int* sendBuff=new int[nbConnectedServer] ;
    valarray<int> nbClient(0,client->serverSize) ;
    
    for(int n=0;n<nbConnectedServer;n++) sendBuff[n]=connectedServer[n] ;
    
    // get connected server for everybody
    MPI_Allgather(&nbConnectedServer,1,MPI_INT,recvCount,1,MPI_INT,client->intraComm) ;
    
    displ[0]=0 ;
    for(int n=1;n<client->clientSize;n++) displ[n]=displ[n-1]+recvCount[n-1] ;
    int recvSize=displ[client->clientSize-1]+recvCount[client->clientSize-1] ;
    int* recvBuff=new int[recvSize] ;
 
    
    MPI_Allgatherv(sendBuff,nbConnectedServer,MPI_INT,recvBuff,recvCount,displ,MPI_INT,client->intraComm) ;
    for(int n=0;n<recvSize;n++) nbClient[recvBuff[n]]++ ;
    
    for(int n=0;n<nbConnectedServer;n++) nbSenders.push_back(nbClient[connectedServer[n]]) ;
   
    delete [] recvCount ;
    delete [] displ ;
    delete [] sendBuff ;
    delete [] recvBuff ;
  }


  void CDomain::sendLonLat(void)
  {
    int ns,n,i,j,ind,nv ;
    CContext* context = CContext::getCurrent() ;
    CContextClient* client=context->client ;
    // send lon lat for each connected server
    CEventClient event(getType(),EVENT_ID_LON_LAT) ;
    
    list<shared_ptr<CMessage> > list_msg ;    
    list< CArray<int,1>* > list_indi,list_indj ;
    list< CArray<double,1>* >list_lon,list_lat ;
    list< CArray<double,2>* >list_boundslon,list_boundslat ;

    for(int ns=0;ns<connectedServer.size();ns++)
    {
      int nbData = nbDataSrv[ns] ;
      CArray<int,1> indi(nbData) ;
      CArray<int,1> indj(nbData) ;
      CArray<double,1> lon(nbData) ;
      CArray<double,1> lat(nbData) ;
      CArray<double,2> boundslon(nvertex,nbData) ;
      CArray<double,2> boundslat(nvertex,nbData) ;
      
      for(n=0;n<nbData;n++) 
      {
        i=i_indSrv[ns][n] ;
        j=j_indSrv[ns][n] ;
        ind=(i-(zoom_ibegin_client-1))+(j-(zoom_jbegin_client-1))*zoom_ni_client ;
        
        lon(n)=lonvalue(ind) ;
        lat(n)=latvalue(ind) ;
        if (hasBounds)
        {
          for(nv=0;nv<nvertex;nv++)
          {
            boundslon(nv,n)=bounds_lon(nv,ind);
            boundslat(nv,n)=bounds_lat(nv,ind);
          }
        }
        indi(n)=ibegin+i_index(i-ibegin+1,j-jbegin+1)-1  ;
        indj(n)=jbegin+j_index(i-ibegin+1,j-jbegin+1)-1  ;
      }
    
      list_indi.push_back(new CArray<int,1>(indi.copy())) ;
      list_indj.push_back(new CArray<int,1>(indj.copy())) ;
      list_lon.push_back(new CArray<double,1>(lon.copy())) ;
      list_lat.push_back(new CArray<double,1>(lat.copy())) ;
      if (hasBounds) list_boundslon.push_back(new CArray<double,2>(boundslon.copy())) ;
      if (hasBounds) list_boundslat.push_back(new CArray<double,2>(boundslat.copy())) ;

      list_msg.push_back(shared_ptr<CMessage>(new CMessage)) ;

      *list_msg.back()<<this->getId()<<(int)type ; // enum ne fonctionne pour les message => ToFix
      *list_msg.back()<<isCurvilinear ;
      *list_msg.back()<<*list_indi.back()<<*list_indj.back()<<*list_lon.back()<<*list_lat.back() ;
      if (hasBounds) *list_msg.back()<<*list_boundslon.back()<<*list_boundslat.back();
      event.push(connectedServer[ns],nbSenders[ns],*list_msg.back()) ;
    }

    client->sendEvent(event) ;
    
    
    for(list<CArray<int,1>* >::iterator it=list_indi.begin();it!=list_indi.end();it++) delete *it;
    for(list<CArray<int,1>* >::iterator it=list_indj.begin();it!=list_indj.end();it++) delete *it;
    for(list<CArray<double,1>* >::iterator it=list_lon.begin();it!=list_lon.end();it++)   delete *it;
    for(list<CArray<double,1>* >::iterator it=list_lat.begin();it!=list_lat.end();it++)   delete *it;
    if (hasBounds) for(list<CArray<double,2>* >::iterator it=list_boundslon.begin();it!=list_boundslon.end();it++)   delete *it;
    if (hasBounds) for(list<CArray<double,2>* >::iterator it=list_boundslat.begin();it!=list_boundslat.end();it++)   delete *it;    
    
  }
  
  
  bool CDomain::dispatchEvent(CEventServer& event)
   {
      
      if (SuperClass::dispatchEvent(event)) return true ;
      else
      {
        switch(event.type)
        {
           case EVENT_ID_SERVER_ATTRIBUT :
             recvServerAttribut(event) ;
             return true ;
             break ;
           case EVENT_ID_LON_LAT :
             recvLonLat(event) ;
             return true ;
             break ;
           default :
             ERROR("bool CContext::dispatchEvent(CEventServer& event)",
                    <<"Unknown Event") ;
           return false ;
         }
      }
   }
   
  void CDomain::recvServerAttribut(CEventServer& event)
  {
    CBufferIn* buffer=event.subEvents.begin()->buffer;
    string domainId ;
    *buffer>>domainId ;
    get(domainId)->recvServerAttribut(*buffer) ;
  }
  
  void CDomain::recvServerAttribut(CBufferIn& buffer)
  {
    int zoom_iend=zoom_ibegin.getValue()+zoom_ni.getValue()-1 ;
    int zoom_jend=zoom_jbegin.getValue()+zoom_nj.getValue()-1 ;

     buffer>>ni_srv>>ibegin_srv>>iend_srv>>nj_srv>>jbegin_srv>>jend_srv;
     
    
    zoom_ibegin_srv = zoom_ibegin.getValue() > ibegin_srv ? zoom_ibegin.getValue() : ibegin_srv ;
    zoom_iend_srv = zoom_iend < iend_srv ? zoom_iend : iend_srv ;
    zoom_ni_srv=zoom_iend_srv-zoom_ibegin_srv+1 ;
      
    zoom_jbegin_srv = zoom_jbegin.getValue() > jbegin_srv ? zoom_jbegin.getValue() : jbegin_srv ;
    zoom_jend_srv = zoom_jend < jend_srv ? zoom_jend : jend_srv ;
    zoom_nj_srv=zoom_jend_srv-zoom_jbegin_srv+1 ;

    if (zoom_ni_srv<=0 || zoom_nj_srv<=0) 
    {
      zoom_ibegin_srv=1 ; zoom_iend_srv=0 ; zoom_ni_srv=0 ;
      zoom_jbegin_srv=1 ; zoom_jend_srv=0 ; zoom_nj_srv=0 ;
    }
    lonvalue_srv.resize(zoom_ni_srv*zoom_nj_srv) ;
    lonvalue_srv = 0. ;
    latvalue_srv.resize(zoom_ni_srv*zoom_nj_srv) ;
    latvalue_srv = 0. ;
    if (hasBounds) 
    {
      bounds_lon_srv.resize(nvertex,zoom_ni_srv*zoom_nj_srv) ;
      bounds_lon_srv = 0. ;
      bounds_lat_srv.resize(nvertex,zoom_ni_srv*zoom_nj_srv) ;
      bounds_lat_srv = 0. ;
    }
  }
    
  void CDomain::recvLonLat(CEventServer& event)
  {
    list<CEventServer::SSubEvent>::iterator it ;
    for (it=event.subEvents.begin();it!=event.subEvents.end();++it)
    {
      CBufferIn* buffer=it->buffer;
      string domainId ;
      *buffer>>domainId ;
      get(domainId)->recvLonLat(*buffer) ;
    }
  }
  
  void CDomain::recvLonLat(CBufferIn& buffer)
  {
    CArray<int,1> indi ;
    CArray<int,1> indj ;
    CArray<double,1> lon ;
    CArray<double,1> lat ;
    CArray<double,2> boundslon ;
    CArray<double,2> boundslat ;
    
    int type_int ;
    buffer>>type_int>>isCurvilinear>>indi>>indj>>lon>>lat ;
    if (hasBounds) buffer>>boundslon>>boundslat ;
    type.setValue((type_attr::t_enum)type_int) ; // probleme des type enum avec les buffers : ToFix

    int i,j,ind_srv ;
    for(int ind=0;ind<indi.numElements();ind++)
    {
      i=indi(ind) ; j=indj(ind) ;
      ind_srv=(i-(zoom_ibegin_srv-1))+(j-(zoom_jbegin_srv-1))*zoom_ni_srv ;
      lonvalue_srv(ind_srv)=lon(ind) ;
      latvalue_srv(ind_srv)=lat(ind) ;
      if (hasBounds) 
      {
        for(int nv=0;nv<nvertex;nv++) 
        {
          bounds_lon_srv(nv,ind_srv)=boundslon(nv,ind) ;
          bounds_lat_srv(nv,ind_srv)=boundslat(nv,ind) ;
        }
      }
    }
  }
   //----------------------------------------------------------------
   
   
   
   ///---------------------------------------------------------------

} // namespace xios
