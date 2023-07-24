
#include "nc4_data_output.hpp"

#include <boost/lexical_cast.hpp>
#include "attribute_template.hpp"
#include "group_template.hpp"

#include "file.hpp"
#include "calendar.hpp"
#include "context.hpp"
#include "context_server.hpp"
#include "netCdfException.hpp"
#include "exception.hpp"

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///
      CNc4DataOutput::CNc4DataOutput
         (const StdString & filename, bool exist)
            : SuperClass()
            , SuperClassWriter(filename, exist)
            , filename(filename)
      {
         StdString timeid = StdString("time_counter");
         SuperClass::type = MULTI_FILE;
//         if (!exist)
//            SuperClassWriter::addDimension(timeid);
      }

      CNc4DataOutput::CNc4DataOutput
         (const StdString & filename, bool exist, MPI_Comm comm_file,bool multifile, bool isCollective)
            : SuperClass()
            , SuperClassWriter(filename, exist, &comm_file,multifile)
            , comm_file(comm_file)
            , filename(filename)
            , isCollective(isCollective)
      {
         StdString timeid = StdString("time_counter");

         SuperClass::type = (multifile) ? MULTI_FILE : ONE_FILE;

 //        if (!exist)
//            SuperClassWriter::addDimension(timeid);
      }


      CNc4DataOutput::~CNc4DataOutput(void)
      { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      const StdString & CNc4DataOutput::getFileName(void) const
      {
         return (this->filename);
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeDomain_(CDomain* domain)
      {
         if (domain->type == CDomain::type_attr::unstructured)
         {
           writeUnstructuredDomain(domain) ;
           return ;
         }

         CContext* context = CContext::getCurrent() ;
         CContextServer* server=context->server ;

         if (domain->IsWritten(this->filename)) return;
         domain->checkAttributes();

         if (domain->isEmpty())
           if (SuperClass::type==MULTI_FILE) return ;

         std::vector<StdString> dim0, dim1;
         StdString domid     = (!domain->name.isEmpty())
                             ? domain->name.getValue() : domain->getId();
         StdString appendDomid  = (singleDomain) ? "" : "_"+domid ;


         StdString dimXid, dimYid ;

         switch (domain->type)
         {
           case CDomain::type_attr::curvilinear :
             dimXid     = StdString("x").append(appendDomid);
             dimYid     = StdString("y").append(appendDomid);
             break ;
           case CDomain::type_attr::regular :
             dimXid     = StdString("lon").append(appendDomid);
             dimYid     = StdString("lat").append(appendDomid);
             break;
           case CDomain::type_attr::unstructured :
             dimXid     = StdString("cell").append(appendDomid);
             break;
         }

         string lonid,latid,bounds_lonid,bounds_latid ;
/*
         StdString lonid_loc = (server->intraCommSize > 1)
                             ? StdString("lon").append(appendDomid).append("_local")
                             : lonid;
         StdString latid_loc = (server->intraCommSize > 1)
                             ? StdString("lat").append(appendDomid).append("_local")
                             : latid;
*/

         try
         {
           switch (SuperClass::type)
           {
              case (MULTI_FILE) :
              {
  //               if (domain->isEmpty()) return;

                 if (server->intraCommSize > 1)
                 {
  //                 SuperClassWriter::addDimension(lonid, domain->zoom_ni.getValue());
  //                 SuperClassWriter::addDimension(latid, domain->zoom_nj.getValue());
                 }

                 switch (domain->type)
                 {
                   case CDomain::type_attr::curvilinear :
                     dim0.push_back(dimYid); dim0.push_back(dimXid);
                     lonid = StdString("nav_lon").append(appendDomid);
                     latid = StdString("nav_lat").append(appendDomid);
                     break ;
                   case CDomain::type_attr::regular :
                     lonid = StdString("lon").append(appendDomid);
                     latid = StdString("lat").append(appendDomid);
                     dim0.push_back(dimYid);
                     dim1.push_back(dimXid);
                     break;
                   case CDomain::type_attr::unstructured :
                     lonid = StdString("lon").append(appendDomid);
                     latid = StdString("lat").append(appendDomid);
                     bounds_lonid=string("bounds_lon").append(appendDomid);
                     bounds_latid=string("bounds_lat").append(appendDomid);
                     dim0.push_back(dimXid);
                     break;
                 }

                 if (domain->type == CDomain::type_attr::unstructured)
                 {
                   SuperClassWriter::addDimension(dimXid, domain->nj_glo);
                 }
                 else
                 {
                   SuperClassWriter::addDimension(dimXid, domain->zoom_ni_srv);
                   SuperClassWriter::addDimension(dimYid, domain->zoom_nj_srv);
                 }

                 if (server->intraCommSize > 1)
                 {
                    if (domain->type != CDomain::type_attr::unstructured)
                    {
                      this->writeLocalAttributes(domain->zoom_ibegin_srv,
                                                 domain->zoom_ni_srv,
                                                 domain->zoom_jbegin_srv,
                                                 domain->zoom_nj_srv,
                                                 appendDomid);

                      if (singleDomain) this->writeLocalAttributes_IOIPSL(domain->zoom_ibegin_srv,
                                                 domain->zoom_ni_srv,
                                                 domain->zoom_jbegin_srv,
                                                 domain->zoom_nj_srv,
                                                 domain->ni_glo,domain->nj_glo,
                                                 server->intraCommRank,server->intraCommSize);
                   }
                 }

                 switch (domain->type)
                 {
                   case CDomain::type_attr::curvilinear :
                     SuperClassWriter::addVariable(latid, NC_FLOAT, dim0);
                     SuperClassWriter::addVariable(lonid, NC_FLOAT, dim0);
                     break ;
                    case CDomain::type_attr::regular :
                      SuperClassWriter::addVariable(latid, NC_FLOAT, dim0);
                      SuperClassWriter::addVariable(lonid, NC_FLOAT, dim1);
                      break ;
                    case CDomain::type_attr::unstructured :
                      SuperClassWriter::addVariable(latid, NC_FLOAT, dim0);
                      SuperClassWriter::addVariable(lonid, NC_FLOAT, dim0);
                 }

                 this->writeAxisAttributes(lonid, "X", "longitude", "Longitude", "degrees_east", domid);
                 this->writeAxisAttributes(latid, "Y", "latitude", "Latitude", "degrees_north", domid);

                 dim0.clear();
                 if (domain->type != CDomain::type_attr::unstructured) dim0.push_back(dimYid);
                 dim0.push_back(dimXid);


  // supress mask               if (server->intraCommSize > 1)
  // supress mask               {
  // supress mask                  SuperClassWriter::addVariable(maskid, NC_INT, dim0);
  // supress mask
  // supress mask                  this->writeMaskAttributes(maskid,
  // supress mask                     domain->data_dim.getValue()/*,
  // supress mask                     domain->data_ni.getValue(),
  // supress mask                     domain->data_nj.getValue(),
  // supress mask                     domain->data_ibegin.getValue(),
  // supress mask                     domain->data_jbegin.getValue()*/);
  // supress mask               }

                 //SuperClassWriter::setDefaultValue(maskid, &dvm);

                 SuperClassWriter::definition_end();

                 switch (domain->type)
                 {
                   case CDomain::type_attr::curvilinear :
                     SuperClassWriter::writeData(domain->latvalue_srv, latid, isCollective, 0);
                     SuperClassWriter::writeData(domain->lonvalue_srv, lonid, isCollective, 0);
                     break;
                   case CDomain::type_attr::regular :
                     CArray<double,1> lat = domain->latvalue_srv(Range(fromStart,toEnd,domain->zoom_ni_srv)) ;
                     SuperClassWriter::writeData(CArray<double,1>(lat.copy()), latid, isCollective, 0);
                     CArray<double,1> lon=domain->lonvalue_srv(Range(0,domain->zoom_ni_srv-1)) ;
                     SuperClassWriter::writeData(CArray<double,1>(lon.copy()), lonid, isCollective, 0);
                     break;
                 }
                 SuperClassWriter::definition_start();

                 break;
              }
              case (ONE_FILE) :
              {
                 SuperClassWriter::addDimension(dimXid, domain->zoom_ni.getValue());
                 SuperClassWriter::addDimension(dimYid, domain->zoom_nj.getValue());


                 switch (domain->type)
                 {
                   case CDomain::type_attr::curvilinear :
                     dim0.push_back(dimYid); dim0.push_back(dimXid);
                     lonid = StdString("nav_lon").append(appendDomid);
                     latid = StdString("nav_lat").append(appendDomid);
                     SuperClassWriter::addVariable(latid, NC_FLOAT, dim0);
                     SuperClassWriter::addVariable(lonid, NC_FLOAT, dim0);
                     break;

                   case CDomain::type_attr::regular :
                     dim0.push_back(dimYid);
                     dim1.push_back(dimXid);
                     lonid = StdString("lon").append(appendDomid);
                     latid = StdString("lat").append(appendDomid);
                     SuperClassWriter::addVariable(latid, NC_FLOAT, dim0);
                     SuperClassWriter::addVariable(lonid, NC_FLOAT, dim1);
                     break;
                 }
                 this->writeAxisAttributes
                    (lonid, "X", "longitude", "Longitude", "degrees_east", domid);
                 this->writeAxisAttributes
                    (latid, "Y", "latitude", "Latitude", "degrees_north", domid);


                 SuperClassWriter::definition_end();
                 switch (domain->type)
                 {
                   case CDomain::type_attr::curvilinear :
                   {
                     std::vector<StdSize> start(2) ;
                     std::vector<StdSize> count(2) ;
                     if (domain->isEmpty())
                     {
                       start[0]=0 ; start [1]=0 ;
                       count[0]=0 ; count[1]=0 ;
                     }
                     else
                     {
                       start[1]=domain->zoom_ibegin_srv-domain->zoom_ibegin.getValue() ; start [0]=domain->zoom_jbegin_srv-domain->zoom_jbegin.getValue() ;
                       count[1]=domain->zoom_ni_srv ; count[0]=domain->zoom_nj_srv ;
                     }

                     SuperClassWriter::writeData(domain->latvalue_srv, latid, isCollective, 0,&start,&count);
                     SuperClassWriter::writeData(domain->lonvalue_srv, lonid, isCollective, 0,&start,&count);
                     break;
                   }
                   case CDomain::type_attr::regular :
                   {
                     std::vector<StdSize> start(1) ;
                     std::vector<StdSize> count(1) ;
                     if (domain->isEmpty())
                     {
                       start[0]=0 ;
                       count[0]=0 ;
                       SuperClassWriter::writeData(domain->latvalue_srv, latid, isCollective, 0,&start,&count);
                       SuperClassWriter::writeData(domain->lonvalue_srv, lonid, isCollective, 0,&start,&count);                 }
                     else
                     {
                       start[0]=domain->zoom_jbegin_srv-domain->zoom_jbegin.getValue() ;
                       count[0]=domain->zoom_nj_srv ;
                       CArray<double,1> lat = domain->latvalue_srv(Range(fromStart,toEnd,domain->zoom_ni_srv)) ;
                       SuperClassWriter::writeData(CArray<double,1>(lat.copy()), latid, isCollective, 0,&start,&count);

                       start[0]=domain->zoom_ibegin_srv-domain->zoom_ibegin.getValue() ;
                       count[0]=domain->zoom_ni_srv ;
                       CArray<double,1> lon=domain->lonvalue_srv(Range(0,domain->zoom_ni_srv-1)) ;
                       SuperClassWriter::writeData(CArray<double,1>(lon.copy()), lonid, isCollective, 0,&start,&count);
                     }
                     break;
                   }
                 }
                 SuperClassWriter::definition_start();
                 break;
              }
              default :
                 ERROR("CNc4DataOutput::writeDomain(domain)",
                       << "[ type = " << SuperClass::type << "]"
                       << " not implemented yet !");
           }
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing the domain : ");
           msg.append(domid); msg.append("\n");
           msg.append("In the context : ");
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeDomain_(CDomain* domain)", << msg);
         }

         domain->addRelFile(this->filename);
      }

      void CNc4DataOutput::writeUnstructuredDomain(CDomain* domain)
      {
         CContext* context = CContext::getCurrent() ;
         CContextServer* server=context->server ;

         if (domain->IsWritten(this->filename)) return;
         domain->checkAttributes();

         if (domain->isEmpty())
           if (SuperClass::type==MULTI_FILE) return ;

         std::vector<StdString> dim0, dim1;
         StdString domid     = (!domain->name.isEmpty())
                             ? domain->name.getValue() : domain->getId();
         StdString appendDomid  = (singleDomain) ? "" : "_"+domid ;


         StdString dimXid = StdString("cell").append(appendDomid);
         StdString dimVertId = StdString("nvertex").append(appendDomid);

         string lonid,latid,bounds_lonid,bounds_latid ;

         try
         {
           switch (SuperClass::type)
           {
              case (MULTI_FILE) :
              {
                 lonid = StdString("lon").append(appendDomid);
                 latid = StdString("lat").append(appendDomid);
                 dim0.push_back(dimXid);

                 SuperClassWriter::addDimension(dimXid, domain->zoom_nj_srv);
                 SuperClassWriter::addVariable(latid, NC_FLOAT, dim0);
                 SuperClassWriter::addVariable(lonid, NC_FLOAT, dim0);

                 bounds_lonid = StdString("bounds_lon").append(appendDomid);
                 bounds_latid = StdString("bounds_lat").append(appendDomid);


                 this->writeAxisAttributes(lonid, "X", "longitude", "Longitude", "degrees_east", domid);
                 if (domain->hasBounds) SuperClassWriter::addAttribute("bounds",bounds_lonid, &lonid);
                 this->writeAxisAttributes(latid, "Y", "latitude", "Latitude", "degrees_north", domid);
                 if (domain->hasBounds) SuperClassWriter::addAttribute("bounds",bounds_latid, &latid);
                 if (domain->hasBounds) SuperClassWriter::addDimension(dimVertId, domain->nvertex);
                 dim0.clear();
                 if (domain->hasBounds)
                 {
                   dim0.push_back(dimXid);
                   dim0.push_back(dimVertId);
                   SuperClassWriter::addVariable(bounds_lonid, NC_FLOAT, dim0);
                   SuperClassWriter::addVariable(bounds_latid, NC_FLOAT, dim0);
                 }

                 dim0.clear();
                 dim0.push_back(dimXid);

                 SuperClassWriter::definition_end();

                 SuperClassWriter::writeData(domain->latvalue_srv, latid, isCollective, 0);
                 SuperClassWriter::writeData(domain->lonvalue_srv, lonid, isCollective, 0);

                 if (domain->hasBounds)
                 {
                   SuperClassWriter::writeData(domain->bounds_lon_srv, bounds_lonid, isCollective, 0);
                   SuperClassWriter::writeData(domain->bounds_lat_srv, bounds_latid, isCollective, 0);
                 }
                 SuperClassWriter::definition_start();
                 break ;
              }

              case (ONE_FILE) :
              {
                 lonid = StdString("lon").append(appendDomid);
                 latid = StdString("lat").append(appendDomid);
                 bounds_lonid = StdString("bounds_lon").append(appendDomid);
                 bounds_latid = StdString("bounds_lat").append(appendDomid);
                 dim0.push_back(dimXid);
                 SuperClassWriter::addDimension(dimXid, domain->nj_glo);
                 SuperClassWriter::addVariable(latid, NC_FLOAT, dim0);
                 SuperClassWriter::addVariable(lonid, NC_FLOAT, dim0);
                 this->writeAxisAttributes(lonid, "X", "longitude", "Longitude", "degrees_east", domid);
                 if (domain->hasBounds) SuperClassWriter::addAttribute("bounds",bounds_lonid, &lonid);
                 this->writeAxisAttributes(latid, "Y", "latitude", "Latitude", "degrees_north", domid);
                 if (domain->hasBounds) SuperClassWriter::addAttribute("bounds",bounds_latid, &latid);
                 if (domain->hasBounds) SuperClassWriter::addDimension(dimVertId, domain->nvertex);
                 dim0.clear();

                 if (domain->hasBounds)
                 {
                   dim0.push_back(dimXid);
                   dim0.push_back(dimVertId);
                   SuperClassWriter::addVariable(bounds_lonid, NC_FLOAT, dim0);
                   SuperClassWriter::addVariable(bounds_latid, NC_FLOAT, dim0);
                 }

                 SuperClassWriter::definition_end();

                 std::vector<StdSize> start(1), startBounds(2) ;
                 std::vector<StdSize> count(1), countBounds(2) ;
                 if (domain->isEmpty())
                 {
                   start[0]=0 ;
                   count[0]=0 ;
                   startBounds[1]=0 ;
                   countBounds[1]=domain->nvertex ;
                   startBounds[0]=0 ;
                   countBounds[0]=0 ;
                 }
                 else
                 {
                   start[0]=domain->zoom_jbegin_srv-domain->zoom_jbegin ;
                   count[0]=domain->zoom_nj_srv ;
                   startBounds[0]=domain->zoom_jbegin_srv-domain->zoom_jbegin ;
                   startBounds[1]=0 ;
                   countBounds[0]=domain->zoom_nj_srv ;
                   countBounds[1]=domain->nvertex ;
                 }
                 SuperClassWriter::writeData(domain->latvalue_srv, latid, isCollective, 0,&start,&count);
                 SuperClassWriter::writeData(domain->lonvalue_srv, lonid, isCollective, 0,&start,&count);
                 if (domain->hasBounds)
                 {
                   SuperClassWriter::writeData(domain->bounds_lon_srv, bounds_lonid, isCollective, 0,&startBounds,&countBounds);
                   SuperClassWriter::writeData(domain->bounds_lat_srv, bounds_latid, isCollective, 0,&startBounds,&countBounds);
                 }


                 SuperClassWriter::definition_start();

                 break;
              }
              default :
                 ERROR("CNc4DataOutput::writeDomain(domain)",
                       << "[ type = " << SuperClass::type << "]"
                       << " not implemented yet !");
           }
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing the domain : ");
           msg.append(domid); msg.append("\n");
           msg.append("In the context : ");
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeUnstructuredDomain(CDomain* domain)", << msg);
         }
         domain->addRelFile(this->filename);
      }
      //--------------------------------------------------------------

      void CNc4DataOutput::writeAxis_(CAxis* axis)
      {
         if (axis->IsWritten(this->filename)) return;
         axis->checkAttributes();
         StdSize zoom_size=axis->zoom_size.getValue() ;
         StdSize zoom_begin=axis->zoom_begin.getValue()-1 ;


         std::vector<StdString> dims;
         StdString axisid = (!axis->name.isEmpty())
                           ? axis->name.getValue() : axis->getId();
         try
         {
           SuperClassWriter::addDimension(axisid, zoom_size);
           dims.push_back(axisid);

           switch (SuperClass::type)
           {
              case (MULTI_FILE ) :
              {}
              case (ONE_FILE) :
              {
                 SuperClassWriter::addVariable(axisid, NC_FLOAT, dims);

                 SuperClassWriter::addAttribute("axis", StdString("Z"), &axisid);

                 if (!axis->standard_name.isEmpty())
                    SuperClassWriter::addAttribute
                       ("standard_name",  axis->standard_name.getValue(), &axisid);

                 if (!axis->long_name.isEmpty())
                    SuperClassWriter::addAttribute
                       ("long_name", axis->long_name.getValue(), &axisid);

                 if (!axis->unit.isEmpty())
                    SuperClassWriter::addAttribute
                       ("units", axis->unit.getValue(), &axisid);

                if (!axis->positive.isEmpty())
                  if (axis->positive==CAxis::positive_attr::up) SuperClassWriter::addAttribute("positive", string("up"), &axisid);
                  else   SuperClassWriter::addAttribute("positive", string("down"), &axisid);

                 SuperClassWriter::definition_end();

                 CArray<double,1> axis_value(zoom_size) ;
                 for(StdSize i = 0 ; i < zoom_size ; i++) axis_value(i)=axis->value(i+zoom_begin) ;
                 SuperClassWriter::writeData(axis_value, axisid, isCollective, 0);

                 SuperClassWriter::definition_start();

                 break;
              }
              default :
                 ERROR("CNc4DataOutput::writeDomain(domain)",
                       << "[ type = " << SuperClass::type << "]"
                       << " not implemented yet !");
           }
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing the axis : ");
           msg.append(axisid); msg.append("\n");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeAxis_(CAxis* axis)", << msg);
         }
         axis->addRelFile(this->filename);
     }

     void CNc4DataOutput::writeTimeDimension_(void)
     {
       try
       {
        SuperClassWriter::addDimension("time_counter");
        SuperClassWriter::addDimension("time_bounds", 2);
       }
       catch (CNetCdfException& e)
       {
         StdString msg("On writing time dimension : time_couter, time_bounds \n");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent() ;
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::writeTimeDimension_(void)", << msg);
       }
     }
      //--------------------------------------------------------------

      void CNc4DataOutput::writeField_(CField* field)
      {
         CContext* context = CContext::getCurrent() ;
         CContextServer* server=context->server ;

         std::vector<StdString> dims, coodinates;
         CGrid* grid = field->grid;
         CDomain* domain = grid->domain;

         if (domain->isEmpty())
           if (SuperClass::type==MULTI_FILE) return ;

         StdString timeid    = StdString("time_counter");
         StdString domid     = (!domain->name.isEmpty())
                             ? domain->name.getValue() : domain->getId();
         StdString appendDomid  = (singleDomain) ? "" : "_"+domid ;

//         bool isCurvilinear = domain->isCurvilinear ;
//         bool isCurvilinear = (domain->type == CDomain::type_attr::curvilinear) ;

         StdString dimXid,dimYid ;

         switch (domain->type)
         {
           case CDomain::type_attr::curvilinear :
             dimXid     = StdString("x").append(appendDomid);
             dimYid     = StdString("y").append(appendDomid);
             break ;
           case CDomain::type_attr::regular :
             dimXid     = StdString("lon").append(appendDomid);
             dimYid     = StdString("lat").append(appendDomid);
             break ;
           case CDomain::type_attr::unstructured :
             dimXid     = StdString("cell").append(appendDomid);
             break ;
        }

/*
         StdString lonid_loc = (server->intraCommSize > 1)
                             ? StdString("lon").append(appendDomid).append("_local")
                             : lonid;
         StdString latid_loc = (server->intraCommSize > 1)
                             ? StdString("lat").append(appendDomid).append("_local")
                             : latid;
*/
         StdString fieldid   = (!field->name.isEmpty())
                             ? field->name.getValue() : field->getBaseFieldReference()->getId();

//         unsigned int ssize = domain->zoom_ni_loc.getValue() * domain->zoom_nj_loc.getValue();
//         bool isCurvilinear = (domain->lonvalue.getValue()->size() == ssize);
//          bool isCurvilinear = domain->isCurvilinear ;

         nc_type type ;
         if (field->prec.isEmpty()) type =  NC_FLOAT ;
         else
         {
           if (field->prec==2) type = NC_SHORT ;
           else if (field->prec==4)  type =  NC_FLOAT ;
           else if (field->prec==8)   type =  NC_DOUBLE ;
         }

         bool wtime   = !(!field->operation.isEmpty() && field->foperation->timeType() == func::CFunctor::once);

         if (wtime)
         {

            //StdOStringStream oss;
           // oss << "time_" << field->operation.getValue()
           //     << "_" << field->getRelFile()->output_freq.getValue();
          //oss
            if (field->foperation->timeType() == func::CFunctor::instant) coodinates.push_back(string("time_instant"));
            else if (field->foperation->timeType() == func::CFunctor::centered) coodinates.push_back(string("time_centered"));
            dims.push_back(timeid);
         }

         if (!grid->axis_ref.isEmpty())
         {
            CAxis* axis = grid->axis ;
            StdString axisid = (!axis->name.isEmpty()) ? axis->name.getValue() : axis->getId();
            dims.push_back(axisid);
            coodinates.push_back(axisid);
         }

         switch (domain->type)
         {
           case CDomain::type_attr::curvilinear :
             coodinates.push_back(StdString("nav_lon").append(appendDomid));
             coodinates.push_back(StdString("nav_lat").append(appendDomid));
             break;
           case CDomain::type_attr::regular :
           case CDomain::type_attr::unstructured :
            coodinates.push_back(StdString("lon").append(appendDomid));
            coodinates.push_back(StdString("lat").append(appendDomid));
             break;
         }

         if ( domain->type == CDomain::type_attr::curvilinear || domain->type == CDomain::type_attr::regular)dims.push_back(dimYid);
         dims.push_back(dimXid);

         try
         {
           SuperClassWriter::addVariable(fieldid, type, dims);

           if (!field->standard_name.isEmpty())
              SuperClassWriter::addAttribute
                 ("standard_name",  field->standard_name.getValue(), &fieldid);

           if (!field->long_name.isEmpty())
              SuperClassWriter::addAttribute
                 ("long_name", field->long_name.getValue(), &fieldid);

           if (!field->unit.isEmpty())
              SuperClassWriter::addAttribute
                 ("units", field->unit.getValue(), &fieldid);

            if (!field->valid_min.isEmpty())
              SuperClassWriter::addAttribute
                 ("valid_min", field->valid_min.getValue(), &fieldid);

           if (!field->valid_max.isEmpty())
              SuperClassWriter::addAttribute
                 ("valid_max", field->valid_max.getValue(), &fieldid);

            if (!field->scale_factor.isEmpty())
              SuperClassWriter::addAttribute
                 ("scale_factor", field->scale_factor.getValue(), &fieldid);

             if (!field->add_offset.isEmpty())
              SuperClassWriter::addAttribute
                 ("add_offset", field->add_offset.getValue(), &fieldid);

           SuperClassWriter::addAttribute
                 ("online_operation", field->operation.getValue(), &fieldid);

          // write child variables as attributes


           vector<CVariable*> listVars = field->getAllVariables() ;
           for (vector<CVariable*>::iterator it = listVars.begin() ;it != listVars.end(); it++) writeAttribute_(*it, fieldid) ;


           if (wtime)
           {
              CDuration duration ;

              duration=CDuration::FromString(field->freq_op) ;
              duration.solveTimeStep(*(context->calendar));
              SuperClassWriter::addAttribute("interval_operation", duration.toString(), &fieldid);

              duration=CDuration::FromString(field->getRelFile()->output_freq) ;
              duration.solveTimeStep(*(context->calendar));
              SuperClassWriter::addAttribute("interval_write", duration.toString(), &fieldid);
           }

           if (!field->default_value.isEmpty())
           {
              double default_value = field->default_value.getValue();
              float fdefault_value = (float)default_value;
              if (type == NC_DOUBLE)
                 SuperClassWriter::setDefaultValue(fieldid, &default_value);
              else
                 SuperClassWriter::setDefaultValue(fieldid, &fdefault_value);
           }
           else
           {
              double * default_value = NULL;
              SuperClassWriter::setDefaultValue(fieldid, default_value);
           }

           {  // Ecriture des coordonnées

              StdString coordstr; //boost::algorithm::join(coodinates, " ")
              std::vector<StdString>::iterator
                 itc = coodinates.begin(), endc = coodinates.end();

              for (; itc!= endc; itc++)
              {
                 StdString & coord = *itc;
                 if (itc+1 != endc)
                       coordstr.append(coord).append(" ");
                 else  coordstr.append(coord);
              }

              SuperClassWriter::addAttribute("coordinates", coordstr, &fieldid);

           }
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing field : ");
           msg.append(fieldid); msg.append("\n");
           msg.append("In the context : ");
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeField_(CField* field)", << msg);
         }
      }

      //--------------------------------------------------------------

      void CNc4DataOutput::writeFile_ (CFile* file)
      {
         StdString filename = (!file->name.isEmpty())
                            ? file->name.getValue() : file->getId();
         StdString description = (!file->description.isEmpty())
                               ? file->description.getValue()
                               : StdString("Created by xios");
         try
         {
           this->writeFileAttributes(filename, description,
                                     StdString ("CF-1.1"),
                                     StdString("An IPSL model"),
                                     this->getTimeStamp());
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing file : ");
           msg.append(filename); msg.append("\n");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeFile_ (CFile* file)", << msg);
         }
         if (file->nbDomain==1) singleDomain=true ;
         else singleDomain=false ;
      }

      void CNc4DataOutput::writeAttribute_ (CVariable* var, const string& fieldId)
      {
        string name ;
        if (!var->name.isEmpty()) name=var->name ;
        else if (var->hasId()) name=var->getId() ;
        else return ;

        try
        {
          if (var->getVarType()==CVariable::t_int) addAttribute(name,var->getData<int>(),&fieldId) ;
          else if (var->getVarType()==CVariable::t_short_int) addAttribute(name,var->getData<short int>(),&fieldId) ;
          else if (var->getVarType()==CVariable::t_long_int) addAttribute(name,var->getData<long int>(),&fieldId) ;
          else if (var->getVarType()==CVariable::t_float) addAttribute(name,var->getData<float>(),&fieldId) ;
          else if (var->getVarType()==CVariable::t_double) addAttribute(name,var->getData<double>(),&fieldId) ;
          else addAttribute(name,var->getData<string>(),&fieldId) ;
        }
       catch (CNetCdfException& e)
       {
         StdString msg("On writing attributes of variable with name : ");
         msg.append(name); msg.append("in the field "); msg.append(fieldId); msg.append("\n");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent() ;
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::writeAttribute_ (CVariable* var, const string& fieldId)", << msg);
       }
     }

     void CNc4DataOutput::writeAttribute_ (CVariable* var)
     {
        string name ;
        if (!var->name.isEmpty()) name=var->name ;
        else if (var->hasId()) name=var->getId() ;
        else return ;
        try
        {
          if (var->getVarType()==CVariable::t_int) addAttribute(name,var->getData<int>()) ;
          else if (var->getVarType()==CVariable::t_short_int) addAttribute(name,var->getData<short int>()) ;
          else if (var->getVarType()==CVariable::t_long_int) addAttribute(name,var->getData<long int>()) ;
          else if (var->getVarType()==CVariable::t_float) addAttribute(name,var->getData<float>()) ;
          else if (var->getVarType()==CVariable::t_double) addAttribute(name,var->getData<double>()) ;
          else addAttribute(name,var->getData<string>()) ;
        }
       catch (CNetCdfException& e)
       {
         StdString msg("On writing attributes of variable with name : ");
         msg.append(name); msg.append("\n");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent() ;
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::writeAttribute_ (CVariable* var)", << msg);
       }
     }

      void CNc4DataOutput::syncFile_ (void)
      {
        try
        {
          SuperClassWriter::sync() ;
        }
        catch (CNetCdfException& e)
        {
         StdString msg("On synchronizing the write among processes");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent() ;
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::syncFile_ (void)", << msg);
        }
      }

      void CNc4DataOutput::closeFile_ (void)
      {
        try
        {
          SuperClassWriter::close() ;
        }
        catch (CNetCdfException& e)
        {
         StdString msg("On closing file");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent() ;
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::syncFile_ (void)", << msg);
        }

      }

      //---------------------------------------------------------------

      StdString CNc4DataOutput::getTimeStamp(void) const
      {
         const int buffer_size = 100;
         time_t rawtime;
         struct tm * timeinfo = NULL;
         char buffer [buffer_size];

         time ( &rawtime );
         timeinfo = localtime ( &rawtime );
         strftime (buffer, buffer_size, "%Y-%b-%d %H:%M:%S %Z", timeinfo);

         return (StdString(buffer));
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeFieldData_ (CField*  field)
      {
         CContext* context = CContext::getCurrent() ;
//          if (field->getRelFile()->isSyncTime()) SuperClassWriter::sync() ;
         CContextServer* server=context->server ;

         CGrid* grid = field->grid ;
         CDomain* domain = grid->domain ;

         if(SuperClass::type==MULTI_FILE || !isCollective) if (domain->isEmpty()) return;


         StdString fieldid   = (!field->name.isEmpty())
                             ? field->name.getValue()
                             : field->getBaseFieldReference()->getId();

         StdOStringStream oss;
         string timeAxisId ;
         if (field->foperation->timeType() == func::CFunctor::instant)  timeAxisId="time_instant" ;
         else if (field->foperation->timeType() == func::CFunctor::centered)  timeAxisId="time_centered" ;

         StdString timeBoundId("time_counter_bounds");

         StdString timeAxisBoundId;
         if (field->foperation->timeType() == func::CFunctor::instant)  timeAxisBoundId="time_instant_bounds" ;
         else if (field->foperation->timeType() == func::CFunctor::centered)  timeAxisBoundId="time_centered_bounds" ;

         CArray<double,1> time_data(1) ;
         CArray<double,1> time_counter(1) ;
         CArray<double,1> time_counter_bound(2);
         CArray<double,1> time_data_bound(2);

        bool wtime   = !(!field->operation.isEmpty() && (field->foperation->timeType() == func::CFunctor::once));

        if (wtime)
        {
          time_counter(0)= (Time(*field->last_Write_srv) + Time(*field->lastlast_Write_srv)) / 2;
          if (field->foperation->timeType() == func::CFunctor::instant)
            time_data(0) = Time(*field->last_Write_srv);
          else if (field->foperation->timeType() == func::CFunctor::centered) time_data(0) = time_counter(0);

          time_counter_bound(0) = Time(*field->lastlast_Write_srv);
          time_counter_bound(1) = Time(*field->last_Write_srv);
          if (field->foperation->timeType() == func::CFunctor::instant)
            time_data_bound(0) = time_data_bound(1) = Time(*field->last_Write_srv);
          else if (field->foperation->timeType() == func::CFunctor::centered)
          {
            time_data_bound(0) = time_counter_bound(0);
            time_data_bound(1) = time_counter_bound(1);
          }
         }

         bool isRoot ;
         if (server->intraCommRank==0) isRoot=true ;
         else isRoot=false ;

         if (!field->scale_factor.isEmpty() || !field->add_offset.isEmpty())
         {
           double scaleFactor=1. ;
           double addOffset=0. ;
           if (!field->scale_factor.isEmpty()) scaleFactor=field->scale_factor ;
           if (!field->add_offset.isEmpty()) addOffset=field->add_offset ;
           field->scaleFactorAddOffset(scaleFactor,addOffset) ;
         }

         try
         {
           if (grid->hasAxis()) // 3D
           {
              CAxis* axis = grid->axis ;
              CArray<double,3> field_data3D(domain->zoom_ni_srv,domain->zoom_nj_srv,axis->zoom_size) ;
              if (!field->default_value.isEmpty()) field_data3D = field->default_value ;

              field->outputField(field_data3D);

              if (!field->prec.isEmpty() && field->prec==2) field_data3D=round(field_data3D) ;

              switch (SuperClass::type)
             {
                case (MULTI_FILE) :
                {
                   SuperClassWriter::writeData(field_data3D, fieldid, isCollective, field->getNStep()-1);
                   if (wtime)
                   {
                     SuperClassWriter::writeData(time_data, timeAxisId, isCollective, field->getNStep()-1);
                     SuperClassWriter::writeData(time_counter, string("time_counter"), isCollective, field->getNStep()-1);
                     SuperClassWriter::writeData(time_counter_bound, timeBoundId, isCollective, field->getNStep()-1);
                     SuperClassWriter::writeData(time_data_bound, timeAxisBoundId, isCollective, field->getNStep()-1);
                   }
                   break ;
                }
                case (ONE_FILE) :
                {
                   std::vector<StdSize> start(3) ;
                   std::vector<StdSize> count(3) ;
                   if (domain->isEmpty())
                   {
                     start[0]=0 ; start[1]=0 ; start[2]=0 ;
                     count[0]=0 ; count[1]=0 ; start[2]=0 ;
                   }
                   else
                   {
  //                 start[2]=domain->zoom_ibegin_loc.getValue()-domain->zoom_ibegin.getValue() ; start [1]=domain->zoom_jbegin_loc.getValue()-domain->zoom_jbegin.getValue() ; start[0]=0 ;
                     start[2]=domain->zoom_ibegin_srv-domain->zoom_ibegin.getValue() ; start [1]=domain->zoom_jbegin_srv-domain->zoom_jbegin.getValue() ; start[0]=0 ;
                     count[2]=domain->zoom_ni_srv ; count[1]=domain->zoom_nj_srv ; count[0] = axis->zoom_size.getValue();
                   }
                   SuperClassWriter::writeData(field_data3D, fieldid, isCollective, field->getNStep()-1,&start,&count );
                   if (wtime)
                   {
                     SuperClassWriter::writeTimeAxisData(time_data, timeAxisId, isCollective, field->getNStep()-1,isRoot );
                     SuperClassWriter::writeTimeAxisData(time_counter, string("time_counter"), isCollective, field->getNStep()-1,isRoot );
                     SuperClassWriter::writeTimeAxisData(time_counter_bound, timeBoundId, isCollective, field->getNStep()-1, isRoot );
                     SuperClassWriter::writeTimeAxisData(time_data_bound, timeAxisBoundId, isCollective, field->getNStep()-1, isRoot);
                   }
                   break;
                }
              }

           }
           else // 2D
           {
              CArray<double,2> field_data2D(domain->zoom_ni_srv,domain->zoom_nj_srv) ;
              if (!field->default_value.isEmpty()) field_data2D = field->default_value ;
              field->outputField(field_data2D);
              if (!field->prec.isEmpty() && field->prec==2) field_data2D=round(field_data2D) ;
              switch (SuperClass::type)
              {
                case (MULTI_FILE) :
                {
                  SuperClassWriter::writeData(field_data2D, fieldid, isCollective, field->getNStep()-1);
                  if (wtime)
                  {
                    SuperClassWriter::writeData(time_data, timeAxisId, isCollective, field->getNStep()-1);
                    SuperClassWriter::writeData(time_counter, string("time_counter"), isCollective, field->getNStep()-1);
                    SuperClassWriter::writeData(time_counter_bound, timeBoundId, isCollective, field->getNStep()-1);
                    SuperClassWriter::writeData(time_data_bound, timeAxisBoundId, isCollective, field->getNStep()-1);
                  }
                  break;
                }
                case (ONE_FILE) :
                {
                   std::vector<StdSize> start(2) ;
                   std::vector<StdSize> count(2) ;
                   if (domain->isEmpty())
                   {
                     start[0]=0 ; start[1]=0 ;
                     count[0]=0 ; count[1]=0 ;
                   }
                   else
                   {
                     start[1]=domain->zoom_ibegin_srv-domain->zoom_ibegin.getValue() ; start[0]=domain->zoom_jbegin_srv-domain->zoom_jbegin.getValue() ;
                     count[1]=domain->zoom_ni_srv ; count[0]=domain->zoom_nj_srv ;
                   }

                   SuperClassWriter::writeData(field_data2D, fieldid, isCollective, field->getNStep()-1,&start,&count);
                   if (wtime)
                   {
                     SuperClassWriter::writeTimeAxisData(time_data, timeAxisId, isCollective, field->getNStep()-1,isRoot);
                     SuperClassWriter::writeTimeAxisData(time_counter, string("time_counter"), isCollective, field->getNStep()-1,isRoot);
                     SuperClassWriter::writeTimeAxisData(time_counter_bound, timeBoundId, isCollective, field->getNStep()-1, isRoot);
                     SuperClassWriter::writeTimeAxisData(time_data_bound, timeAxisBoundId, isCollective, field->getNStep()-1, isRoot);
                   }
                   break;

                }
              }
           }
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing field data: ");
           msg.append(fieldid); msg.append("\n");
           msg.append("In the context : ");
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeFieldData_ (CField*  field)", << msg);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeTimeAxis_
                  (CField*    field,
                   const boost::shared_ptr<CCalendar> cal)
      {
         StdOStringStream oss;

//         if (field->operation.getValue().compare("once") == 0) return ;
         if (field->foperation->timeType() == func::CFunctor::once) return ;

//         oss << "time_" << field->operation.getValue()
//             << "_" << field->getRelFile()->output_freq.getValue();

//         StdString axisid = oss.str();
//         if (field->foperation->timeType() == func::CFunctor::centered) axisid="time_centered" ;
//         else if (field->foperation->timeType() == func::CFunctor::instant) axisid="time_instant" ;

         StdString axisid("time_centered") ;
         StdString axisBoundId("time_centered_bounds");
         StdString timeid("time_counter");
         StdString timeBoundId("time_bounds");

         if (field->foperation->timeType() == func::CFunctor::instant)
         {
            axisid = "time_instant";
            axisBoundId = "time_instant_bounds";
         }

         try
         {
          // Adding time_instant or time_centered
           std::vector<StdString> dims;
           dims.push_back(timeid);
           if (!SuperClassWriter::varExist(axisid))
           {
              SuperClassWriter::addVariable(axisid, NC_DOUBLE, dims);

              CDate timeOrigin=cal->getTimeOrigin() ;
              StdOStringStream oss2;
  //            oss2<<initDate.getYear()<<"-"<<initDate.getMonth()<<"-"<<initDate.getDay()<<" "
  //                <<initDate.getHour()<<"-"<<initDate.getMinute()<<"-"<<initDate.getSecond() ;
              StdString strInitdate=oss2.str() ;
              StdString strTimeOrigin=timeOrigin.toString() ;
              this->writeTimeAxisAttributes
                 (axisid, cal->getType(),
                  StdString("seconds since ").append(strTimeOrigin),
                  strTimeOrigin, axisBoundId);
           }

           // Adding time_instant_bounds or time_centered_bounds variables
           if (!SuperClassWriter::varExist(axisBoundId))
           {
              dims.clear() ;
              dims.push_back(timeid);
              dims.push_back(timeBoundId);
              SuperClassWriter::addVariable(axisBoundId, NC_DOUBLE, dims);
           }

           // Adding time_counter
           axisid = "time_counter" ;
           axisBoundId = "time_counter_bounds" ;
           dims.clear() ;
           dims.push_back(timeid);
           if (!SuperClassWriter::varExist(axisid))
           {
              SuperClassWriter::addVariable(axisid, NC_DOUBLE, dims);
              SuperClassWriter::addAttribute("axis", string("T"), &axisid);
              CDate timeOrigin=cal->getTimeOrigin() ;
              StdString strTimeOrigin=timeOrigin.toString() ;

              this->writeTimeAxisAttributes
                 (axisid, cal->getType(),
                  StdString("seconds since ").append(strTimeOrigin),
                  strTimeOrigin, axisBoundId);
           }

           // Adding time_counter_bound dimension
           if (!SuperClassWriter::varExist(axisBoundId))
           {
              dims.clear();
              dims.push_back(timeid);
              dims.push_back(timeBoundId);
              SuperClassWriter::addVariable(axisBoundId, NC_DOUBLE, dims);
           }
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing time axis data: ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeTimeAxis_ (CField*    field, \
                  const boost::shared_ptr<CCalendar> cal)", << msg);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeTimeAxisAttributes(const StdString & axis_name,
                                                   const StdString & calendar,
                                                   const StdString & units,
                                                   const StdString & time_origin,
                                                   const StdString & time_bounds,
                                                   const StdString & standard_name,
                                                   const StdString & long_name,
                                                   const StdString & title)
      {
         try
         {
           SuperClassWriter::addAttribute("standard_name", standard_name, &axis_name);
           SuperClassWriter::addAttribute("long_name",     long_name    , &axis_name);
           SuperClassWriter::addAttribute("title",         title        , &axis_name);
           SuperClassWriter::addAttribute("calendar",      calendar     , &axis_name);
           SuperClassWriter::addAttribute("units",         units        , &axis_name);
           SuperClassWriter::addAttribute("time_origin",   time_origin  , &axis_name);
           SuperClassWriter::addAttribute("bounds",        time_bounds  , &axis_name);
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing time axis Attribute: ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeTimeAxisAttributes(const StdString & axis_name, \
                                                   const StdString & calendar,\
                                                   const StdString & units, \
                                                   const StdString & time_origin, \
                                                   const StdString & time_bounds, \
                                                   const StdString & standard_name, \
                                                   const StdString & long_name, \
                                                   const StdString & title)", << msg);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeAxisAttributes(const StdString & axis_name,
                                               const StdString & axis,
                                               const StdString & standard_name,
                                               const StdString & long_name,
                                               const StdString & units,
                                               const StdString & nav_model)
      {
         try
         {
          SuperClassWriter::addAttribute("axis"         , axis         , &axis_name);
          SuperClassWriter::addAttribute("standard_name", standard_name, &axis_name);
          SuperClassWriter::addAttribute("long_name"    , long_name    , &axis_name);
          SuperClassWriter::addAttribute("units"        , units        , &axis_name);
          SuperClassWriter::addAttribute("nav_model"    , nav_model    , &axis_name);
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing Axis Attribute: ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeAxisAttributes(const StdString & axis_name, \
                                               const StdString & axis, \
                                               const StdString & standard_name, \
                                               const StdString & long_name, \
                                               const StdString & units, \
                                               const StdString & nav_model)", << msg);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeLocalAttributes
         (int ibegin, int ni, int jbegin, int nj, StdString domid)
      {
        try
        {
         SuperClassWriter::addAttribute(StdString("ibegin").append(domid), ibegin);
         SuperClassWriter::addAttribute(StdString("ni"    ).append(domid), ni);
         SuperClassWriter::addAttribute(StdString("jbegin").append(domid), jbegin);
         SuperClassWriter::addAttribute(StdString("nj"    ).append(domid), nj);
        }
        catch (CNetCdfException& e)
        {
           StdString msg("On writing Local Attributes: ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeLocalAttributes \
                  (int ibegin, int ni, int jbegin, int nj, StdString domid)", << msg);
        }

      }

     void CNc4DataOutput::writeLocalAttributes_IOIPSL
         (int ibegin, int ni, int jbegin, int nj, int ni_glo, int nj_glo, int rank, int size)
      {
         CArray<int,1> array(2) ;

         try
         {
           SuperClassWriter::addAttribute("DOMAIN_number_total",size ) ;
           SuperClassWriter::addAttribute("DOMAIN_number", rank) ;
           array=1,2 ;
           SuperClassWriter::addAttribute("DOMAIN_dimensions_ids",array) ;
           array=ni_glo,nj_glo ;
           SuperClassWriter::addAttribute("DOMAIN_size_global", array) ;
           array=ni,nj ;
           SuperClassWriter::addAttribute("DOMAIN_size_local", array) ;
           array=ibegin,jbegin ;
           SuperClassWriter::addAttribute("DOMAIN_position_first", array) ;
           array=ibegin+ni-1,jbegin+nj-1 ;
           SuperClassWriter::addAttribute("DOMAIN_position_last",array) ;
           array=0,0 ;
           SuperClassWriter::addAttribute("DOMAIN_halo_size_start", array) ;
           SuperClassWriter::addAttribute("DOMAIN_halo_size_end", array);
           SuperClassWriter::addAttribute("DOMAIN_type",string("box")) ;
  /*
           SuperClassWriter::addAttribute("DOMAIN_DIM_N001",string("x")) ;
           SuperClassWriter::addAttribute("DOMAIN_DIM_N002",string("y")) ;
           SuperClassWriter::addAttribute("DOMAIN_DIM_N003",string("axis_A")) ;
           SuperClassWriter::addAttribute("DOMAIN_DIM_N004",string("time_counter")) ;
  */
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing Local Attributes IOI PSL \n");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeLocalAttributes_IOIPSL \
                  (int ibegin, int ni, int jbegin, int nj, int ni_glo, int nj_glo, int rank, int size)", << msg);
         }
      }
      //---------------------------------------------------------------

      void CNc4DataOutput:: writeFileAttributes(const StdString & name,
                                                const StdString & description,
                                                const StdString & conventions,
                                                const StdString & production,
                                                const StdString & timeStamp)
      {
         try
         {
           SuperClassWriter::addAttribute("name"       , name);
           SuperClassWriter::addAttribute("description", description);
           SuperClassWriter::addAttribute("conventions", conventions);
           SuperClassWriter::addAttribute("production" , production);
           SuperClassWriter::addAttribute("timeStamp"  , timeStamp);
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing File Attributes \n ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput:: writeFileAttributes(const StdString & name, \
                                                const StdString & description, \
                                                const StdString & conventions, \
                                                const StdString & production, \
                                                const StdString & timeStamp)", << msg);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeMaskAttributes(const StdString & mask_name,
                                               int data_dim,
                                               int data_ni,
                                               int data_nj,
                                               int data_ibegin,
                                               int data_jbegin)
      {
         try
         {
           SuperClassWriter::addAttribute("data_dim"   , data_dim   , &mask_name);
           SuperClassWriter::addAttribute("data_ni"    , data_ni    , &mask_name);
           SuperClassWriter::addAttribute("data_nj"    , data_nj    , &mask_name);
           SuperClassWriter::addAttribute("data_ibegin", data_ibegin, &mask_name);
           SuperClassWriter::addAttribute("data_jbegin", data_jbegin, &mask_name);
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing Mask Attributes \n ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeMaskAttributes(const StdString & mask_name, \
                                               int data_dim, \
                                               int data_ni, \
                                               int data_nj, \
                                               int data_ibegin, \
                                               int data_jbegin)", << msg);
         }
      }

      ///--------------------------------------------------------------

} // namespace xios
