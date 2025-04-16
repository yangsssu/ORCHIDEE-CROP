#ifndef __XIOS_OPERATOR_EXPR_HPP__
#define __XIOS_OPERATOR_EXPR_HPP__

#include <map>
#include <string>
#include <cmath>
#include "exception.hpp"
#include "array_new.hpp"

using namespace std ;

namespace xios
{
  class COperatorExpr
  {
    public: 
    typedef double (*functionScalar)(double) ;
    typedef double (*functionScalarScalar)(double, double) ;
    typedef CArray<double,1> (*functionField)(const CArray<double,1>&) ;
    typedef CArray<double,1> (*functionFieldField)(const CArray<double,1>&, const CArray<double,1>&) ;
    typedef CArray<double,1> (*functionFieldScalar)(const CArray<double,1>&, double) ;
    typedef CArray<double,1> (*functionScalarField)(double, const CArray<double,1>&) ;
    
    COperatorExpr(void)
    {
      opScalar[string("neg")]=neg_s ;
      opScalar[string("cos")]=cos_s ;
      opScalar[string("sin")]=sin_s ;
      opScalar[string("tan")]=tan_s ;
      opScalar[string("exp")]=exp_s ;
      opScalar[string("log")]=log_s ;
      opScalar[string("log10")]=log10_s ;
      opScalar[string("sqrt")]=sqrt_s ;
 
      opScalarScalar[string("add")]=add_ss ;
      opScalarScalar[string("minus")]=minus_ss ;
      opScalarScalar[string("mult")]=mult_ss ;
      opScalarScalar[string("div")]=div_ss ;
      opScalarScalar[string("pow")]=pow_ss ;

      opField[string("neg")]=neg_f ;
      opField[string("cos")]=cos_f ;
      opField[string("sin")]=sin_f ;
      opField[string("tan")]=tan_f ;
      opField[string("exp")]=exp_f ;
      opField[string("log")]=log_f ;
      opField[string("log10")]=log10_f ;
      opField[string("sqrt")]=sqrt_f ;
 
      opFieldField[string("add")]=add_ff ;
      opFieldField[string("minus")]=minus_ff ;
      opFieldField[string("mult")]=mult_ff ;
      opFieldField[string("div")]=div_ff ;
      opFieldField[string("pow")]=pow_ff ;

      opFieldScalar[string("add")]=add_fs ;
      opFieldScalar[string("minus")]=minus_fs ;
      opFieldScalar[string("mult")]=mult_fs ;
      opFieldScalar[string("div")]=div_fs ;
      opFieldScalar[string("pow")]=pow_fs ;
      
      opScalarField[string("add")]=add_sf ;
      opScalarField[string("minus")]=minus_sf ;
      opScalarField[string("mult")]=mult_sf ;
      opScalarField[string("div")]=div_sf ;
    }    

    functionScalar getOpScalar(const string& id)
    {
      map<string,double (*)(double)>::iterator it ;
      it=opScalar.find(id) ;
      if (it==opScalar.end()) ERROR("double (*)(double) COperatorExpr::getOpScalar(const string& id)",<<"unknown operator : "<<id) 
      return it->second ;
    }
  
    functionScalarScalar getOpScalarScalar(const string& id)
    {
      map<string,double (*)(double,double)>::iterator it ;
      it=opScalarScalar.find(id) ;
      if (it==opScalarScalar.end()) ERROR("double (*)(double) COperatorExpr::getOpScalarScalar(const string& id)",<<"unknown operator : "<<id)
      return it->second ;    
    }
    
    functionField getOpField(const string& id)
    {
      map<string,functionField>::iterator it ;
      it=opField.find(id) ;
      if (it==opField.end()) ERROR("functionField COperatorExpr::getOpField(const string& id)",<<"unknown operator : "<<id)
      return it->second ;    
    }
 
    functionFieldField getOpFieldField(const string& id)
    {
      map<string,functionFieldField>::iterator it ;
      it=opFieldField.find(id) ;
      if (it==opFieldField.end()) ERROR("dfunctionFieldField COperatorExpr::getOpFieldField(const string& id)",<<"unknown operator : "<<id)
      return it->second ;    
    }
    
    functionFieldScalar getOpFieldScalar(const string& id)
    {
      map<string,functionFieldScalar>::iterator it ;
      it=opFieldScalar.find(id) ;
      if (it==opFieldScalar.end()) ERROR("functionFieldScalar COperatorExpr::getOpFieldScalar(const string& id)",<<"unknown operator : "<<id)
      return it->second ;    
    }
    
    functionScalarField getOpScalarField(const string& id)
    {
      map<string,functionScalarField>::iterator it ;
      it=opScalarField.find(id) ;
      if (it==opScalarField.end()) ERROR("functionScalarField COperatorExpr::getOpFieldField(const string& id)",<<"unknown operator : "<<id)
      return it->second ;    
    }
    
    map<string,functionScalar> opScalar ;
    map<string,functionScalarScalar> opScalarScalar ;
    map<string,functionField> opField ;
    map<string,functionFieldField> opFieldField ;
    map<string,functionFieldScalar> opFieldScalar ;
    map<string,functionScalarField> opScalarField ;

    static inline double neg_s(double x)   {return -x;}
    static inline double cos_s(double x)   {return std::cos(x);}
    static inline double sin_s(double x)   {return std::sin(x);}
    static inline double tan_s(double x)   {return std::tan(x);}
    static inline double exp_s(double x)   {return std::exp(x);}
    static inline double log_s(double x)   {return std::log(x);}
    static inline double log10_s(double x) {return std::log10(x);}
    static inline double sqrt_s(double x)  {return std::sqrt(x);}
  
    static inline double add_ss(double x, double y)    {return x+y;}
    static inline double minus_ss(double x, double y)  {return x-y;}
    static inline double mult_ss(double x, double y)   {return x*y;}
    static inline double div_ss(double x, double y)    {return x/y;}
    static inline double pow_ss(double x, double y)    {return std::pow(x,y);}
    
    static inline CArray<double,1> neg_f(const CArray<double,1>& x)   {return Array<double,1>(-x);}
    static inline CArray<double,1> cos_f(const CArray<double,1>& x)   {return Array<double,1>(cos(x));}
    static inline CArray<double,1> sin_f(const CArray<double,1>& x)   {return Array<double,1>(sin(x));}
    static inline CArray<double,1> tan_f(const CArray<double,1>& x)   {return Array<double,1>(tan(x));}
    static inline CArray<double,1> exp_f(const CArray<double,1>& x)   {return Array<double,1>(exp(x));}
    static inline CArray<double,1> log_f(const CArray<double,1>& x)   {return Array<double,1>(log(x));}
    static inline CArray<double,1> log10_f(const CArray<double,1>& x) {return Array<double,1>(log10(x));}
    static inline CArray<double,1> sqrt_f(const CArray<double,1>& x)  {return Array<double,1>(sqrt(x));}
    
    static inline CArray<double,1> add_ff(const CArray<double,1>& x, const CArray<double,1>& y)    {return Array<double,1>(x+y);}
    static inline CArray<double,1> minus_ff(const CArray<double,1>& x, const CArray<double,1>& y)  {return Array<double,1>(x-y);}
    static inline CArray<double,1> mult_ff(const CArray<double,1>& x, const CArray<double,1>& y)   {return Array<double,1>(x*y);}
    static inline CArray<double,1> div_ff(const CArray<double,1>& x, const CArray<double,1>& y)    {return Array<double,1>(x/y);}
    static inline CArray<double,1> pow_ff(const CArray<double,1>& x, const CArray<double,1>& y)    {return Array<double,1>(pow(x,y));}

    static inline CArray<double,1> add_fs(const CArray<double,1>& x, double y)    {return Array<double,1>(x+y);}
    static inline CArray<double,1> minus_fs(const CArray<double,1>& x, double y)  {return Array<double,1>(x-y);}
    static inline CArray<double,1> mult_fs(const CArray<double,1>& x, double y)   {return Array<double,1>(x*y);}
    static inline CArray<double,1> div_fs(const CArray<double,1>& x, double y)    {return Array<double,1>(x/y);}
    static inline CArray<double,1> pow_fs(const CArray<double,1>& x, double y)    {return Array<double,1>(pow(x,y));}  

    static inline CArray<double,1> add_sf(double x, const CArray<double,1>& y)    {return Array<double,1>(x+y);}
    static inline CArray<double,1> minus_sf(double x, const CArray<double,1>& y)  {return Array<double,1>(x-y);}
    static inline CArray<double,1> mult_sf(double x, const CArray<double,1>& y)   {return Array<double,1>(x*y);}
    static inline CArray<double,1> div_sf(double x, const CArray<double,1>& y)    {return Array<double,1>(x/y);}
    
     
  } ;
  
  extern COperatorExpr operatorExpr ;
  
}

#endif

