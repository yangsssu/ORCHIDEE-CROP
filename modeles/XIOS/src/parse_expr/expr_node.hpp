#ifndef __XIOS_EXPR_NODE_HPP__
#define __XIOS_EXPR_NODE_HPP__

#include "variable.hpp"
#include "simple_node_expr.hpp"
#include "operator_expr.hpp"
#include "lex_parser.hpp"

namespace xios
{
  class CField ;
///////////////////////////////////////
//         class CNodeExpr           //
///////////////////////////////////////
 
  class CNodeExpr 
  {
    public:
    CNodeExpr(void) : reduced(false) {};
    
    bool reduced ;
    bool isReduced(void) { return reduced ; }  
    virtual ~CNodeExpr() {}
  } ;

////////////////////////////////////////////////////////////////////////

///////////////////////////////////////
//         class CScalarNode           //
///////////////////////////////////////

  class CScalarNode : public CNodeExpr
  {
    public:
    CScalarNode(void) : CNodeExpr() {}
    static CScalarNode* newNode(CSimpleNodeExpr* simpleNode) ;
    virtual void reduce(void)=0 ; 
      
    double val ;
    virtual ~CScalarNode() {}
  } ;

////////////////////////////////////////////////////////////////////////

///////////////////////////////////////
//      class CScalarDouble          //
///////////////////////////////////////

  class CScalarDouble : public CScalarNode
  {
    public :
    
    CScalarDouble(CSimpleNodeExpr* simpleNode) : CScalarNode()
    {
      strVal=simpleNode->id ;
    }
    
    virtual void reduce(void)
    {
      if (!reduced)
      {
        CType<double> tval ;
        tval.fromString(strVal) ;
        val=tval ;
        reduced=true ;
      }
    }
    
    virtual ~CScalarDouble() {}
    string strVal ;
  } ;


////////////////////////////////////////////////////////////////////////

///////////////////////////////////////
//       class CScalarVariable       //
///////////////////////////////////////



  class CScalarVariable : public CScalarNode 
  {
    public :
    CScalarVariable(CSimpleNodeExpr* simpleNode) : CScalarNode()
    {
      varId=simpleNode->id ;
    }
    
    void reduce(void)
    {
      if (!reduced)
      {
        if (CVariable::has(varId)) 
        {
          val=CVariable::get(varId)->getData<double>() ;
          reduced=true ;
        }
        else ERROR("void CScalarVariable::reduce(void)",<<" Variable "<<varId<<" does not exist")
      }
    }
    virtual ~CScalarVariable() {}
    string varId ;
  } ;

////////////////////////////////////////////////////////////////////////

///////////////////////////////////////
//      class COperatorScalarNode    //
///////////////////////////////////////


  class COperatorScalarNode : public CScalarNode
  {
    public :
    COperatorScalarNode(CSimpleNodeExpr* simpleNode) : CScalarNode()
    {
      opId=simpleNode->id ;
      child=newNode(simpleNode->children[0]) ;
    }
    
    void reduce(void)
    {
      child->reduce() ;
      op=operatorExpr.getOpScalar(opId) ;
      if (child->isReduced())
      {
       val=op(child->val) ;
       reduced=true ;
      }
    }
        
    virtual ~COperatorScalarNode() { delete child; }
    
    CScalarNode* child ;
    string opId ;
    double (*op)(double) ;
  } ;

////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////
//   class COperatorScalarScalarNode       //
/////////////////////////////////////////////

  class COperatorScalarScalarNode : public CScalarNode
  {
    public : 
    COperatorScalarScalarNode(CSimpleNodeExpr* simpleNode) : CScalarNode()
    {
      opId=simpleNode->id ;
      child1=newNode(simpleNode->children[0]) ;
      child2=newNode(simpleNode->children[1]) ;
    }
    
    void reduce(void)
    {
      child1->reduce() ;
      child2->reduce() ;
      op=operatorExpr.getOpScalarScalar(opId) ;
      if (child1->isReduced() && child2->isReduced())
      {
        op=operatorExpr.getOpScalarScalar(opId) ;
        val=op(child1->val,child2->val) ;
        reduced=true ;
      }
    }
    
    virtual ~COperatorScalarScalarNode()
    {
      delete child1 ;
      delete child2 ;
    }
    
    CScalarNode* child1 ;
    CScalarNode* child2 ;
    double (*op)(double,double) ;
    string opId ;
  } ;

  
////////////////////////////////////////////////////////////////////////

///////////////////////////////////////
//         class CFieldNode          //
///////////////////////////////////////


  class CFieldNode : public CNodeExpr
  {
    public:
    CFieldNode(void) : CNodeExpr() {}
    static CFieldNode* newNode(CSimpleNodeExpr* simpleNode) ;
    virtual void reduce(CField* thisField, map<string,CField*>& associatedInstantField, map<string,CField*>& associatedAverageField) =0 ; 
    virtual CArray<double,1> compute(void)=0 ;
    virtual void getFieldIds(set<string>& fieldIds)=0 ;
    virtual void getInstantFieldIds(set<string>& fieldIds)=0 ;
    virtual void getAverageFieldIds(set<string>& fieldIds)=0 ;
    virtual void getFields(set<CField*>& fields)=0 ;
    virtual void getInstantFields(set<CField*>& fields)=0 ;
    virtual void getAverageFields(set<CField*>& fields)=0 ;
      
    virtual ~CFieldNode() {}
  } ;


////////////////////////////////////////////////////////////////////////

///////////////////////////////////////
//       class CInstantFieldNode     //
///////////////////////////////////////  

  class CInstantFieldNode : public CFieldNode
  {
    public:
    CInstantFieldNode(CSimpleNodeExpr* simpleNode) : CFieldNode(), fieldId(simpleNode->id) {}
    virtual void reduce(CField* thisField, map<string,CField*>& associatedInstantField, map<string,CField*>& associatedAverageField) ; 
    virtual CArray<double,1> compute(void) { return CArray<double,1>(*array);}
    virtual ~CInstantFieldNode() { }
    virtual void getFieldIds(set<string>& fieldIds) { fieldIds.insert(fieldId) ;}
    virtual void getInstantFieldIds(set<string>& fieldIds) { fieldIds.insert(fieldId) ;}
    virtual void getAverageFieldIds(set<string>& fieldIds) { }
    virtual void getFields(set<CField*>& fields) { fields.insert(field) ;}
    virtual void getInstantFields(set<CField*>& fields) { fields.insert(field) ;}
    virtual void getAverageFields(set<CField*>& fields) { }
       
    string fieldId;
    CField* field ;
    CArray<double,1>* array ;
  };



////////////////////////////////////////////////////////////////////////

///////////////////////////////////////
//       class CAverageFieldNode     //
///////////////////////////////////////


  class CAverageFieldNode : public CFieldNode
  {
    public:
    
    CAverageFieldNode(CSimpleNodeExpr* simpleNode) : CFieldNode(), fieldId(simpleNode->id) {}
    
    virtual void reduce(CField* thisField, map<string,CField*>& associatedInstantField, map<string,CField*>& associatedAverageField) ; 
    virtual CArray<double,1> compute(void) { return CArray<double,1>(*array); }
    virtual void getFieldIds(set<string>& fieldIds) { fieldIds.insert(fieldId) ;}
    virtual void getInstantFieldIds(set<string>& fieldIds) { }
    virtual void getAverageFieldIds(set<string>& fieldIds) { fieldIds.insert(fieldId) ;}
    virtual void getFields(set<CField*>& fields) { fields.insert(field) ;}
    virtual void getInstantFields(set<CField*>& fields) { } 
    virtual void getAverageFields(set<CField*>& fields) { fields.insert(field) ;}
    virtual ~CAverageFieldNode() {}
    string fieldId;
    CField* field ;
    CArray<double,1>* array ;
  };


////////////////////////////////////////////////////////////////////////

///////////////////////////////////////
//         class COperatorFieldNode  //
///////////////////////////////////////
  
  class COperatorFieldNode : public CFieldNode
  {
    public:
    
    COperatorFieldNode(CSimpleNodeExpr* simpleNode) : CFieldNode(), opId(simpleNode->id)
    {
      child=newNode(simpleNode->children[0]) ;
    }
    
    virtual void reduce(CField* thisField, map<string,CField*>& associatedInstantField, map<string,CField*>& associatedAverageField) 
    {
      child->reduce(thisField, associatedInstantField, associatedAverageField) ;
      op=operatorExpr.getOpField(opId) ;
      reduced=true ;
    }
    
    virtual void getFieldIds(set<string>& fieldIds) {child-> getFieldIds(fieldIds);}
    virtual void getInstantFieldIds(set<string>& fieldIds) {child->  getInstantFieldIds(fieldIds) ;}
    virtual void getAverageFieldIds(set<string>& fieldIds) {child->  getAverageFieldIds(fieldIds) ;}
    virtual void getFields(set<CField*>& fields) { child-> getFields(fields) ;}
    virtual void getInstantFields(set<CField*>& fields) {child-> getInstantFields(fields) ; } 
    virtual void getAverageFields(set<CField*>& fields) {child-> getAverageFields(fields) ; }    
    virtual CArray<double,1> compute(void)
    {
      return op(child->compute()) ;
    }
    virtual ~COperatorFieldNode() { delete child; }
     
    CFieldNode* child ;
    string opId ;
    CArray<double,1> (*op)( const CArray<double,1>&) ;
  };


////////////////////////////////////////////////////////////////////////

///////////////////////////////////////
//     COperatorFieldFieldNode       //
///////////////////////////////////////

  
  class COperatorFieldFieldNode : public CFieldNode
  {
    public:
    
    COperatorFieldFieldNode(CSimpleNodeExpr* simpleNode) : CFieldNode(),opId(simpleNode->id) 
    {
      child1=newNode(simpleNode->children[0]) ;
      child2=newNode(simpleNode->children[1]) ;
    }
    
    virtual void reduce(CField* thisField, map<string,CField*>& associatedInstantField, map<string,CField*>& associatedAverageField)
    {
      child1->reduce(thisField, associatedInstantField, associatedAverageField) ;
      child2->reduce(thisField, associatedInstantField, associatedAverageField) ;
      op=operatorExpr.getOpFieldField(opId) ;
      reduced=true ;
    }

    virtual void getFieldIds(set<string>& fieldIds) {child1-> getFieldIds(fieldIds); child2-> getFieldIds(fieldIds);}
    virtual void getInstantFieldIds(set<string>& fieldIds) {child1-> getInstantFieldIds(fieldIds); child2-> getInstantFieldIds(fieldIds);}
    virtual void getAverageFieldIds(set<string>& fieldIds) {child1-> getAverageFieldIds(fieldIds); child2-> getAverageFieldIds(fieldIds);}
    virtual void getFields(set<CField*>& fields) {child1-> getFields(fields); child2-> getFields(fields);}
    virtual void getInstantFields(set<CField*>& fields) {child1-> getInstantFields(fields); child2-> getInstantFields(fields);}
    virtual void getAverageFields(set<CField*>& fields) {child1-> getAverageFields(fields); child2-> getAverageFields(fields);}

    virtual CArray<double,1> compute(void)
    {
      return op(child1->compute(),child2->compute()) ;
    }
    
    virtual ~COperatorFieldFieldNode() { delete child1; delete child2 ; }

    CFieldNode* child1 ;
    CFieldNode* child2 ;
    string opId ;
    CArray<double,1> (*op)( const CArray<double,1>&,const CArray<double,1>&) ;    
  };


////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////
//   class COperatorScalarFieldNode       //
////////////////////////////////////////////
  
  class COperatorScalarFieldNode : public CFieldNode
  {
    public:
    
    COperatorScalarFieldNode(CSimpleNodeExpr* simpleNode) : CFieldNode(), opId(simpleNode->id)
    {
      child1=CScalarNode::newNode(simpleNode->children[0]) ;
      child2=newNode(simpleNode->children[1]) ;
    }
    
    virtual void reduce(CField* thisField, map<string,CField*>& associatedInstantField, map<string,CField*>& associatedAverageField)
    {
      child1->reduce() ;
      child2->reduce(thisField, associatedInstantField, associatedAverageField) ;
      op=operatorExpr.getOpScalarField(opId) ;
      reduced=true ;
    }
   
    virtual void getFieldIds(set<string>& fieldIds) {child2-> getFieldIds(fieldIds);}
    virtual void getInstantFieldIds(set<string>& fieldIds) {child2-> getInstantFieldIds(fieldIds);}
    virtual void getAverageFieldIds(set<string>& fieldIds) {child2-> getAverageFieldIds(fieldIds);}
    virtual void getFields(set<CField*>& fields) {child2-> getFields(fields);}
    virtual void getInstantFields(set<CField*>& fields) {child2-> getInstantFields(fields);}
    virtual void getAverageFields(set<CField*>& fields) {child2-> getAverageFields(fields);}

    virtual CArray<double,1> compute(void)
    {
      return op(child1->val,child2->compute()) ;
    }    
    
    ~COperatorScalarFieldNode() { delete child1 ; delete child2 ; }
    
    CScalarNode* child1 ;
    CFieldNode* child2 ;
    string opId ;
    CArray<double,1> (*op)(double,const CArray<double,1>&) ;        
  };


////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////
//    class COperatorFieldScalarNode     //
///////////////////////////////////////////
  
  class COperatorFieldScalarNode : public CFieldNode
  {
    public:
    
    COperatorFieldScalarNode(CSimpleNodeExpr* simpleNode) : CFieldNode(), opId(simpleNode->id)
    {
      child1=newNode(simpleNode->children[0]) ;
      child2=CScalarNode::newNode(simpleNode->children[1]) ;
    }
    
    virtual void reduce(CField* thisField, map<string,CField*>& associatedInstantField, map<string,CField*>& associatedAverageField)
    {
      child1->reduce(thisField, associatedInstantField, associatedAverageField) ;
      child2->reduce() ;
      op=operatorExpr.getOpFieldScalar(opId) ;
      reduced=true ;
    }
    
    virtual CArray<double,1> compute(void)
    {
      return op(child1->compute(),child2->val) ;
    }
    
    virtual void getFieldIds(set<string>& fieldIds) {child1-> getFieldIds(fieldIds);}
    virtual void getInstantFieldIds(set<string>& fieldIds) {child1-> getInstantFieldIds(fieldIds);}
    virtual void getAverageFieldIds(set<string>& fieldIds) {child1-> getAverageFieldIds(fieldIds);}
    virtual void getFields(set<CField*>& fields) {child1-> getFields(fields);}
    virtual void getInstantFields(set<CField*>& fields) {child1-> getInstantFields(fields);}
    virtual void getAverageFields(set<CField*>& fields) {child1-> getAverageFields(fields);}

    ~COperatorFieldScalarNode() {delete child1, delete child2; }
         
    CFieldNode* child1 ;
    CScalarNode* child2 ;
    string opId ;
    CArray<double,1> (*op)( const CArray<double,1>&, double) ;    
  };
  
}

#endif
