#include "expr_node.hpp"
#include "field.hpp"

namespace xios
{

  CScalarNode* CScalarNode::newNode(CSimpleNodeExpr* simpleNode)
  {
    if (simpleNode->nodeType==CSimpleNodeExpr::scalarDouble) return new CScalarDouble(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::scalarVariable) return new CScalarVariable(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::opScalar) return new COperatorScalarNode(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::opScalarScalar) return new COperatorScalarScalarNode(simpleNode) ;
    else 
    {
      ERROR("CScalarNode* CScalarNode::allocateChild(CSimpleNodeExpr* simpleNode)",<<"Non coherent node")
      return NULL;
    }
  };


  CFieldNode* CFieldNode::newNode(CSimpleNodeExpr* simpleNode)
  {
    if (simpleNode->nodeType==CSimpleNodeExpr::fieldInstant) return new CInstantFieldNode(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::fieldAverage) return new  CAverageFieldNode(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::opFieldScalar) return new COperatorFieldScalarNode(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::opScalarField) return new COperatorScalarFieldNode(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::opFieldField) return new COperatorFieldFieldNode(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::opField) return new COperatorFieldNode(simpleNode) ;
    else 
    {
      ERROR("CScalarNode* CScalarNode::allocateChild(CSimpleNodeExpr* simpleNode)",<<"Non coherent node")
      return NULL;
    }
  };

  void CInstantFieldNode::reduce(CField* thisField, map<string,CField*>& associatedInstantField, map<string,CField*>& associatedAverageField)
  {
    if (!reduced)
    {
      if (fieldId=="this")
      {
        field=thisField ;
        array=thisField->getInstantData() ;
        reduced=true ;
      }
      else
      {
        field=associatedInstantField[fieldId] ;
        array=field->getInstantData() ;
        reduced=true ;
      }
    }
  }   

  void CAverageFieldNode::reduce(CField* thisField, map<string,CField*>& associatedInstantField, map<string,CField*>& associatedAverageField)
  {
    if (!reduced)
    {
      field=associatedAverageField[fieldId] ;
      array=field->getInstantData() ;
      reduced=true ;
    }
  }
  
}
