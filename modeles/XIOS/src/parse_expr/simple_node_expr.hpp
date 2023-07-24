#ifndef __XIOS_SIMPLE_NODE_EXPR_HPP__
#define __XIOS_SIMPLE_NODE_EXPR_HPP__

#include<string>
#include<vector>

class CSimpleNodeExpr
{
  public :

  enum ENodeType
  {
    scalarDouble, scalarVariable, opScalar, opScalarScalar, 
    fieldInstant, fieldAverage, opFieldScalar, opScalarField, opFieldField, opField 
  } ;
  
  CSimpleNodeExpr(ENodeType nodeType, const std::string* str) : nodeType(nodeType)
  {
    id=*str ;
  }

  CSimpleNodeExpr(ENodeType nodeType, const char* str) : nodeType(nodeType)
  {
    id=std::string(str) ;
  }
  
  void addChild(CSimpleNodeExpr* child)  
  {
    children.push_back(child) ;
  }
  
  std::string print(void)
  {
    if (nodeType==scalarDouble) return id ;
    else if (nodeType==scalarVariable) return "$"+id ;
    else if (nodeType==fieldInstant) return id ;
    else if (nodeType==fieldAverage) return "@"+id ;
    else if (nodeType==opScalar || nodeType==opField) return id+"("+children[0]->print()+")" ;
    else  return "("+children[0]->print()+id+children[1]->print()+")" ;
  } 
  ~CSimpleNodeExpr()
  {
    for(std::vector<CSimpleNodeExpr*>::iterator it=children.begin();it!=children.end();++it) delete *it ;
  } 
  
  ENodeType nodeType ;
  std::string id ;

  std::vector<CSimpleNodeExpr*> children ;
} ;

#endif 
