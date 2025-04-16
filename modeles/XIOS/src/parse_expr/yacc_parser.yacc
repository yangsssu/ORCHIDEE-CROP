%{
#include "simple_node_expr.hpp"
#include <string>
#include <iostream>
#include "exception.hpp"

using namespace std ;
using namespace xios ;

extern "C"
{
  int yyparse(void);
  int yylex(void);
  int yyerror(const char *s) ;
}

   CSimpleNodeExpr* parsed ;
   std::string globalInputText;
   int globalReadOffset=0;
   
   int readInputForLexer( char *buffer, int *numBytesRead, int maxBytesToRead )
   {
    int numBytesToRead = maxBytesToRead;
    int bytesRemaining = globalInputText.length()-globalReadOffset;
    int i;
    if ( numBytesToRead > bytesRemaining ) numBytesToRead = bytesRemaining;
    for ( i = 0; i < numBytesToRead; i++ ) buffer[i] = globalInputText.c_str()[globalReadOffset+i];
    *numBytesRead = numBytesToRead;
    globalReadOffset += numBytesToRead;
    return 0;
   }

%}

%union
{
    std::string* str ;                /* symbol table index */
    CSimpleNodeExpr* node ;
};

%token <str> NUMBER
%token <str>  VAR ID AVERAGE
%token PLUS MINUS TIMES DIVIDE POWER
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS
%token <str> END

%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc NEG
%right POWER

%type <node> Line Expression Field_expr
%start Line
%%


Line:
     END                           {  }
   | Field_expr END {  parsed=$1 ;}
   ;

Expression:
            NUMBER { $$=new CSimpleNodeExpr(CSimpleNodeExpr::scalarDouble,$1); delete $1 }
          | VAR    { $$=new CSimpleNodeExpr(CSimpleNodeExpr::scalarVariable,$1) ; delete $1}
          | Expression PLUS Expression { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opScalarScalar,"add") ; $$->addChild($1) ; $$->addChild($3); }
          | Expression MINUS Expression { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opScalarScalar,"minus") ; $$->addChild($1) ; $$->addChild($3); }
          | Expression TIMES Expression { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opScalarScalar,"mult") ; $$->addChild($1) ; $$->addChild($3); }
          | Expression DIVIDE Expression { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opScalarScalar,"div") ; $$->addChild($1) ; $$->addChild($3); }
          | MINUS Expression %prec NEG { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opScalar,"neg") ;  $$->addChild($2); }
          | Expression POWER Expression { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opScalarScalar,"pow") ; $$->addChild($1) ; $$->addChild($3); }
          | LEFT_PARENTHESIS Expression RIGHT_PARENTHESIS { $$=$2 ; }
          | ID LEFT_PARENTHESIS Expression RIGHT_PARENTHESIS { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opScalar,$1) ; $$->addChild($3) ; delete $1 }
          ;

Field_expr:
            ID    { $$=new CSimpleNodeExpr(CSimpleNodeExpr::fieldInstant,$1); delete $1}
          | AVERAGE  { $$=new CSimpleNodeExpr(CSimpleNodeExpr::fieldAverage,$1); delete $1}
          | Field_expr PLUS Field_expr { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opFieldField,"add") ; $$->addChild($1) ; $$->addChild($3); }
          | Field_expr MINUS Field_expr { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opFieldField,"minus") ; $$->addChild($1) ; $$->addChild($3); }
          | Field_expr TIMES Field_expr { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opFieldField,"mult") ; $$->addChild($1) ; $$->addChild($3); }
          | Field_expr DIVIDE Field_expr { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opFieldField,"div") ; $$->addChild($1) ; $$->addChild($3); }
          | MINUS Field_expr %prec NEG { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opField,"neg") ; $$->addChild($2);}
          | Field_expr POWER Field_expr { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opFieldField,"pow") ; $$->addChild($1) ; $$->addChild($3); }
          | LEFT_PARENTHESIS Field_expr RIGHT_PARENTHESIS	{ $$=$2 ;}
          | Field_expr PLUS Expression { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opFieldScalar,"add") ; $$->addChild($1) ; $$->addChild($3); }
          | Expression PLUS Field_expr { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opScalarField,"add") ; $$->addChild($1) ; $$->addChild($3); }
          | Field_expr MINUS Expression { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opFieldScalar,"minus") ; $$->addChild($1) ; $$->addChild($3); }
          | Expression MINUS Field_expr { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opScalarField,"minus") ; $$->addChild($1) ; $$->addChild($3); }
          | Field_expr TIMES Expression { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opFieldScalar,"mult") ; $$->addChild($1) ; $$->addChild($3); }
          | Expression TIMES Field_expr { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opScalarField,"mult") ; $$->addChild($1) ; $$->addChild($3); }
          | Field_expr DIVIDE Expression { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opFieldScalar,"div") ; $$->addChild($1) ; $$->addChild($3); }
          | Expression DIVIDE Field_expr { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opScalarField,"div") ; $$->addChild($1) ; $$->addChild($3); }
          | Field_expr POWER Expression { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opFieldScalar,"pow") ; $$->addChild($1) ; $$->addChild($3); }
          | ID LEFT_PARENTHESIS Field_expr RIGHT_PARENTHESIS { $$=new CSimpleNodeExpr(CSimpleNodeExpr::opField,$1) ;  $$->addChild($3) ; delete $1}
          ;
%%

extern "C"
{
  int yyerror(const char *s) 
  {
    ERROR("int yyerror(const char *s)", <<"Parsing error :"<<s<<endl) ; 
  }
}

namespace xios
{
  CSimpleNodeExpr* parseExpr(const string& strExpr)
  {
    globalInputText=strExpr ;
    globalReadOffset=0 ;
    yyparse();
    return  parsed ;
  }
}


