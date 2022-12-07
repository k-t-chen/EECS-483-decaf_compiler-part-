/* File: parser.y
 * --------------
 * Yacc input file to generate the parser for the compiler.
 *
 * pp2: your job is to write a parser that will construct the parse tree
 *      and if no parse errors were found, print it.  The parser should
 *      accept the language as described in specification, and as augmented
 *      in the pp2 handout.
 */

%{

/* Just like lex, the text within this first region delimited by %{ and %}
 * is assumed to be C/C++ code and will be copied verbatim to the y.tab.c
 * file ahead of the definitions of the yyparse() function. Add other header
 * file inclusions or C++ variable declarations/prototypes that are needed
 * by your code here.
 */
#include "parser.h"
#include "errors.h"
#include "scanner.h" // for yylex
#include <algorithm>

  void yyerror(const char *msg); // standard error-handling routine

  /*
   * Constants
   * ---------
   */
  const char *const equalOp_c = "=";
  const char *const lessOrEqualOp_c = "<=";
  const char *const lessOp_c = "<";
  const char *const greaterOrEqualOp_C = ">=";
  const char *const greaterOp_c = ">";
  const char *const equalEqualOp_C = "==";
  const char *const notEqualOp_c = "!=";
  const char *const andAndOp_c = "&&";
  const char *const minusOp_c = "-";
  const char *const plusOp_c = "+";
  const char *const multOp_c = "*";
  const char *const divOp_c = "/";
  const char *const modOp_c = "%";
  const char *const incremOp_c = "++";
  const char *const decremOp_c = "--";
  const char *const notOp_c = "!";
  const char *const orOp_c = "||";

%}

/* The section before the first %% is the Definitions section of the yacc
 * input file. Here is where you declare tokens and types, add precedence
 * and associativity options, and so on.
 */

/* yylval
 * ------
 * Here we define the type of the yylval global variable that is used by
 * the scanner to store attibute information about the token just scanned
 * and thus communicate that information to the parser.
 *
 * pp2: You will need to add new fields to this union as you add different
 *      attributes to your non-terminal symbols.
 */
%union {
  int integerConstant;
  bool boolConstant;
  char *stringConstant;
  double doubleConstant;
  char identifier[MaxIdentLen + 1];

  Identifier *ident;

  Decl *decl;
  List<Decl *> *declList;

  VarDecl *varDecl;
  List<VarDecl *> *varDeclList;

  ClassDecl *classDecl;
  List<NamedType *> *namedTypeList;

  InterfaceDecl *interfaceDecl;

  Type *type;

  FnDecl *fnDecl;

  Stmt *stmt;
  List<Stmt *> *stmtList;
  StmtBlock *stmtBlock;
  SwitchCaseStmt *switchCaseStmt;
  List<SwitchCaseStmt *> *switchCaseList;

  Expr *expr;
  List<Expr *> *exprList;
}

    /* Tokens
     * ------
     * Here we tell yacc about all the token types that we are using.
     * Yacc will assign unique numbers to these and export the #define
     * in the generated y.tab.h header file.
     */
%token T_Void T_Bool T_Int T_Double T_String T_Class 
%token T_LessEqual T_GreaterEqual T_Equal T_NotEqual T_Dims 
%token T_And T_Or T_Null T_Extends T_This T_Interface T_Implements 
%token T_While T_For T_If T_Else T_Return T_Break 
%token T_New T_NewArray T_Print T_ReadInteger T_ReadLine

%token<identifier> T_Identifier 
%token<stringConstant> T_StringConstant 
%token<integerConstant> T_IntConstant 
%token<doubleConstant> T_DoubleConstant 
%token<boolConstant> T_BoolConstant

%token T_Increase T_Decrease T_Switch T_Case T_Default

%nonassoc '=' 
%left T_Or 
%left T_And 
%left T_Equal T_NotEqual 
%nonassoc '<' T_LessEqual '>' T_GreaterEqual 
%left '+' '-' 
%left '*' '/' '%' 
%right UMINUS '!' T_Increase T_Decrease 
%left T_Dims '.'

/* Non-terminal types
  * ------------------
  * In order for yacc to assign/access the correct field of $$, $1, we
  * must to declare which field is appropriate for the non-terminal.
  * As an example, this first type declaration establishes that the DeclList
  * non-terminal uses the field named "declList" in the yylval union. This
  * means that when we are setting $$ for a reduction for DeclList ore
  * reading $n which corresponds to a DeclList nonterminal we are accessing
  * the field of the union named "declList" which is of type List<Decl*>.
  * pp2: You'll need to add many of these of your own.
  */

%type<ident> Ident

%type<declList> DeclList FieldList PrototypeList 
%type<decl> Decl Field Prototype 
%type<varDeclList> VariableDeclList VariableDeclListComma Formals 
%type<varDecl> Variable VariableDecl 
%type<fnDecl> FunctionDecl 
%type<expr> Expr OptExpr LValue Call Constant 
%type<exprList> ExprList Actuals 
%type<interfaceDecl> InterfaceDecl 
%type<type> Type 
%type<classDecl> ClassDecl 
%type<namedTypeList> NamedTypeList
%type<stmt> Stmt IfStmt WhileStmt ForStmt ReturnStmt BreakStmt PrintStmt
        SwitchStmt 
%type<switchCaseStmt> SwitchCase SwitchDefaultCase 
%type<switchCaseList> SwitchCaseList 
%type<stmtList> StmtList 
%type<stmtBlock> StmtBlock

%%
/* Rules
  * -----
  * All productions and actions should be placed between the start and stop
  * %% markers which delimit the Rules section.

  */
Program : DeclList {
  /* pp2: The @1 is needed to convince
   * yacc to set up yylloc. You can remove
   * it once you have other uses of @n*/
  Program *program = new Program($1);
  // if no errors, advance to next phase
  if (ReportError::NumErrors() == 0)
    program->Print(0);
};

DeclList : DeclList Decl { ($$ = $1)->Append($2); }
| Decl { ($$ = new List<Decl *>)->Append($1); };

Decl : VariableDecl { $$ = $1; }
| FunctionDecl { $$ = $1; }
| ClassDecl { $$ = $1; }
| InterfaceDecl { $$ = $1; };

VariableDeclList :
{
  $$ = new List<VarDecl *>;
}
| VariableDeclList VariableDecl { ($$ = $1)->Append($2); };

VariableDecl : Variable ';' { $$ = $1; };

Variable : Type Ident { $$ = new VarDecl($2, $1); };

Type : T_Int { $$ = Type::intType; }
| T_Double { $$ = Type::doubleType; }
| T_Bool { $$ = Type::boolType; }
| T_String { $$ = Type::stringType; }
| Ident { $$ = new NamedType($1); }
| Type T_Dims { $$ = new ArrayType(@1, $1); }
| T_Null { $$ = Type::nullType; };

FunctionDecl : Type Ident '(' Formals ')' StmtBlock {
  $$ = new FnDecl($2, $1, $4);
  $$->SetFunctionBody($6);
}
| T_Void Ident '(' Formals ')' StmtBlock {
  $$ = new FnDecl($2, Type::voidType, $4);
  $$->SetFunctionBody($6);
};

VariableDeclListComma : Variable {
  ($$ = new List<VarDecl *>)->Append($1); 
}
| VariableDeclListComma ',' Variable {
  ($$ = $1)->Append($3);
};

Formals :
/* Empty */ {
  $$ = new List<VarDecl *>;
}
| VariableDeclListComma { $$ = $1; };

ClassDecl : T_Class Ident '{' FieldList '}' {
  $$ = new ClassDecl($2, nullptr, new List<NamedType *>, $4);
}
| T_Class Ident T_Extends Ident '{' FieldList '}' {
  $$ = new ClassDecl($2, new NamedType($4), new List<NamedType *>, $6);
}
| T_Class Ident T_Implements NamedTypeList '{' FieldList '}' {
  $$ = new ClassDecl($2, nullptr, $4, $6);
}
| T_Class Ident T_Extends Ident T_Implements NamedTypeList '{' FieldList '}' {
  $$ = new ClassDecl($2, new NamedType($4), $6, $8);
};

NamedTypeList : NamedTypeList ',' Ident { ($$ = $1)->Append(new NamedType($3)); }
| Ident { ($$ = new List<NamedType *>)->Append(new NamedType($1)); };

FieldList : FieldList Field { ($$ = $1)->Append($2); }
|
/* Empty */ {
  $$ = new List<Decl *>;
};

Field : FunctionDecl { $$ = $1; }
| VariableDecl { $$ = $1; };

InterfaceDecl : T_Interface Ident '{' PrototypeList '}' {
  $$ = new InterfaceDecl($2, $4);
};

PrototypeList : PrototypeList Prototype { ($$ = $1)->Append($2); }
|
/* Empty */ {
  $$ = new List<Decl *>;
};

Prototype : Type Ident '(' Formals ')' ';' { $$ = new FnDecl($2, $1, $4); }
| T_Void Ident '(' Formals ')' ';' { $$ = new FnDecl($2, Type::voidType, $4); };

StmtBlock : '{' VariableDeclList StmtList '}' { $$ = new StmtBlock($2, $3); };

StmtList : Stmt StmtList { ($$ = $2)->InsertAt($1, 0); }
|
/* Empty */ {
  $$ = new List<Stmt *>;
};

Stmt : ';' { $$ = new EmptyExpr(); }
| Expr ';' { $$ = $1; }
| IfStmt { $$ = $1; }
| SwitchStmt { $$ = $1; }
| WhileStmt { $$ = $1; }
| ForStmt { $$ = $1; }
| BreakStmt { $$ = $1; }
| ReturnStmt { $$ = $1; }
| PrintStmt { $$ = $1; }
| StmtBlock { $$ = $1; };

SwitchCaseList : SwitchCaseList SwitchCase { ($$ = $1)->Append($2); }
| SwitchCase { ($$ = new List<SwitchCaseStmt *>)->Append($1); };

SwitchCase : T_Case T_IntConstant ':' StmtList {
  $$ = new SwitchCaseStmt(new IntConstant(@2, $2), $4);
};

SwitchDefaultCase : T_Default ':' StmtList {
  $$ = new SwitchCaseStmt(nullptr, $3);
};

IfStmt : T_If '(' Expr ')' Stmt { $$ = new IfStmt($3, $5, nullptr); }
| T_If '(' Expr ')' Stmt T_Else Stmt { $$ = new IfStmt($3, $5, $7); };

SwitchStmt : T_Switch '(' Expr ')' '{' SwitchCaseList SwitchDefaultCase '}' {
  $$ = new SwitchStmt($3, $6, $7);
}
| T_Switch '(' Expr ')' '{' SwitchCaseList '}' {
  $$ = new SwitchStmt($3, $6, nullptr);
};

WhileStmt : T_While '(' Expr ')' Stmt { $$ = new WhileStmt($3, $5); };

ForStmt : T_For '(' OptExpr ';' Expr ';' OptExpr ')' Stmt {
  $$ = new ForStmt($3, $5, $7, $9);
};

ReturnStmt : T_Return OptExpr ';' { $$ = new ReturnStmt(@1, $2); };

BreakStmt : T_Break ';' { $$ = new BreakStmt(@1); };

PrintStmt : T_Print '(' ExprList ')' ';' { $$ = new PrintStmt($3); };

ExprList : ExprList ',' Expr { ($$ = $1)->Append($3); }
| Expr { ($$ = new List<Expr *>)->Append($1); };

OptExpr : Expr { $$ = $1; }
|
/* Empty */ {
  $$ = new EmptyExpr();
};

Expr : LValue '=' Expr {
  $$ = new AssignExpr($1, new Operator(@2, equalOp_c), $3);
}
| Constant { $$ = $1; }
| LValue { $$ = $1; }
| T_This { $$ = new This(@1); }
| Call { $$ = $1; }
| '(' Expr ')' { $$ = $2; }
| Expr '+' Expr { $$ = new ArithmeticExpr($1, new Operator(@2, plusOp_c), $3); }
| Expr '-' Expr {
  $$ = new ArithmeticExpr($1, new Operator(@2, minusOp_c), $3);
}
| Expr '*' Expr { $$ = new ArithmeticExpr($1, new Operator(@2, multOp_c), $3); }
| Expr '/' Expr { $$ = new ArithmeticExpr($1, new Operator(@2, divOp_c), $3); }
| Expr '%' Expr { $$ = new ArithmeticExpr($1, new Operator(@2, modOp_c), $3); }
| '-' Expr %prec UMINUS {
  $$ = new ArithmeticExpr(new Operator(@1, minusOp_c), $2);
}
| Expr '<' Expr { $$ = new RelationalExpr($1, new Operator(@2, lessOp_c), $3); }
| Expr T_LessEqual Expr {
  $$ = new RelationalExpr($1, new Operator(@2, lessOrEqualOp_c), $3);
}
| Expr '>' Expr {
  $$ = new RelationalExpr($1, new Operator(@2, greaterOp_c), $3);
}
| Expr T_GreaterEqual Expr {
  $$ = new RelationalExpr($1, new Operator(@2, greaterOrEqualOp_C), $3);
}
| Expr T_Equal Expr {
  $$ = new EqualityExpr($1, new Operator(@2, equalEqualOp_C), $3);
}
| Expr T_NotEqual Expr {
  $$ = new EqualityExpr($1, new Operator(@2, notEqualOp_c), $3);
}
| Expr T_And Expr {
  $$ = new LogicalExpr($1, new Operator(@2, andAndOp_c), $3);
}
| Expr T_Or Expr { $$ = new LogicalExpr($1, new Operator(@2, orOp_c), $3); }
| '!' Expr { $$ = new LogicalExpr(new Operator(@1, notOp_c), $2); }
| T_ReadInteger '(' ')' { $$ = new ReadIntegerExpr(@1); }
| T_ReadLine '(' ')' { $$ = new ReadLineExpr(@1); }
| T_New '(' Ident ')' { $$ = new NewExpr(@1, new NamedType($3)); }
| T_NewArray '(' Expr ',' Type ')' { $$ = new NewArrayExpr(@1, $3, $5); }
| LValue T_Increase { $$ = new PostfixExpr($1, new Operator(@2, incremOp_c)); }
| LValue T_Decrease { $$ = new PostfixExpr($1, new Operator(@2, decremOp_c)); };

LValue : Ident { $$ = new FieldAccess(nullptr, $1); }
| Expr '.' Ident { $$ = new FieldAccess($1, $3); }
| Expr '[' Expr ']' { $$ = new ArrayAccess(@1, $1, $3); };

Call : Ident '(' Actuals ')' { $$ = new Call(@1, nullptr, $1, $3); }
| Expr '.' Ident '(' Actuals ')' { $$ = new Call(@3, $1, $3, $5); };

Actuals :
/* Empty */ {
  $$ = new List<Expr *>;
}
| ExprList { $$ = $1; };

Constant : T_IntConstant { $$ = new IntConstant(@1, $1); }
| T_StringConstant { $$ = new StringConstant(@1, $1); }
| T_Null { $$ = new NullConstant(@1); }
| T_DoubleConstant { $$ = new DoubleConstant(@1, $1); }
| T_BoolConstant { $$ = new BoolConstant(@1, $1); };

Ident : T_Identifier { $$ = new Identifier(@1, $1); };

%%
    /* The closing %% above marks the end of the Rules section and the beginning
     * of the User Subroutines section. All text from here to the end of the
     * file is copied verbatim to the end of the generated y.tab.c file.
     * This section is where you put definitions of helper functions.
     */

    /* Function: InitParser
     * --------------------
     * This function will be called before any calls to yyparse().  It is
     * designed to give you an opportunity to do anything that must be done to
     * initialize the parser (set global variables, configure starting state,
     * etc.). One thing it already does for you is assign the value of the
     * global variable yydebug that controls whether yacc prints debugging
     * information about parser actions (shift/reduce) and contents of state
     * stack during parser. If set to false, no information is printed. Setting
     * it to true will give you a running trail that might be helpful when
     * debugging your parser. Please be sure the variable is set to false when
     * submitting your final version.
     */
    void InitParser() {
  PrintDebug("parser", "Initializing parser");
  yydebug = false;
}
