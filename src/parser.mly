%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> ID
%token CLASS
%token EXTENDS
%token NEW
%token WHILE
%token IF
%token ELSE
%token MAIN

%token TINT
%token TFLOAT
%token TBOOL
%token TVOID

%token NULL
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token OPARENT
%token CPARENT
%token COLON
%token SEMICOLON
%token DOT
%token COMMA
%token HASHTAG
%token AND
%token OR
%token NOT
%token FPLUS
%token FMINUS
%token FMULTIPLY
%token FDIVIDE
%token IPLUS
%token IMINUS
%token IMULTIPLY
%token IDIVIDE
%token LESS
%token LESS_EQUAL
%token EQ_EQUAL
%token GREATER_EQUAL
%token GREATER
%token NOT_EQUAL
%token EQUAL
%token INSTANCEOF
%token EOF

/*%left IPLUS IMINUS
%left IMULTIPLY IDIVIDE*/

%{ open Syntax %}
%start <Syntax.program option> program

%%
program:EOF { None }
| clist = classDeclarationList {Some (Program(clist))}
;

classDeclarationList:
| EOF  {[]}
| classDeclaration classDeclarationList {$1 :: $2}
;

classDeclaration:
| CLASS obj = ID LEFT_BRACE RIGHT_BRACE {Class(obj, "Object", [], [])}
| CLASS obj = ID EXTENDS parent = ID LEFT_BRACE RIGHT_BRACE {Class(obj, parent, [], [])}
| CLASS obj = ID EXTENDS parent = ID LEFT_BRACE fields = fieldList RIGHT_BRACE {Class(obj, parent, fields, [])}
| CLASS obj = ID EXTENDS parent = ID LEFT_BRACE fields = fieldList HASHTAG methods = methodList RIGHT_BRACE {Class(obj, parent, fields, methods)}
;

fieldList:
| (* empty *) {[]}
| fieldDeclaration fieldList {$1 :: $2}
;

fieldDeclaration:
| varDecl SEMICOLON {$1}
;

methodList:
| (* empty *) {[]}
| methodDeclaration  methodList {$1 :: $2}
;


methodDeclaration:
| typeD MAIN OPARENT methodParameterList CPARENT be = blockExpression {MainMethod($1, $4, be)}
| typeD ID OPARENT methodParameterList CPARENT be = blockExpression   {Method($1, $2, $4, be)}
;
methodParameterList:
|(* empty *) {[]}
| methodParameterListAux {$1}
;

methodParameterListAux:
| methodParameter {[$1]}
| methodParameter COMMA methodParameterListAux {$1 :: $3}
;
methodParameter: typeD ID {($2, $1)}
;

blockExpression:
| LEFT_BRACE vars = varDeclList e = expression RIGHT_BRACE {BlockExpression(vars, e) }
;
varDeclList:
| (* empty *) {[]}
| varDeclListAux {$1}
;

varDeclListAux:
| varDecl  {[$1]}
| varDecl varDeclListAux {$1 :: $2}
;

varDecl:
| TINT ID {($2,IntType)}
| TFLOAT ID {($2,FloatType)}
| TBOOL ID {($2,BoolType)}
| TVOID ID {($2,VoidType)}
| ID ID {($2,ObjectType($1))}
;

/*Add semicolon at the end of some expressions*/
expression:
/*TODO: ADD LocalVariableDeclaration*/
| INT       {Value(IntV($1))}
| FLOAT     {Value(FloatV($1))}
| BOOL      {Value(BoolV($1))}
| ID        {Variable($1)}
| ID DOT ID {ObjectField($1, $3)}
| ID EQUAL expression {VariableAssignment($1, $3)}
| ID DOT ID EQUAL expression {ObjectFieldAssignment(($1, $3), $5)}
| e1 = expression e2 = expression{Sequence(e1, e2)}
| IF OPARENT guard = ID CPARENT thenExp = expression
  ELSE LEFT_BRACE elseExp = expression RIGHT_BRACE  {If(guard, thenExp, elseExp)}

| exp1 = expression op = binaryOperator exp2 = expression {Operation(exp1, op, exp2)}

| NOT expression {Negation($2)}
| NEW ID OPARENT argsList CPARENT {New($2, $4)}
| ID DOT ID OPARENT argsList CPARENT {MethodCall($1, $3, $5)}
| WHILE OPARENT guard = ID CPARENT be = blockExpression {While(guard, be)}
/*modify these 2 productions to support also non primitive types.*/
| OPARENT ID CPARENT ID {Cast($2, $4)}
| ID INSTANCEOF ID {InstanceOf($1, $3)}
| blockExpression {$1}
;

binaryOperator:
| IPLUS         {IPlus}
| IMINUS        {IMinus}
| IMULTIPLY     {IMultiply}
| IDIVIDE       {IDivide}
| FPLUS         {FPlus}
| FMINUS        {FMinus}
| FMULTIPLY     {FMultiply}
| EQ_EQUAL      {EqEqual}
| GREATER_EQUAL {GreaterEqual}
| GREATER       {Greater}
| NOT_EQUAL     {NotEqual}
| AND           {And}
| OR            {Or}
;

argsList:
| /*empty*/ {[]}
| ID {[$1]}
| ID COMMA argsList {$1 :: $3}
;

typeD:
| TINT {IntType}
| TFLOAT {FloatType}
| TBOOL{BoolType}
| TVOID {VoidType}

;
