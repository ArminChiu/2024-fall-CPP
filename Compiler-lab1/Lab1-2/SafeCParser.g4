parser grammar SafeCParser;
options { tokenVocab = SafeCLexer; }

compUnit: (decl | funcDef) + EOF;

decl: constDecl | varDecl;

funcDef: Void Identifier LeftParen RightParen block;

constDecl: Const bType constDef (Comma constDef)* SemiColon;

constDef: Identifier Assign exp | array Assign LeftBrace exp (Comma exp)* RightBrace;

varDecl: bType varDef (Comma varDef)* SemiColon;

bType: Int;

varDef
    : Identifier (Assign exp)?
    | array (Assign LeftBrace exp (Comma exp)* RightBrace)?;

array: obcArray | unobcArray;

obcArray: Obc unobcArray;

unobcArray: Identifier LeftBracket (exp)? RightBracket;

block: LeftBrace blockItem* RightBrace;

blockItem: decl | stmt;

stmt
    : block 
    | SemiColon
    | Identifier LeftParen RightParen SemiColon
    | lval Assign exp SemiColon 
    | If LeftParen cond RightParen stmt (Else stmt)? 
    | While LeftParen cond RightParen stmt; 

cond: LeftParen cond RightParen
    | exp (Equal | NonEqual | Less | Greater | LessEqual | GreaterEqual) exp;

lval: Identifier (LeftBracket exp RightBracket)?;

number: IntConst;

exp: exp (Multiply | Divide | Modulo) exp
    | exp (Plus | Minus) exp
    | (Plus | Minus) exp
    | lval
    | number 
    | LeftParen exp RightParen;