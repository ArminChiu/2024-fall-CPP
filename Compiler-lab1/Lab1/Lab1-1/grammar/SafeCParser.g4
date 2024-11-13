parser grammar SafeCParser;
options { tokenVocab = SafeCLexer; }

compUnit: (decl | funcDef) + EOF;

decl: constDecl | varDecl;

funcDef: Void Identifier LeftParen RightParen block;

constDecl:

constDef:

varDecl:

bType: Int;

varDef:

array: obcArray | unobcArray;

obcArray: Obc unobcArray;

unobcArray:

block:

blockItem: decl | stmt;

stmt:

cond:

lval:

number: IntConst;

exp: