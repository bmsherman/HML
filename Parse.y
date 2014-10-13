{

module Parse where

import Data.Char (toLower)
import Lex
import AST

}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { TokenEOF }
%error { happyError }

%token
  '('              { Paren L }
  ')'              { Paren R }
  '{'              { Brace L }
  '}'              { Brace R }
--  '['              { Bracket L }
--  ']'              { Bracket R }
  ':'              { Colon }
  ','              { Comma }
  ';'              { Semi }
  '+'              { IntBinOp Plus }
  '-'              { IntBinOp Minus }
  '*'              { IntBinOp Times }
  '/'              { IntBinOp Div   }
  intbincmp        { IntBinCmp $$ }
  '~'              { Negate }
  '|'              { Or }
  '=>'             { To }
  '>>'             { Seq }
  int              { Int $$ }
  string           { String $$ }
  '='              { Equals  }
  data             { Data }
  case             { Case }
  of               { Of   }
  let              { Let  }
  in               { In   }
  intty            { UName x | x == "Int" }
  strty            { UName x | x == "String" }
  uname            { UName $$ }
  lname            { LName $$ }

%right '|'
%right in
%left ':'
%right '>>'
%right ','
%nonassoc '='
%nonassoc intbincmp
%left '+' '-'
%left '*' '/'
%right '~'

%%

DeclList :: { [Decl] }
  : { [ ] }
  | Decl ';' DeclList { $1 : $3 }

Decl :: { Decl }
  : data uname '(' IdentList ')' of DataAltList 
     { DataDecl (NTyCon $2) (DataDefn $4 $7) }
  | lname '(' TypedIdentList ')' Typing '=' Expr
     { FuncDecl (NTerm $1) (FuncDefn $3 $5 $7) }

Expr :: { Expr }
  : int { EInt $1 }
  | string { EStr $1 }
  | lname { EVar $1 }
  | uname '(' ExprList ')' { EConstrAp (NDataCon $1) $3 }
  | lname '(' ExprList ')' { EAp (NTerm $1) $3 }
  | case Expr of '{' ProdList '}' { ECase $2 $5 }
  | let TypedIdent '=' Expr in Expr { ELet $2 $4 $6 }
  | Expr '+' Expr { EIntBinOp Plus $1 $3 }
  | Expr '-' Expr { EIntBinOp Minus $1 $3 }
  | Expr '*' Expr { EIntBinOp Times $1 $3 }
  | Expr '/' Expr { EIntBinOp Div   $1 $3 }
  | Expr intbincmp Expr { EIntBinCmp $2 $1 $3 }
  | '~' Expr { ENegate $2 }
  | Expr '>>' Expr { ESeq $1 $3 }
  | Expr ':' TyExpr { Typed $1 $3 }
  | '(' Expr ')' { $2 }

ExprList :: { [Expr] }
  :    { [] }
  | ExprList1 { $1 }

ExprList1 :: { [Expr] }
  : Expr { [ $1 ] }
  | Expr ',' ExprList1 { $1 : $3 }

TyExpr :: { TyExpr }
  : intty { IntTy }
  | strty { StrTy }
  | lname { TyVar $1 }
  | uname '(' TyExprList ')' { TAp (NTyCon $1) $3 }

TyExprList :: { [TyExpr] }
 :    { [] }
 | TyExprList1  { $1 }

TyExprList1 :: { [TyExpr] }
 : TyExpr { [ $1 ] }
 | TyExpr ',' TyExprList1 { $1 : $3 }

Production :: { Production Expr }
  : Pattern '=>' Expr { Production $1 $3 }

Pattern :: { Pattern }
  : uname '(' IdentList ')' { Pattern (NDataCon $1) $3 }

IdentList :: { [NTerm] }
  :   { [] }
  | IdentList1 { $1 }

IdentList1 :: { [NTerm] }
  : lname { [ NTerm $1 ] }
  | lname ',' IdentList1 { (NTerm $1) : $3 }

TypedIdentList :: { [TypedIdent] }
  :   { [] }
  | TypedIdentList1 { $1 }

TypedIdentList1 :: { [TypedIdent] }
  : TypedIdent { [ $1 ] }
  | TypedIdent ',' TypedIdentList1 { $1 : $3 }

ProdList :: { [Production Expr] }
  :   { [] }
  | ProdList1 { $1 }

ProdList1 :: { [Production Expr] }
  : Production  { [ $1 ] }
  | Production '|' ProdList1 { $1 : $3 }

DataAlt :: { DataAlt }
  : uname '(' TyExprList ')' { DataAlt (NDataCon $1) $3 }

DataAltList :: { [DataAlt] }
  :   { [] }
  | DataAltList1 { $1 }

DataAltList1 :: { [DataAlt] }
  : DataAlt  { [ $1 ] }
  | DataAlt '|' DataAltList1 { $1 : $3 }

TypedIdent :: { TypedIdent }
  : lname Typing { TypedIdent (NTerm $1) $2 }

Typing :: { Maybe TyExpr }
  :   { Nothing }
  | ':' TyExpr { Just $2 }

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

happyError :: Token -> Alex a
happyError tok = do
  (AlexPn _ l c, _, _, _) <- alexGetInput
  alexError $ "Line " ++ show l ++ ", column " ++ show c 
    ++ ": Parse error on Token: " ++ show tok ++ "\n"

parseDecls :: String -> Either String [Decl]
parseDecls s = runAlex s parse

testParse :: String -> [Decl]
testParse s = case parseDecls s of
  Left err -> error err
  Right x -> x

}
