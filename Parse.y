{

module Parse where

import Control.Applicative ((<$>))
import Control.Arrow (left)
import Control.Monad (liftM, liftM2)

import Data.Char (toLower)
import Data.List (intercalate)
import Lex
import AST
import TypeCheck

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
  : { [] }
  | DeclList Decl ';' {% liftM (: $1) (processDecl $2)  }

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
  | case Expr '{' ProdList '}' { ECase $2 $4 }
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
  | lname { TyVar (TV Flex $1) }
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


processDecl :: Decl -> Alex Decl
processDecl d = do
  ctxt <- alexGetUserState
  --traceShow d True `seq` return ()
  dctxt <- eToA $ case d of
    DataDecl tycon@(NTyCon tyN) datadef ->
      fmap dataDefCtxt (elabDataDef tycon datadef)
    FuncDecl n@(NTerm nt) funcDef ->
      left (\x -> ["In function declaration '" ++ nt ++ "', " ++ x]) 
        $ funcCtxt ctxt n funcDef
  ctxt' <- eToA $ unionC ctxt dctxt
  alexSetUserState ctxt'
  return d
  where
  eToA (Left xs) = do
    prefix <- prefixPos
    alexError $ intercalate "\n" [ prefix ++ ": " ++ x | x <- xs ]
  eToA (Right y) = return y

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

prefixPos :: Alex String
prefixPos = do
  (AlexPn _ l c, _, _, _) <- alexGetInput
  return $ "Line " ++ show l ++ ", column " ++ show c

lineError :: String -> Alex a
lineError s = do
  prefix <- prefixPos
  alexError $ prefix ++ ": " ++ s

happyError :: Token -> Alex a
happyError tok = lineError ("Parse error on token: '" ++ show tok ++ "'")

parseDecls :: String -> Either String ([Decl], Context)
parseDecls s = runAlex s $
  liftM2 (,) (liftM reverse parse) alexGetUserState

}
