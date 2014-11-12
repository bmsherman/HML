{

module Parse where

import Control.Applicative ((<$>))
import Control.Arrow (left)
import Control.Monad (liftM, liftM2)

import Data.Char (toLower)
import Data.List (intercalate)
import Lex
import AST
import Typecheck

}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { TokenEOF }
%error { happyError }

%token
  '('              { Tok "(" }
  ')'              { Tok ")" }
  '{'              { Tok "{" }
  '}'              { Tok "}" }
  '['              { Tok "[" }
  ']'              { Tok "]" }
  ':'              { Tok ":" }
  ','              { Tok "," }
  ';'              { Tok ";" }
  '+'              { Tok "+" }
  '-'              { Tok "-" }
  '*'              { Tok "*" }
  '/'              { Tok "/"   }
  intbincmp        { Tok x | x `elem` ["<=", "==", "<", ">", ">="] }
  '~'              { Tok "~" }
  '|'              { Tok "|" }
  '=>'             { Tok "=>" }
  '>>'             { Tok ">>" }
  int              { Int $$ }
  string           { String $$ }
  '='              { Equals  }
  '->'             { FuncArr }
  data             { Tok "data" }
  case             { Tok "case" }
  of               { Tok "of"   }
  let              { Tok "let"  }
  in               { Tok "in"   }
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
  : data uname '(' IdentList ')' DataAltList 
     { DataDecl $2 (DataDefn $4 $6) }
  | lname '(' TypedIdentList ')' Typing '=' Expr
     { FuncDecl $1 (FuncDefn $3 $5 $7) }

Expr :: { Expr }
  : int { EInt $1 }
  | string { EStr $1 }
  | lname { EVar $1 }
  | uname '(' ExprList ')' { EAp DataCon $1 $3 }
  | lname '(' ExprList ')' { EAp Func $1 $3 }
  | case Expr '{' ProdList '}' { ECase $2 $4 }
  | let TypedIdent '=' Expr in Expr { ELet $2 $4 $6 }
  | Expr '+' Expr { EAp Func "plus" [$1, $3] }
  | Expr '-' Expr { EAp Func "minus" [$1, $3] }
  | Expr '*' Expr { EAp Func "times" [$1, $3] }
  | Expr '/' Expr { EAp Func "div" [$1, $3] }
  | Expr intbincmp Expr { EAp Func (cmpFunc $2) [$1, $3] }
  | '~' Expr { EAp Func "negate" [$2] }
  | Expr '>>' Expr { EAp Func "seq" [$1, $3] }
  | Expr ':' TyExpr { Typed $1 $3 }
  | '(' Expr ')' { $2 }
  | '[' ExprList ']' { foldr (\x y -> EAp DataCon "Cons" [x, y])
                       (EAp DataCon "Nil" []) $2 }

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
  | uname '(' TyExprList ')' { TAp $1 $3 }
  | '(' TyExprList ')' '->' TyExpr { TArr $2 $5 }

TyExprList :: { [TyExpr] }
 :    { [] }
 | TyExprList1  { $1 }

TyExprList1 :: { [TyExpr] }
 : TyExpr { [ $1 ] }
 | TyExpr ',' TyExprList1 { $1 : $3 }

Production :: { Production Expr }
  : Pattern '=>' Expr { Production $1 $3 }

Pattern :: { Pattern }
  : uname '(' IdentList ')' { Pattern $1 $3 }

IdentList :: { [NTerm] }
  :   { [] }
  | IdentList1 { $1 }

IdentList1 :: { [NTerm] }
  : lname { [ $1 ] }
  | lname ',' IdentList1 { $1 : $3 }

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
  : uname '(' TyExprList ')' { DataAlt $1 $3 }

DataAltList :: { [DataAlt] }
  :   { [] }
  | of DataAltList1 { $2 }

DataAltList1 :: { [DataAlt] }
  : DataAlt  { [ $1 ] }
  | DataAlt '|' DataAltList1 { $1 : $3 }

TypedIdent :: { TypedIdent }
  : lname Typing { TypedIdent $1 $2 }

Typing :: { Maybe TyExpr }
  :   { Nothing }
  | ':' TyExpr { Just $2 }

{

processDecl :: Decl -> Alex Decl
processDecl d = do
  (ctxt, prefix, errors) <- alexGetUserState
  let result = (unionC ctxt =<<) $ case d of
	DataDecl tycon datadef ->
	  left (map (\x -> "In data declaration '" ++ tycon ++ "', \n  " ++ x) ) $
            dataDefCtxt (tycons ctxt) tycon datadef
	FuncDecl nt funcDef ->
	  left (:[]) 
	    $ funcCtxt ctxt nt funcDef
  prefix' <- prefixPos
  alexSetUserState $ case result of
    Left es -> (ctxt, prefix', [ prefix ++ ": " ++ x | x <- es ] ++ errors)
    Right ctxt' -> (ctxt', prefix', errors)
  return d

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

cmpFunc :: Token -> String
cmpFunc (Tok x) = (++ "Int") $ case x of
  "<" -> "lt"
  "<=" -> "lte"
  "==" -> "eq"
  ">=" -> "gte"
  ">" -> "gt"

prefixPos :: Alex String
prefixPos = do
  (AlexPn _ l _, _, _, _) <- alexGetInput
  return $ "Line " ++ show l

happyError :: Token -> Alex a
happyError tok = do
  prefix <- prefixPos
  alexError $ prefix ++ ": " ++ "Parse error on token: '" ++ show tok ++ "'"

parseDecls :: Context -> String -> Either String ([Decl], (Context, [String]))
parseDecls ctxt s = runAlex s $ do
  prefix <- prefixPos
  alexSetUserState (ctxt,prefix, [])
  decls <- parse
  (ctxt, _, errors) <- alexGetUserState
  return (reverse decls, (ctxt, reverse errors))

}
