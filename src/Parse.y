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
  '('              { Delim "(" }
  ')'              { Delim ")" }
  '{'              { Delim "{" }
  '}'              { Delim "}" }
  '['              { Delim "[" }
  ']'              { Delim "]" }
  ':'              { Ctrl ":" }
  ','              { Ctrl "," }
  ';'              { Ctrl ";" }
  '+'              { FuncT "+" }
  '-'              { FuncT "-" }
  '*'              { FuncT "*" }
  '/'              { FuncT "/"   }
  intbincmp        { FuncT x | x `elem` ["<=", "==", "<", ">", ">="] }
  '~'              { FuncT "~" }
  '|'              { Ctrl "|" }
  '=>'             { Ctrl "=>" }
  '>>'             { FuncT ">>" }
  int              { Int $$ }
  string           { String $$ }
  '='              { Equals  }
  data             { Keyword "data" }
  case             { Keyword "case" }
  of               { Keyword "of"   }
  let              { Keyword "let"  }
  in               { Keyword "in"   }
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
  | uname '(' ExprList ')' { EAp DataCon $1 $3 }
  | lname '(' ExprList ')' { EAp Func $1 $3 }
  | case Expr '{' ProdList '}' { ECase $2 $4 }
  | let TypedIdent '=' Expr in Expr { ELet $2 $4 $6 }
  | Expr '+' Expr { (primFunc "+") [$1, $3] }
  | Expr '-' Expr { (primFunc "-") [$1, $3] }
  | Expr '*' Expr { (primFunc "*") [$1, $3] }
  | Expr '/' Expr { (primFunc "/") [$1, $3] }
  | Expr intbincmp Expr { (primFunc (unFunc $2)) [$1, $3] }
  | '~' Expr { (primFunc "~") [$2] }
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

primFunc :: String -> [Expr] -> Expr
primFunc s = EAp Func ('_' : s)

processDecl :: Decl -> Alex Decl
processDecl d = do
  (ctxt, errors) <- alexGetUserState
  let result = (unionC ctxt =<<) $ case d of
	DataDecl tycon@(NTyCon tyN) datadef ->
	  left (map (\x -> "In data declaration '" ++ tyN ++ "', " ++ x) ) $
            dataDefCtxt (tycons ctxt)
              =<< elabDataDef tycon datadef
	FuncDecl n@(NTerm nt) funcDef ->
	  left (:[]) 
	    $ funcCtxt ctxt n funcDef
  prefix <- prefixPos
  alexSetUserState $ case result of
    Left es -> (ctxt, [ prefix ++ ": " ++ x | x <- es ] ++ errors)
    Right ctxt' -> (ctxt', errors)
  return d

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

unFunc :: Token -> String
unFunc (FuncT x) = x

prefixPos :: Alex String
prefixPos = do
  (AlexPn _ l c, _, _, _) <- alexGetInput
  return $ "Line " ++ show l ++ ", column " ++ show c

happyError :: Token -> Alex a
happyError tok = do
  prefix <- prefixPos
  alexError $ prefix ++ ": " ++ "Parse error on token: '" ++ show tok ++ "'"

parseDecls :: Context -> String -> Either String ([Decl], (Context, [String]))
parseDecls ctxt s = runAlex s $ do
  alexSetUserState (ctxt, [])
  decls <- parse
  (ctxt, errors) <- alexGetUserState
  return (reverse decls, (ctxt, reverse errors))

}
