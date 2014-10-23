module AST where

import Data.List (intercalate)

newtype NTerm = NTerm String deriving (Eq, Show, Ord)
newtype NTyCon = NTyCon String deriving (Eq, Show, Ord)
newtype NDataCon = NDataCon String deriving (Eq, Show, Ord)

data Expr = EInt Int
  | EStr String
  | EVar String
  | EAp Inj String [Expr]
  | ECase Expr [Production Expr]
  | ELet TypedIdent Expr Expr
  | Typed Expr TyExpr
  deriving Show

data TyExpr = IntTy | StrTy
  | TyVar TyVar
  | TAp NTyCon [TyExpr]
  deriving Show

data TyVar = TV Flex String deriving (Eq, Show)

data Flex = Flex | Rigid deriving (Eq, Show)

data Inj = Func | DataCon deriving Show

pprintTyExpr :: TyExpr -> String
pprintTyExpr x = case x of
  IntTy -> "Int"
  StrTy -> "String"
  TyVar (TV _ s) -> s
  TAp (NTyCon tycon) exprs -> tycon ++ "(" 
    ++ intercalate ", " (map pprintTyExpr exprs) ++ ")"

data DataDefn = DataDefn [NTerm] [DataAlt]
  deriving Show

data DataAlt = DataAlt NDataCon [TyExpr]
  deriving Show

data FuncDefn = FuncDefn [TypedIdent] (Maybe TyExpr) Expr
  deriving Show

data TypedIdent = TypedIdent NTerm (Maybe TyExpr)
  deriving Show

data Decl = DataDecl NTyCon DataDefn | FuncDecl NTerm FuncDefn
  deriving Show

data Production e = Production Pattern e
  deriving Show

data Pattern = Pattern NDataCon [NTerm]
  deriving Show

