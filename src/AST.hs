module AST where

import Data.List (intercalate)

-- | names of terms
newtype NTerm = NTerm String deriving (Eq, Show, Ord)

-- | names of type constructors
newtype NTyCon = NTyCon String deriving (Eq, Show, Ord)

-- | names of data constructors
newtype NDataCon = NDataCon String deriving (Eq, Show, Ord)

-- | Term-level expressions
data Expr = EInt Int              -- ^ constant integer
  | EStr String                   -- ^ constant string
  | EVar String                   -- ^ variable reference
  | EAp Inj String [Expr]         -- ^ application of function or 
                                  -- data constructor
  | ECase Expr [Production Expr]  -- ^ case expression
  | ELet TypedIdent Expr Expr     -- ^ let binding
  | Typed Expr TyExpr             -- typing assertion
  deriving Show

-- | Type-level expressions
data TyExpr = IntTy | StrTy  -- ^ the types of integers and strings
  | TyVar TyVar              -- ^ type variable
  | TAp NTyCon [TyExpr]      -- ^ type constructor application
  deriving Show

-- | A type variable specifies its flexibility (how type inference ought to
-- work) and its name
data TyVar = TV Flex String deriving (Eq, Show)

-- | A flexible variable may be unified to a more specific type, but
-- a rigid variable must not ever be unified with something it is not
-- already requal to.
data Flex = Flex | Rigid deriving (Eq, Show)

-- | Distingiush between functions and data constructors
data Inj = Func | DataCon deriving Show

-- | pretty-print type expression
pprintTyExpr :: TyExpr -> String
pprintTyExpr x = case x of
  IntTy -> "Int"
  StrTy -> "String"
  TyVar (TV _ s) -> s
  TAp (NTyCon tycon) exprs -> tycon ++ "(" 
    ++ intercalate ", " (map pprintTyExpr exprs) ++ ")"

-- | Data type declaration
data DataDefn = DataDefn [NTerm] [DataAlt]
  deriving Show

data DataAlt = DataAlt NDataCon [TyExpr]
  deriving Show

-- | Function declaration
data FuncDefn = FuncDefn [TypedIdent] (Maybe TyExpr) Expr
  deriving Show

-- | An identifier, possibly along with a type
data TypedIdent = TypedIdent NTerm (Maybe TyExpr)
  deriving Show

-- | Top-level declarations
data Decl = DataDecl NTyCon DataDefn | FuncDecl NTerm FuncDefn
  deriving Show

-- | Production = pattern => expr
data Production e = Production Pattern e
  deriving Show

-- | Constructor pattern for binding case variables
data Pattern = Pattern NDataCon [NTerm]
  deriving Show

