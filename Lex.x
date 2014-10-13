{
{-# OPTIONS_GHC -w #-}
module Lex where

import AST (IntBinOp (..), IntBinCmp (..))
import Prelude hiding (lex)
}

%wrapper "monad"

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  $eol                          ;
  $white+                       ;
  "--".*                        ;
  $digit+                       { lex (Int . read) }
  "="                           { lexc Equals }
  "+"                           { lexc $ IntBinOp Plus }
  "-"                           { lexc $ IntBinOp Minus }
  "*"                           { lexc $ IntBinOp Times }
  "/"                           { lexc $ IntBinOp Div   }
  "~"                           { lexc Negate }
  "=="                          { lexc $ IntBinCmp CmpEQ }
  "<="                          { lexc $ IntBinCmp CmpLEQ }
  "<"                           { lexc $ IntBinCmp CmpLT }
  ">>"                          { lexc Seq }
  :                             { lexc Colon }
  ";"                           { lexc Semi }
  \,                            { lexc Comma }
  \(                            { lexc $ Paren L }
  \)                            { lexc $ Paren R }
  \[                            { lexc $ Bracket L }
  \]                            { lexc $ Bracket R }
  \{                            { lexc $ Brace L }
  \}                            { lexc $ Brace R }
  "|"                           { lexc Or }
  "=>"                          { lexc To }
  \" ([^\"])* \"                { lex String }
  "data"                        { lexc Data }
  "case"                        { lexc Case }
  "of"                          { lexc Of }
  "let"                         { lexc Let }
  "in"                          { lexc In }
  $lower [$alpha $digit \_ \']* { lex LName }
  $upper [$alpha $digit \_ \']* { lex UName }

{

data Token =
    Int Int
  | String String
  | LName String
  | UName String
  | Paren Side | Bracket Side | Brace Side
  | Colon | Comma | Semi
  | Or | To | Data | Case | Of | Let | In
  | Seq
  | IntBinOp IntBinOp
  | IntBinCmp IntBinCmp
  | Negate
  | Equals
  | TokenEOF
  deriving Show

alexEOF = return TokenEOF

lex :: (String -> a) -> AlexAction a
lex f = \(_,_,_,s) i -> return (f (take i s))

lexc :: a -> AlexAction a
lexc = lex . const

data Side = L | R deriving Show

}  
