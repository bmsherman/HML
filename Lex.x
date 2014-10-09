{
{-# OPTIONS_GHC -w #-}
module Lex (runLex, Token (..), Side (..), IntBinOp (..)) where

import AST (IntBinOp (..), IntBinCmp (..))
}

%wrapper "basic"

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  $eol                          ;
  $white+                       ;
  "--".*                        ;
  $digit+                       { Int . read }
  "="                           { const Equals }
  "+"                           { const $ IntBinOp Plus }
  "-"                           { const $ IntBinOp Minus }
  "*"                           { const $ IntBinOp Times }
  "~"                           { const Negate }
  "=="                          { const $ IntBinCmp CmpEQ }
  "<="                          { const $ IntBinCmp CmpLEQ }
  "<"                           { const $ IntBinCmp CmpLT }
  ">>"                          { const Seq }
  :                             { const Colon }
  ";"                           { const Semi }
  \,                            { const Comma }
  \(                            { const $ Paren L }
  \)                            { const $ Paren R }
  \[                            { const $ Bracket L }
  \]                            { const $ Bracket R }
  \{                            { const $ Brace L }
  \}                            { const $ Brace R }
  "||"                          { const Or }
  "=>"                          { const To }
  \" ([^\"])* \"                { String }
  "data"                        { const Data }
  "func"                        { const Func }
  "case"                        { const Case }
  "of"                          { const Of }
  "let"                         { const Let }
  "in"                          { const In }
  $lower [$alpha $digit \_ \']* { LName }
  $upper [$alpha $digit \_ \']* { UName }

{
data Token =
    Int Int
  | String String
  | LName String
  | UName String
  | Paren Side | Bracket Side | Brace Side
  | Colon | Comma | Semi
  | Or | To | Data | Func | Case | Of | Let | In
  | Seq
  | IntBinOp IntBinOp
  | IntBinCmp IntBinCmp
  | Negate
  | Equals
  deriving Show

data Side = L | R deriving Show

runLex = alexScanTokens  

}  
