{
{-# OPTIONS_GHC -w #-}
module Lex where

import Data.Int (Int32)
import Typecheck (Context, emptyContext)
import Prelude hiding (lex)
}

%wrapper "monadUserState"

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  $eol                          ;
  $white+                       ;
  "--".*                        ;
  $digit+                               { lex (Int . read) }
  "="                                   { lexc Equals }
  "->"                                  { lexc FuncArr }
  "+" | "-" | "*" | "/" | "~"           { lex Tok }
  "==" | "<=" | "<" | ">" | ">="        { lex Tok }
  ">>"                                  { lex Tok }
  : | ";" | "," | "|" | "=>"            { lex Tok }
  [\( \) \{ \}] | "[" | "]"             { lex Tok }
  "data" | "case" | "of" | "let" | "in" { lex Tok }
  \" ([^\"])* \"                        { lex (String . init . tail) }
  $lower [$alpha $digit \_ \']*         { lex LName }
  $upper [$alpha $digit \_ \']*         { lex UName }

{

data Token =
    Int Int32
  | String String
  | LName String
  | UName String
  | Tok String
  | Equals
  | FuncArr
  | TokenEOF

instance Show Token where
  show t = case t of
    Int i -> show i
    String s -> show s
    LName s -> s
    UName s -> s
    Tok s -> s
    Equals -> "="
    FuncArr -> "->"
    TokenEOF -> "EOF"

alexEOF = return TokenEOF

lex :: (String -> a) -> AlexAction a
lex f = \(_,_,_,s) i -> return (f (take i s))

lexc :: a -> AlexAction a
lexc = lex . const

type AlexUserState = (Context, String, [String])

alexInitUserState :: AlexUserState
alexInitUserState = (emptyContext, "", [])

}  
