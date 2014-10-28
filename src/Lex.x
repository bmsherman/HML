{
{-# OPTIONS_GHC -w #-}
module Lex where

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
  "+" | "-" | "*" | "/" | "~"           { lex FuncT }
  "==" | "<=" | "<" | ">" | ">="        { lex FuncT }
  ">>"                                  { lex FuncT }
  : | ";" | "," | "|" | "=>"            { lex Ctrl }
  [\( \) \{ \}] | "[" | "]"             { lex Delim }
  \" ([^\"])* \"                        { lex String }
  "data" | "case" | "of" | "let" | "in" { lex Keyword }
  $lower [$alpha $digit \_ \']*         { lex LName }
  $upper [$alpha $digit \_ \']*         { lex UName }

{

data Token =
    Int Int
  | String String
  | LName String
  | UName String
  | Delim String
  | Ctrl String
  | Keyword String
  | FuncT String
  | Equals
  | TokenEOF

instance Show Token where
  show t = case t of
    Int i -> show i
    String s -> show s
    LName s -> s
    UName s -> s
    Delim s -> s
    Ctrl s -> s
    Keyword s -> s
    Equals -> "="
    TokenEOF -> "EOF"
    FuncT s -> s

alexEOF = return TokenEOF

lex :: (String -> a) -> AlexAction a
lex f = \(_,_,_,s) i -> return (f (take i s))

lexc :: a -> AlexAction a
lexc = lex . const

type AlexUserState = (Context, [String])

alexInitUserState :: AlexUserState
alexInitUserState = (emptyContext, [])

}  
