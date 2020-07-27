{
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Parser where

import Data.Char

import qualified State
import qualified Aexp
import qualified Bexp
import qualified Stm

import State (State (State))
import Aexp (Aexp)
import Bexp (Bexp)
import Stm (Stm)
}

%name readAexp Aexp
%name readBexp Bexp
%name readState State
%name readStm StmList

%tokentype { Token }
%error { parseError }

%token
    -- Aexp tokens
    int { TokenInt $$ }
    var { TokenVar $$ }
    '+' { TokenAdd }
    '*' { TokenMult }
    '-' { TokenSub }
    '(' { TokenOP }
    ')' { TokenCP }
    -- Bexp tokens
    "true" { TokenTrue }
    "false" { TokenFalse }
    '=' { TokenEq }
    '<' { TokenLT }
    '!' { TokenNot }
    '&' { TokenAnd }
    '|' { TokenOr }
    -- State tokens
    '[' { TokenOSB }
    ']' { TokenCSB }
    '>' { TokenGT }
    ',' { TokenComma }
    -- Stm tokens
    ';' { TokenSemicolon }
    "skip" { TokenSkip }
    "if" { TokenIf }
    "then" { TokenThen }
    "else" { TokenElse }
    "while" { TokenWhile }
    "do" { TokenDo }
    '{' { TokenOCB }
    '}' { TokenCCB }
    ':' { TokenColon }

%%

-- Aexp
Aexp : var { Aexp.Var $1 }
     | int { Aexp.Num $1 }
     | '(' Aexp ')' { $2 }
     | '-' Aexp { Aexp.Sim $2 }
     | Aexp '+' Aexp { Aexp.Add $1 $3 }
     | Aexp '*' Aexp { Aexp.Mult $1 $3 }
     | Aexp '-' Aexp { Aexp.Sub $1 $3 }
     ;

-- Bexp
Bexp : "true" { Bexp.True }
     | "false" { Bexp.False }
     | '(' Bexp ')' { $2 }
     | Aexp '=' '=' Aexp { Bexp.Equals $1 $4 }
     | Aexp '>' Aexp { Bexp.GT $1 $3 }
     | Aexp '>' '=' Aexp { Bexp.GE $1 $4 }
     | Aexp '<' Aexp { Bexp.LT $1 $3 }
     | Aexp '<' '=' Aexp { Bexp.LE $1 $4 }
     | '!' '(' Bexp ')' { Bexp.Not $3 }
     | Bexp '&' '&' Bexp { Bexp.And $1 $4 }
     | Bexp '|' '|' Bexp { Bexp.Or $1 $4 }
     ;
    

-- State
State : '[' StateVarList ']' { State $2 }
      | StateList { State $1 }
      | { State [] }
      ;

StateVarList : StateVar { [$1] }
             | StateVarList ',' StateVar { $1 ++ [$3] }
             ;

StateList : '[' StateVar ']' { [$2] }
          | StateList '[' StateVar ']' { $1 ++ [$3] }
          ;

StateVar : var '-' '>' int { ($1, $4) }
         | var '-' '>' '-' int { ($1, -$5) }
         ;
    
-- Stm
StmList : StmBlock { $1 }
        | StmList ';' StmBlock { Stm.Comp $1 $3 }
        ;

StmBlock : Stm { $1 }
         | '{' StmList '}' { $2 }
         ;

Stm : "skip" { Stm.Skip }
    | var ':' '=' Aexp { Stm.Assign $1 $4 }
    | "if" Bexp "then" StmBlock "else" StmBlock { Stm.ITE $2 $4 $6 }
    | "if" Bexp "then" StmBlock { Stm.If $2 $4 }
    | "while" Bexp "do" StmBlock { Stm.While $2 $4 }
    ;
{

-- Called in case of parsing errors
parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens

-- Defines the tokens that the lexer can parse
data Token =
            -- Aexp tokens
             TokenInt State.Val
           | TokenVar State.Var
           | TokenAdd
           | TokenMult
           | TokenSub
           | TokenOP
           | TokenCP
           -- Bexp tokens
           | TokenTrue
           | TokenFalse
           | TokenEq
           | TokenLT
           | TokenNot
           | TokenAnd
           | TokenOr
           -- State tokens
           | TokenOSB
           | TokenCSB
           | TokenGT
           | TokenComma
           -- Stm tokens
           | TokenSemicolon
           | TokenSkip
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenWhile
           | TokenDo
           | TokenOCB
           | TokenCCB
           | TokenColon

           deriving (Show)

-- The lexer splits the input string into tokens
lexer :: String -> [Token]
lexer [] = []

lexer ('+':cs) = TokenAdd : lexer cs
lexer ('-':cs) = TokenSub : lexer cs
lexer ('*':cs) = TokenMult : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs

lexer ('t':'r':'u':'e':cs) = TokenTrue : lexer cs
lexer ('f':'a':'l':'s':'e':cs) = TokenFalse : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('<':cs) = TokenLT : lexer cs
lexer ('!':cs) = TokenNot : lexer cs
lexer ('&':cs) = TokenAnd : lexer cs
lexer ('|':cs) = TokenOr : lexer cs

lexer ('[':cs) = TokenOSB : lexer cs
lexer (']':cs) = TokenCSB : lexer cs
lexer ('>':cs) = TokenGT : lexer cs
lexer (',':cs) = TokenComma : lexer cs

lexer (';':cs) = TokenSemicolon : lexer cs
lexer ('s':'k':'i':'p':cs) = TokenSkip : lexer cs
lexer ('i':'f':cs) = TokenIf : lexer cs
lexer ('t':'h':'e':'n':cs) = TokenThen : lexer cs
lexer ('e':'l':'s':'e':cs) = TokenElse : lexer cs
lexer ('w':'h':'i':'l':'e':cs) = TokenWhile : lexer cs
lexer ('d':'o':cs) = TokenDo : lexer cs
lexer ('{':cs) = TokenOCB : lexer cs
lexer ('}':cs) = TokenCCB : lexer cs
lexer (':':cs) = TokenColon : lexer cs

lexer ('"':cs) = lexer cs -- Ignore quotes around strings
lexer (c:cs) | isSpace c = lexer cs
             | isAlpha c = lexVar (c:cs)
             | isDigit c = lexNum (c:cs)

-- Lexer helper functions
lexVar cs = TokenVar var : lexer rest
            -- Nomes de variáveis podem ter dígitos em todos os caracteres, excepto no primeiro
            where (var, rest) = span (\x -> isAlpha x || isDigit x) cs

lexNum cs = TokenInt (read num) : lexer rest
            where (num, rest) = span isDigit cs

-- Define instances of read for the datatypes that can be parsed

instance Read Aexp.Aexp where
   readsPrec _ input = [(readAexp $ lexer input, "")]

instance Read Bexp.Bexp where
   readsPrec _ input = [(readBexp $ lexer input, "")]

instance Read State.State where
    readsPrec _ input = [(readState $ lexer input, "")]

instance Read Stm.Stm where
    readsPrec _ input = [(readStm $ lexer input, "")]
}
