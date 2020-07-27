{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Bexp where

    import Prelude hiding (GT, LT)
    import State
    import Aexp

    data Bexp = True                -- True
              | False               -- False
              | Equals  Aexp Aexp   -- Igualdade
              | GT      Aexp Aexp   -- Maior
              | GE      Aexp Aexp   -- Maior ou Igual
              | LT      Aexp Aexp   -- Menor
              | LE      Aexp Aexp   -- Menor ou Igual
              | Not     Bexp        -- Negação
              | And     Bexp Bexp   -- Conjunção
              | Or      Bexp Bexp   -- Disjunção
              deriving Eq

    -- Implementa uma função de show para o tipo Bexp, para ser fácil de visualizar
    -- a sua saída
    -- A aparente complexidade desta função é apenas para permitir uma apresentação bonita dos resultados
    instance Show Bexp where
        show (Bexp.True)                      = "true"
        show (Bexp.False)                     = "false"


        show (Equals a b)                     = (show a) ++ " = "  ++ (show b)
        show (GT     a b)                     = (show a) ++ " > "  ++ (show b)
        show (GE     a b)                     = (show a) ++ " >= " ++ (show b)
        show (LT     a b)                     = (show a) ++ " < "  ++ (show b)
        show (LE     a b)                     = (show a) ++ " <= " ++ (show b)


        show (Not    a)                       = "!(" ++ (show a) ++ ")"

        
        show (And  a@(And _ _)  b@(And _ _))  = "(" ++ (show a) ++ ")" ++ " && " ++ "(" ++ (show b) ++ ")"
        show (And  a@(And _ _)  b@(Or  _ _))  = "(" ++ (show a) ++ ")" ++ " && " ++ "(" ++ (show b) ++ ")"
        
        show (And  a@(Or  _ _)  b@(And _ _))  = "(" ++ (show a) ++ ")" ++ " && " ++ "(" ++ (show b) ++ ")"
        show (And  a@(Or  _ _)  b@(Or  _ _))  = "(" ++ (show a) ++ ")" ++ " && " ++ "(" ++ (show b) ++ ")"

        show (And  a@(And _ _)  b          )  = "(" ++ (show a) ++ ")" ++ " && " ++        (show b)
        show (And  a@(Or  _ _)  b          )  = "(" ++ (show a) ++ ")" ++ " && " ++        (show b)

        show (And  a            b@(And _ _))  =        (show a)        ++ " && " ++ "(" ++ (show b) ++ ")"
        show (And  a            b@(Or  _ _))  =        (show a)        ++ " && " ++ "(" ++ (show b) ++ ")"

        show (And  a            b          )  =        (show a)        ++ " && " ++        (show b)


        show (Or   a@(And _ _) b@(And _ _))   = "(" ++ (show a) ++ ")" ++ " || " ++ "(" ++ (show b) ++ ")"
        show (Or   a@(And _ _) b@(Or  _ _))   = "(" ++ (show a) ++ ")" ++ " || " ++ "(" ++ (show b) ++ ")"

        show (Or   a@(Or  _ _) b@(And _ _))   = "(" ++ (show a) ++ ")" ++ " || " ++ "(" ++ (show b) ++ ")"
        show (Or   a@(Or  _ _) b@(Or  _ _))   = "(" ++ (show a) ++ ")" ++ " || " ++ "(" ++ (show b) ++ ")"

        show (Or   a@(And _ _) b          )   = "(" ++ (show a) ++ ")" ++ " || " ++        (show b)
        show (Or   a@(Or  _ _) b          )   = "(" ++ (show a) ++ ")" ++ " || " ++        (show b)

        show (Or   a           b@(And _ _))   =        (show a)        ++ " || " ++ "(" ++ (show b) ++ ")"
        show (Or   a           b@(Or  _ _))   =        (show a)        ++ " || " ++ "(" ++ (show b) ++ ")"

        show (Or   a           b          )   =        (show a)        ++ " || " ++        (show b)
    