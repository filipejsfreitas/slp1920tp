{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Aexp where

    import State

    data Aexp = Num  Int        -- Inteiro
              | Var  Var        -- Variável
              | Sim  Aexp       -- Simétrico
              | Add  Aexp Aexp  -- Soma
              | Mult Aexp Aexp  -- Multiplicação
              | Sub  Aexp Aexp  -- Subtração
              deriving Eq

    -- Implementa uma função de show para o tipo Aexp, para ser fácil de visualizar
    -- a sua saída
    -- A aparente complexidade desta função é apenas para tornar a saída bonita
    instance Show Aexp where
        show (Num x)                 = show x
        show (Var x)                 = x

        show (Sim (Num x))           = "-" ++ (show x)
        show (Sim (Var x))           = "-" ++ x
        show (Sim      x )           = "-" ++ (show x)
        
        show (Add (Num x) (Num y))   =        (show x)        ++ " + " ++        (show y)
        show (Add (Var x) (Var y))   =              x         ++ " + " ++              y
        show (Add (Var x) (Num y))   =              x         ++ " + " ++        (show y)
        show (Add (Num x) (Var y))   =        (show x)        ++ " + " ++              y
        show (Add (Num x)      y )   =        (show x)        ++ " + " ++ "(" ++ (show y) ++ ")"
        show (Add (Var x)      y )   =              x         ++ " + " ++ "(" ++ (show y) ++ ")"
        show (Add      x  (Num y))   = "(" ++ (show x) ++ ")" ++ " + " ++        (show y)
        show (Add      x  (Var y))   = "(" ++ (show x) ++ ")" ++ " + " ++              y
        show (Add      x       y )   =        (show x)        ++ " + " ++        (show y)
        
        show (Mult (Num x) (Num y))  =        (show x)        ++ " * " ++        (show y)
        show (Mult (Var x) (Var y))  =              x         ++ " * " ++              y
        show (Mult (Var x) (Num y))  =              x         ++ " * " ++        (show y)
        show (Mult (Num x) (Var y))  =        (show x)        ++ " * " ++              y
        show (Mult (Num x)      y )  =        (show x)        ++ " * " ++ "(" ++ (show y) ++ ")"
        show (Mult (Var x)      y )  =              x         ++ " * " ++ "(" ++ (show y) ++ ")"
        show (Mult      x  (Num y))  = "(" ++ (show x) ++ ")" ++ " * " ++        (show y)
        show (Mult      x  (Var y))  = "(" ++ (show x) ++ ")" ++ " * " ++              y
        show (Mult      x       y )  =        (show x)        ++ " * " ++        (show y)

        show (Sub (Num x) (Num y))   =        (show x)        ++ " - " ++        (show y)
        show (Sub (Var x) (Var y))   =              x         ++ " - " ++              y
        show (Sub (Var x) (Num y))   =              x         ++ " - " ++        (show y)
        show (Sub (Num x) (Var y))   =        (show x)        ++ " - " ++              y
        show (Sub (Num x)      y )   =        (show x)        ++ " - " ++ "(" ++ (show y) ++ ")"
        show (Sub (Var x)      y )   =              x         ++ " - " ++ "(" ++ (show y) ++ ")"
        show (Sub      x  (Num y))   = "(" ++ (show x) ++ ")" ++ " - " ++        (show y)
        show (Sub      x  (Var y))   = "(" ++ (show x) ++ ")" ++ " - " ++              y
        show (Sub      x       y )   =        (show x)        ++ " - " ++        (show y)

