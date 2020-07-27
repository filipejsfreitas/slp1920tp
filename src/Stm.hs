{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Stm where

    import State
    import Aexp
    import Bexp

    data Stm = Skip                    -- Skip
             | Assign   Var  Aexp      -- Atribuição
             | Comp     Stm  Stm       -- Composição
             | ITE      Bexp Stm  Stm  -- If Then Else
             | If       Bexp Stm       -- If Then
             | While    Bexp Stm       -- While do
             deriving Eq

    -- Implementa uma função de show para o tipo Stm, para ser fácil de visualizar
    -- a sua saída
    instance Show Stm where
        show Skip = "skip"

        show (Assign var exp) = "" ++ (id  var) ++ " := " ++ (show exp)

        show (Comp c1 c2)     = "" ++ (show c1) ++ "; "   ++ (show c2)

        show (ITE b c1 c2)    = "if "    ++ (show b) ++ " then {" ++ (show c1) ++ "} else {" ++ (show c2) ++ "}"
        show (If b c)         = "if "    ++ (show b) ++ " then {" ++ (show c)  ++ "}"

        show (While b c)      = "while " ++ (show b) ++   " do {" ++ (show c)  ++ "}"
    