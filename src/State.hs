{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module State where

    import Prelude

    type Var = String
    type Val = Int

    -- Um VarState é apenas a descrição de uma variável (identificada pelo seu nome, uma String) e do seu valor inteiro
    type VarState = (Var, Val)

    -- Um Estado é apenas uma lista de variáveis, com os seus respetivos valores definidos
    newtype State = State [VarState] deriving Eq

    instance Show State where
        show (State [])              = ""
        show (State ((var, val):xs)) = "[" ++ var ++ " -> " ++ (show val) ++ "]" ++ (show (State xs))
    
    update :: VarState -> State -> State
    update (x, v) (State []) = State [(x, v)]
    update (x, v) (State ((var, val):s))
            | var == x  = State $ (var, v) : s
            | otherwise = let (State s') = update (x, v) (State s)
                          in  (State $ (var, val):s')

    fetch :: Var -> State -> Val
    fetch v (State []) = 0
    fetch v (State ((var, val):s)) | var == v  = val
                                   | otherwise = fetch v (State s)
    