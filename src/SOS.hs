{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module SOS where

    import Prelude hiding (GT, LT)
    import State
    import Aexp
    import Bexp
    import Stm
    import Parser
    import StmRepl (replStm)

    -- Função de utilidade para simplificar algum do código abaixo
    stepAux f c (Num a) (Num b) s           = Left $ a `f` b
    stepAux f c (Num a)      b  s           = case stepA (b, s) of
                                                  Left  b' -> Right $ c (Num a) (Num b')
                                                  Right b' -> Right $ c (Num a)      b'
    stepAux f c      a       b  s           = case stepA (a, s) of
                                                  Left  a' -> Right $ c (Num a')     b
                                                  Right a' -> Right $ c      a'      b

    stepA :: (Aexp, State) -> Either Val Aexp
    
    stepA (Num n,                     _)    = Left n
    stepA (Var v,                     s)    = Left $ fetch v s
    stepA (Sim x,                     s)    = case stepA (x, s) of
                                                  Left  x' -> Left  $ 0-x'
                                                  Right x' -> Right $ Sim x'

    stepA (Add  a b,                  s)    = stepAux (+) (Add)  a b s
    stepA (Mult a b,                  s)    = stepAux (*) (Mult) a b s
    stepA (Sub  a b,                  s)    = stepAux (-) (Sub)  a b s

    -- Função de utilidade, apenas para facilitar os casos abaixo
    toBexp Prelude.True                     = Bexp.True
    toBexp Prelude.False                    = Bexp.False

    stepB :: (Bexp, State) -> Either Bool Bexp

    stepB (Bexp.True,                 _)    = Left Prelude.True
    stepB (Bexp.False,                _)    = Left Prelude.False

    stepB (Equals a b,                s)    = stepAux (==) (Equals) a b s
    stepB (GT     a b,                s)    = stepAux (>)  (GT)     a b s
    stepB (GE     a b,                s)    = stepAux (>=) (GE)     a b s
    stepB (LT     a b,                s)    = stepAux (<)  (LT)     a b s
    stepB (LE     a b,                s)    = stepAux (<=) (LE)     a b s

    stepB (Not Bexp.True,             s)    = Left Prelude.False
    stepB (Not Bexp.False,            s)    = Left Prelude.True
    stepB (Not a,                     s)    = case stepB (a, s) of
                                                  Left  a' -> Right $ Not $ toBexp a'
                                                  Right a' -> Right $ Not a'

    stepB (And Bexp.True  Bexp.True,  s)    = Left Prelude.True
    stepB (And Bexp.True  Bexp.False, s)    = Left Prelude.False
    stepB (And Bexp.False Bexp.True,  s)    = Left Prelude.False
    stepB (And Bexp.False Bexp.False, s)    = Left Prelude.False
    stepB (And Bexp.True  b,          s)    = case stepB (b, s) of
                                                  Left  b' -> Right $ And Bexp.True $ toBexp b'
                                                  Right b' -> Right $ And Bexp.True          b'
    stepB (And Bexp.False b,         s)     = Right Bexp.False                                      -- False && x == False, para todo o x. Avaliação curto-circuito do C.
    stepB (And a          b,         s)     = case stepB (a, s) of
                                                  Left  a' -> Right $ And (toBexp a')        b
                                                  Right a' -> Right $ And         a'         b

    stepB (Or Bexp.True  Bexp.True,  s)     = Left  Prelude.True
    stepB (Or Bexp.True  Bexp.False, s)     = Left  Prelude.True
    stepB (Or Bexp.False Bexp.True,  s)     = Left  Prelude.True
    stepB (Or Bexp.False Bexp.False, s)     = Left  Prelude.False
    stepB (Or Bexp.True  b,          s)     = Right Bexp.True                                       -- True || x == True, para todo o x. Avaliação curto-circuito do C.
    stepB (Or Bexp.False b,          s)     = case stepB (b, s) of
                                                  Left  b' -> Right $ Or Bexp.False $ toBexp b'
                                                  Right b' -> Right $ Or Bexp.False          b'
    stepB (Or a          b,          s)     = case stepB (a, s) of
                                                  Left  a' -> Right $ Or (toBexp a')         b
                                                  Right a' -> Right $ Or         a'          b

    -- Função que executa apenas um passo de avaliação
    stepSOS :: (Stm, State) -> Either State (Stm, State)
    
    stepSOS (Skip,                   s)     = Left s
    stepSOS (Assign var (Num n),     s)     = Left s'
                                            where s' = update (var, n) s
    stepSOS (Assign var exp,         s)     = case stepA (exp, s) of
                                                  Left  n    -> Right (Assign var (Num n), s)
                                                  Right exp' -> Right (Assign var exp',    s)

    stepSOS (Comp c1 c2,             s)     = case stepSOS (c1, s) of
                                                  Left       s'  -> Right (c2,         s')
                                                  Right (c3, s') -> Right (Comp c3 c2, s')

    stepSOS (ITE Bexp.True  c1 c2,   s)     = Right (c1, s)
    stepSOS (ITE Bexp.False c1 c2,   s)     = Right (c2, s)
    stepSOS (ITE   b        c1 c2,   s)     = case stepB (b, s) of
                                                  Left b' -> Right (ITE (toBexp b') c1 c2, s)
                                                  Right b' -> Right (ITE b' c1 c2, s)

    stepSOS (If    b        c1,      s)     = stepSOS (ITE b c1 Skip, s)

    stepSOS (While b        c,       s)     = Right (ITE b (Comp c (While b c)) Skip, s)


    -- Função que executa n passos de avaliação
    nstepsSOS 1 (c, s)                      = stepSOS (c, s)
    nstepsSOS n (c, s)                      = stepSOS (c, s) >>= (nstepsSOS (n-1))

    -- Função responsável por avaliar comandos num determinado estado.
    -- Retorna o estado final, após avaliação dos comandos.
    evalSOS (c, s)                          = stepSOS (c, s) >>= evalSOS


    main = replStm stepSOS
