{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module NS where

    import Prelude hiding (GT, LT)
    import State
    import Aexp
    import Bexp
    import Stm
    import Parser
    import StmRepl (replStm)
    
    -- Uma Aexp é avaliável num inteiro, mas apenas dentro de um estado
    -- Esta implementação corresponde à função semântica A.
    evalA :: Aexp -> State -> Val

    evalA (Num  n    )  _   = n
    evalA (Var  v    )  s   = fetch v s
    evalA (Sim  x    )  s   = -(evalA x s)

    evalA (Add  e1 e2)  s   = (evalA e1 s) + (evalA e2 s)
    evalA (Mult e1 e2)  s   = (evalA e1 s) * (evalA e2 s)
    evalA (Sub  e1 e2)  s   = (evalA e1 s) - (evalA e2 s)

    -- Uma Bexp é avaliável num booleano, mas apenas dentro de um estado
    -- Esta implementação corresponde à função semântica B.
    evalB :: Bexp -> State -> Bool

    evalB Bexp.True     _ = Prelude.True
    evalB Bexp.False    _ = Prelude.False

    evalB (Equals a b) s  =  (evalA a s) == (evalA b s)
    evalB (GT     a b) s  =  (evalA a s) >  (evalA b s)
    evalB (GE     a b) s  =  (evalA a s) >= (evalA b s)
    evalB (LT     a b) s  =  (evalA a s) <  (evalA b s)
    evalB (LE     a b) s  =  (evalA a s) <= (evalA b s)
    
    evalB (Not    a  ) s  =  not $ evalB a s

    evalB (And    a b) s  =  (evalB a s) && (evalB b s)
    evalB (Or     a b) s  =  (evalB a s) || (evalB b s)


    -- Função responsável por avaliar comandos num determinado estado.
    -- Retorna o estado final, após avaliação dos comandos.
    evalNS :: (Stm, State) -> State

    evalNS (Skip, s)             = s
    evalNS (Assign var exp, s)   = update (var, evalA exp s) s

    evalNS (Comp c1 c2,     s)   = let s' = evalNS (c1, s)
                                   in evalNS (c2, s')

    evalNS (ITE b c1 c2,    s)   | cond      = evalNS (c1, s)
                                 | otherwise = evalNS (c2, s)
                                 where cond  = evalB b s
    evalNS (If b c1,        s)   = evalNS (ITE b c1 Skip, s)

    evalNS (While b c,      s)   | cond      = evalNS ((While b c), s')
                                 | otherwise = s
                                 where cond  = evalB b s
                                       s'    = evalNS (c, s)
    
    
    main = replStm $ Left . evalNS
