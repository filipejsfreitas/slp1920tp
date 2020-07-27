module AM where

    import Prelude hiding (EQ, LE)
    import State
    import Aexp
    import Bexp
    import Stm
    import Stack
    import AMCode
    
    -- Semântica para a máquina AM
    stepAM :: (Code, Stack, State) -> (Code, Stack, State)

    -- Semântica para as instruções PUSH, ADD, MULT, SUB, TRUE, FALSE
    stepAM (Code ((PUSH n):c), Stack e,       s) = (Code c, Stack $ n:e,   s)
    stepAM (Code (ADD:c),      Stack (a:b:e), s) = (Code c, Stack $ a+b:e, s)
    stepAM (Code (MULT:c),     Stack (a:b:e), s) = (Code c, Stack $ a*b:e, s)
    stepAM (Code (SUB:c),      Stack (a:b:e), s) = (Code c, Stack $ a-b:e, s)
    stepAM (Code (TRUE:c),     Stack e,       s) = (Code c, Stack $ 1:e,   s)
    stepAM (Code (FALSE:c),    Stack e,       s) = (Code c, Stack $ 0:e,   s)

    -- Semântica para as instruções de comparação EQ, GT, GE, LT, LE
    stepAM (Code (EQ:c), Stack (a:b:e), s) | a == b    = (Code c, Stack $ 1:e, s)
                                           | otherwise = (Code c, Stack $ 0:e, s)
    stepAM (Code (AMCode.GT:c), Stack (a:b:e), s) | a > b     = (Code c, Stack $ 1:e, s)
                                                  | otherwise = (Code c, Stack $ 0:e, s)
    stepAM (Code (AMCode.GE:c), Stack (a:b:e), s) | a >= b    = (Code c, Stack $ 1:e, s)
                                                  | otherwise = (Code c, Stack $ 0:e, s)
    stepAM (Code (AMCode.LT:c), Stack (a:b:e), s) | a < b     = (Code c, Stack $ 1:e, s)
                                                  | otherwise = (Code c, Stack $ 0:e, s)
    stepAM (Code (AMCode.LE:c), Stack (a:b:e), s) | a <= b    = (Code c, Stack $ 1:e, s)
                                                  | otherwise = (Code c, Stack $ 0:e, s)

    -- Semântica para as instruções AND, OR, NEG
    stepAM (Code (AND:c), Stack (a:b:e), s) | a /= 0 && b /= 0 = (Code c, Stack $ 1:e, s)
                                            | otherwise        = (Code c, Stack $ 0:e, s)
    stepAM (Code (OR:c),  Stack (a:b:e), s) | a /= 0 || b /= 0 = (Code c, Stack $ 1:e, s)
                                            | otherwise        = (Code c, Stack $ 0:e, s)
    stepAM (Code (NEG:c), Stack (a:e),   s) | a /= 0           = (Code c, Stack $ 0:e, s)
                                            | otherwise        = (Code c, Stack $ 1:e, s)

    -- Semântica para a instrução FETCH
    stepAM (Code ((FETCH v):c), Stack e, s)     = (Code c, Stack $ (fetch v s):e, s)

    -- Semântica para a instrução STORE
    stepAM (Code ((STORE v):c), Stack (x:e), s) = let s' = update (v, x) s
                                                  in (Code c, Stack e, s')

    -- Semântica para a instrução NOOP
    stepAM (Code (NOOP:c), e, s)                = (Code c, e, s)

    -- Semântica para a instrução BRANCH
    stepAM (Code ((BRANCH (Code c1) (Code c2)):c), Stack (t:e), s)
            | t /= 0 = (Code $ c1 ++ c, Stack e, s)
            | otherwise = (Code $ c2 ++ c, Stack e, s)
    
    -- Semântica para a instrução LOOP
    stepAM (Code ((LOOP (Code c1) (Code c2)):c), e, s) = (Code $ c1 ++ [BRANCH (Code $ c2 ++ [LOOP (Code c1) (Code c2)]) (Code [NOOP])], e, s)

    -- Avaliação completa de um bloco de código da máquina AM
    evalAM :: (Code, Stack, State) -> (Code, Stack, State)
    evalAM (Code [], e, s) = (Code [], e, s)
    evalAM (c, e, s)       = (evalAM . stepAM) (c, e, s)

    -- Função de utilidade, apenas para simplificar algum do código que se segue
    cmpAux f i x1 x2 = Code $ x2' ++ x1' ++ [i]
                       where (Code x1') = f x1
                             (Code x2') = f x2

    -- Compilador de expressões Aexp
    cmpA :: Aexp -> Code

    cmpA (Num n)      = Code [PUSH n]
    cmpA (Sim n)      = cmpA $ Sub (Num 0) n -- -x == 0-x
    cmpA (Var v)      = Code [FETCH v]

    cmpA (Add a1 a2)  = cmpAux (cmpA) ADD a1 a2
    cmpA (Mult a1 a2) = cmpAux (cmpA) MULT a1 a2
    cmpA (Sub a1 a2)  = cmpAux (cmpA) SUB a1 a2

    -- Compilador de expressões Bexp
    cmpB :: Bexp -> Code

    cmpB Bexp.True       = Code $ [TRUE]
    cmpB Bexp.False      = Code $ [FALSE]

    cmpB (Equals  a1 a2) = cmpAux (cmpA) (EQ) a1 a2
    cmpB (Bexp.GT a1 a2) = cmpAux (cmpA) (AMCode.GT) a1 a2
    cmpB (Bexp.GE a1 a2) = cmpAux (cmpA) (AMCode.GE) a1 a2
    cmpB (Bexp.LT a1 a2) = cmpAux (cmpA) (AMCode.LT) a1 a2
    cmpB (Bexp.LE a1 a2) = cmpAux (cmpA) (AMCode.LE) a1 a2

    cmpB (Not b)         = Code $ b' ++ [NEG]
                           where (Code b') = cmpB b

    cmpB (And b1 b2)     = cmpAux (cmpB) (AND) b1 b2
    cmpB (Or  b1 b2)     = cmpAux (cmpB) (OR) b1 b2

    -- Compilador de expressões Stm
    cmpS :: Stm -> Code

    cmpS Skip            = Code $ [NOOP]

    cmpS (Assign x a)    = Code $ a' ++ [STORE x]
                           where (Code a') = cmpA a

    cmpS (Comp s1 s2)    = Code $ s1' ++ s2'
                           where (Code s1') = cmpS s1
                                 (Code s2') = cmpS s2

    cmpS (ITE b s1 s2) = Code $ b' ++ [BRANCH (cmpS s1) (cmpS s2)]
                         where (Code b') = cmpB b
    cmpS (If  b s)     = cmpS (ITE b s Skip)
    
    cmpS (While b s)   = Code $ [LOOP (cmpB b) (cmpS s)]

    -- Função de semântica Sam
    sAM :: Stm -> (State -> State)
    sAM c = \s -> let (_, _, s') = evalAM (cmpS c, Stack [], s)
                  in s'
    