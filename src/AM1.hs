module AM1 where

    import Prelude hiding (EQ, LE)
    import State
    import Aexp
    import Bexp
    import Stm
    import Stack
    import Memory
    import AM1Code
    
    -- Semântica para a máquina AM
    stepAM1 :: (Code, Stack, Memory) -> (Code, Stack, Memory)

    -- Semântica para as instruções PUSH, ADD, MULT, SUB, TRUE, FALSE
    stepAM1 (Code ((PUSH n):c), Stack e,       s) = (Code c, Stack $   n:e, s)
    stepAM1 (Code (ADD:c),      Stack (a:b:e), s) = (Code c, Stack $ a+b:e, s)
    stepAM1 (Code (MULT:c),     Stack (a:b:e), s) = (Code c, Stack $ a*b:e, s)
    stepAM1 (Code (SUB:c),      Stack (a:b:e), s) = (Code c, Stack $ a-b:e, s)
    stepAM1 (Code (TRUE:c),     Stack e,       s) = (Code c, Stack $   1:e, s)
    stepAM1 (Code (FALSE:c),    Stack e,       s) = (Code c, Stack $   0:e, s)

    -- Semântica para as instruções de comparação EQ, GT, GE, LT, LE
    stepAM1 (Code (EQ:c), Stack (a:b:e), s) | a == b = (Code c, Stack $ 1:e, s)
                                            | otherwise = (Code c, Stack $ 0:e, s)
    stepAM1 (Code (AM1Code.GT:c), Stack (a:b:e), s) | a > b = (Code c, Stack $ 1:e, s)
                                                    | otherwise = (Code c, Stack $ 0:e, s)
    stepAM1 (Code (AM1Code.GE:c), Stack (a:b:e), s) | a >= b = (Code c, Stack $ 1:e, s)
                                                    | otherwise = (Code c, Stack $ 0:e, s)
    stepAM1 (Code (AM1Code.LT:c), Stack (a:b:e), s) | a < b = (Code c, Stack $ 1:e, s)
                                                    | otherwise = (Code c, Stack $ 0:e, s)
    stepAM1 (Code (AM1Code.LE:c), Stack (a:b:e), s) | a <= b = (Code c, Stack $ 1:e, s)
                                                    | otherwise = (Code c, Stack $ 0:e, s)

    -- Semântica para as instruções AND, OR, NEG
    stepAM1 (Code (AND:c), Stack (a:b:e), s) | a /= 0 && b /= 0 = (Code c, Stack $ 1:e, s)
                                             | otherwise = (Code c, Stack $ 0:e, s)
    stepAM1 (Code (OR:c),  Stack (a:b:e), s) | a /= 0 || b /= 0 = (Code c, Stack $ 1:e, s)
                                             | otherwise = (Code c, Stack $ 0:e, s)
                                             
    stepAM1 (Code (NEG:c), Stack (a:e),   s) | a /= 0 = (Code c, Stack $ 0:e, s)
                                             | otherwise = (Code c, Stack $ 1:e, s)

    -- Semântica para a instrução GET
    stepAM1 (Code ((GET a):c), Stack e, Memory m) = (Code c, Stack $ (get a (Memory m)):e, Memory m)

    -- Semântica para a instrução PUT
    stepAM1 (Code ((PUT a):c), Stack (n:e), m) = let m' = put (a, n) m
                                                 in (Code c, Stack e, m')

    -- Semântica para a instrução NOOP
    stepAM1 (Code (NOOP:c), e, s) = (Code c, e, s)

    -- Semântica para a instrução BRANCH
    stepAM1 (Code ((BRANCH (Code c1) (Code c2)):c), Stack (t:e), s)
            | t /= 0 = (Code $ c1 ++ c, Stack e, s)
            | otherwise = (Code $ c2 ++ c, Stack e, s)
    
    -- Semântica para a instrução LOOP
    stepAM1 (Code ((LOOP (Code c1) (Code c2)):c), e, s) = (Code $ c1 ++ [BRANCH (Code $ c2 ++ [LOOP (Code c1) (Code c2)]) (Code [NOOP])], e, s)

    -- Avaliação completa de um bloco de código da máquina AM
    evalAM1 :: (Code, Stack, Memory) -> (Code, Stack, Memory)
    evalAM1 (Code [], e, s) = (Code [], e, s)
    evalAM1 (c, e, s)       = (evalAM1 . stepAM1) (c, e, s)

    -- Função de utilidade, apenas para simplificar algum do código que se segue
    cmpAux f i x1 x2 s = (Code $ x2' ++ x1' ++ [i], s'')
                         where (Code x1', s') = f (x1, s)
                               (Code x2', s'') = f (x2, s')

    -- Compilador de expressões Aexp
    cmpA :: (Aexp, State) -> (Code, State)
    cmpA (Num n, s) = (Code [PUSH n], s)
    cmpA (Sim n, s) = cmpA (Sub (Num 0) n, s) -- -x == 0-x
    cmpA (Var v, s) = (Code [GET addr], s')
                      where addr = env v s
                            s' = update (v, addr) s
    cmpA ((Add a1 a2), s)  = cmpAux (cmpA) ADD  a1 a2 s
    cmpA ((Mult a1 a2), s) = cmpAux (cmpA) MULT a1 a2 s
    cmpA ((Sub a1 a2), s)  = cmpAux (cmpA) SUB  a1 a2 s

    -- Compilador de expressões Bexp
    cmpB :: (Bexp, State) -> (Code, State)

    cmpB (Bexp.True,  s) = (Code [TRUE], s)
    cmpB (Bexp.False, s) = (Code [FALSE], s)

    cmpB (Equals  a1 a2, s) = cmpAux (cmpA) (EQ) a1 a2 s
    cmpB (Bexp.GT a1 a2, s) = cmpAux (cmpA) (AM1Code.GT) a1 a2 s
    cmpB (Bexp.GE a1 a2, s) = cmpAux (cmpA) (AM1Code.GE) a1 a2 s
    cmpB (Bexp.LT a1 a2, s) = cmpAux (cmpA) (AM1Code.LT) a1 a2 s
    cmpB (Bexp.LE a1 a2, s) = cmpAux (cmpA) (AM1Code.LE) a1 a2 s

    cmpB (Not b, s) = (Code $ b' ++ [NEG], s')
                      where (Code b', s') = cmpB (b, s)

    cmpB (And b1 b2, s) = cmpAux (cmpB) (AND) b1 b2 s
    cmpB (Or  b1 b2, s) = cmpAux (cmpB) (OR)  b1 b2 s

    -- Compilador de expressões Stm
    -- Aqui, o estado representa apenas a associação de variáveis ao seu endereço
    cmpSAux :: (Stm, State) -> (Code, State)

    cmpSAux (Skip, s)             = (Code $ [NOOP], s)

    cmpSAux (Assign x a, s)       = (Code $ a' ++ [PUT addr], s'')
                                    where addr = env x s
                                          s'   = update (x, addr) s
                                          (Code a', s'') = cmpA (a, s')

    cmpSAux (Comp s1 s2, s)       = (Code $ s1' ++ s2', s'')
                                    where (Code s1', s') = cmpSAux (s1, s)
                                          (Code s2', s'') = cmpSAux (s2, s')

    cmpSAux (ITE b s1 s2, s)      = (Code $ b' ++ [BRANCH s1' s2'], s''')
                                    where (Code b', s')  = cmpB (b, s)
                                          (s1', s'')  = cmpSAux (s1, s')
                                          (s2', s''') = cmpSAux (s2, s'')
    cmpSAux (If b c, s)           = cmpSAux (ITE b c Skip, s)

    cmpSAux (While b c, s)        = (Code $ [LOOP b' c'], s'')
                                    where (b', s') = cmpB (b, s)
                                          (c', s'') = cmpSAux (c, s')

    cmpS :: Stm -> (Code, State)
    cmpS c = cmpSAux (c, State [])

    -- Função de semântica Sam
    sAM1 :: Stm -> (Memory -> Memory)
    sAM1 c = \s -> let (_, _, s') = evalAM1 (fst $ cmpS c, Stack [], s)
                   in s'
    