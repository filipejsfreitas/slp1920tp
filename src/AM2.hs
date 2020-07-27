module AM2 where

    import Prelude hiding (EQ, LE)
    import State
    import Aexp
    import Bexp
    import Stm
    import Stack
    import Memory
    import AM2Code

    -- Função que obtém a instrução presente no endereço de memória especificado
    getInstructionAt :: Address -> Code -> Inst
    getInstructionAt 0 (Code (i:c)) = i
    getInstructionAt n (Code (i:c)) = getInstructionAt (n-1) (Code c)
    
    -- Semântica para a máquina AM
    stepAM2Aux :: (Address, Inst, Stack, Memory) -> (Address, Stack, Memory)

    -- Semântica para as instruções PUSH, ADD, MULT, SUB, TRUE, FALSE
    stepAM2Aux (pc, PUSH n, Stack e,       s) = (pc + 1, Stack $ n:e,   s)
    stepAM2Aux (pc, ADD,    Stack (a:b:e), s) = (pc + 1, Stack $ a+b:e, s)
    stepAM2Aux (pc, MULT,   Stack (a:b:e), s) = (pc + 1, Stack $ a*b:e, s)
    stepAM2Aux (pc, SUB,    Stack (a:b:e), s) = (pc + 1, Stack $ a-b:e, s)
    stepAM2Aux (pc, TRUE,   Stack e,       s) = (pc + 1, Stack $ 1:e,   s)
    stepAM2Aux (pc, FALSE,  Stack e,       s) = (pc + 1, Stack $ 0:e,   s)

    -- Semântica para as instruções de comparação EQ, GT, GE, LT, LE
    stepAM2Aux (pc, EQ,         Stack (a:b:e), s) | a == b    = (pc + 1, Stack $ 1:e, s)
                                                  | otherwise = (pc + 1, Stack $ 0:e, s)
    stepAM2Aux (pc, AM2Code.GT, Stack (a:b:e), s) | a > b     = (pc + 1, Stack $ 1:e, s)
                                                  | otherwise = (pc + 1, Stack $ 0:e, s)
    stepAM2Aux (pc, AM2Code.GE, Stack (a:b:e), s) | a >= b    = (pc + 1, Stack $ 1:e, s)
                                                  | otherwise = (pc + 1, Stack $ 0:e, s)
    stepAM2Aux (pc, AM2Code.LT, Stack (a:b:e), s) | a < b     = (pc + 1, Stack $ 1:e, s)
                                                  | otherwise = (pc + 1, Stack $ 0:e, s)
    stepAM2Aux (pc, AM2Code.LE, Stack (a:b:e), s) | a <= b    = (pc + 1, Stack $ 1:e, s)
                                                  | otherwise = (pc + 1, Stack $ 0:e, s)

    -- Semântica para as instruções AND, OR, NEG
    stepAM2Aux (pc, AND, Stack (a:b:e), s) | a /= 0 && b /= 0 = (pc + 1, Stack $ 1:e, s)
                                           | otherwise        = (pc + 1, Stack $ 0:e, s)

    stepAM2Aux (pc, OR, Stack (a:b:e), s) | a /= 0 || b /= 0  = (pc + 1, Stack $ 1:e, s)
                                          | otherwise         = (pc + 1, Stack $ 0:e, s)

    stepAM2Aux (pc, NEG, Stack (a:e), s) | a /= 0             = (pc + 1, Stack $ 0:e, s)
                                         | otherwise          = (pc + 1, Stack $ 1:e, s)

    -- Semântica para a instrução GET
    stepAM2Aux (pc, GET a, Stack e, Memory m) = (pc + 1, Stack $ (get a (Memory m)):e, Memory m)

    -- Semântica para a instrução PUT
    stepAM2Aux (pc, PUT a, Stack (n:e), m) = let m' = put (a, n) m
                                             in (pc + 1, Stack e, m')

    -- Semântica para a instrução NOOP
    stepAM2Aux (pc, NOOP, e, s) = (pc + 1, e, s)

    -- Semântica para a instrução LABEL
    stepAM2Aux (pc, LABEL l, e, s) = (pc + 1, e, s)
    
    -- Semântica para a instrução JUMP-l
    stepAM2Aux (pc, JUMP l n, e, s) = (pc + n, e, s)

    -- Semântica para a instrução JUMPFALSE-l
    stepAM2Aux (pc, JUMPFALSE l n, Stack (x:e), s) | x == 0    = (pc + n, Stack e, s)
                                                   | otherwise = (pc + 1, Stack e, s)

    -- Função de semântica da máquina AM2
    stepAM2 :: (Address, Code, Stack, Memory) -> (Address, Code, Stack, Memory)
    stepAM2 (pc, Code c, e, m) | pc >= length c = (pc, Code c, e, m)
                               | otherwise      = (pc', Code c, e', m')
                                                  where (pc', e', m') = stepAM2Aux (pc, getInstructionAt pc (Code c), e, m)

    -- Avaliação completa de um bloco de código da máquina AM2
    evalAM2 :: (Address, Code, Stack, Memory) -> (Address, Code, Stack, Memory)
    evalAM2 (pc, Code c, e, m) | pc >= length c = (pc, Code c, e, m)
                               | otherwise = (evalAM2 . stepAM2) (pc, Code c, e, m)

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
    cmpB (Bexp.GT a1 a2, s) = cmpAux (cmpA) (AM2Code.GT) a1 a2 s
    cmpB (Bexp.GE a1 a2, s) = cmpAux (cmpA) (AM2Code.GE) a1 a2 s
    cmpB (Bexp.LT a1 a2, s) = cmpAux (cmpA) (AM2Code.LT) a1 a2 s
    cmpB (Bexp.LE a1 a2, s) = cmpAux (cmpA) (AM2Code.LE) a1 a2 s

    cmpB (Not b, s) = (Code $ b' ++ [NEG], s')
                      where (Code b', s') = cmpB (b, s)

    cmpB (And b1 b2, s) = cmpAux (cmpB) (AND) b1 b2 s
    cmpB (Or  b1 b2, s) = cmpAux (cmpB) (OR)  b1 b2 s

    -- Compilador de expressões Stm
    cmpSAux :: (Stm, State) -> (Code, State)

    cmpSAux (Skip, s)             = (Code $ [NOOP], s)

    cmpSAux (Assign x a, s)       = (Code $ a' ++ [PUT addr], s'')
                                    where addr = env x s
                                          s'   = update (x, addr) s
                                          (Code a', s'') = cmpA (a, s')
    cmpSAux (Comp s1 s2, s)       = (Code $ s1' ++ s2', s'')
                                    where (Code s1', s') = cmpSAux (s1, s)
                                          (Code s2', s'') = cmpSAux (s2, s')

    cmpSAux (ITE b s1 s2, s) = (Code $ b' ++ [JUMPFALSE labelElse offsetElse] ++ s1' ++ [JUMP labelAfter offsetAfter, LABEL labelElse] ++ s2' ++ [LABEL labelAfter], s''')
                               where labelElse        = fetch "$LABEL" s
                                     labelAfter       = labelElse + 1
                                     (Code b', s')    = cmpB (b, s)
                                     (Code s1', s'')  = cmpSAux (s1, update ("$LABEL", 1 + labelAfter) s')
                                     (Code s2', s''') = cmpSAux (s2, s'')
                                     offsetElse       = 1 + length s1'
                                     offsetAfter      = 2 + length s2'

    cmpSAux (If b c, s) = cmpSAux (ITE b c Skip, s)

    cmpSAux (While b c, s) = (Code $ [LABEL labelBefore] ++ b' ++ [JUMPFALSE labelAfter offsetAfter] ++ c' ++ [JUMP labelBefore offsetBefore, LABEL labelAfter], s'')
                             where labelBefore    = fetch "$LABEL" s
                                   labelAfter     = labelBefore + 1
                                   (Code b', s')  = cmpB (b, s)
                                   (Code c', s'') = cmpSAux (c, update ("$LABEL", 1 + labelAfter) s')
                                   offsetBefore   = -(length c') - 1 - (length b') - 1
                                   offsetAfter    = 1 + (length c') + 1

    cmpS :: Stm -> (Code, State)
    cmpS c = cmpSAux (c, State [])

    -- Função de semântica Sam
    sAM2 :: Stm -> (Memory -> Memory)
    sAM2 c = \s -> let (_, _, _, s') = evalAM2 (0, fst $ cmpS c, Stack [], s)
                   in s'
    