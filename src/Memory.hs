module Memory where

    import State

    type Address = Val
    newtype Memory = Memory [Val] deriving Eq

    instance Show Memory where
        show (Memory []) = ""
        show (Memory (x:[])) = show x
        show (Memory (x:xs)) = (show x) ++ ":" ++ (show $ Memory xs)
    
    put :: (Address, Val) -> Memory -> Memory
    put (0, v) (Memory []) = Memory [v]
    put (n, v) (Memory []) = Memory $ (replicate n 0) ++ [v]
    put (n, v) (Memory mem) | length mem <  n = Memory $ mem ++ (replicate (n - 1 - length mem) 0) ++ [v]
                            | length mem == n = Memory $ (take (n) mem) ++ [v]
                            | otherwise       = Memory $ memBefore ++ [v] ++ memAfter
                                                where memBefore = take (n) mem
                                                      memAfter = drop (n+1) mem
    
    get :: Address -> Memory -> Val
    get _ (Memory []) = 0
    get n (Memory m) = m !! n

    -- Função que consulta o endereço de uma variável, se existir, no estado de endereços de variáveis
    lookupVar :: Var -> State -> Maybe Val
    lookupVar v (State []) = Nothing
    lookupVar v (State ((var, val):vs)) | v == var = Just val
                                        | otherwise = lookupVar v (State vs)

    -- Calcula o número de variáveis presentes no estado dado
    -- Necessário pois a função length não ignora o $LABEL, que não é variável
    numVars :: State -> Val
    numVars (State []) = 0
    numVars (State (("$LABEL", val):vs)) = numVars $ State vs
    numVars (State ((var, val):vs)) = numVars $ State vs

    -- Função que associa a cada variável o seu endereço de memória
    env :: Var -> State -> Val
    env v (State []) = 0
    env v s = case lookupVar v s of
                  Nothing -> 1 + numVars s
                  Just  n -> n

    -- Função para criar a memória inicial que se passa a um programa, dado o seu respetivo estado
    setupMemory :: State -> State -> Memory -> Memory
    setupMemory (State []) addresses mem = mem
    setupMemory (State ((v, x):vs)) addresses mem = setupMemory (State vs) addresses mem'
                                                  where address = env v addresses
                                                        mem' = put (address, x) mem
    
