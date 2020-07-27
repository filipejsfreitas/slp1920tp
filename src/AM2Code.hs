module AM2Code where

    import Prelude hiding (EQ, LE)
    import State
    import Memory
    import Stack

    data Inst = PUSH Val
                | ADD
                | MULT
                | SUB
                | TRUE
                | FALSE
                | EQ
                | GT
                | GE
                | LT
                | LE
                | AND
                | OR
                | NEG
                | PUT Address
                | GET Address
                | NOOP
                | LABEL Val
                | JUMP Val Val -- JUMP-l: o primeiro Val é o l, e o segundo Val é o endereço relativo da localização da instrução LABEL-l respetiva
                | JUMPFALSE Val Val -- JUMPFALSE-l: o primeiro Val é o l, e o segundo Val é o endereço relativo da localização da instrução LABEL-l respetiva

    newtype Code = Code [Inst]

    instance Show Inst where
        show (PUSH n) = "PUSH-" ++ show n
        show ADD = "ADD"
        show MULT = "MULT"
        show SUB = "SUB"
        show TRUE = "tt"
        show FALSE = "ff"
        show EQ = "EQ"
        show AM2Code.GT = "GT"
        show AM2Code.GE = "GE"
        show AM2Code.LT = "LT"
        show AM2Code.LE = "LE"
        show AND = "AND"
        show OR = "OR"
        show NEG = "NEG"
        show (PUT n) = "PUT-" ++ show n
        show (GET n) = "GET-" ++ show n
        show NOOP = "NOOP"
        show (LABEL l) = "LABEL-" ++ show l
        show (JUMP l n) = "JUMP-" ++ show l
        show (JUMPFALSE l n) = "JUMPFALSE-" ++ show l

    showCode (Code []) n = ""
    showCode (Code (i:[])) n = (show n) ++ ": " ++ show i
    showCode (Code (i:c)) n = (show n) ++ ": " ++ (show i) ++ "\n" ++ (showCode (Code c) (n+1))

    instance Show Code where
        show c = showCode c 0
    
