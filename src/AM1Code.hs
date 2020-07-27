module AM1Code where

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
                | BRANCH Code Code
                | LOOP Code Code

    newtype Code = Code [Inst]

    instance Show Inst where
        show (PUSH n) = "PUSH-" ++ show n
        show ADD = "ADD"
        show MULT = "MULT"
        show SUB = "SUB"
        show TRUE = "tt"
        show FALSE = "ff"
        show EQ = "EQ"
        show AM1Code.GT = "GT"
        show AM1Code.GE = "GE"
        show AM1Code.LT = "LT"
        show AM1Code.LE = "LE"
        show AND = "AND"
        show OR = "OR"
        show NEG = "NEG"
        show (PUT n) = "PUT-" ++ show n
        show (GET n) = "GET-" ++ show n
        show NOOP = "NOOP"
        show (BRANCH c1 c2) = "BRANCH(" ++ (show c1) ++ ", " ++ (show c2) ++ ")"
        show (LOOP c1 c2) = "LOOP(" ++ (show c1) ++ ", " ++ (show c2) ++ ")"

    instance Show Code where
        show (Code []) = ""
        show (Code (i:[])) = show i
        show (Code (i:c)) = (show i) ++ ":" ++ (show $ Code c)
    
