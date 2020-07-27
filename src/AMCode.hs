module AMCode where

    import Prelude hiding (EQ, LE)
    import State
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
                | FETCH Var
                | STORE Var
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
        show AMCode.GT = "GT"
        show AMCode.GE = "GE"
        show AMCode.LT = "LT"
        show AMCode.LE = "LE"
        show AND = "AND"
        show OR = "OR"
        show NEG = "NEG"
        show (FETCH x) = "FETCH-" ++ x
        show (STORE x) = "STORE-" ++ x
        show NOOP = "NOOP"
        show (BRANCH c1 c2) = "BRANCH(" ++ (show c1) ++ ", " ++ (show c2) ++ ")"
        show (LOOP c1 c2) = "LOOP(" ++ (show c1) ++ ", " ++ (show c2) ++ ")"

    instance Show Code where
        show (Code []) = ""
        show (Code (i:[])) = show i
        show (Code (i:c)) = (show i) ++ ":" ++ (show $ Code c)
    
