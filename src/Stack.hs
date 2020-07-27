module Stack where
    
    import State

    newtype Stack = Stack [Val]

    instance Show Stack where
        show (Stack []) = ""
        show (Stack (n:[])) = show n
        show (Stack (n:xs)) = (show n) ++ ":" ++ (show $ Stack xs)
    