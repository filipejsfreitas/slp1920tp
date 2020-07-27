module AMRepl where

    import State
    import Aexp
    import Bexp
    import Stm
    import Parser
    import Stack
    import AMCode
    import AM
    import Data.List.Split

    printSteps :: (Code, Stack, State) -> IO ()
    printSteps (Code [], e, s) = do
                                 putStrLn ">  CODE: "
                                 putStr "  STACK: "
                                 putStrLn $ show e
                                 putStr "  STATE: "
                                 putStrLn $ show s
                                 putStrLn ""
    printSteps (c, e, s) = do
                                putStr ">  CODE: "
                                putStrLn $ show c
                                putStr "  STACK: "
                                putStrLn $ show e
                                putStr "  STATE: "
                                putStrLn $ show s
                                putStrLn ""

                                printSteps $ stepAM (c, e, s)

    replAM :: IO ()
    replAM = do
        putStrLn "---------------------------------------------------------"
        putStr "> "
        line <- getLine

        let tokens = split (keepDelimsL $ oneOf "[") line

        let c = read (tokens !! 0) :: Stm
        let s = read (concat $ tail tokens) :: State
        
        let compiledC = cmpS c

        printSteps (compiledC, Stack [], s)
        putStrLn "---------------------------------------------------------"
        putStrLn ""

        replAM

    main = replAM
    