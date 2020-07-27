module AM1Repl where

    import State
    import Aexp
    import Bexp
    import Stm
    import Parser
    import Stack
    import Memory
    import AM1Code
    import AM1
    import Data.List.Split

    printSteps :: (Code, Stack, Memory) -> IO ()
    printSteps (Code [], e, s) = do
                                 putStrLn ">   CODE: "
                                 putStr "   STACK: "
                                 putStrLn $ show e
                                 putStr "  MEMORY: "
                                 putStrLn $ show s
                                 putStrLn ""
    printSteps (c, e, s) = do
                                putStr ">   CODE: "
                                putStrLn $ show c
                                putStr "   STACK: "
                                putStrLn $ show e
                                putStr "  MEMORY: "
                                putStrLn $ show s
                                putStrLn ""

                                printSteps $ stepAM1 (c, e, s)

    replAM :: IO ()
    replAM = do
        putStrLn "---------------------------------------------------------"
        putStr "> "
        line <- getLine

        let tokens = split (keepDelimsL $ oneOf "[") line

        let c = read (tokens !! 0) :: Stm
        let s = read (concat $ tail tokens) :: State
        
        let (compiledC, addresses) = cmpS c
        let s' = setupMemory s addresses (Memory [])

        printSteps (compiledC, Stack [], s')
        putStrLn "---------------------------------------------------------"
        putStrLn ""

        replAM

    main = replAM
    