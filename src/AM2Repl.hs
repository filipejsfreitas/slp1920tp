module AM2Repl where

    import State
    import Aexp
    import Bexp
    import Stm
    import Parser
    import Stack
    import Memory
    import AM2Code
    import AM2
    import Data.List.Split

    printSteps :: (Address, Code, Stack, Memory) -> IO ()
    printSteps (pc, Code c, e, s) = do
                                    putStrLn ""
                                    putStrLn $ ">     PC: " ++ (show pc)
                                    putStrLn "    CODE: "
                                    putStrLn ""
                                    putStrLn $ show $ Code c
                                    putStrLn ""
                                    putStr "   STACK: "
                                    putStrLn $ show e
                                    putStr "  MEMORY: "
                                    putStrLn $ show s
                                    putStrLn ""
                                    putStrLn "---------------------------------------------------------"

                                    if pc < length c
                                    then printSteps $ stepAM2 (pc, Code c, e, s) 
                                    else putStrLn ""

    replAM :: IO ()
    replAM = do
        putStrLn "---------------------------------------------------------"
        putStrLn "---------------------------------------------------------"
        putStrLn "---------------------------------------------------------"
        putStr "> "
        line <- getLine

        let tokens = split (keepDelimsL $ oneOf "[") line

        let c = read (tokens !! 0) :: Stm
        let s = read (concat $ tail tokens) :: State
        
        let (compiledC, addresses) = cmpS c
        let s' = setupMemory s addresses (Memory [])

        printSteps (0, compiledC, Stack [], s')
        putStrLn ""

        replAM

    main = replAM
    