module StmRepl where

    import Parser
    import State
    import Aexp
    import Bexp
    import Stm

    import Data.List.Split

    printSteps :: (Show s) => ((s, State) -> Either State (s, State)) -> Either State (s, State) -> IO ()
    printSteps f (Left (State [])) = return ()
    printSteps f (Left (State s)) = do
                                putStr "> "
                                putStrLn $ show (State s)
    printSteps f (Right (c, State [])) = do
                                        putStr "> "
                                        putStrLn $ show c

                                        printSteps f $ f (c, State [])
    printSteps f (Right (c, s)) = do
                                        putStr "> "
                                        putStrLn $ show c
                                        putStr "  "
                                        putStrLn $ show s
                                
                                        printSteps f $ f (c, s)

    replStm :: ((Stm, State) -> Either State (Stm, State)) -> IO ()
    replStm f = do
                putStrLn "---------------------------------------------------------"
                putStr "> "
                line <- getLine

                let tokens = split (keepDelimsL $ oneOf "[") line
            
                let c = read (tokens !! 0) :: Stm
                let s = read (concat $ tail tokens) :: State

                printSteps f $ Right (c, s)
                putStrLn "---------------------------------------------------------"
                putStrLn ""

                replStm f
    