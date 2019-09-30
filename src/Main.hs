module Main (
    main
) where

import Term
import Tree
import Debug.Trace
import System.Directory
import System.IO
import System.Timeout
import Control.Monad
import Data.List
import Data.Ord
import System.Exit

data Command = Load String
             | Prog
             | Eval
             | Trans
             | Quit
             | Help
             | Unknown

command str = let res = words str
              in case res of
                   [":load",f] -> Load f
                   [":prog"] -> Prog
                   [":eval"] -> Eval
                   [":trans"] -> Trans
                   [":quit"] -> Quit
                   [":help"] -> Help
                   _ -> Unknown

helpMessage = "\n:load filename\t\tTo load the given filename\n"++
               ":prog\t\t\tTo print the current program\n"++
               ":eval\t\t\tTo evaluate the current goal\n"++
               ":trans\t\t\tTo transform the current program\n"++
               ":quit\t\t\tTo quit\n"++
               ":help\t\t\tTo print this message\n"


-- entry point for main program - a simple REPL

main = toplevel Nothing

toplevel :: Maybe Prog -> IO ()
toplevel p = do putStr "LOG> "
                hFlush stdout
                x <-  getLine
                case command x of
                   Load f -> do x <-  doesFileExist $ f++".pl"
                                if x
                                   then do c <-  readFile $ f++".pl"
                                           case parseProg c of
                                              Left s -> do putStrLn $ "Could not parse term in file "++f++".pl: "++ show s
                                                           toplevel p
                                              Right t -> do putStrLn $ "Loading file: "++f++".pl"
                                                            toplevel $ Just t
                                   else do putStrLn $ "No such file: "++f++".pl"
                                           toplevel Nothing
                   Prog -> case p of
                              Nothing -> do putStrLn "No program loaded"
                                            toplevel p
                              Just prog  -> do putStrLn $ showprog prog
                                               toplevel p
                   Eval -> case p of
                              Nothing -> do putStrLn "No program loaded"
                                            toplevel p
                              Just (ts,cs)  -> f (foldr varsTerm' [] ts) ts cs []
                                               where
                                               f [] ts cs env = let ts' = map (instantiateTerm env) ts
                                                                    xs = foldr varsTerm' [] ts'
                                                                    tss = eval (map Var xs) ts' cs
                                                                in  case tss of
                                                                       [] -> do putStrLn "False"
                                                                                toplevel p
                                                                       _ -> do putStrLn "True"
                                                                               g xs tss
                                                                               where
                                                                               g xs [] = toplevel p
                                                                               g xs (ts:tss) = h xs ts
                                                                                               where
                                                                                               h [] [] = g xs tss
                                                                                               h (x:xs) (t:ts) = do putStrLn $ x++" = "++show t
                                                                                                                    h xs ts
                                               f (x:xs) ts cs env = do putStr $ x++" = "
                                                                       hFlush stdout
                                                                       l <-  getLine
                                                                       case parseTerm l of
                                                                          Left s -> do putStrLn $ "Could not parse term: "++ show s
                                                                                       f (x:xs) ts cs env
                                                                          Right u -> if   u == Var x
                                                                                     then f xs ts cs env
                                                                                     else f xs ts cs ((x,u):env)

                   Trans -> case p of
                               Nothing -> do putStrLn "No program loaded"
                                             toplevel p
                               Just (ts,cs)  -> do let (ts',cs') = residualise $ trans ts cs
                                                   let cs'' = sortBy (comparing (atomName.fst)) cs'
                                                   putStrLn $ showprog (ts',cs'')
                                                   toplevel $ Just (ts',cs'')
                   Quit -> return ()
                   Help -> do putStrLn helpMessage
                              toplevel p
                   Unknown -> do putStrLn "Err: Could not parse command, type ':help' for a list of commands"
                                 toplevel p


