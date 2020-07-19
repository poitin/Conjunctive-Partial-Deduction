module Main (
    main
) where

import Term
import Tree
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
                                              Right (t,cs) -> toplevel $ Just (t,cs)
                                   else do putStrLn $ "No such file: "++f++".pl"
                                           toplevel Nothing
                   Prog -> case p of
                              Nothing -> do putStrLn "No program loaded"
                                            toplevel p
                              Just (t,d)  -> do putStrLn $ showprog (t,d)
                                                toplevel p
                   Eval -> case p of
                              Nothing -> do putStrLn "No program loaded"
                                            toplevel p
                              Just (t,d) -> f (vars t) t [] d
                                            where
                                            f [] t e d = let t' = walk e t
                                                             xs = vars t'
                                                             rs = eval t' d
                                                         in  case rs of
                                                                [] -> do putStrLn "False"
                                                                         toplevel p
                                                                _ -> do putStrLn "True"
                                                                        g xs rs
                                                                        where
                                                                        g xs [] = toplevel p
                                                                        g xs (r:rs) = h xs r
                                                                                      where
                                                                                      h [] [] = g xs rs
                                                                                      h (x:xs) (t:ts) = do putStrLn $ x++" = "++show t
                                                                                                           h xs ts
                                            f (x:xs) t e d = do putStr $ x++" = "
                                                                hFlush stdout
                                                                l <-  getLine
                                                                case parseTerm l of
                                                                   Left s -> do putStrLn $ "Could not parse term: "++ show s
                                                                                f (x:xs) t e d
                                                                   Right u -> if   u == Var x
                                                                              then f xs t e d
                                                                              else f xs t ((x,u):e) d

                   Trans -> case p of
                               Nothing -> do putStrLn "No program loaded"
                                             toplevel p
                               Just (t,d) -> do let p' = trans (t,d)
                                                putStrLn $ showprog p'
                                                toplevel $ Just p'
                   Quit -> return ()
                   Help -> do putStrLn helpMessage
                              toplevel p
                   Unknown -> do putStrLn "Err: Could not parse command, type ':help' for a list of commands"
                                 toplevel p
