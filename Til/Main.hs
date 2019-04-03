module Main where

import Parser
import Interpreter

import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
  putStrLn "Til (Tiny interpreter of lists) version 0"
  repl

repl :: IO ()
repl = do
  input <- getInput
  unless (input == "quit")
      $ put (eval input)
     >> repl

getInput :: IO String
getInput = putStr "> "
        >> hFlush stdout
        >> getLine

eval :: String -> Value
eval xs = interpret (tree xs)

-- Takes a value and turns it into a readable string, then prints
-- that string.
put :: Value -> IO ()
put (Str s) = putStrLn s
put (Num n) = putStrLn (show n)
put (Boo b) = if b then (putStrLn "#t") else (putStrLn "#f")
put (Lst l) = putStrLn (formatList l)
put (Err e) = putStrLn ("* error: " ++ e)

-- A specific function for turning List values into readable strings.
formatList :: List -> String
formatList (Root f r) = "(" ++ f ++ " " ++ (intercalate " " r) ++ ")"
formatList (Node f r) = "(" ++ f ++ " " ++
    (intercalate " " [formatList x | x <- r]) ++ ")"
