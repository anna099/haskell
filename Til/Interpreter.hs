module Interpreter where

import Parser

import Data.List
import Data.Char

data Value = Str String | Num Int | Boo Bool | Lst List | Err String
  deriving Show

-- Take a list of nodes (that, together, make an expression) and
-- evaluate them to return a single value.
interpret :: List -> Value
interpret (Root name vals) = apply name vals
interpret x = Err "I don't know how to evaluate layered lists yet"

-- Given a function name and a set of arguments, return a value
-- based on a certain evaluation of these arguments which is to
-- be determined by the name of the function.
apply :: String -> [String] -> Value
apply "list"   xs = Lst (tree (wrap (intercalate " " xs)))
apply "string" xs = Str (intercalate "" xs)
apply "sum"    xs = applySum xs
apply fn       xs = Err ("No known function '" ++ fn ++ "'")

-- What follows is a list of functions only called by `apply`. They are
-- simply more specific forms of `apply` that are detailed enough to
-- deserve their own function. If an `applyX` function is followed by
-- any functions whose names do not begin with `apply`, that is because
-- they are utility functions for the `applyX` function above them.

applySum :: [String] -> Value
applySum xs | (all isNum xs) = Num (sum [read x | x <- xs])
            | otherwise = Err ("Arguments to `sum` must all be numbers")

isNum :: String -> Bool
isNum xs = all isDigit xs
