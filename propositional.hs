-- evaluates expressions of a very basic form of propositional logic.
-- examples:
--      1&1 returns true
--      1>0 returns false
--      1&(0v(0>1)) returns true
module Propositional (eval) where

import Data.List

conjunction = '&'
disjunction = 'v'
conditional = '>'
syntax = conjunction : disjunction : conditional : "01"

eval :: [Char] -> Bool
eval "" = False
eval expr
  | (length (elemIndices '(' expr))
      /= (length (elemIndices ')' expr)) =
        error "Unmatching parentheses in expression."
  | head expr == '(' && last expr == ')' = eval (init (tail expr))
  | elem '(' expr = eval ((withoutParen expr) ++ strAsBool (eval (parentheses expr)))
  | otherwise = not (unexpected expr) && (length expr == 3) && getExprValue expr

parentheses :: [Char] -> [Char]
parentheses str = init (tail (
  take (last (elemIndices ')' str)) (drop (head (elemIndices '(' str)) str)))

withoutParen :: [Char] -> [Char]
withoutParen str = take (head (elemIndices '(' str)) str

unexpected :: [Char] -> Bool
unexpected expr = any (\x -> notElem x syntax) expr

getExprValue :: [Char] -> Bool
getExprValue expr
  | expr !! 1 == conjunction =
      getCharValue (head expr) && getCharValue (last expr)
  | expr !! 1 == disjunction =
      getCharValue (head expr) || getCharValue (last expr)
  | expr !! 1 == conditional =
      evalConditional (getCharValue (head expr)) (getCharValue (last expr))

getCharValue :: Char -> Bool
getCharValue '1' = True
getCharValue '0' = False

evalConditional :: Bool -> Bool -> Bool
evalConditional True False = False
evalConditional x y = True

strAsBool :: Bool -> [Char]
strAsBool False = "0"
strAsBool True = "1"
