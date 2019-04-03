module Parser ( tree
              , List (Node, Root)
              , wrap ) where

import Data.List
import Data.List.Split
import Data.Char

data List = Node [Char] [List] | Root [Char] [[Char]]
  deriving Show

-- Take a single expression as a string and return the list of
-- nodes that make up the expression.
tree :: String -> List
tree xs | excessive xs = error "expression is overly parenthesised"
        | unwrapped xs = error "expressions must be in parentheses"
        | unmatched xs = error "unmatching parentheses in expression"
        | layered xs   = Node (words !! 0) [tree x | x <- (layers xs)]
        | otherwise    = Root (words !! 0) rest
          where str    = trim $ unwrap $ trim xs
                words  = filter (/= "") (splitOn " " str)
                rest   = (tail words)

-- Does xs start with multiple open parentheses ((like this))?
excessive :: String -> Bool
excessive xs = (take 2 xs) == "(("

-- Is xs in parentheses (like this)?
unwrapped :: String -> Bool
unwrapped xs = let str = trim xs
                in not (head str == '(' && last str == ')')

-- Take xs out of parentheses, e.g. "(hello)" becomes "hello"
unwrap :: String -> String
unwrap xs | unwrapped xs = error "attempt to unwrap unwrapped string"
          | otherwise    = init (tail xs)

-- Put xs in parentheses, e.g. "hello" becomes "hello"
wrap :: String -> String
wrap xs = "(" ++ xs ++ ")"

-- Does xs contain unclosed parentheses? Or too many close-parentheses?
unmatched :: String -> Bool
unmatched xs = (length (elemIndices '(' xs))
                 /= (length (elemIndices ')' xs))

-- Does xs contain at least one parenthetical substring? (e.g. "1 + (2 + 3)")
layered :: String -> Bool
layered xs = elem '(' (unwrap xs)

-- Return an array of all parenthetical substrings in xs.
-- e.g. "(x (y) (z))" -> ["(y)", "(z)"]
layers :: String -> [String]
layers xs | not (layered xs) = []
          | otherwise        = firstLayer xs : layers (rest)
          where rest = wrap (trim (afterFirstLayer xs))

-- Return the first parenthetical substring of xs.
-- e.g. "(x (y) (z))" -> "(y)"
firstLayer :: String -> String
firstLayer xs = let str           = unwrap $ trim xs
                    firstClose    = head (elemIndices ')' str)
                    untilClose    = substring 0 firstClose str
                    opensTilClose = length (elemIndices '(' untilClose)
                    nthClose      = (elemIndices ')' str) !! (opensTilClose - 1)
                 in substring (head (elemIndices '(' str)) nthClose str

-- Return the remainder of a string following its first parenthetical
-- substring. e.g. "(x (y) (z))" returns " (z)"
afterFirstLayer :: String -> String
afterFirstLayer xs = let str = unwrap xs
                      in drop (head (elemIndices ')' str) + 1) str

-- Return the substring of xs between indices 'from' and 'to',
-- including the characters at those indices.
substring :: Int -> Int -> String -> String
substring from until xs = let to = until + 1
                           in take (to - from) (drop from xs)

-- Remove extra whitespace from the beginning and end of a string.
-- e.g. "   x   " -> "x"
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
