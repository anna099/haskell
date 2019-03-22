-- evaluates expressions of a very basic form of propositional logic.
-- the language has conjunctive and disjunctive operators, and the
-- program currently only evaluates formulas with two values.

conjunction = '&'
disjunction = 'v'
conditional = '>'
syntax = conjunction : disjunction : conditional : "01"

eval :: [Char] -> Bool
eval expr = not (unexpected expr) && (length expr == 3) && getExprValue expr

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
getCharVAlue '0' = False

evalConditional :: Bool -> Bool -> Bool
evalConditional True False = False
evalConditional x y = True
