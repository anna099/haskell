-- converts a binary number, represented as a list
-- (e.g. [1,0,1,1,0,1]) to a decimal number.

values :: Int -> [Int]
values len = reverse (take len [2 ^ x | x <- [0..len]])

bin2dec :: [Int] -> Int
bin2dec xs = sum (zipWith (*) xs (values (length xs)))
