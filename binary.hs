values :: Int -> [Int]
values len = reverse (take len [2 ^ x | x <- [0..len]])

bin2dec :: [Int] -> Int
bin2dec digits = sum (zipWith (*) digits (values (length digits)))
