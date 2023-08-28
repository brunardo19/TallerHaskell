lastd xs = drop ((length xs) - 1) xs
lasth xs = head (reverse xs)
lastex xs = xs !! ((length xs) - 1)

initd xs = reverse (drop 1 (reverse (xs)))

f x xs = x + length xs

factores n = [x | x <- [1..n], n `mod` x == 0]

esDivisor n x = n `mod` x == 0
divisores n = [x | x <- [1..n], esDivisor n x]

perfect n = [x | x <- [1..n], sum (init (factores x)) == x]