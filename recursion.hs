factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)

remover 0 xs = xs
remover 0 xs = xs
remover _ [] = []
remover n (_:xs) = remover (n-1) xs


