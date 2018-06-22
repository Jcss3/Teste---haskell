somna_um = map (+1) [1,2,3,4,5,6]

filtra_maiorTres = filter (>3) [1,2,3,4,5,6]


is_prime ::Int -> Bool
is_prime 1 = False
is_prime 2 = True
is_prime n | (length [x | x <- [2 .. n-1], n `mod` x == 0]) > 0 = False
		   | otherwise = True

filtrar_primo = filter is_prime [1..10]