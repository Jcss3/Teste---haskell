module Funcoes where

eh_par :: Int - > Bool
eh_par n  = | (mod n 2 == 0) = True
			| otherwise = False

eh_Impar :: Int -> Bool
eh_Impar n = | (mod n 2 == 0)	= False
			 | otherwise = True

