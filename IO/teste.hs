import Control.Concurrent

obter_fatores :: Int -> [Int]
obter_fatores num = [x | x <- [1..num-1], ((mod num x) == 0)]

eh_perfeito:: Int -> Bool
eh_perfeito num  
			| ((sum (obter_fatores num)) == num)	= True
			| otherwise = False

--lista de numeros perfeitos ate n
obter_perfeitos :: Integer -> [Int]
obter_perfeitos n = [x | x <- [1..n], ((eh_perfeito x )== True)]


main = do 
	forkIO $ do {
		putStrLn ("Obter todos numeros perfeitos ate 10000...");
		putStrLn ("Numeros perfeitos: " ++ (show(obter_perfeitos 10000)));
		putStrLn ("Acabou de obter os números perfeitos!");
	}
	forkIO $ do {
	ThreadDelay 3000000;
	putStrLn("\nExecutando outra thread");
	}
	ThreadDelay 4000000 -- supende a execução por 40 seg
		putStrLn("FIM !");
