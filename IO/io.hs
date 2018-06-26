import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
--monada
main :: IO()
main = do
			putStr "Digite o primeiro número: "
			n1 <- getLine
			putStr "Digite o segundo numero: "
			n2 <- getLine
			putStrLn("Soma: " ++ (show (read n1 + read n2)))


-- putStrLn imprimi strings
writefoo :: IO ()
writefoo = putStrLn "JEFF" 

--getLine :: IO string | getLine le uma linha do teclado

-- putStr :: String -> IO () | escreve uma string na tela
-- putStrLn mesma coisa que o putStr mas com quebra de linha

--getChar le do teclado um char

-- do usado para sequenciar ações de IO

teste :: Int -> String -> IO ()
teste n str = do 
	if n <= 10 then putStrLn ("Hello " ++ str)
		else do putStrLn ("Bye " ++ str)

