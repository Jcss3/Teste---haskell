import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM

waitThreads fim = 
  do f <- takeMVar fim
     if (f > 0) then
         do putMVar fim f
            waitThreads fim
     else 
         return ()

fornecedor :: TVar Int -> TVar Int -> TVar Int -> MVar Int -> IO ()
fornecedor pao carne tomate fim 
				= do 
					f <- takeMVar fim
					atomically ( do
					writeTVar pao 30
					writeTVar carne 30
					writeTVar tomate 30 
					)
					putMVar fim (f - 1)
					fornecedor pao carne tomate fim


produtor :: TVar Int -> TVar Int -> TVar Int -> MVar Int ->  MVar Int -> IO ()
produtor pao carne tomate faca fim = do
			
			f <- takeMVar faca
			fi <- takeMVar fim
			
			atomically( do
						p <- readTVar pao
						c <- readTVar carne
						t <- readTVar tomate

						writeTVar pao (p - 1)
						writeTVar carne (c - 1)
						writeTVar tomate (t - 1)
						 )
			putMVar faca f
			putMVar fim (fi - 1)
			produtor pao carne tomate faca fim


main = do
	pao <- atomically (newTVar 30)
	carne <- atomically (newTVar 30)
	tomate <- atomically (newTVar 30)
	faca <- MVar False
	fim <- MVar 10
	forkIO (fornecedor pao carne tomate fim)
	forkIO (produtor pao carne tomate faca fim)
	forkIO (produtor pao carne tomate faca fim)
	waitThreads fim
    p <- atomically (readTVar pao)
    c <- atomically (readTVar carne)
    t <- atomically (readTVar tomate)
    putStrLn ("Pao: " ++ show p ++ " Carne: "  ++ show c ++ " Tomate: " ++ show t )         
        
    putStrLn "Terminou"