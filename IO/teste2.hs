import Control.Concurrent

{-
	Mvar: sincronização entre threads
-}


main = do 
	--cria uma mvar que inicialmente é vazia
	n <- newEmptyMVar

	--putmvar coloca um valor numa mvar
	forkIO $ putMVar n 'x'

	-- takemvar retorna o conteudo da mvar
	r <- takeMVar n

	print r

-- exemplo

main2 = do 
	m <- newEmptyMVar

	forkIO $ putMVar m "Hello Haskell"
	forkIO $ putMVar m "Hello MVar"

	r <- takeMVar m
	print r
	r<- takeMVar m 
	print r
	putStrLn "FIm :)"

--sem usar mvar
main3 = do
	let msg1 = "Hello Haskell"
	let msg2 = "Hello mvar"

	forkIO $ putStrLn msg1
	forkIO $ putStrLn msg2
	putStrLn "FIm :)"


--
threadA :: MVar Float -> MVar Float -> IO () 
threadA toSend toReceive 
			= do
			putMVar toSend 72
			v <- takeMVar toReceive
			print (show v)


threadB :: MVar Float -> MVar Float -> IO ()
threadB toReceive toSend
			= do
				z <- takeMVar toReceive
				putMVar toSend (1.2 * z)


main5 :: IO ()
main5 = do
		aMVar <- newEmptyMVar
		bMVar <- newEmptyMVar
		forkIO (threadA aMVar bMVar)
		forkIO (threadB aMVar bMVar)
		threadDelay 1000
		--espera um tempo