module TAD  where

data Fila t = F [t] deriving(Show)

getFila = F [1,2,3,4]


novaFila :: Fila t
novaFila = F []

--inseri no final da fila
inserirFila :: Fila t -> t -> Fila t
inserirFila (F lista) n = F (lista ++ [n])

--remover do começo da fila
removerFila :: Fila t -> Fila t
removerFila (F []) = error "Fila Vazia"
removerFila (F (x:xs)) = F xs

frente :: Fila t -> t
frente (F []) = error "Fila Vazia"
frente (F (x:xs)) = x

filaVazia :: Fila t -> Bool
filaVazia (F []) = True
filaVazia _ = False
