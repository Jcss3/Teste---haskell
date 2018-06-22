--valor do no
type Vertice = Int
--Origem e destino
type Aresta = (Vertice , Vertice)
type Grafo = [Aresta]

grafo :: [Aresta]
grafo = [(1,2),(1,3),(1,4),(1,5)
		 (2,6),(2,7),(4,8),(5,9)]

adjacentes :: Grafo -> Vertice -> [Vertice]
adjacentes [] _ = []
adjacentes ((a,b):as) v = | (a == v) = b:(adjacentes as v)
						  | (b == v) = a:(adjacentes as v)
						  | otherwise = adjacentes as v
