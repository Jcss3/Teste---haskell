import Data.Array
import Data.List

--Aulas 2

--somar numeros
-- 4 -> 1 + 2  + 3 + 4
-- 3 -> 1 + 2 + 3



soma:: Int -> Int
soma n | n <= 1 = 1
	   | otherwise = soma(n-1) + n 

--Aula 3

fatorial ::Int -> Int
fatorial n 	| n == 0 	= 1
			| otherwise = n*fatorial (n-1)

-- Aula 4
-- fibonacci  1 1 2 3 5 8 13..
{-
se n = 0 , entao fib(n) = 0
se n = 1 , entao fib(n) = 1
se n > 1, entao fib(n - 1) + fib(n-2)  
-}

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci n | n == 1 = 1
			| otherwise = fibonacci(n-1) + fibonacci(n-2)


--Aula 5
{-guarda x | (x==0) = 0
			|otherwise = 10
-}

--Aula 6
-- Variavel Anonima

and1 :: Bool -> Bool -> Bool
and1 False _ = False
and1 _ False = False
and1 True True = True

-- Aula7 
--Tuplas

somatupla :: (Int,Int) -> (Int,Int) -> (Int,Int)
somatupla (a,b) (x,y) = (a+x, b+y) 

-- Aula8
--Tuplas -> extrair dados

-- fst -> primeiro elemento da tupla
-- snd -> segundo elemento da tupla

nome = ("Jeff","Estudante","PLC")

primeiroElemento :: (String,String ,String) -> String
primeiroElemento (a,_,_) = a

segundoElemento :: (String,String ,String) -> String
segundoElemento (_,a,_) = a

terceiroElemento :: (String,String ,String) -> String
terceiroElemento (_,_,a) = a

--Aula 9
-- Novos Tipos
type Nome1 = String
type Idade1= Int
type Linguagem1 = String
type Pessoa1 = (Nome1 , Idade1 , Linguagem1)

pessoa :: Pessoa1
pessoa = ("Joao", 20 ,"Haskell")

mynome:: Pessoa1 -> Nome1
mynome (n , i , l ) = n

myidade:: Pessoa1-> Idade1
myidade (n , i , l ) = i

mylinguagem:: Pessoa1 -> Linguagem1
mylinguagem (n , i , l ) = l

--Aula 10
--Listas

-- lista[1,2,3]
{-
	head 1 
	tail 2,3
	init 1,2
	last3
-}

--Aula 11 
-- listas

listavazia = []

-- criando listas 1:2:3:4:5[]
--tamanho da lista
size_list :: [Int] -> Int
size_list [] = 0
size_list (x:xs) = 1 + size_list xs

-- Aula 12
{-Lista

verificar se  duas listas são iguais
msm numero de elementos 
msm elementos 
na msm posição

-}

ehigual::[Int] -> [Int] -> Bool
ehigual [] [] = True
ehigual	[] _ = False
ehigual	 _ [] = False
ehigual (a:as) (b:bs) | a == b = ehigual as bs
					  | otherwise = False


--Aula 13
--Lista
{-Funçao que recebe uma lista e retorna ela invertida

Entrada [1,2,3]
saida [3,2,1]

-}
inv_aux :: [Int] -> [Int] -> [Int]
inv_aux [] l_inv =  l_inv
inv_aux (x:xs) l_inv = inv_aux xs l_inv ++ [x]


inverte_list :: [Int] -> [Int]
inverte_list [] = []
inverte_list l = inv_aux l []


inv_lista :: [Int] -> [Int]
inv_lista [] = []
inv_lista (x:xs) = inv_lista xs ++ [x] 


--Aula 14
-- Exercicios Listas

{-
verifcar se pertence a uma lista
-}

pertence :: [Integer] -> Integer -> Bool
pertence [] _ = False
pertence (x:xs) n | x == n = True
				  | otherwise = pertence xs n


{-retornar o maior elemento da lista
-}

maior::[Int] -> Int
maior [] = 0
maior [x] = x
maior (x:xs) | (x > maior xs) = x
			 | otherwise = maior xs

{-
recebe uma lista e verifica se todos os elementos são pares
-}

todospares::[Int] -> Bool 
todospares [] = True
todospares (x:xs) | ( x `mod` 2 ) /= 0 	= False
				  | otherwise = todospares xs


--Aula 15
--Compreensão de Listas
{-
	[x| x <- [1,2,3]]  == [1,2,3]
	[x + 1| x <- [1,2,3]] == [2,3,4]
	[x * x | x <- [1,2,3]] == [1,4,9]
	[x | x <- [1..10] , x `mod` 2 == 0] == [2,4,6,8,10]
-}

par :: Int -> Bool
par x = x `mod` 2 == 0

lista = [x*x | x <- [1..10] , par x, x > 5]

--gerando todos os pares ordenados da tupla

tupla = [ (x,y) | x <- [1..5], y <- [6..10]]
dobro = [x *2 | x <- [1..10 ], x < 5] 

--Aula 16
--zip
{-
	zip :: [a] -> [b] -> [(a,b)]
	zip [1,2,3] ['a','b','c'] 
	--definida na prelude
-}


--Aula 17
-- ordenar listas

{-
	funçã ordena vai ordenar uma lista de inteiros crescentemente
	exe entrada [5,8,1,10]
	saida [1,5,8,10]
-}

exemplo = [5,2,10,3,9]

get_menor::[Integer] -> Integer
get_menor[x] = x 
get_menor (x:xs) | x < get_menor xs = x
				 | otherwise = get_menor xs
 

remove_menor::[Integer] -> [Integer]
remove_menor [] = []
remove_menor (x:xs) | ( x == (get_menor (x:xs))) = xs
					| otherwise = (x:remove_menor xs)

aux_ordena::[Integer] -> [Integer] -> [Integer]
aux_ordena lista_ordenada [] = lista_ordenada
aux_ordena lista_ordenada (x:xs) = aux_ordena (lista_ordenada++[get_menor(x:xs)]) (remove_menor(x:xs))



ordena_lista::[Integer] -> [Integer]
ordena_lista [] = []
ordena_lista lista = aux_ordena [] lista


-- Aula 18
-- Inverter Lista

inverte :: (Enum a) => [a] -> [a]
inverte [] = []
inverte (x:xs) = (inverte xs) ++ [x]

-- Aula 19
--da Prelude -> Funções de entrade e saida

-- show imprimi valores (string na saida)

--putStr imprimir palavra di tipo string

--read ler e converte para numeros "10" + 10 = 20



--Aula 20
-- Operadores e funções

-- div retorna a parte inteira de uma divisao div 3 2 retorna 1
-- mod retorna resto da divisao e mod 2    1
-- abs valor absoluta de um inteiro (-20 )  = 20


---usando data.char
-- import data.char ord 'g' 103 na tabela ascii
-- chr 120 -> 'x' converte para char
-- isLower 'a' True 'A' false
-- isUpper 'a' false "A" true
-- toLower converte para miniculo
-- toUpper converte para maiusculo
-- isDigit '1' true verifica se é numero
-- 


--Aula 21
---Polimorfismo

my_length :: [a] -> Int
my_length [] = 0
my_length (x:xs) = 1 + my_length xs

--Aula 22
--if then else e case

if_par :: Int -> Bool
if_par n = if (n `mod` 2 == 0) then True else False

case_par ::Int -> Bool
case_par n = case(n `mod` 2 == 0) of
				True -> True
				False -> False

guarda_par :: Int -> Bool
guarda_par n | n `mod` 2 == 0 	= True
			 | otherwise = False

--Aula 23
--where

quadrado :: Int-> Int
quadrado n = quad_n
			where quad_n = n*n

--Aula 24
--Funções Lambdas (funções anonimas quando queremos usar apenas uma vez)
-- \

f_lambda = (\x -> (x*x))

f x = x * x

--Aula 25
--Funções prontas

{-
	null :verifica se uma lista esta nula/vazia
	head : primeiro elemento
	tail : retorna cauda da lista
	last : ultimo elemento da lista
	init : devolve a lista sem o ultimo elemento
	length : tamanho da lista
	drop : apaga n primeiro elementos de uma lista
	lines : lista de listas
	take : pega n elementos de um lista
	words : lista de palavras de um string ex words "aprendendo plc" saida ["aprendendo", "plc"]
	reverse :  inverso da lista
	repeat :retorna uma lista  potencialmente infinta
	replicate : lista com o elemento passsado replicado n vezes ex replicate 2 "1" saida ["1","1"]
	cycle : parecido com repeat
	splitAt : separa uma lista na posição passada, retorna uma tupla de listas
	zip : retorna lista de tuplas com elementos de listas
	unzip : retorna tupla com listas originais
-}

--Aula 26 Vetores

get_array = array (1,4) [(1,'A'),(2,'B'),(3,'C'),(4,'D')]

--matriz 2x2
{-
	get_array2 = array ((1,1),(2,2)) [((1,1), "A"), ((2,2) "B")((2,1), "C"), ((2,2) "D")]
-}

--Aula 27 Pilha

-- insere na Pilha
push :: [Int] -> Int -> [Int]
push pilha x = pilha ++ [x]

-- retornar o elemento do topo da pilha
top ::[Int] -> Int
top [x] = x
top (x:xs) = top xs

--remover 
pop:: [Int] -> [Int]
pop [] = error "Pilha Vazia"
pop (x:xs) | x == top (x:xs)	= xs
		   | otherwise = x:(pop xs)

-- verificar se estar vazia
is_empty :: [Int] -> Bool
is_empty n  | n == [] 	= True
			| otherwise = False

--Aula 28 Tipos algebricos

type Nome = String
type Linguagem = String
type Universidade = String

-- tipo algebrico pessoa
data Pessoa = Programador Nome Linguagem | Aluno Nome Universidade 
 				deriving (Show)

programador = Programador "Marcos" "Haskell"
aluno = Aluno "Marcos" "UFPI"

is_programador :: Pessoa -> Bool
is_programador (Programador _ _) = True
is_programador _ = False

is_aluno :: Pessoa -> Bool
is_aluno (Aluno _ _) = True
is_aluno _ = False


-- Aula 29 Revisao

-- concatenar 2 listas

my_concat :: [a] -> [a] -> [a]
my_concat [] [] = []
my_concat [] y = y
my_concat x [] = x
my_concat (x:xs) y = x: my_concat xs y

--inverte
inv :: [Int] -> [Int]
inv [] = []
inv (c:cs) = (inv cs) ++ [c]

-- gerar lista
gerar_lista :: Int -> [Int]
gerar_lista n = n:gerar_lista(n-1) 

-- soma dos elementos da lista
my_sum ::[Int] -> Int
my_sum [] = 0
my_sum (x:xs) = x + (my_sum xs)

--gera lista com numeros pares
lista_par = [x | x <- [1..100], x `mod` 2 == 0]

--tail
my_tail ::[Int] -> [Int]
my_tail [] = []
my_tail (_:xs) = xs


-- 
concatena :: [a] -> [a] -> [a]
concatena [] y = y
concatena x [] = x
concatena (x:xs) y = x : concatena xs y

-- ex[1,2,3] => [3] ++ [2] ++ [1] == (inv xs) ++ [x]
inverso :: [a] -> [a]
inverso [] = []
inverso (x:xs) = (inverso xs) ++ [x]


gerar_lista :: Int -> [Int]
gerar_lista n = n:gerar_lista(n + 1)
				where n < 10

--tipos algebricos

type Pessoa_2 = String
type Carro = String
type Idade2 = Int
type Registro = (Pessoa_2,Carro,Idade2)
type BD = [Registro]

f_bd:: BD
f_bd = [("JOAO","BMW",50),("ELENA","CAMARO", 25)]




get_nome :: Registro -> Pessoa2
get_nome (n ,_ ,_) = n

carros :: BD-> [String]
carros [] = []
carros ((_ , car, _):xs) = car : carros xs

--Aula 30
-- Arvore Binaria

data ArvBin = Nulo | No Int ArvBin ArvBin

arv ::ArvBin
arv = (No 1 
		(No 2 
			(No 4 Nulo Nulo)(No 5 Nulo Nulo)) 
		(No 3 
			(No 6 Nulo Nulo) Nulo) )

em_ordem ::ArvBin -> [Int]
em_ordem Nulo = []
em_ordem (No num esq dir) = (em_ordem esq ) ++ [num] ++ (em_ordem dir)


--Aula 31 Map Filter

{-
	map (a -> b) -> [a] -> [b] || recebe uma função e uma lista , 
		retornando uma lista com cada elemento aplicado na função
	Filter recebe uma predicado que verifica Bool e uma lista
-}

is_prime :: Integer -> Bool
is_prime 1 = False
is_prime 2 = True
is_prime n | (length [x | x <- [2..n-1], n`mod`x == 0 ]) > 0 	= False
			| otherwise = True  


--Aula 32
--função sort
type Nome3 = String
type Linguagem3 = String
data Pessoa3 = Programador3 Nome3 Linguagem3 deriving (Ord,Eq,Show)

prog1 = Programador3 "Rafa" "JAVA"
prog2 = Programador3 "Jota" "Ruby"
prog3 = Programador3 "TU" "Haskell"

l = [ prog1 ,prog2 , prog3]

inicio = sort l




--Aula 38
--IO,monadas,DO

main:: IO ()
main = do
		putStr "Digite o primeiro numero: "
		n1 <- getLine
		putStr "Digite o segundo numero: "
		n2<- getLine
		putStrLn( "Soma: " ++ show ( read n1 + read n2))