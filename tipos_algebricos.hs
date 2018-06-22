--tipos algebricos

type Pessoa = String
type Carro = String
type Idade = Int
type Registro = (Pessoa,Carro,Idade)
type BD = [Registro]

f_bd :: BD
f_bd = [("Joao","Ferrari",26) , ("Maria","Mercedes", 30)]


get_nome :: Registro -> Pessoa
get_nome (n , _ , _) = n

getCarros :: BD -> [String]
getCarros [] = []
getCarros ((_ , carro , _):xs) = carro : getCarros xs 

