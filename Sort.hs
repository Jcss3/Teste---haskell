import Data.List

--função sort
ordenar = sort [8,4,2,9,8,1,2,0]

type Nome = String
type Linguagem = String
data Pessoa = Programador Nome Linguagem deriving (Show,Ord,Eq)


programador1 = Programador "Rafael" "Cobol"
programador2 = Programador "Joao" "Python"
programador3 = Programador "Leandro" "Java"


lista = [programador1,programador2 ,programador3]

ordena = sort lista

