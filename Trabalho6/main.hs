import Text.Printf
import Data.Char

{- 1. Usando List Comprehension escreva uma função, chamada divisoresDeN, que devolva uma lista dos divisores de um número dado. -}
divisoresDeN :: Int -> [Int]
divisoresDeN n = [x | x <- [1..n], (n `mod` x) == 0]

{-2. Usando  List Comprehension  escreva  uma  função, chamada  contaCaractere,  que  conte  a ocorrência de um caractere específico, em uma string dada. -}
contaCaractere :: String -> Char -> Int
contaCaractere str char = length [c | c <- str, (toLower c) == (toLower char)]

{-3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada. -}
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo lista = [n*2 | n <- lista, n >= 0]

{-4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado. -}
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras n = [(a,b,c) | a <-[1..n], b <-[1..n], c <- [1..n], ((a^2) + (b^2)) == (c^2), c > a, c > b, b > a]


{-5. Números  perfeitos  são  aqueles  cuja  soma  dos seus  divisores  é  igual  ao  próprio  número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado. -}
numerosPerfeitos :: Int -> [Int]
numerosPerfeitos n = [x | x <- [1..n], (sum (init (divisoresDeN x))) == x]

{-6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis.-}

produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar v1 v2 = (sum [i*k | (i, k) <- zip v1 v2])

{-7. Usando  List Comprehension  escreva  uma  função, chamada  primeirosPrimos,  que  devolva uma lista contendo, os n primeiros números primos a partir do número 2. -}
primeirosPrimos :: Int -> [Int]
primeirosPrimos valor = takeWhile (\x -> length [n | n <- [2..(x-1)], length((divisoresDeN n)) == 2] < valor ) [n | n <- [2..], length((divisoresDeN n)) == 2]

{-8. Usando  List Comprehension  escreva  uma  função,  chamada  paresOrdenados,  que  devolva uma  lista  de  par ordenados  contendo  uma  potência  de  2  e  uma potência de  3  até  um determinado número dado. Observe que estes números podem ser bem grandes. -}
paresOrdenados :: Int -> [(Double, Double)]
paresOrdenados n = [(2^x, 3^x) | x <- [0..n]]

main = do
  printf "\nFuncao 1: entrada: %d; resultado: %s \n" (17 :: Int) (show(divisoresDeN 17))
  printf "\nFuncao 2: entrada: %s, %s; resultado: %s \n" ("Arara" :: String) ("a" :: String) (show(contaCaractere "Arara" 'a'))
  printf "\nFuncao 3: entrada: %s; resultado: %s \n" ("[-10..10]" :: String) (show(dobroNaoNegativo [(-10)..10]))
  printf "\nFuncao 4: entrada: %d; resultado: %s \n" (20 :: Int) (show(pitagoras 20))
  printf "\nFuncao 5: entrada: %d; resultado: %s \n" (1000 :: Int) (show(numerosPerfeitos 1000))
  printf "\nFuncao 6: entrada: %s, %s; resultado: %s \n" ("[3,4,5]" :: String) ("[3,4,5]" :: String) (show(produtoEscalar [3,4,5] [3,4,5]))
  printf "\nFuncao 7: entrada: %d; resultado: %s \n" (100 :: Int) (show(primeirosPrimos 100))
  printf "\nFuncao 8: entrada: %d; resultado: %s \n" (10 :: Int) (show(paresOrdenados 10))