import Text.Printf

{-1. Escreva  uma  função  para  o  cálculo  dos  números  da  sequência  de  Fibonacci,  utilizando 
Haskell.  -}
fibonacci :: Int -> [Int]
fibonacci 1 = [0]
fibonacci 2 = [0,1]
fibonacci x = (fibonacci (x-1)) ++[last (fibonacci (x -1)) +  last (fibonacci (x -2))]

{-2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum  (MDC)  de  Euclides  publicado  por  volta do  ano 300  AC.  Podemos simplificar  este algoritmo  dizendo  que  dados  dois  inteiros  A  e  B,  o  MDC  entre  eles  será dado  pelo  valor absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma  função  para  o  cálculo  do  MDC  entre  dois  números  inteiros  positivos,  usando  o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.  -}
mdc :: Int -> Int -> Int
mdc a b
    | b == 0 = (abs a)
    | otherwise = mdc b (a `mod` b)

{-3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste  número.  Exemplo:  dado  1234  a  função  deverá  devolver  10.  Utilizando  Haskell  e recursividade. -}
somaDigitos :: Int -> Int
somaDigitos 0 = 0
somaDigitos x = (x `mod` 10) + (somaDigitos (div x 10))

{-4. Escreva  uma  função  que  devolva  a  soma  de  todos  os  números  menores  que  10000  que sejam múltiplos de 3 ou 5. -}

fizzbuzz :: Int -> Int
fizzbuzz 0 = 0
fizzbuzz x
  | (y `mod` 3) == 0 = y + (fizzbuzz y) -- Fizz
  | (y `mod` 5) == 0 = y + (fizzbuzz y) -- Buzz
  | otherwise = (fizzbuzz y)
  where y = x-1

{-5. Escreva  uma  função que,  recebendo  uma  lista  de  inteiros,  apresente  a  diferença  entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade. -}

somaDosQuadrados :: [Int] -> Int
somaDosQuadrados [] = 0
somaDosQuadrados x = (head x)^2 + somaDosQuadrados (tail x)

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista x = (head x) + somaLista (tail x)

quadradoDaSoma :: [Int] -> Int
quadradoDaSoma x = (somaLista x)^2

questao5 :: [Int] -> Int
questao5 x = (somaDosQuadrados x) - (quadradoDaSoma x)




{-6.O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números 
primos menores que um determinado inteiro dado.-}
divisivelLista :: Int -> [Int] -> Bool
divisivelLista x [] = False
divisivelLista x y 
    | (x `mod` (head y)) == 0 = True
    | otherwise = divisivelLista x (tail y)

filtrarLista :: Int -> [Int] -> [Int]
filtrarLista x [] = []
filtrarLista x y
    | (head y) `mod` x == 0 = filtrarLista x (tail y)
    | otherwise = [head y] ++ filtrarLista x (tail y)

crivo :: [Int] -> [Int]
crivo [] = []
crivo x = [head x] ++ crivo (filtrarLista (head x) (tail x))

{-7. Nem  só  de  Fibonacci  vivem  os  exemplos  de  recursão.  Escreva  uma  função  que  devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado. -}
euSouOLucas :: Int -> Int -> Int -> [Int]
euSouOLucas 0 p q = [2]
euSouOLucas 1 p q = (euSouOLucas 0 p q) ++ [p] 
euSouOLucas x p q = (euSouOLucas (x-1) p q) ++ [p * (last (euSouOLucas (x-1) p q)) - q * (last (euSouOLucas (x-2) p q))]

{-8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] devolva [3,2,1].  -}
aoContrario :: [Int] -> [Int]
aoContrario [] = []
aoContrario x = (aoContrario (tail x)) ++ [head x]

{-9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação. -}
somaRecursiva :: Int -> Int -> Int
somaRecursiva x 0 = 0
somaRecursiva x y = x + (somaRecursiva x (y-1))

{-10. Escreva uma função chamada comprimento que receba uma lista de  inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista. -}
tamanhoLista :: [Int] -> Int
tamanhoLista [] = 0
tamanhoLista x = (tamanhoLista (tail x)) + 1

main = do
  printf "Func. 1: entrada: %d; resultado: %s\n" (13 :: Int) (show (fibonacci 13))
  printf "Func. 2: entrada: %d %d; resultado: %d\n" (13 :: Int) (10 :: Int) (mdc 13 10)
  printf "Func. 3: entrada: %d ; resultado: %d\n" (1234 :: Int) (somaDigitos 1234)
  printf "Func. 4: entrada: %d ; resultado: %d\n" (10000 :: Int) (fizzbuzz 10000)
  printf "Func. 5: entrada: %s ; resultado: %d\n" (show [1,2,3]) (questao5 [1,2,3])
  printf "Func. 6: entrada: [1..%d] ; resultado: %s\n" (30 :: Int) (show (crivo [2..30]))
  printf "Func. 7: entrada: %d ; resultado: %s\n" (10 :: Int) (show (euSouOLucas 10 1 (-1)))
  printf "Func. 8: entrada: %s ; resultado: %s\n" (show [1,2,3]) (show (aoContrario [1,2,3]))
  printf "Func. 9: entrada: %d %d ; resultado: %d\n" (2 :: Int) (3 :: Int) (somaRecursiva 2 3)
  printf "Func. 10: entrada: %s ; resultado: %d\n" (show [1,2,3]) (tamanhoLista [1,2,3])