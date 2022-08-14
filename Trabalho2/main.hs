import Text.Printf



{-1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada. -}
soma1 :: Int -> Int
soma1 n = ((+) n 1)

{-2. Escreva  uma  função  chamada  sempre  que,  não  importando  o  valor  de  entrada, devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo. -}
sempre :: generico -> Int
sempre _ = 0

{-3. Escreva  uma  função  chamada  treco  que  receba  três  valores  em  ponto  flutuantes com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro. -}
treco :: Double->Double->Double->Double
treco x y z = ((*)((+) x y)z)

{-4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros.-}
resto :: Int->Int->Int
resto x y = (x `mod` y)

{-5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores monetários. -}
precoMaior :: Double->Double->Double->Double->Double
precoMaior x y z w = max (max x y) (max z w)

{-6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar. -}

impar :: Int->Int->Bool
impar x y
  |  prod `mod` 2 == 0 = False 
  |  otherwise = True
  where
    prod = ((*) x y)

{-?. Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟∷(𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros.-}
funcaoEmHaskell :: (Int,Int)->Int
funcaoEmHaskell x = ((+) (fst x) (snd x))

{-7. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação 𝑥ˆ2 +𝑦/2 +𝑧. -}

funcaoEmHaskell2 :: Double->Double->Double->Double
funcaoEmHaskell2 x y z = x^2 + y/2 + z

{-8. Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima um  diagnóstico  de  obesidade,  segundo  a  tabela  que  pode  ser  encontrada  no  link: Sobrepeso,  obesidade  e  obesidade  mórbida:  entenda  a  diferença  entre  os  três  termos (cuidadospelavida.com.br).  Observe  que  este  diagnóstico  é  meramente  estatístico  e  não tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. Todo e qualquer diagnóstico deve ser feito por um profissional médico.  -}

-- Não há instrução para receber a altura como parametro, portanto a altura média foi adotada
diagnostico :: Double->String
diagnostico x
  | imc < 17 = "Muito abaixo do peso"
  | imc < 18.5 = "Abaixo do peso"
  | imc < 25 = "Peso normal"
  | imc < 30 = "Sobrepeso"
  | imc < 35 = "Obesidade leve"
  | imc < 40 = "Obesidade severa"
  | otherwise = "Obesidade morbida"
  where imc = x / 1.739^2

{-9. Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o 
ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:  
𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4 
      𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100 
            𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400 
1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto. -}
bissexto :: Int->Bool
bissexto ano
  | div4 && div100 && div400 = True
  | div4 && not div100 = True
  | otherwise = False
  where div4 = (ano `mod` 4) == 0
        div100 = (ano `mod` 100) == 0
        div400 = (ano `mod` 400) == 0

main = do
  -- 1
  printf "Func. %d: entrada:%d; resultado:%d \n" (1 :: Int) (2 :: Int) (soma1 2)
  -- 2
  printf "Func. %d: entrada:%d; resultado:%d \n" (2 :: Int) (1024 :: Int) (sempre 1024)
  -- 3
  printf "Func. %d: entrada:%f %f %f; resultado:%f \n" (3 :: Int) (1 :: Double) (2 :: Double) (3 :: Double) (treco 1 2 3)
  -- 4
  printf "Func. %d: entrada:%d %d; resultado:%d \n" (4 :: Int) (15 :: Int) (10 :: Int)  (resto 15 10)
  -- 5
  printf "Func. %d: entrada:%f %f %f %f; resultado:%f \n" (5 :: Int) (10 :: Double) (9 :: Double) (8 :: Double) (1 :: Double)  (precoMaior 10 9 8 1)
  -- 6
  printf "Func. %d: entrada:%d %d; resultado:%s \n" (6 :: Int) (13 :: Int) (9 :: Int)  (show (impar 13 9))
  -- ?
  printf "Func. %s: entrada: (%d,%d); resultado:%d \n" "?" (1 :: Int) (2 :: Int)  (funcaoEmHaskell (1,2) )
  -- 7
  printf "Func. %d: entrada:%f %f %f; resultado:%f \n" (7 :: Int) (15 :: Double) (10 :: Double) (10 :: Double)  (funcaoEmHaskell2 15 10 10)
  -- 8
  printf "Func. %d: entrada:%f; resultado:%s \n" (8 :: Int) (50 :: Double)  (diagnostico 50)
  -- 9
  printf "Func. %d: entrada:%d; resultado:%s \n" (9 :: Int) (2000 :: Int)  (show (bissexto 2000))
  
