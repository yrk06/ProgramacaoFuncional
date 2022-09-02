import Text.Printf
import Data.Char

fatorialn :: Int -> Int
fatorialn x = foldr (*) 1 [1 .. x]


quadradoReal :: [Double] -> [Double]
quadradoReal x = map (^2) x


comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras x = map (length) x


maiorMultiploDe29 :: Int
maiorMultiploDe29 = last (filter (\x -> (x `mod` 29) == 0) [0..100000])


maiorMultiploDe :: Int -> Int
maiorMultiploDe y = last (filter (\x -> (x `mod` y) == 0) [0..100000])


somaQuadrados :: Int -> Int
somaQuadrados x = foldr (\x y ->  x^2 + y) 0 [1..x]


comprimento :: [a] -> Int
comprimento x = foldl (\x y -> x + 1) 0 x

main = do
  printf "Func. 1: entrada: %d; resultado: %d\n" (5 :: Int) (fatorialn 5)
  printf "Func. 2: entrada: %s; resultado: %s\n" (show [1,2,3,4,-2]) (show (quadradoReal [1,2,3,4,-2]))
  printf "Func. 3: entrada: %s; resultado: %s\n" (show ["Ola","Mundo"]) (show (comprimentoPalavras ["Ola","Mundo"]))
  printf "Func. 4: entrada: -; resultado: %d\n" (maiorMultiploDe29)
  printf "Func. 5: entrada: %d; resultado: %d\n" (30 :: Int) (maiorMultiploDe 30)
  printf "Func. 6: entrada: %d; resultado: %d\n" (3 :: Int) (somaQuadrados 3)
  printf "Func. 7: entrada: %s; resultado: %d\n" (show ["Ola","Mundo"]) (comprimento ["Ola","Mundo"])

  printf "Func. 8-flip-1: entrada: %s; resultado: %f\n" ("flip (/) 1 2") ((flip (/) 1 2) :: Double)
  printf "Func. 8-flip-2: entrada: %s; resultado: %s\n" ("flip (>) 3 5") (show (flip (>) 3 5))
  printf "Func. 8-ord-1: entrada: %s; resultado: %d\n" ("a") (ord 'a')
  printf "Func. 8-ord-2: entrada: %s; resultado: %d\n" ("z") (ord 'z')
  printf "Func. 8-max-1: entrada: %s; resultado: %d\n" ("max 2 4") (max 2 4 :: Int)
  printf "Func. 8-max-1: entrada: %s; resultado: %d\n" ("max 9 4") (max 9 4 :: Int)
  printf "Func. 8-min-1: entrada: %s; resultado: %d\n" ("min 2 4") (min 2 4 :: Int)
  printf "Func. 8-min-2: entrada: %s; resultado: %d\n" ("min 4 9") (min 9 4 :: Int)
  printf "Func. 8-curry-1: entrada: %s; resultado: %d\n" ("curry fst 2 3") (curry fst 2 3 :: Int)
  printf "Func. 8-curry-2: entrada: %s; resultado: %d\n" ("curry snd 2 3") (curry snd 2 3 :: Int)
  printf "Func. 8-uncurry-1: entrada: %s; resultado: %d\n" ("uncurry mod (5,4)") (uncurry mod (5,4) :: Int)
  printf "Func. 8-uncurry-2: entrada: %s; resultado: %d\n" ("(uncurry (*)) (5,4)") ((uncurry (*)) (5,4) :: Int)
