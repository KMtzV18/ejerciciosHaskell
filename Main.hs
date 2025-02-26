module Main where

-- 1. Suma de elementos en una lista
sumarLista :: [Int] -> Int
sumarLista = sum

-- 2. Factorial de un número
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 3. Números pares hasta un número dado
numerosPares :: Int -> [Int]
numerosPares n = [x | x <- [0..n], even x]

-- 4. Longitud de una cadena
longitudCadena :: String -> Int
longitudCadena = length

-- 5. Reverso de una lista
reversoLista :: [a] -> [a]
reversoLista = reverse

-- 6. Duplicar elementos de una lista
duplicarElementos :: [Int] -> [Int]
duplicarElementos = concatMap (\x -> [x, x])

-- 7. Filtrar elementos pares de una lista
filtrarPares :: [Int] -> [Int]
filtrarPares = filter even

-- 8. Fibonacci 
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- 9. Divisores de un número
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

-- 10. Verificar si una cadena es un palíndromo
esPalindromo :: String -> Bool
esPalindromo str = str == reverse str

-- Función main para pruebas
main :: IO ()
main = do
    putStrLn "Pruebas de las funciones:"
    print $ sumarLista [1, 2, 3, 4, 5] -- 15
    print $ factorial 5 -- 120
    print $ numerosPares 10 -- [0,2,4,6,8,10]
    print $ longitudCadena "Haskell" -- 7
    print $ reversoLista [1, 2, 3, 4, 5] -- [5,4,3,2,1]
    print $ duplicarElementos [1, 2, 3] -- [1,1,2,2,3,3]
    print $ filtrarPares [1, 2, 3, 4, 5] -- [2,4]
    print $ fibonacci 6 -- 8
    print $ divisores 12 -- [1,2,3,4,6,12]
    print $ esPalindromo "reconocer" -- True
