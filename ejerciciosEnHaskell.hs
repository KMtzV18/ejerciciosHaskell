import Data.List
import Data.Char (toUpper)
import qualified Data.Map as M

-- 1 Funciones para aplicar descuento e IVA
aplicarDescuento :: Double -> Double -> Double
aplicarDescuento precio descuento = precio * (1 - descuento / 100)

aplicarIVA :: Double -> Double -> Double
aplicarIVA precio iva = precio * (1 + iva / 100)

aplicarAProductos :: M.Map String (Double, Double) -> (Double -> Double -> Double) -> M.Map String Double
aplicarAProductos cesta f = M.map (\(precio, porcentaje) -> f precio porcentaje) cesta

-- Casos de prueba
cestaEjemplo :: M.Map String (Double, Double)
cestaEjemplo = M.fromList [("Pan", (1.0, 10)), ("Leche", (1.5, 5))]

-- 2 Aplicar una función a una lista
aplicarFuncionLista :: (a -> b) -> [a] -> [b]
aplicarFuncionLista f xs = map f xs

-- Casos de prueba
pruebaLista = aplicarFuncionLista (*2) [1,2,3,4]

-- 3 Diccionario de palabras y sus longitudes
longitudPalabras :: String -> M.Map String Int
longitudPalabras frase = M.fromList [(palabra, length palabra) | palabra <- words frase]

-- Casos de prueba
pruebaLongitudes = longitudPalabras "Hola mundo Haskell"

-- 4 Convertir notas a calificaciones
convertirCalificacion :: Double -> String
convertirCalificacion nota
    | nota >= 95 = "Excelente"
    | nota >= 85 = "Notable"
    | nota >= 75 = "Bueno"
    | nota >= 70 = "Suficiente"
    | otherwise  = "Desempeño insuficiente"

convertirNotas :: M.Map String Double -> M.Map String String
convertirNotas notas = M.mapKeys (map toUpper) $ M.map convertirCalificacion notas

-- Casos de prueba
notasEjemplo = M.fromList [("Matematicas", 92), ("Historia", 88)]
pruebaNotas = convertirNotas notasEjemplo

-- 5 Módulo de un vector
moduloVector :: [Double] -> Double
moduloVector v = sqrt $ sum $ map (^2) v

-- Casos de prueba
pruebaModulo = moduloVector [3,4]

-- 6 Valores atípicos
valoresAtipicos :: [Double] -> [Double]
valoresAtipicos xs = [x | x <- xs, abs ((x - media) / desviacion) > 3]
  where
    media = sum xs / fromIntegral (length xs)
    desviacion = sqrt $ sum (map (\x -> (x - media)^2) xs) / fromIntegral (length xs)

-- Casos de prueba
pruebaAtipicos = valoresAtipicos [10, 12, 15, 100, 101, 102]
