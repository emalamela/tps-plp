import Diccionario
import Data.Maybe
import Arbol23
import Test.HUnit

--Este módulo sirve para utilizar el diccionario sin acceder a su estructura interna. Pueden agregar otras funciones o casos de prueba.

{- Función a implementar. -}

búsquedaDelTesoro::Eq a=>a->(a->Bool)->Diccionario a a->Maybe a 
búsquedaDelTesoro pista funcion dicc = foldr (\x rec -> case x of 
                                                          Nothing -> Nothing
                                                          Just x -> if funcion x then Just x else rec)
                                            Nothing
                                            (tail (pistas (Just pista)))
                                where pistas = iterate (\x -> case x of 
                                                              Nothing -> Nothing 
                                                              Just x -> obtener x dicc)                                        

{- Diccionarios de prueba: -}

diccVacioMenor::Diccionario Int String
diccVacioMenor = vacio (<)

dicc1::Diccionario Int String
dicc1 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (<))

dicc2::Diccionario String String
dicc2 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),("casa","escalera"),("ropero","alfajor"),("escalera","ropero")] (vacio (<))

dicc3::Diccionario Int String
dicc3 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (\x y->x `mod` 5 < y `mod` 5))

dicc4::Diccionario Int String
dicc4 = definir 10 "Messi" diccVacioMenor

dicc5::Diccionario Int String
dicc5 = definir 5 "Gago" dicc4

dicc6::Diccionario String String
dicc6 = definirVarias [("Quién","es"),("es","el"),("el","mejor?"),("mejor?","Respuesta"),("Respuesta","Messi")] (vacio (>))

{- Funciones Auxiliares -}

negarInternos::Num b => Arbol23 a b -> Arbol23 a b
negarInternos = mapA23 (id) (*(-1))

--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8,
  "ejercicio9" ~: testsEj9,
  "ejercicio10" ~: testsEj10
  ]

testsEj2 = test [
  "Test Ej 2 - internos arbolito1 devuelve los primeros 8 naturales" ~: [0,1,2,3,4,5,6,7] ~=? internos arbolito1,
  "Test Ej 2 - internos arbolito2 devuelve [True,False,True]" ~: [True,False,True] ~=? internos arbolito2,
  "Test Ej 2 - hojas arbolito1 devuelve las primeras 9 letras del abcedario" ~: "abcdefghi" ~=? hojas arbolito1,
  "Test Ej 2 - las primeras 10 hojas de arbolito3" ~: [1,2,3,2,3,4,3,4,5,4] ~=? take 10 (hojas arbolito3),
  "Test Ej 2 - esHoja de arbolito1 es False" ~: False ~=? esHoja arbolito1,
  "Test Ej 2 - esHoja de (Hoja 1) es True" ~: True ~=? esHoja (Hoja 1)
  ]

testsEj3 = test [
  "Test Ej 3 - incrementarHojas arbolito2 incrementa en 1 el valor de todas las hojas " ~: [0,1,-1,5] ~=? hojas (incrementarHojas arbolito2),
  "Test Ej 3 - negarInternos arbolito1 devuelve los primeros 8 naturales en negativo" ~: [ (-x) | x <- [0..7] ] ~=? internos (negarInternos arbolito1)
  ]

testsEj4 = test [
  "Test Ej 4 - hojas truncar 0 6 arbolito3" ~: [1,2,3,2,3,4,3,4,5,4,5,6,0,0,0,0,0] ~=? hojas (truncar 0 6 arbolito3),
  "Test Ej 4 - internos truncar 0 6 arbolito4 se mantienen intactos" ~: "plgrapnde" ~=? internos (truncar 0 6 arbolito4),
  "Test Ej 4 - hojas truncar 'e' 8 arbolito1 devuelve las primeras 9 letras del abcedario" ~: "abcdefghi" ~=? hojas (truncar 'e' 8 arbolito1),
  "Test Ej 4 - hojas truncar 'e' 1 arbolito1 devuelve \"eee\"" ~: (replicate 3 'e') ~=? hojas (truncar 'e' 1 arbolito1)
  ]

testsEj5 = test [
  "Test Ej 5 - evaluar truncar 0 3 arbolito3 devuelve (-1)" ~: (-1) ~=? evaluar (truncar 0 3 arbolito3),
  "Test Ej 5 - evaluar truncar 0 4 arbolito3 devuelve 1" ~: 1 ~=? evaluar (truncar 0 4 arbolito3),
  "Test Ej 5 - evaluar truncar 0 5 arbolito3 devuelve 8" ~: 8 ~=? evaluar (truncar 0 5 arbolito3),
  "Test Ej 5 - evaluar truncar 0 6 arbolito3 devuelve 22" ~: 22 ~=? evaluar (truncar 0 6 arbolito3)
  ]

testsEj6 = test [
  "Test Ej 6 - mapping de diccionario vacio devuelve lista vacia" ~: [] ~=? mapping diccVacioMenor
  ]

testsEj7 = test [
  "Test Ej 7 - mapping de dicc4 devuelve [(10, \"Messi\")]" ~: [(10, "Messi")] ~=? mapping dicc4,
  "Test Ej 7 - mapping de dicc5 devuelve [(5, \"Gago\"), (10, \"Messi\")] " ~: [(5, "Gago"), (10, "Messi")] ~=? mapping dicc5
  ]

testsEj8 = test [
  "Test Ej 8 - obtener 10 de dicc4 devuelve Just \"Messi\"" ~: Just "Messi" ~=? obtener 10 dicc4,
  "Test Ej 8 - obtener 20 de dicc4 devuelve Nothing" ~: Nothing ~=? obtener 20 dicc4,
  "Test Ej 8 - obtener (-10) de dicc1 devuelve Just \"Chau\"" ~: Just "Chau" ~=? obtener (-10) dicc1
  ]
  
testsEj9 = test [
  "Test Ej 9 - claves de dicc4 devuelve [10]" ~: [10] ~=? claves dicc4,
  "Test Ej 9 - claves de dicc5 devuelve [5, 10]" ~: [5, 10] ~=? claves dicc5,
  "Test Ej 9 - claves de dicc1 devuelve [(-10), 0, 2, 9, 15]" ~: [(-10), 0, 2, 9, 15] ~=? claves dicc1
  ]
  
testsEj10 = test [
  "Test Ej 10 - busquedaDelTesoro \"inicio\" ((=='a').head) dicc2 devuelve Just \"alfajor\"" ~: Just "alfajor" ~=? búsquedaDelTesoro "inicio" ((=='a').head) dicc2,
  "Test Ej 10 - busquedaDelTesoro \"Quién\" (==\"Messi\") dicc6 devuelve Just \"Messi\"" ~: Just "Messi" ~=? búsquedaDelTesoro "Quién" (== "Messi") dicc6,
  "Test Ej 10 - busquedaDelTesoro \"Quién\" (==\"Cristiano\") dicc6 devuelve Nothing" ~: Nothing ~=? búsquedaDelTesoro "Quién" (== "Cristiano") dicc6
  ]