module Arbol23 where

import Data.Char 

data Arbol23 a b = Hoja a | Dos b (Arbol23 a b) (Arbol23 a b) | Tres b b (Arbol23 a b) (Arbol23 a b) (Arbol23 a b)

{- Funciones para mostrar el árbol. -}

instance (Show a, Show b) => Show (Arbol23 a b) where
    show = ("\n" ++) . (padTree 0 0 False)

padlength = 5    
    
padTree:: (Show a, Show b) => Int -> Int -> Bool -> (Arbol23 a b)-> String
padTree nivel acum doPad t = case t of 
				  (Hoja x) -> initialPad ++ stuff x
                                  (Dos x i d) -> initialPad ++ stuff x ++ 
                                                 pad padlength ++ rec x False i ++ "\n" ++
                                                 rec x True d ++ "\n"
                                  (Tres x y i m d) -> initialPad ++ stuff x ++ --(' ':tail (stuff y)) ++
                                                      pad padlength ++ rec x False i ++ "\n" ++
                                                      pad levelPad ++ stuff y ++ pad padlength ++ rec x False m ++ "\n" ++
                                                      rec x True d ++ "\n" 
  where l = length . stuff
	levelPad = (padlength*nivel + acum)
	initialPad = (if doPad then pad levelPad else "")
	rec x = padTree (nivel+1) (acum+l x)
            
stuff:: Show a => a -> String
stuff x = if n > l then pad (n-l) ++ s else s
  where s = show x
        l = length s
        n = padlength

pad:: Int -> String
pad i = replicate i ' '

{- Funciones pedidas. -}

{- Ejercicio 1 -}

--Fold para la estructura Arbol23.
foldA23::(a->c)->(b->c->c->c)->(b->b->c->c->c->c)->Arbol23 a b->c
foldA23 fHoja fDos fTres arbol23 =
          case arbol23 of
              Hoja a -> fHoja a
              Dos b r1 r2 -> fDos b (rec r1) (rec r2)
              Tres b1 b2 r1 r2 r3 -> fTres b1 b2 (rec r1) (rec r2) (rec r3)
          where rec = foldA23 fHoja fDos fTres


{- Ejercicio 2 -}

--Lista en preorden de los internos del árbol.
internos::Arbol23 a b->[b]
internos = foldA23 (\_ -> []) (\b i1 i2 -> b:(i1 ++ i2)) (\b1 b2 i1 i2 i3 -> b1:b2:(i1 ++ i2 ++ i3))

--Lista las hojas de izquierda a derecha.
hojas::Arbol23 a b->[a]
hojas = foldA23 (\h ->[h]) (\_ h1 h2 -> h1 ++ h2) (\_ _ h1 h2 h3 -> h1 ++ h2 ++ h3)

--Nos dice si un Arbol23 es una Hoja o no.
esHoja::Arbol23 a b->Bool
esHoja = foldA23 (\h -> True) (\_ _ _ -> False) (\_ _ _ _ _ -> False)

{- Ejercicio 3 -}

--Recibe 2 funciones y un Arbol23, y lo transforma en otro Arbol23 aplica la primer función a las hojas del Arbol 
--y la segunda a todos los nodos internos del mismo.
mapA23::(a->c)->(b->d)->Arbol23 a b->Arbol23 c d
mapA23 fHoja fInterno = foldA23 (\h -> Hoja (fHoja h)) (\b h1 h2 -> Dos (fInterno b) h1 h2) 
    (\b1 b2 h1 h2 h3 -> Tres (fInterno b1) (fInterno b2) h1 h2 h3)

--Ejemplo de uso de mapA23.
--Incrementa en 1 el valor de las hojas.
incrementarHojas::Num a =>Arbol23 a b->Arbol23 a b
incrementarHojas = mapA23 (+1) id

{- Ejercicio 4 -}

--Trunca el árbol hasta un determinado nivel. Cuando llega a 0, reemplaza el resto del árbol por una hoja con el valor indicado.
truncar::a->Integer->Arbol23 a b->Arbol23 a b
truncar a i a23 = foldA23 (\h -> cortar (Hoja h))
          (\d r1 r2 nivel -> cortar (Dos d (r1 (nivel -1)) (r2 (nivel-1))) nivel)
          (\t1 t2 r1 r2 r3 nivel -> cortar (Tres t1 t2 (r1 (nivel-1)) (r2 (nivel-1)) (r3 (nivel-1))) nivel)
          a23 i
          where cortar = \value nivel -> if nivel > 0 then value else Hoja a

{- Ejercicio 5 -}

--Evalúa las funciones tomando los valores de los hijos como argumentos.
--En el caso de que haya 3 hijos, asocia a izquierda.
evaluar::Arbol23 a (a->a->a)->a
evaluar = foldA23 (id) (\b i1 i2 -> (b i1 i2)) (\b1 b2 i1 i2 i3 -> (b2 (b1 i1 i2) i3))

{- Árboles de ejemplo. -}
arbolito1::Arbol23 Char Int
arbolito1 = Tres 0 1
        (Dos 2 (Hoja 'a') (Hoja 'b'))
        (Tres 3 4 (Hoja 'c') (Hoja 'd') (Dos 5 (Hoja 'e') (Hoja 'f')))
        (Dos 6 (Hoja 'g') (Dos 7 (Hoja 'h') (Hoja 'i')))

arbolito2::Arbol23 Int Bool
arbolito2 = Dos True (Hoja (-1)) (Tres False True (Hoja 0) (Hoja (-2)) (Hoja 4))

arbolito3::Arbol23 Int (Int->Int->Int)
arbolito3 = Dos (+) (Tres (*) (-) (Hoja 1) (Hoja 2) (Hoja 3)) (incrementarHojas arbolito3)

arbolito4::Arbol23 Int Char
arbolito4 = Dos 'p' (Dos 'l' (Dos 'g' (Hoja 5) (Hoja 2)) (Tres 'r' 'a' (Hoja 0)(Hoja 1)(Hoja 12))) 
                    (Dos 'p' (Tres 'n' 'd' (Hoja (-3))(Hoja 4)(Hoja 9)) (Dos 'e' (Hoja 20)(Hoja 7)))

