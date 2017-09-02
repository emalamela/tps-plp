module Diccionario (Diccionario, vacio, definir, definirVarias, obtener, claves, mapping) where

import Data.Maybe
import Data.List
import Arbol23


{- Definiciones de tipos. -}

type Comp clave = clave->clave->Bool
type Estr clave valor = Arbol23 (clave,valor) clave

data Diccionario clave valor = Dicc {cmp :: Comp clave, estructura :: Maybe (Estr clave valor)}
--El comparador es por menor.

{- Funciones provistas por la cátedra. -}

--Inserta un nuevo par clave, valor en una estructura que ya tiene al menos un dato.
insertar::clave->valor->Comp clave->Estr clave valor-> Estr clave valor
insertar c v comp a23 = interceptar (insertarYPropagar c v comp a23) id (\s1 (c1, s2)->Dos c1 s1 s2)

--Maneja el caso de que la segunda componente sea Nothing.
interceptar::(a,Maybe b)->(a->c)->(a->b->c)->c
interceptar (x,y) f1 f2 = case y of
                   Nothing -> f1 x
                   Just z -> f2 x z

{- Inserta una clave con su valor correspondiente. Si se actualiza el índice, el cambio se propaga hacia arriba
   para mantener balanceado el árbol.
   Usamos recursión explícita porque este tipo de recursión no es estructural (no se aplica a todos los hijos). -}
insertarYPropagar::clave->valor->Comp clave->Estr clave valor-> (Estr clave valor, Maybe (clave, Estr clave valor))
insertarYPropagar c v comp a23 = let rec = insertarYPropagar c v comp in case a23 of
    --Si es hoja, elegimos la máxima de las claves y propagamos el balanceo hacia arriba.
    Hoja (ch,vh) -> if comp c ch 
                then (Hoja (c,v), Just (ch, Hoja (ch,vh)))
                else (Hoja (ch, vh), Just (c, Hoja (c,v)))
    {- Si el actual es Nodo-Dos, o se queda en forma Nodo-Dos o se transforma en 
       Nodo-Tres; no puede ocurrir que haya propagación hacia arriba (retornamos Nothing). -}
    Dos c1 a1 a2 -> (if comp c c1
                then 
                -- La clave c va del lado izquierdo.
                    interceptar (rec a1) 
                        (\s1 -> Dos c1 s1 a2)
                        (\s1 (c3, s2) -> Tres c3 c1 s1 s2 a2)
                else 
                -- La clave c va del lado derecho.
                    interceptar (rec a2) 
                        (\s1 -> Dos c1 a1 s1)
                        (\s1 (c3, s2) -> Tres c1 c3 a1 s1 s2), Nothing)
    {- Nodo-tres sólo propaga si de abajo propagan, los tres casos son muy similares
       Sólo cambia en que árbol se inserta. -}
    Tres c1 c2 a1 a2 a3 -> if comp c c1
                then 
                    -- La clave debe ir en el primer árbol.
                    interceptar (rec a1) 
                        (\s1 -> (Tres c1 c2 s1 a2 a3, Nothing))
                        (\s1 (c3, s2) -> (Dos c3 s1 s2, Just(c1, Dos c2 a2 a3)))
                else if comp c c2
                then 
                    -- La clave debe ir en el árbol del medio.
                    interceptar (rec a2) 
                        (\s1 -> (Tres c1 c2 a1 s1 a3, Nothing))
                        (\s1 (c3, s2) -> (Dos c1 a1 s1, Just(c3, Dos c2 s2 a3)))
                else 
                    --La clave debe ir en el último árbol.
                    interceptar (rec a3) 
                        (\s1 -> (Tres c1 c2 a1 a2 s1, Nothing))
                        (\s1 (c3, s2) -> (Dos c1 a1 a2, Just(c2, Dos c3 s1 s2)))

--Se asume que la lista no tiene claves repetidas.
definirVarias::[(clave,valor)]->Diccionario clave valor->Diccionario clave valor
definirVarias = (flip.foldr.uncurry) definir

{- Funciones a implementar. -}

{- Ejercicio 6 -}

--Devuelve un árbol vacío.
vacio::Comp clave->Diccionario clave valor
vacio cmp = Dicc cmp Nothing

{- Ejercicio 7 -}

--Inserta en un diccionario una (Hoja (c,v))
definir::clave->valor->Diccionario clave valor->Diccionario clave valor
definir c v (Dicc cmp estructura) | isNothing estructura = Dicc cmp (Just (Hoja (c,v)))
                                  | otherwise = Dicc cmp (Just (insertar c v cmp (fromJust estructura)))

{- Ejercicio 8 -}

obtener::Eq clave=>clave->Diccionario clave valor->Maybe valor
obtener clave (Dicc cmp estructura) | isNothing estructura = Nothing 
                                    | otherwise = buscarClave clave cmp (fromJust estructura)

buscarClave::Eq clave=>clave->Comp clave ->Estr clave valor -> Maybe valor
buscarClave clave cmp estr = foldA23 (\(claveHoja, valorHoja) -> if claveHoja == clave then Just valorHoja else Nothing) 
                        (\i r1 r2 -> if cmp clave i then r1 else r2)
                        (\i1 i2 r1 r2 r3 -> if cmp clave i1 then r1 else 
                            (if cmp clave i2 then r2 else r3)) estr

{- Ejercicio 9 -}

claves::Diccionario clave valor->[clave]
claves dicc = map fst (mapping dicc)

{- Auxiliares -}

mapping::Diccionario clave valor->[(clave,valor)]
mapping (Dicc _ estructura) | isNothing estructura = []
                            | otherwise = hojas (fromJust (estructura))

{- Diccionarios de prueba: -}

dicc1::Diccionario Int String
dicc1 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (<))

dicc2::Diccionario String String
dicc2 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),("casa","escalera"),("ropero","alfajor"),("escalera","ropero")] (vacio (<))

dicc3::Diccionario Int String
dicc3 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (\x y->x `mod` 5 < y `mod` 5))