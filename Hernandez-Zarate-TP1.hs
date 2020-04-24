module Tp1 where

--Autoras: Clara y Aldana

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.Maybe

data TTree k v = Node k (Maybe v ) (TTree k v ) (TTree k v ) (TTree k v ) 
                | Leaf k v 
                | E 
                deriving Show

t = Node 'r' Nothing E 
                    (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E)
                                        (Node 'o' (Just 2) (Leaf 'd' 9)
                                                             E
                                                            (Leaf 's' 4))
                                        E)
                    (Node 's' Nothing E (Node 'i' (Just 4)  (Leaf 'e' 8)
                                                            (Leaf 'n' 7)
                                                             E)
                                        E)

--devuelve el valor asociado a una clave.
-- search 'sin' t = 7

search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E = error "El arbol está vacío"
search (x:y:xs) (Leaf k v) = error "La palabra buscada no está en el dic"
search (x: []) (Leaf k v) = if (x == k) then (Just v) else error "La palabra buscada no está en el dic"
search (x:[]) (Node k v i m d) = if (k == x)    then v
                                                else 
                                                    if(x > k) then search (x:[]) d else search (x:[]) i
search (x:xs) (Node k v i m d) = if (x < k) then search (x:xs) i
                                            else if (x>k)   then search (x:xs) d
                                                            else search xs m

insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert (x:[]) v E = Leaf x v
insert (x:xs) v E = Node x Nothing E (insert xs v E) E
insert (x:[]) v (Leaf k val) = if (x == k) then Leaf k v
                                           else if (x < k) then (Node k (Just val) (Leaf x v) E E)
                                                           else (Node k (Just val) E E (Leaf x v))
insert (x:xs) v (Leaf k val) = if (x == k) then (Node k (Just val)  E (insert xs v E) E)
                                           else if (x < k) then (Node k (Just val) (insert (x:xs) v E) E E)
                                                           else (Node k (Just val) E E (insert (x:xs) v E))
insert (x:[]) v (Node k val i m d) = if (x == k) then (Node k (Just v) i m d)
                                                 else if (x < k) then (Node k val (insert (x:[]) v i) m d)
                                                                 else (Node k val i m (insert (x:[]) v d))
insert (x:xs) v (Node k val i m d) = if (x == k) then (Node k Nothing i (insert xs v m) d)
                                                 else if (x < k) then (Node k val (insert (x:xs) v i) m d)
                                                                 else (Node k val i m (insert (x:xs) v d))                                                         

--t1 = (insert "se" 8 (insert "si" 4 (insert "sin" 7 (insert "ras" 1 (insert "re" 16 (insert "red" 9 (insert "res" 4 (insert "reo" 2 E))))))))

delete :: Ord k => [k] -> TTree k v -> TTree k v
delete = undefined

-- {("se", 8); ("si", 4); ("sin", 7); ("ras", 1); ("re", 16); ("red", 9); ("res", 4); ("reo", 2)}
-- [['s','e'],['s','i'],['s','i','n'],...]

--si voy para la izquierda o para la derecha, desestimo la clave del nodo de ese momento
-- appendeo para formar la palabra solo si puedo ir por el medio
{-
keys :: TTree k v -> [[k]]
keys E = []
keys (Leaf k v) = [[k]]
keys (Node k v i m d) = case v of
                        Just a ->  [[k]] ++ keys i  ++ keys m ++ keys d 
                        Nothing -> keys i ++ (k:keys m) ++ keys d
-}
-- string actual: lista de chars
{-
keys :: TTree k v -> [[k]]
keys E = []
keys t@((Node k v i m d)) = keys' t k
  where keys' E stringActual = [[stringActual]]
        keys' (Leaf k' v') stringActual =
        keys' (Node key val i' m' d') stringActual =
                case m of
-}                  



{-ultElem (x:[]) = x
ultElem (x:xs) = ultElem(xs)
-}

{- class Dic k v d | d -> k v where 
  vacio :: d
  insertar :: Ord k => [k] -> v -> d -> d
  buscar :: Ord k => [k] -> d -> Maybe v
  eliminar :: Ord k => [k] -> d -> d
  claves :: Ord k => d -> [k]
-}
