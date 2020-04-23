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
insert = undefined

delete :: Ord k => [k] -> TTree k v -> TTree k v
delete = undefined

keys :: Ord k => d -> [k]
keys = undefined 


{- class Dic k v d | d -> k v where 
  vacio :: d
  insertar :: Ord k => [k] -> v -> d -> d
  buscar :: Ord k => [k] -> d -> Maybe v
  eliminar :: Ord k => [k] -> d -> d
  claves :: Ord k => d -> [k]
-}
