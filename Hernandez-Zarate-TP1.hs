{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Tp1 where

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


search :: Ord k => [k] -> TTree k v -> Maybe v
search [] _ = error "Se ingresó una clave no valida."
search _ E = Nothing
search (x:y:xs) (Leaf k v) = Nothing
search (x: []) (Leaf k v) = if (x == k) then (Just v) else Nothing
search (x:[]) (Node k v i m d)  | (k == x) = v
                                | (x > k)  = search (x:[]) d 
                                |otherwise = search (x:[]) i
search (x:xs) (Node k v i m d)  | (x < k)  = search (x:xs) i
                                | (x > k)  = search (x:xs) d
                                |otherwise = search xs m

insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert (x:[]) v E = Leaf x v
insert (x:xs) v E = Node x Nothing E (insert xs v E) E
insert (x:[]) v (Leaf k val)       | (x == k) = Leaf k v
                                   | (x < k)  = (Node k (Just val) (Leaf x v) E E)
                                   |otherwise = (Node k (Just val) E E (Leaf x v))
                                                             
insert (x:xs) v (Leaf k val)       | (x == k) = (Node k (Just val)  E (insert xs v E) E)
                                   | (x < k)  = (Node k (Just val) (insert (x:xs) v E) E E)
                                   |otherwise = (Node k (Just val) E E (insert (x:xs) v E))
                                   
insert (x:[]) v (Node k val i m d) | (x == k) = (Node k (Just v) i m d)
                                   | (x < k)  = (Node k val (insert (x:[]) v i) m d)
                                   |otherwise = (Node k val i m (insert (x:[]) v d))
                                   
insert (x:xs) v (Node k val i m d) | (x == k) = (Node k Nothing i (insert xs v m) d)
                                   | (x < k)  = (Node k val (insert (x:xs) v i) m d)
                                   |otherwise =  (Node k val i m (insert (x:xs) v d))                                                         

--t1 = (insert "se" 8 (insert "si" 4 (insert "sin" 7 (insert "ras" 1 (insert "re" 16 (insert "red" 9 (insert "res" 4 (insert "reo" 2 E))))))))

delete :: Ord k => [k] -> TTree k v -> TTree k v
delete _ E  = E
delete [] _ = error "La clave no es válida."
delete (x:y:xs) (Leaf k v) = (Leaf k v) 
delete (x:[]) (Leaf k v) = if (x == k) then E else (Leaf k v)
delete (x:[]) (Node k v i m d)  | (k == x) = (Node k Nothing i m d)
                                | (x > k)  = (Node k v i m (delete (x:[]) d)) 
                                |otherwise = (Node k v (delete (x:[]) i) m d)
                                
delete (x:xs) (Node k v i m d)  | (x < k)  = (Node k v (delete (x:xs) i) m d)
                                | (x > k)  = (Node k v i m (delete (x:xs) d))
                                |otherwise = (Node k v i (delete xs m) d)          

keys :: TTree k v -> [[k]]
keys E = []
keys t@((Node k v i m d)) = keys' t []
  where keys' E stringActual = []
        keys' (Leaf k' v') stringActual = [ stringActual ++ [k']  ]
        keys' (Node key Nothing i' m' d') stringActual = 
          (keys' i' stringActual) ++ (keys' m' (stringActual ++ [key])) ++ (keys' d' stringActual)
        keys' (Node key (Just valor) i' m' d') stringActual = 
          (keys' i' stringActual) ++ [(stringActual ++ [key])] ++ (keys' m' (stringActual ++ [key])) ++ (keys' d' stringActual)



class Dic k v d | d -> k v where 
  vacio :: d
  insertar :: Ord k => k -> v -> d -> d
  buscar :: Ord k => k -> d -> Maybe v
  eliminar :: Ord k => k -> d -> d
  claves :: Ord k => d -> [k]

instance Ord k => Dic [k] v (TTree k v) where
  vacio = E
  insertar k v d = insert k v d
  buscar k d = search k d
  eliminar k d = delete k d
  claves d = keys d