module Tp1 where


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.Maybe

data TTree k v = Node k (Maybe v ) (TTree k v ) (TTree k v ) (TTree k v ) 
                | Leaf k v 
                | E 
                deriving Show

t = Node "r" Nothing E (Node "e" (Just 16) (Node "a" Nothing E (Leaf "s" 1) E)
                                            (Node "o" (Just 2) (Leaf "d" 9)
                                                                E
                                                                (Leaf "s" 4))
                                            E)
                        (Node "s" Nothing E (Node "i" (Just 4) (Leaf "e" 8)
                                                                (Leaf "n" 7)
                                                                E)
                                        E)
search :: Ord k =>[k] -> TTree k v -> Maybe v
search = undefined

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