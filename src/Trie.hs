module Trie
    ( empty
    , insert
    , mkTrie
    , dictionary
    , member
    , lengthOfChildNodes
    , deletable
    , delete
    , Show
    ) where

import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)

data Trie a = Trie Bool (Map.Map a (Trie a)) deriving (Eq, Read, Show)

filterTrie :: Ord a => (Trie a -> Trie a) -> [a] -> Trie a -> Trie a
filterTrie f as t = if member as t then filterTrie' f as t else t
  where
    filterTrie' f as@(x:xs) t@(Trie end nodes) = fromMaybe t (f <$> Map.lookup x nodes)

empty :: Trie a
empty = Trie False Map.empty

insert :: Ord a => [a] -> Trie a -> Trie a
insert []     (Trie _ nodes)   = Trie True nodes
insert (x:xs) (Trie end nodes) = Trie end (Map.alter (Just . insert xs . fromMaybe empty) x nodes)

mkTrie :: Ord a => [[a]] -> Trie a
mkTrie as = mkTrie' as empty
  where
    mkTrie' []     trie = trie
    mkTrie' (x:xs) trie = mkTrie' xs $ insert x trie

dictionary = mkTrie ["bad", "good", "heat", "heater", "help", "helper", "hot", "hotter", "hottest", "p", "pi", "sad", "said"]

member :: Ord a => [a] -> Trie a -> Bool
member []     (Trie end _)    = end
member (x:xs) (Trie _ nodes)  = fromMaybe False (member xs <$> Map.lookup x nodes)

lengthOfChildNodes :: Trie a -> Int
lengthOfChildNodes (Trie _ nodes) = Map.size nodes

deletable :: Ord a => [a] -> Trie a -> Bool
deletable []       (Trie _ nodes) = Map.null nodes
deletable (x : xs) (Trie end nodes) =
  (length xs == 0 || not end) &&
  maybe False (\t -> deletable xs t && (length xs == 0 || (lengthOfChildNodes t) < 1)) (Map.lookup x nodes)

delete :: Ord a => [a] -> Trie a -> Trie a
delete as t = if member as t then delete' as t else t
  where
    delete' as@(x : xs) t@(Trie end nodes) =
      if deletable as t
        then Trie end (Map.delete x nodes)
        else Trie end (Map.alter (Just . delete' xs . fromMaybe empty) x nodes)
    delete' [] t@(Trie end nodes) = if Map.size nodes > 0 then Trie False nodes else t
