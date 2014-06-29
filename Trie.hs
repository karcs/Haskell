module Trie
    ( Trie(Node)
    , addToTrie
    , makeTrie
    , trieToMap
    , getTrie
    , startChar
    , endChar
    ) where

import qualified Data.Map as M

data Trie = Node (M.Map Char Trie) deriving Show

startChar :: Char
startChar = '#'

endChar :: Char
endChar = '_'

addToTrie :: String -> Trie -> Trie
addToTrie ""      (Node m) = Node (M.insert endChar (Node M.empty) m)
addToTrie (c : s) (Node m) = t'
  where
    t' = if M.member c m
         then Node (M.adjust (const (addToTrie s (m M.! c))) c m) 
         else Node (M.insert c (addToTrie s (Node M.empty)) m)
      
makeTrie :: [String] -> Trie
makeTrie ss = foldr addToTrie (Node M.empty) ss

trieToMap :: Trie -> M.Map Char Trie
trieToMap (Node m) = m

getTrie :: String -> Trie -> Trie
getTrie [] t = t
getTrie (s : ss) (Node m) = getTrie ss (m M.! s) 
