-- | Module for tries.
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

-- | A trie where each node is implemented as a map.
data Trie = Node (M.Map Char Trie) deriving Show

-- | For reference purposes, this character is the character denoting the begin of a string. As one cannot pattern match constants,
-- @'#'@ is used throughout the code instead of @startChar@
startChar :: Char
startChar = '#'

-- | Same as @startChar@, character which marks the end of a string.
endChar :: Char
endChar = '_'

-- | Adds a new string into the trie.
addToTrie :: String -> Trie -> Trie
addToTrie ""      (Node m) = Node (M.insert endChar (Node M.empty) m)
addToTrie (c : s) (Node m) = t'
  where
    t' = if M.member c m
         then Node (M.adjust (const (addToTrie s (m M.! c))) c m) 
         else Node (M.insert c (addToTrie s (Node M.empty)) m)

-- | Generates a trie out of a list of strings.
makeTrie :: [String] -> Trie
makeTrie ss = foldr addToTrie (Node M.empty) ss

-- | Returns the underlying map of a trie.
trieToMap :: Trie -> M.Map Char Trie
trieToMap (Node m) = m

-- | Returns the subtrie reached by the given path (string).
getTrie :: String -> Trie -> Trie
getTrie [] t = t
getTrie (s : ss) (Node m) = getTrie ss (m M.! s) 
