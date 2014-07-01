-- | Module for minimum edit distance algorithms.
module MinEdit
    ( findBest,
      findNeighbourhood
    ) where

import Trie
import Control.Monad.State
import Data.List (insertBy)
import qualified Data.Map as M


-- | A column in the matrix for evaluating the minimum edit distance. First entry is the current character,
-- second is the column itself.
type DistanceMatrixColumn = (Char, [(Char, Int)])

-- | A type representing the current evaluation of a node in a trie. It has the most recent columns of the
-- minimum edit distance matrix, the characters determining the path to the node and the subtrie.
type StateEntry = ([DistanceMatrixColumn], String, Trie)

-- | A map of state entries. Each state entry is to be indexed by some heuristic value.
type StateEntries = M.Map Int [StateEntry]

-- | A big enough number to initialize unplausible distances.
bigNum :: Int
bigNum = 9999

-- | Costs of deletion of a character
delcost :: Char -> Int
delcost _ = 1

-- | Costs of insertion of a character
inscost :: Char -> Int
inscost _ = 1

-- | Costs of substituion of a character with another
subcost :: Char -> Char -> Int
subcost c1 c2 = if c1 == c2 then 0 else 1

-- | Costs of swaping two consecutive characters
swapcost :: (Char,Char) -> (Char,Char) -> Int
swapcost (c0,c1) (d0,d1) = if c0 == d1 && d0 == c1 && c0 /= '#' && c1 /= '#' && d0 /= '#' && c1 /= '#' then
                             if c0 == c1 then 0
                             else 1
                           else
                             bigNum

-- | Calculates a new column in the distance matrix and inserts it in front of the list of columns.
-- Expects the character determing the new column and the current list of columns and returns the updated list and
-- an heuristic value for the minimum edit distance, which can be used to prune the trie. @#@ is a character denoting
-- the begin of the string.
update :: Char -> [DistanceMatrixColumn] -> (Int, [DistanceMatrixColumn])
update c0 ccis0 = case ccis0 of
  cci0 : cci1 : _ -> (h0, take 2 ((c0, cis0):ccis0)) -- only the first two tables need to be memoized
    where (h0, cis0) = go (0, 0, '#') (0 : 0 : map snd (snd cci1)) (0, snd cci0)
            where
              -- first is (subcost add, inscost add, last char)
              go :: (Int, Int, Char) -> [Int] -> (Int, [(Char, Int)]) -> (Int, [(Char, Int)])
              go _ is0 (_, ('#', i0) : cis1) = (min i1 $ fst ici0, ('#', i1) : snd ici0)
                where i1 = i0 + delcost c0
                      ici0 = go (i0, i1, '#') (tail is0) (i1, cis1)
              go (j0, j1, d0) is0 (h1, (d1, i1) : cis1) = (min h2 $ fst ici0, (d1, i2) : snd ici0)
                where i2 = minimum [ i1 + delcost c0, j0 + subcost c0 d1, j1 + inscost d1,
                                        swapcost (fst cci0, c0) (d0, d1) + head is0 ]
                      ici0 = go (i1, i2, d1) (tail is0) (h1, cis1)
                      h2 = min h1 i2
              go _ _ (h1,[]) = (h1,[])
  cci0 : _ -> (h0, (c0, cis0) : ccis0)
    where (h0, cis0) = go (0,0) (0, snd cci0)
            where
              go :: (Int, Int) -> (Int, [(Char, Int)]) -> (Int, [(Char, Int)])
              go _ (_,(('#', i1) : ci2)) = (min i2 $ fst ici0, ('#', i2) : snd ici0)
                where i2 = i1 + delcost c0
                      ici0 = go (i1, i2) (i2,ci2) -- rest of the 'list'
              go (j0, j1) (h1,((c1,i1) : ci2)) = (min h2 $ fst ici0, (c1, i2) : snd ici0)
                where i2 = minimum [ i1 + delcost c0, j0 + subcost c0 c1, j1 + inscost c1 ] -- newly defined distance
                      ici0 = go (i1, i2) (h2, ci2)
                      h2 = min h1 i2
              go _ (h1,[]) = (h1,[])

-- | Initializes the first column from the original string. It adds the start character @#@ in front of it.
initializeDistanceMatrixColumn :: String -> [DistanceMatrixColumn]
initializeDistanceMatrixColumn s = [('#',(zip ('#' : s) [0..]))]


-- | @findNeighbourhood i s t@ determines all words in the trie @t@, which have a minimum edit distance to @s@ which is smaller then @t@.
-- It prunes all subtries, whose heuristic distance as given by the update function is lower then @t@.
findNeighbourhood :: Int -> String -> Trie -> M.Map Int [String]
findNeighbourhood r0 s0 t0 = findNeighbourhood' r0 "" t0 $ initializeDistanceMatrixColumn s0
  where
    -- first : radius,
    -- second : current string,
    -- third : subtrie
    -- forth : assoc list with substrings (chars) and min edt dists
    findNeighbourhood' :: Int -> String -> Trie -> [DistanceMatrixColumn] -> M.Map Int [String]
    findNeighbourhood' r1 s1 (Node m0) ci0 = M.foldr' (M.unionWith (++)) M.empty (M.mapWithKey go m0)
      where
        go :: Char -> Trie -> M.Map Int [String] -- recursion function
        go '_' _ = if d0 > r1 then
                     M.empty -- return nothing (matching is out of threshold r1)
                   else
                     M.singleton (snd . last . snd $ head ci0) [s1] -- return current string with minimum edit distance ('_' is endChar)
          where d0 = snd . last . snd $ head ci0 -- current minimum edit distance
        go c0 t1 = if h0>r1 then
                     M.empty
                   else
                      findNeighbourhood' r1 (s1++[c0]) t1 ci1
          where (h0,ci1) = update c0 ci0 -- updated assoc list

-- | @findBest i s t@ gives @i@ proposals of words of @t@ with the lowest minimum edit distance to @s@.
-- Internally it uses operates on a list of nodes in the trie and expands the node with the best heuristic as given by update.
-- Fully expanded nodes are kept in a second list of length @i@, only the entries with shortest distance remain.
-- Not fully expanded nodes are removed, if their heuristic is lower then the lowest minimum edit distance of this second list.
-- If there is no node to expand the second list is returned.
findBest :: Int -> String -> Trie -> [(Int, String)]
findBest i s t = map (\(i, s) -> (i, reverse $ tail s)) $ reverse result
    where
      result = evalState findBest' (expand (initializeDistanceMatrixColumn s, "", t), take i $ repeat (bigNum, "_"))
     
      findBest' :: State (StateEntries, [(Int, String)]) [(Int, String)]
      findBest' = do
                     (se, fse) <- get
                     let (se', fse') = expandFirst se fse
                         se'' = M.filterWithKey (\k _ -> (<=) k . fst $ head fse') se'
                     put (se'', fse')
                     if M.null se''
                     then return fse'
                     else findBest'

-- | Expands the node with smallest heuristic minimum edit distance of the map of not fully expanded nodes. If the
-- node represents a finished word of the trie (it has a child with indey '_', the end character), it will be inserted into
-- the second argument, in case it has a higher minimum edit distance then the lowest in the list. All other childs are merged into
-- the map of nodes.
expandFirst :: StateEntries -> [(Int, String)]-> (StateEntries, [(Int, String)])
expandFirst mse fse = (mse'', fse'')
    where
      ((ccis, s, Node m), mse') = extractFirst mse
      fse'' = maybe fse (const . tail $ insertBy
                               customOrder
                               (snd . last . snd $ head ccis, '_' : s)
                               fse
                        ) $ M.lookup '_' m
      mse'' = M.filter (not . null) (M.unionWith (++) mse' $ expand (ccis, s, Node m))

      customOrder :: (Int, String) -> (Int, String) -> Ordering
      customOrder x y = compare (fst y) (fst x)

-- | Extracts the entry of the map of nodes with the smallest key and returns this node and the updated map,
-- where this node is removed.
extractFirst :: StateEntries -> (StateEntry, StateEntries)
extractFirst mse = (se, mes')
    where
      mes' = M.adjust (const ses) k mse
      Just (k, se : ses) = M.lookupGE 0 mse

-- | Expands a state entry and returns all its children, which are updated in between.
expand :: StateEntry -> StateEntries
expand (ccis, s, Node m) =
    mapKeysValuesWith (++) (\(c, t) -> let (hi, ccis') = update c ccis in
                                       (hi, [(ccis', c : s, t)])) b
        where
          b = M.filterWithKey (\c _ -> c /= '_') m
              
          -- An function analoguesly defined to M.mapKeysWith. Here both keys and values can be transformed,
          -- keys which occur multiple times (because the mapping function is not injective) are folded with
          -- the function given in the first argument.
          mapKeysValuesWith :: Ord k2 => (b -> b -> b) -> ((k1, a) -> (k2, b)) -> M.Map k1 a -> M.Map k2 b
          mapKeysValuesWith c f = M.fromListWith c . M.foldrWithKey (\k x xs -> (f (k, x)) : xs) []

    


