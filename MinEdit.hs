module MinEdit
    ( findBest,
      findNeighbourhood,
      extractFirst
    ) where

import Trie
import Control.Monad.State
import qualified Data.Map as M
import Data.List (sortBy, find)
import Data.Maybe (fromJust, maybe)

import Debug.Trace

type CharInts = [(Char, Int)]
type StateEntry = ((Int, Int, [(Char,CharInts)]), String, Trie)

bigNum :: Int
bigNum = 9999

delcost :: Char -> Int
delcost _ = 1

subcost :: Char -> Char -> Int
subcost c1 c2 = if c1 == c2 then 0 else 1

inscost :: Char -> Int
inscost _ = 1

swapcost :: (Char,Char) -> (Char,Char) -> Int
swapcost (c0,c1) (d0,d1) = if c0 == d1 && d0==c1 && c0 /= '#' && c1 /= '#' && d0 /= '#' && c1 /= '#' then
                             if c0 == d0 then 0
                             else 1
                           else
                             bigNum

medFromUpdate = fst
heuFromUpdate = snd
thd :: (a,b,c) -> c
thd (a,b,c) = c
ccisFromUpdate = thd

-- update with one character the table (first int is current minimum edit distance, second is heuristic)
update :: Char -> [(Char, CharInts)] -> (Int, Int, [(Char, CharInts)])
update c0 ccis0 = case ccis0 of
  cci0 : cci1 : _ -> (med0, h0, take 2 ((c0, cis0):ccis0)) -- only the first two tables need to be memoized
    where (h0, cis0) = update' (0, 0, '#') (0 : 0 : map snd (snd cci1)) (0, snd cci0)
            where
              -- first is (subcost add, inscost add, last char)
              update' :: (Int, Int, Char) -> [Int] -> (Int,CharInts) -> (Int,CharInts)
              update' _ is0 (_, ('#', i0) : cis1) = (min i1 (fst ici0), ('#', i1) : snd ici0)
                where i1 = i0 + delcost c0
                      ici0 = update' (i0, i1, '#') (tail is0) (i1, cis1)
              update' (j0, j1, d0) is0 (h1, (d1, i1) : cis1) = (min h2 (fst ici0), (d1, i2) : snd ici0)
                where i2 = minimum [ i1 + delcost c0, j0 + subcost c0 d1, j1 + inscost d1, swapcost (fst cci1, fst cci0) (d0, d1) + head is0 ]
                      ici0 = update' (i1, i2, d1) (tail is0) (h1, cis1)
                      h2 = min h1 i2
              update' _ _ (h1,[]) = (h1,[]) 
          med0 = snd $ last cis0
  cci0 : _ -> (med0, h0, (c0, cis0) : ccis0)
    where (h0, cis0) = update' (0,0) (0, snd cci0)
            where
              update' :: (Int, Int) -> (Int, CharInts) -> (Int, CharInts)
              update' _ (_,(('#', i1) : ci2)) = (min i2 (fst ici0),(startChar, i2) : snd ici0)
                where i2 = i1 + delcost c0
                      ici0 = update' (i1, i2) (i2,ci2) -- rest of the 'list'
              update' (j0, j1) (h1,((c1,i1) : ci2)) = (min h2 (fst ici0),(c1,i2) : snd ici0)
                where i2 = minimum [ i1 + delcost c0, j0 + subcost c0 c1, j1 + inscost c1 ] -- newly defined distance
                      ici0 = update' (i1, i2) (h2,ci2)
                      h2 = min h1 i2
              update' _ (h1,[]) = (h1,[])
          med0 = snd $ last cis0
  
minEditDist :: CharInts -> Int
minEditDist [] = bigNum
minEditDist x  = snd $ last x

readableEntry :: [StateEntry] -> [(Int, String)]
readableEntry = map (\(i, s, _) -> (i, reverse s))

stringToCI :: String -> [(Char, Int)]
stringToCI s = (zip (startChar : s) [0..length s])

expandFirst :: [StateEntry] -> [StateEntry]
expandFirst st = --trace ("New:   " ++ (show $ readableEntry newElems)) $ 
                 mergeBy customOrder st' (sortBy customOrder newElems)
    where
      (((_, hci), ss, Node m), st') = extractFirst st
      newElems     = map (\(c, t) -> if c == '_'
                                     then ((snd (last hci), hci), c : ss, t)
                                     else (update c (0, hci), c : ss, t)
                                         )  $ M.toList m

      customOrder :: StateEntry -> StateEntry -> Ordering
      customOrder (x, _, _) (y, _, _) = compare (fst x) (fst y)

      -- deliberately copied from the (experimental) Data.List.Ordered package.
      mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
      mergeBy cmp = loop
          where
            loop [] ys  = ys
            loop xs []  = xs
            loop (x:xs) (y:ys)
                = case cmp x y of
                    GT -> y : loop (x:xs) ys
                    _  -> x : loop xs (y:ys)

extractFirst :: [StateEntry] -> (StateEntry, [StateEntry])
extractFirst ((cis, b, Node m) : ss) = extractFirst' (cis, b, Node m) ss
    where
      extractFirst' :: StateEntry -> [StateEntry] -> (StateEntry, [StateEntry])
      extractFirst' (cis, ('_' : s), t) (c : ss) =
          (rs, (cis, ('_' : s), t) : rss)
              where (rs, rss) = extractFirst' c ss
      extractFirst' (cis, s, t) ss =  ((cis, s, t), ss)
          


findBest :: Int -> String -> Trie -> [(Int, String)]
findBest i s t = map (\(i, s) -> (i,  reverse $ tail s)) result
    where
      result = evalState (findBest' i s) [((0, stringToCI s), "", t)]
     
      findBest' :: Int -> String -> State [StateEntry] [(Int, String)]
      findBest' i s = do
                     st <- get
                     let st' = expandFirst st
                     let
                         hList     = take i $ filter (\(_, (s:_), _) -> s == '_') st'                     
                         st''      = if null hList
                                     then st'
                                     else takeWhile (\((x, _), _, _) ->  (threshold >= x)) st'
                                         where ((threshold, _), _, _) = last hList 
                     put -- $ trace ("State: " ++ (show $ readableEntry st'')) $ 
                         st''
                     

                     if isComplete st''
                     then return (take i (map (\((k, _), x, _) -> (k, x)) st''))
                     else findBest' i s
          where
            isComplete :: [StateEntry] -> Bool
            isComplete cit = and $ map (\(_, (s:_), _) -> s == '_') cit


-- operations to do to get from string in list to s0
-- first is radius
findNeighbourhood :: Int -> String -> Trie -> M.Map Int [String]
findNeighbourhood r0 s0 t0 = findNeighbourhood' r0 "" t0 (zip ('#':s0) [0..length s0]) 
  where
    -- first  : radius,
    -- second : current string,
    -- third  : length of current string,
    -- forth  : assoc list with substrings (chars) and min edt dists 
    findNeighbourhood' :: Int -> String -> Trie -> [(Char,Int)] -> M.Map Int [String]
    findNeighbourhood' r1 s1 (Node m0) ci0 = M.foldr' (M.unionWith (++)) M.empty (M.mapWithKey go m0)
      where
        go :: Char -> Trie -> M.Map Int [String] -- recursion function
        go '_' _ = if d0 > r1 then
                     M.empty -- return nothing (matching is out of threshold r1)
                   else
                     M.singleton (snd (last ci0)) [s1] -- return current string with minimum edit distance ('_' is endChar)
          where d0 = snd (last ci0) -- current minimum edit distance
        go c0 t1 = if h0>r1 then
                     M.empty
                   else
                      findNeighbourhood' r1 (s1++[c0]) t1 ci1
          where (h0,ci1) = update c0 (0,ci0) -- updated assoc list
    

