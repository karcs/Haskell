module Main where


import qualified Data.Map as Map
import System.Environment (getArgs)

data Trie = Node (Map.Map Char Trie) deriving Show

main :: IO()
main = do
  args <- getArgs
  case args of
    ("hello": _) -> hello
    ["lineRev"] -> lineRev
    ("trieTest" : _) -> trie -- Just reads the contents of given file and converts it to a trie.
    ("minEdit" : _) -> minedit
    ("cmdArgs" : _) -> cmdArgs
    _ -> error "Sorry, I do not understand."

hello :: IO()
hello = do
  args <- fmap tail getArgs
  putStrLn $ "Hello " ++ unwords args ++ "."

lineRev :: IO()
lineRev = interact $ unlines . map (unwords . reverse . words) . lines

trie :: IO()
trie = do
  args <- getArgs
  file <- readFile (args!!1)
  putStrLn $ "Trie " ++ (args!!1) ++ "."
  putStrLn $ show $ Map.size $ getMap $ makeTrie $ lines file
      where getMap (Node m) = m

minedit :: IO()
minedit = do
  args <- getArgs
  file <- readFile (args!!1)
  putStrLn $ show $ (map fst (findNeighbourhood (args !! 2) (makeTrie $ lines file)))

cmdArgs :: IO()
cmdArgs = do
  args <- getArgs
  putStr $ show args



addToTrie :: String -> Trie -> Trie
addToTrie ""      (Node m) = Node (Map.insert endChar (Node Map.empty) m)
  where
    endChar :: Char
    endChar = '_'

addToTrie (c : s) (Node m) = t'
  where
    t' = if Map.member c m
         then Node (Map.adjust (const (addToTrie s (m Map.! c))) c m) 
         else Node (Map.insert c (addToTrie s (Node Map.empty)) m)
      
makeTrie :: [String] -> Trie
makeTrie ss = foldr addToTrie (Node Map.empty) ss

findNeighbourhood :: String -> Trie -> [(String,Int)]
findNeighbourhood s t = findNeighbourhood' "#" t (zip ('#':s) [0..length s]) 
  where
    findNeighbourhood' :: String ->  Trie -> [(Char, Int)] -> [(String, Int)]
    findNeighbourhood' s (Node m) si = Map.fold (++) [] (Map.mapWithKey (\c t -> findNeighbourhood' (s ++ [c]) t (update c si)) m)

        --Name subject to change
update :: Char -> [(Char, Int)] -> [(Char, Int)]
update c ((n, i) : si) = update' 0 c si
update' :: Int -> Char -> [(Char, Int)] -> [(Char, Int)]
update' q c ((n, i) : (n2, i2) : si)  = ((n, i') : (update' i' c ((n2, i2) : si)))
  where
    i' = if n == '#'
         then 0
         else minimum
              [ (delcost n) + q
              , (subcost n c) + i
              , (inscost n2) + i2 ]
update' _ _ _ = []

delcost _ = 1
subcost c1 c2 = if c1 == c2 then 0 else 1
inscost _ = 1
            

-- diff :: Eq a => [a] -> [a] -> (Int, [Edit a])
-- diff s t = (cost, reverse edits) -- edits are build back to front... so they need to be reversed
--     where
--       n            = length t
--       m            = length s
--       inscost      = 1                              -- cost for insertion
--       delcost      = 1                              --          deletion
--       subcost :: Eq a => a -> a -> Int              --          substitution (depending on the two letters)
--       subcost x y  = if x == y then 0 else n + m    -- if they differ choose a number larger then maximum possible
      
--       nm = (n + 1) * (m + 1) -- just an abbreviation for the next two definitions

--       -- equals [0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3] for n = 3, m = 4
--       listn = take nm $ cycle $ take (n+1) [0 ..]
--       -- equals [0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4] for n = 3, m = 4
--       listm = take nm $ concat $ map (\x -> take (n+1) (repeat x)) [0 ..]
--       listnm = zip listn listm

--       index k l = l * (n + 1) + k

--       -- The following function memoizes the outcomes of distance. For this it maps the distance onto 
--       -- the previously defined list of all pairs and picks the right value.
--       memodistance = \k l -> (list !! (index k l))
-- 	  where 
--             list = map (\(a, b) -> distance a b) listnm
--             -- distance :: Int -> Int -> (Int, [Edit a])
--             -- Interestingly the program does not compile then provided the type above, since the function
--             -- makes use of the parameters s and t. Is there a way to explicitly state the type description
--             -- which depends on the type of the function, whose where clause the function is in?

--             -- On to this function: It implements the min-edit-distance function/matrix pretty straightforward as
--             -- described in the paper/textbook. The difference is, that this version appends the used edit in front
--             -- of an accumulating list of edits.
--             distance 0 0 = (0, [])
--             distance i 0 = (c + inscost, Insertion (t !! (i-1)) : ed)
--                 where (c, ed ) = memodistance (i-1) 0
--             distance 0 j = (c + delcost, Deletion (s !! (j-1)) : ed)
--                 where (c, ed ) = memodistance 0 (j-1)
--             distance i j = minimumBy (\x y -> compare (fst x) (fst y))
--                            [ (c1 + inscost, (Insertion (t !! (i-1)) : ed1))
--                            , (c2 + subcost (s !! (j-1)) (t !! (i-1)), (Substitution (s !! (j-1))) : ed2)
--                            , (c3 + delcost, (Deletion (s !! (j-1))) : ed3)
--                            ]
--                 where 
--                   (c1, ed1) = memodistance (i-1) j
--                   (c2, ed2) = memodistance (i-1) (j-1)
--                   (c3, ed3) = memodistance i (j-1)
      
--       (cost, edits) = memodistance n m
