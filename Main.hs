module Main where

import Trie
import MinEdit

import System.Environment (getArgs)
import qualified Data.Map as M


main :: IO()
main = do
  args <- getArgs
  case args of
    ("trieTest" : _) -> trie -- Just reads the contents of given file and converts it to a trie.
    ("minEdit" : _) -> minedit
    ("minEdits": _) -> minedits
    ("findBest": _) -> findbest
    ("cmdArgs" : _) -> cmdArgs
    ("corrText":_) -> correctText
    _ -> error "Sorry, I do not understand."


trie :: IO()
trie = do
  args <- getArgs
  file <- readFile (args!!1)
  putStrLn $ "Trie " ++ (args!!1) ++ "."
  putStrLn $ show {-$ M.size-} $ getMap $ makeTrie $ lines file
      where getMap (Node m) = m

minedit :: IO()
minedit = do
  args <- getArgs
  file <- readFile (args!!1)
  let rad = read (args !! 3) :: Int
      word = args !! 2
  putStrLn $ show $ findNeighbourhood rad word (makeTrie $ lines file)

minedits :: IO()
minedits = do
  args <- getArgs
  dict <- readFile (args !! 1)
  text <- readFile (args !! 2)
  putStrLn $ show $ map ((flip (findNeighbourhood 4)) (makeTrie $ lines dict)) (lines text) 

findbest :: IO()
findbest = do
  args <- getArgs
  dict <- readFile (args !! 1)
  let s = args !! 2
  putStrLn $ show $ findBest (read (args !! 3) :: Int) s (makeTrie $ lines dict)

correctText :: IO()            
correctText = do
  args <- getArgs
  dict <- readFile (args !! 1)
  text <- readFile (args !! 2)
  putStrLn "You want me to correct the following text:"
  putStrLn text
  let rad = read (args !! 3) :: Int
      tree = makeTrie $ lines dict
  correctWords (words text) rad tree 

               
correctWords :: [String] -> Int -> Trie -> IO()
correctWords w0 r0 t0 = do
  case w0 of
    [] -> return ()
    (w1:w1s) -> correctWord w1 r0 t0 >> correctWords w1s r0 t0

correctWord :: String -> Int ->  Trie -> IO()
correctWord s0 r0 t0 = do
  messCorrPoss r0 s0 l0
    where l0 = findNeighbourhood r0 s0 t0

messCorrPoss :: Int -> String -> M.Map Int [String] -> IO()
messCorrPoss r0 s0 m0 =
  case M.size m0 of
    0 -> putStrLn $ "The word '" ++ s0 ++ "' you typed is most likely incorrect or not known to me. If it is incorrect there are >"++(show r0) ++ " mistakes in it." ++ "\n" -- first case --- no correction possible since out of threshold
    _ -> if M.member 0 m0
                 then return ()
                 else putStrLn $ "My suggestions are: " ++ unwords (M.foldr' (++) [] m0) ++ "\n"

  
cmdArgs :: IO()
cmdArgs = do
  args <- getArgs
  putStr $ show args
