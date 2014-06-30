module Main where

import Trie
import MinEdit
import System.IO
import System.Environment (getArgs)
import System.Console.ANSI
import Data.List
import Data.Char (ord)
import Debug.Trace
import qualified Data.Map as M


main :: IO()
main = do
  args <- getArgs
  case args of
    ["--minEdit" , _, _, _] -> minedit     -- Gives correction proposals within a given distance. 
    ["--minEdits", _, _, _] -> minedits    -- Gives correction proposals within a given distance for a whole text.
    ["--findBest", _, _, _] -> findbest    -- Gives the n nearst proposals to a given word.
    ["--help"             ] -> help        -- Print help.
    ["--corrText", _, _   ] -> correctText -- Corrects the given file "text" and writes in "corrected_text" all changes made.
    ["--dumbCorr", _, _   ] -> useBest     -- Corrects the given file "text" by choosing the best proposal.
    ["--showWrong",   _, _] -> showWrong   -- Shows all words not contained in the dictionary.
    _ -> error "Sorry, I do not understand."

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

correctText :: IO ()
correctText = do
  args <- getArgs
  dict <- readFile (args !! 1)
  text <- readFile (args !! 2)
  let t                 = makeTrie $ lines dict
      wordList          = words text
 
  corrWords <- correctWords wordList t
  writeFile ("corrected_" ++ (args !! 2)) corrWords
  clearScreen
    where
      correctWords :: [String] -> Trie -> IO String
      correctWords [] _ = do
                return []
      correctWords (w : ws) t  = do
                if M.null $ findNeighbourhood 0 w t
                then do
                  clearScreen
                  hSetBuffering stdin NoBuffering
                  setSGR [SetColor Foreground Vivid Blue]
                  putStr $ (++) w " "
                  setSGR [SetColor Foreground Vivid White]
                  putStr $ intercalate " "  $ take 10 ws
                  putStrLn ""
                  let prop = findBest 5 w t
                  setSGR [SetColor Foreground Vivid White]
                  putStr $ concat $ snd $ mapAccumL 
                     (\acc x -> (acc + 1, (show acc) ++ ": " ++ snd x ++ " ")) 1 $ prop
                  putStrLn ""
                  c <- getChar
                  if ord c >= 49 && ord c < 49 + length prop
                  then do
                    a <- correctWords ws t
                    return $ (++) (' ' : (snd (prop !! (ord c - 49)))) $ a
                  else correctWords (w:ws) t
                else do
                    a <- correctWords ws t
                    return (' ' : w ++ a)

useBest :: IO ()
useBest = do 
  args <- getArgs
  dict <- readFile (args !! 1)
  text <- readFile (args !! 2)
  let t                 = makeTrie $ lines dict
      wordList          = words text
      corrWords = correctWords wordList t
  writeFile ("corrected_" ++ (args !! 2)) corrWords
    where
      correctWords :: [String] -> Trie ->String
      correctWords []      _ = []
      correctWords (w: ws) t = traceShow (w, word) (' ' : word ++ (correctWords ws t))
          where word = snd ((findBest 1 w t) !! 0)

showWrong :: IO ()
showWrong = do
  args <- getArgs
  dict <- readFile (args !! 1)
  text <- readFile (args !! 2)
  let t                 = makeTrie $ lines dict
      wordList          = words text
  putStr $ wrongWords wordList t
    where
      wrongWords :: [String] -> Trie ->String
      wrongWords []      _ = []
      wrongWords (w: ws) t = (if M.null $ findNeighbourhood 2 w t
                              then ' ' : w  
                              else "") ++ (wrongWords ws t)

help :: IO ()
help = do
  putStrLn ""
  putStrLn "knuspell MODE dict [args]"
  putStrLn ""
  putStrLn "where MODE is one of the following and \"dict\" is the filename of dictionary to use:"
  putStrLn "  --minEdit   Gives correction proposals for a given distance. args contains a single word and a distance (int)"
  putStrLn "              Example: knuspell --minEdit dict.txt foo 3"
  putStrLn "  --findBest  Lists the n nearest proposals to a given word. args contains a single word and the number of proposals"
  putStrLn "              Example: knuspell --findBest dict.txt foo 3"
  putStrLn "  --corrText  Corrects a given text in an interactive operating mode. args contains the name \"filename\" of a text file"
  putStrLn "                   containing words to check. The changes made will be written \"corrected_filename\""
  putStrLn ""
  putStrLn "  --help      Shows this text. Ba dum tish."
  putStrLn ""

