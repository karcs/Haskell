-- | Main module, contains all things IO.
module Main where

import Trie
import MinEdit
import System.IO
import System.Environment (getArgs)
import System.Console.ANSI
import Data.List
import Data.Char (ord)
import qualified Data.Map as M

-- | Entry point of the program.
main :: IO()
main = do
  args <- getArgs
  case args of
    "--help" : _ -> help        -- Print help.
    "--findWithin" : args' -> if l == 3 then
                                findWithin    -- Gives correction proposals within a given distance.
                            else
                                errorWrongNumberOfArguments 3 l
                                where l = length args'
    "--findBest" : args' -> if l == 3 then
                                findbest      -- Gives correction proposals within a given distance.
                            else
                                errorWrongNumberOfArguments 3 l
                                where l = length args'
    "--corrText" : args' -> if l == 2 then
                                correctText -- Corrects the given file "text" and writes in "corrected_text" all changes made.
                            else
                                errorWrongNumberOfArguments 2 l
                                where l = length args'
    "--dumbCorr" : args' -> if l == 2 then
                                dumbCorr    -- Corrects the given file "text" by choosing the best proposal.
                            else
                                errorWrongNumberOfArguments 2 l
                                where l = length args'
    "--showWrong" : args' -> if l == 2 then
                                      showWrong   -- Shows all words not contained in the dictionary.proposal.
                            else
                                errorWrongNumberOfArguments 2 l
                                where l = length args'
    _ -> help

-- | Gives correction proposals within a given distance. 
findWithin :: IO()
findWithin = do
  args <- getArgs
  dict <- readFile (args!!1)
  let rad = read (args !! 3) :: Int
      word = args !! 2
      props = findNeighbourhood rad word (makeTrie $ lines dict)
  showProposals props

-- | Shows proposals
showProposals :: M.Map Int [String] -> IO ()
showProposals = M.foldlWithKey f (return ())
    where f a k b = do
            a
            if k == 0 then
                putStrLn $ "Congratulations. This word is in our dictionary."
            else
                putStrLn $ "Words with minimum edit distance " ++ show k ++ " are the following:"
            foldl g (return ()) b 
                where g c d = do
                        c
                        putStrLn d
                  
-- | Gives the n nearst proposals to a given word.
findbest :: IO()
findbest = do
  args <- getArgs
  dict <- readFile (args !! 1)
  let word = args !! 2
      idx = read (args !! 3) :: Int
      trie = makeTrie $ lines dict
      props = foldr (uncurry $ M.insertWith (++)) M.empty $ map (\(a,b) -> (a,[b])) $ findBest idx word trie
  showProposals props

-- | Corrects the given file "text" and writes in "corrected_text" all changes made.
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
                  putStr $ intercalate " "  $ take 20 ws
                  putStrLn ""
                  let prop = (0,w ++ " (leave uncorrected)") : (take 10 $ zip [1..] (M.foldl (++) [] (findNeighbourhood ((length w) `div` 3) w t))) -- findBest 5 w t
                  setSGR [SetColor Foreground Vivid White]
                  putStr $ concat $ snd $ mapAccumL 
                     (\acc x -> (acc + 1, (show acc) ++ ": " ++ snd x ++ " ")) 1 $ prop
                  putStrLn ""
                  c <- getChar
                  let idx = ord c - 49
                  if idx > 0 && idx < length prop -- first entry of proposals 'prop' does not change anything
                  then do
                    a <- correctWords ws t
                    return $ (++) (' ' : (snd (prop !! idx))) $ a
                  else if idx == 0 then do
                           a <- correctWords ws t
                           return $ (++) (' ' : w) $ a
                       else correctWords (w:ws) t
                else do
                    a <- correctWords ws t
                    return (' ' : w ++ a)

-- | Corrects the given file "text" by choosing the best proposal.
dumbCorr :: IO ()
dumbCorr = do 
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
      correctWords (w: ws) t = (' ' : word ++ (correctWords ws t))
          where word = snd ((findBest 1 w t) !! 0)

-- | Shows all words not contained in the dictionary.
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

-- | Wrong number of arguments
errorWrongNumberOfArguments :: Int -> Int -> IO ()
errorWrongNumberOfArguments a b =
  error $ "Wrong number of arguments. The number of expected arguments was " ++ show a ++ " but you entered " ++ show b ++ "."
                             
-- | Print help.
help :: IO ()
help = do
  putStrLn ""
  putStrLn "knuspell <mode> <dict> <args>"
  putStrLn ""
  putStrLn "where <mode> is one of the modi described below, <dict> is the filename of dictionary and <args> is a space-separated"
  putStrLn "list of arguments. The dictionary has to be a newline separated list of words, whereas the text to be corrected has to"
  putStrLn "be a whitespace separated file of words."
  putStrLn "<mode> has to be one of the following modi:"
  putStrLn "  --findWithin Gives correction proposals for a given distance. <args> contains a single word and a distance"
  putStrLn "               Example: knuspell --findWithin dict.txt foo 3"
  putStrLn "  --findBest   Lists the n nearest proposals to a given word. args contains a single word and the number of proposals"
  putStrLn "               Example: knuspell --findBest dict.txt foo 3"
  putStrLn "  --corrText   Corrects a given text in an interactive operating mode. <args> contains the name <filename> of a text file"
  putStrLn "               containing words to check. The changes made will be written corrected_$<filename>"
  putStrLn "               Example: knuspell --dumbCorr dict text"
  putStrLn "  --dumbCorr   Corrects a given text by substituting every word with its nearest neighbour in the dictionary."
  putStrLn "               Example: knuspell --dumbCorr dict text"
  putStrLn "  --help       Shows this text."

