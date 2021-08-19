{-# LANGUAGE NamedFieldPuns #-}

module Main where

import App
import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.List.Split
import Language.Haskell.Ghcid
import System.Random (randomRIO)
import Term

data Input = Skip | Quit | Blank | Guess String
  deriving (Show, Eq)

parseInput :: String -> Input
parseInput "" = Blank
parseInput "-s" = Skip
parseInput "-q" = Quit
parseInput g = Guess g

getRandomTerm :: App Term
getRandomTerm = do
  terms <- use allTerms
  i <- randomRIO (0, length terms - 1)
  return $ terms !! i

checkGuess :: String -> App Bool
checkGuess g = do
  Term {_name} <- use term
  res <- execute (":t " ++ _name ++ " :: " ++ g)
  putStrLnIO $ concat res
  pure $ _name `isPrefixOf` concat res

getContextString :: GameState -> String
getContextString gameState = do
  if gameState ^. guessScore <= 7
    then gameState ^. term . context ++ " => "
    else ""

printPrompt :: App ()
printPrompt = do
  gameState <- get
  putStrLnIO $ "Current score: " ++ show (getTotalScore gameState)
  putStrIO $ gameState ^. term . name ++ " :: " ++ getContextString gameState

mainLoop :: App ()
mainLoop = do
  printPrompt
  inp <- parseInput <$> liftIO getLine
  case inp of
    Skip -> do
      term <- getRandomTerm
      modify (newState term)
      mainLoop
    Quit -> guard False
    Blank -> mainLoop
    Guess g -> do
      res <- checkGuess g
      case res of
        True -> do
          putStrLnIO "Correct!"
          term <- getRandomTerm
          modify (newState term)
          mainLoop
        False -> do
          modify decGuessScore
          mainLoop

-- fix fully qualified aka remove
parseBrowse :: [String] -> [Term]
parseBrowse = map f . filter (not . isPrefixOf " ")
  where
    f s = let termName : _ = splitOn " ::" s in Term {_name = termName, _context = "", _termType = ""}

main :: IO ()
main = do
  (ghci, _) <- startGhci "ghci" (Just ".") (\q s -> print q >> print s)
  -- exec ghci ":browse Data.List" >>= mapM print
  exec ghci "import Data.List"
  ls <- exec ghci ":browse Data.List"
  let terms = parseBrowse ls
  -- mapM print terms
  -- stopGhci ghci
  -- t <- getRandomTerm
  execApp terms ghci mainLoop
  stopGhci ghci
