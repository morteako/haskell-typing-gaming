{-# LANGUAGE NamedFieldPuns #-}

module Main where

import App
import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Language.Haskell.Ghcid
import Term

data Input = Skip | Quit | Blank | Guess String
  deriving (Show, Eq)

parseInput :: String -> Input
parseInput "" = Blank
parseInput "-s" = Skip
parseInput "-q" = Quit
parseInput g = Guess g

getRandomTerm :: IO Term
getRandomTerm = return $ Term {_name = "1", _context = "Num p", _termType = "p"}

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
  get >>= printIO
  printPrompt
  inp <- parseInput <$> liftIO getLine
  case inp of
    Skip -> mainLoop
    Quit -> guard False
    Blank -> mainLoop
    Guess g -> do
      res <- checkGuess g
      case res of
        True -> do
          putStrLnIO "Correct!"
          modify newState
          mainLoop
        False -> do
          modify decGuessScore
          mainLoop

main :: IO ()
main = do
  (ghci, _) <- startGhci "ghci" (Just ".") (\q s -> print q >> print s)
  -- executeStatement ":browse Data.List" >>= mapM print
  -- stopGhci ghci
  t <- getRandomTerm
  execApp t ghci mainLoop
  stopGhci ghci
