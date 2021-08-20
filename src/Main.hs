{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import App
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.List (isPrefixOf)
import Data.List.Split
import Data.Maybe (isJust, mapMaybe)
import GHC.IO (unsafePerformIO)
import GHC.IO.Handle
import GHC.IO.Handle.FD (stdin, stdout)
import Language.Haskell.Ghcid
import System.Random (randomRIO)
import System.Random.Shuffle (shuffleM)
import Term

data Input = Skip | Quit | Blank | Guess String
  deriving (Show, Eq)

data TypeCheckResult = Incorrect | Specialized | MostGeneral
  deriving (Show, Eq)

parseInput :: String -> Input
parseInput "" = Blank
parseInput "-skip" = Skip
parseInput "-quit" = Quit
parseInput "-q" = Quit
parseInput g = Guess g

--todo:shuffle list instead
getRandomTerm :: MonadIO m => [Term] -> m Term
getRandomTerm terms = do
  i <- randomRIO (0, length terms - 1)
  return $ terms !! i

parens :: String -> String
parens s = "(" ++ s ++ ")"

(*::) :: String -> String -> String
term *:: type' = term ++ " :: " ++ type'

checkGuess :: String -> App TypeCheckResult
checkGuess "-g" = pure MostGeneral
checkGuess "-s" = pure Specialized
checkGuess g = do
  Term {_name, _termType} <- use term
  let guessInput = ":t " ++ parens (_name *:: g)
  let isValidGuess ghciAnswer = ("(" ++ _name) `isPrefixOf` concat ghciAnswer
  mostGeneralGuess <- isValidGuess <$> execute (guessInput *:: _termType)
  specializedGuess <- isValidGuess <$> execute guessInput
  pure $ toTypeCheckResult mostGeneralGuess specializedGuess
  where
    toTypeCheckResult True _ = MostGeneral
    toTypeCheckResult _ True = Specialized
    toTypeCheckResult _ False = Incorrect

-- getContextString :: GameState -> String
-- getContextString gameState = do
--   if gameState ^. guessScore <= 7
--     then gameState ^. term . context ++ " => "
--     else ""

printPrompt :: App ()
printPrompt = do
  gameState <- get
  putStrLnIO $ "Score: " ++ show (getTotalScore gameState) ++ ". Guesses left : " ++ gameState ^. (guessScore . getGuessScore . to show)
  putStrIO $ gameState ^. term . name ++ " :: "

mainLoop :: App ()
mainLoop = do
  printPrompt
  inp <- parseInput <$> liftIO getLine
  case inp of
    Skip -> do
      update UpdateSkipOrNoMoreGuesses
      mainLoop
    Quit -> guard False
    Blank -> mainLoop
    Guess g -> do
      res <- checkGuess g
      actionTypeCheckResult res
  where
    actionTypeCheckResult MostGeneral = do
      s <- use currentGuessScore
      putStrLnIO $ "Completely correct! +" ++ show s
      update MostGenGuess
      mainLoop
    actionTypeCheckResult Specialized = do
      betterGuess <- update SpecializedGuess
      s <- use currentGuessScore
      if betterGuess
        then putStrLnIO $ "Partially correct, but not the most general type! +" ++ show s
        else putStrLnIO $ "Still not the most general type!"
      mainLoop
    actionTypeCheckResult Incorrect = do
      putStrLnIO "Incorrect!"
      update DecreaseScore
      mainLoop

data UpdateReason a where
  DecreaseScore :: UpdateReason ()
  UpdateSkipOrNoMoreGuesses :: UpdateReason ()
  SpecializedGuess :: UpdateReason Bool
  MostGenGuess :: UpdateReason ()

update :: GhciWithState m => UpdateReason a -> m a
update DecreaseScore = do
  ns <- use (to decGuessScore)
  case ns of
    Just newState' -> put newState'
    Nothing -> do
      resetTerm
      void resetGuessScore
update UpdateSkipOrNoMoreGuesses = do
  resetTerm
  void resetGuessScore
update SpecializedGuess = do
  mayGuess <- preuse (guessScore . _partialGuess)
  let f partialGuess = do
        s <- get
        guessScore .= partialGuess
        scores %= cons (toScore partialGuess)
  betterGuess <- isJust <$> traverse f mayGuess
  update DecreaseScore
  pure betterGuess
update MostGenGuess = do
  oldGuessScore <- resetGuessScore
  scores %= cons (toScore oldGuessScore)
  resetTerm
  void resetGuessScore

resetTerm :: MonadState GameState m => m ()
resetTerm = do
  newTerm <- use (allTerms . to head)
  term .= newTerm
  allTerms %= tail

resetGuessScore :: MonadState GameState m => m GuessScore
resetGuessScore = guessScore <<.= Unguessed 5

modifyMaybe :: MonadState s m => (s -> Maybe s) -> m ()
modifyMaybe f = do
  a <- get
  case f a of
    Nothing -> pure ()
    Just b -> put b

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  (ghci, _) <- startGhci "ghci" (Just ".") (\_ s -> print s)
  exec ghci "import Data.List"
  ls <- exec ghci ":browse Data.List"
  terms <- shuffleM $ parseBrowse ls
  execApp terms ghci mainLoop
  stopGhci ghci
