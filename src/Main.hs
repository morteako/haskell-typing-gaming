{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import App
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List (isPrefixOf)
import Data.Maybe (isJust, mapMaybe)
import GHC.IO (unsafePerformIO)
import GHC.IO.Handle
import GHC.IO.Handle.FD (stdin, stdout)
import Language.Haskell.Ghcid
import Parse (parseBrowse)
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
  printIO (gameState ^. allTerms)
  let scorePromp = "Score: " ++ show (getTotalScore gameState)
  let guessPrompt = ". Guesses left : " ++ gameState ^. (guessScore . getGuessScore . to show)
  let termsLeftPrompt = ". Terms left : " ++ show (gameState ^. allTerms . to length + 1)
  putStrLnIO $ scorePromp ++ guessPrompt ++ termsLeftPrompt
  putStrIO $ gameState ^. term . name ++ " :: "

mainLoopCatch :: App ()
mainLoopCatch = do
  catchError mainLoop $ \e -> do
    printIO "DONE"
    gameState <- get
    printIO $ gameState ^. totalScore

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

resetTerm :: GhciWithState m => m ()
resetTerm = do
  newTerm <- preuse (allTerms . _head)
  case newTerm of
    Nothing -> throwError "oo"
    Just newTerm -> do
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
  let limit = 10
  execApp (take limit terms) ghci mainLoopCatch
  stopGhci ghci
