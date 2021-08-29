{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import App
import Args
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.List (isPrefixOf)
import Data.Maybe (isJust, mapMaybe)
import GHC.IO (unsafePerformIO)
import GHC.IO.Handle
import GHC.IO.Handle.FD (stdin, stdout)
import Language.Haskell.Ghcid
import Options.Applicative
import Parse (groupTerms, parseBrowse)
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

checkGuess :: GhciSession m => Term -> String -> m TypeCheckResult
checkGuess _ "-g" = pure MostGeneral
checkGuess _ "-s" = pure Specialized
checkGuess term@Term{_name} g = do
  let guessInput = ":t " ++ parens (_name *:: g)
  let isValidGuess ghciAnswer = ("(" ++ _name) `isPrefixOf` concat ghciAnswer
  mostGeneralGuess <- isValidGuess <$> execute (guessInput *:: prettyTermtype term)
  specializedGuess <- isValidGuess <$> execute guessInput
  pure $ toTypeCheckResult mostGeneralGuess specializedGuess
 where
  toTypeCheckResult True _ = MostGeneral
  toTypeCheckResult _ True = Specialized
  toTypeCheckResult _ False = Incorrect

--fix prints
printPrompt :: InputOutput m => GameState -> m ()
printPrompt gameState = do
  printIO (gameState ^. allTerms)
  let scorePromp = "Score: " ++ show (getTotalScore gameState)
  let guessPrompt = ". Guesses left : " ++ gameState ^. guessScore . getGuessScore . to show
  let termsLeftPrompt = ". Terms left : " ++ gameState ^. allTerms . to length . to succ . to show
  putStrLnIO $ scorePromp ++ guessPrompt ++ termsLeftPrompt
  putStrIO $ gameState ^. term . name ++ " :: "

mainLoopCatch :: App ()
mainLoopCatch = do
  catchError mainLoop $ \_ -> do
    printIO "DONE"
    gameState <- get
    printIO $ gameState ^. totalScore

mainLoop :: App ()
mainLoop = do
  gameState <- get
  printPrompt gameState
  inp <- parseInput <$> liftIO getLine
  case inp of
    Skip -> do
      update UpdateSkipOrNoMoreGuesses
      mainLoop
    Quit -> guard False
    Blank -> mainLoop
    Guess g -> do
      t <- use term
      res <- checkGuess t g
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

update :: (MonadState GameState m, MonadError String m) => UpdateReason a -> m a
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

resetTerm :: (MonadState GameState m, MonadError String m) => m ()
resetTerm = do
  newTerm <- preuse (allTerms . _head)
  case newTerm of
    Nothing -> throwError "oo"
    Just newTerm -> do
      term .= newTerm
      allTerms %= tail

resetGuessScore :: MonadState GameState m => m GuessScore
resetGuessScore = guessScore <<.= Unguessed 5

main :: IO ()
main = do
  Args{numQuestions, difficulty} <- execArgsParser
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  (ghci, _) <- startGhci "ghci" (Just ".") (\_ s -> print s)
  let moduleWithTerms = getModule difficulty
  exec ghci $ "import " ++ moduleWithTerms
  ls <- exec ghci $ ":browse " ++ moduleWithTerms
  -- mapM print $ groupTerms ls
  terms <- shuffleM $ parseBrowse ls
  case take numQuestions terms of
    [] ->
      putStrLn "Error todo"
    (startTerm : restOfTerms) ->
      void $ execApp startTerm restOfTerms ghci mainLoopCatch
  stopGhci ghci
