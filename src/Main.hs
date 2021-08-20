{-# LANGUAGE NamedFieldPuns #-}

module Main where

import App
import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.List.Split
import Data.Maybe
import GHC.IO.Handle
import GHC.IO.Handle.FD (stdin, stdout)
import GHC.Natural
import Language.Haskell.Ghcid
import System.Random (randomRIO)
import Term

data Input = Skip | Quit | Blank | Guess String
  deriving (Show, Eq)

data TypeCheckResult = Incorrect | Specialized | MostGeneral
  deriving (Show, Eq)

parseInput :: String -> Input
parseInput "" = Blank
parseInput "-skip" = Skip
parseInput "-quit" = Quit
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
  putStrLnIO $ "score: " ++ show (getTotalScore gameState) ++ ". Guess score : " ++ gameState ^. (guessScore . to toScore . to show)
  putStrIO $ gameState ^. term . name ++ " :: "

mainLoop :: App ()
mainLoop = do
  printPrompt
  inp <- parseInput <$> liftIO getLine
  case inp of
    Skip -> do
      putStrLnIO "Skipped"
      terms <- use allTerms
      newTerm <- liftIO $ getRandomTerm terms
      term .= newTerm
      mainLoop
    Quit -> guard False
    Blank -> mainLoop
    Guess g -> do
      res <- checkGuess g
      printIO res
      actionTypeCheckResult res
  where
    actionTypeCheckResult MostGeneral = do
      s <- use guessScore
      putStrLnIO $ "Completely correct! +" ++ show s
      term <- use allTerms >>= getRandomTerm
      modify $ newState (NewTerm term)
      mainLoop
    actionTypeCheckResult Specialized = do
      s <- use currentGuessScore
      putStrLnIO $ "Partially correct, ie not the most general type! +" ++ show s
      term <- use allTerms >>= getRandomTerm
      -- modify (newState term)
      mainLoop
    actionTypeCheckResult Incorrect = do
      putStrLnIO "Incorrect!"
      modifyMaybe decGuessScore
      mainLoop

data UpdateReason = DecreaseScore | UpdateSkipOrNoMoreGuesses | SpecializedGuess | MostGenGuess

update :: GhciWithState m => UpdateReason -> m Bool
update DecreaseScore = do
  ns <- use (to decGuessScore)
  case ns of
    Just newState' -> put newState' >> pure True
    Nothing -> update UpdateSkipOrNoMoreGuesses
update UpdateSkipOrNoMoreGuesses = do
  newTerm <- use allTerms >>= getRandomTerm --remove and remove IO
  term .= newTerm
  guessScore .= Unguessed 5
  pure True
update SpecializedGuess = do
  mayGuess <- preuse (guessScore . _partialGuess)
  isJust <$> traverse (guessScore .=) mayGuess
update MostGenGuess = do
  oldGuessScore <- guessScore <.= Unguessed 5
  scores %= cons (toScore oldGuessScore)
  newTerm <- use allTerms >>= getRandomTerm
  term .= newTerm
  pure True

modifyMaybe :: MonadState s m => (s -> Maybe s) -> m ()
modifyMaybe f = do
  a <- get
  case f a of
    Nothing -> pure ()
    Just b -> put b

parseType :: String -> Maybe Term
parseType str =
  case splitOn " ::" str of
    termName : typeStr : _ -> Just $ Term {_name = termName, _context = "", _termType = typeStr}
    _ -> Nothing

-- fix fully qualified aka remove
parseBrowse :: [String] -> [Term]
parseBrowse = mapMaybe parseType . filter isNormalTerm
  where
    isNormalTerm x = all ($ x) [notNewline, notTypeAlias, notTypeClass]
    notNewline = not . isPrefixOf " "
    notTypeAlias = not . isPrefixOf "type "
    notTypeClass = not . isPrefixOf "class "

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  (ghci, _) <- startGhci "ghci" (Just ".") (\_ s -> print s)
  exec ghci "import Data.List"
  ls <- exec ghci ":browse Data.List"
  let terms = parseBrowse ls
  t <- getRandomTerm terms
  execApp t terms ghci mainLoop
  stopGhci ghci
