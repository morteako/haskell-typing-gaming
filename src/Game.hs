{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Game (runGame) where

import App
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List (isPrefixOf)
import Data.Maybe (isJust, mapMaybe)
import GameState
import Language.Haskell.Ghcid (Ghci)

runGame :: Term -> [Term] -> Ghci -> IO GameState
runGame t ts ghci = runReaderT (execStateT (runExceptT $ runApp mainLoopCatch) gameState) ghci
  where
    gameState = GameState{_scores = [], _term = t, _allTerms = ts, _guessScore = Unguessed 5}

mainLoopCatch :: App ()
mainLoopCatch = do
    catchError mainLoop $ \() -> do
        putStrLnIO "Game done"
        gameState <- get
        putStrIO "Final score :"
        printIO $ gameState ^. totalScore

mainLoop :: App ()
mainLoop = do
    gameState <- get
    curTerm <- use term
    curGuessScore <- use guessScore
    let context = findContextHint (curTerm ^. termType) curGuessScore
    printPrompt context gameState
    inp <- parseInput <$> liftIO getLine
    case inp of
        Skip -> do
            updateUpdateSkipOrNoMoreGuesses
            mainLoop
        Quit -> guard False
        Blank -> mainLoop
        Guess g -> do
            res <- checkGuess curTerm g
            actionTypeCheckResult res
  where
    actionTypeCheckResult MostGeneral = do
        s <- use currentGuessScore
        putStrLnIO $ "Completely correct! +" ++ show s
        updateMostGenGuess
        mainLoop
    actionTypeCheckResult Specialized = do
        betterGuess <- updateSpecializedGuess
        s <- use currentGuessScore
        if betterGuess
            then putStrLnIO $ "Partially correct, but not the most general type! +" ++ show s
            else putStrLnIO $ "Still not the most general type!"
        mainLoop
    actionTypeCheckResult Incorrect = do
        putStrLnIO "Incorrect!"
        updateDecreaseScore
        mainLoop

checkGuess :: GhciSession m => Term -> String -> m TypeCheckResult
checkGuess _ "-g" = pure MostGeneral
checkGuess _ "-s" = pure Specialized
checkGuess term@Term{_name} guess = do
    let guessInput = ":t " ++ parens (_name *:: guess)
    let isValidGuess ghciAnswer = ("(" ++ _name) `isPrefixOf` concat ghciAnswer
    mostGeneralGuess <- isValidGuess <$> execute (guessInput *:: prettyTermtype term)
    specializedGuess <- isValidGuess <$> execute guessInput
    pure $ toTypeCheckResult mostGeneralGuess specializedGuess
  where
    toTypeCheckResult True _ = MostGeneral
    toTypeCheckResult _ True = Specialized
    toTypeCheckResult _ False = Incorrect

printPrompt :: InputOutput m => ContextHint -> GameState -> m ()
printPrompt (ContextHint contextHint) gameState = do
    let scorePromp = "Score: " ++ show (getTotalScore gameState)
    let guessPrompt = ". Guesses left : " ++ gameState ^. getGuessesLeft . to show
    let termsLeftPrompt = ". Terms left : " ++ gameState ^. getTermsLeft . to show
    putStrLnIO $ scorePromp ++ guessPrompt ++ termsLeftPrompt
    let currentTermName = gameState ^. term . name
    putStrIO $ currentTermName *:: contextHint

updateDecreaseScore :: (MonadState GameState m, MonadError () m) => m ()
updateDecreaseScore = do
    ns <- use (to decGuessScore)
    case ns of
        Just newState' -> put newState'
        Nothing -> do
            resetTerm
            void resetGuessScore

updateUpdateSkipOrNoMoreGuesses :: (MonadState GameState m, MonadError () m) => m ()
updateUpdateSkipOrNoMoreGuesses = do
    resetTerm
    void resetGuessScore

updateSpecializedGuess :: (MonadState GameState m, MonadError () m) => m Bool
updateSpecializedGuess = do
    mayGuess <- preuse (guessScore . _partialGuess)
    let f partialGuess = do
            guessScore .= partialGuess
            scores %= cons (toScore partialGuess)
    betterGuess <- isJust <$> traverse f mayGuess
    updateDecreaseScore
    pure betterGuess

updateMostGenGuess :: (MonadState GameState m, MonadError () m) => m ()
updateMostGenGuess = do
    oldGuessScore <- resetGuessScore
    scores %= cons (toScore oldGuessScore)
    resetTerm
    void resetGuessScore

resetTerm :: (MonadState GameState m, MonadError () m) => m ()
resetTerm = do
    newTerm <- preuse (allTerms . _head)
    case newTerm of
        Nothing -> throwError ()
        Just newTerm -> do
            term .= newTerm
            allTerms %= tail

resetGuessScore :: MonadState GameState m => m GuessScore
resetGuessScore = guessScore <<.= Unguessed 5

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