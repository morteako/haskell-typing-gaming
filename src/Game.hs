{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Game (runGame) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List (isPrefixOf)
import Data.Maybe (isJust, mapMaybe)
import GameState
import Language.Haskell.Ghcid (Ghci)

import Control.Applicative (Alternative)
import Control.Monad.State (
    MonadState,
    StateT (StateT),
    execStateT,
 )
import Data.Bool
import Debug.Trace
import Language.Haskell.Ghcid (Ghci, exec)

newtype Game a = Game {getGame :: ExceptT () (StateT GameState (ReaderT Ghci IO)) a}
    deriving newtype (Functor, Applicative, Monad, Alternative, MonadReader Ghci, MonadIO, MonadState GameState, MonadError ())

class Monad m => GhciSession m where
    execute :: String -> m [String]

class Monad m => InputOutput m where
    readLine :: m String
    printIO :: Show s => s -> m ()
    putStrLnIO :: String -> m ()
    putStrIO :: String -> m ()

instance InputOutput Game where
    readLine = liftIO getLine
    printIO = liftIO . print
    putStrLnIO = liftIO . putStrLn
    putStrIO = liftIO . putStr

instance GhciSession Game where
    execute s = do
        ghci <- ask
        liftIO $ exec ghci s

runGame :: Term -> [Term] -> Ghci -> IO GameState
runGame t ts ghci = runReaderT (execStateT (runExceptT $ getGame mainLoopCatch) gameState) ghci
  where
    gameState = GameState{_scores = [], _term = t, _allTerms = ts, _guessScore = Unguessed 5}

mainLoopCatch :: Game ()
mainLoopCatch = do
    catchError mainLoop $ \() -> do
        putStrLnIO "Game done"
        gameState <- get
        putStrIO "Final score :"
        printIO $ gameState ^. totalScore

mainLoop :: Game ()
mainLoop = do
    gameState <- get
    printIO (gameState ^. guessScore)
    curTerm <- use term
    curGuessScore <- use guessScore
    let context = findContextHint (curTerm ^. termType) curGuessScore
    printPrompt context gameState
    inp <- parseInput <$> liftIO getLine
    case inp of
        Skip -> do
            resetTerm
            void resetGuessScore
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
        oldGuessScore <- resetGuessScore
        scores %= cons (toScore oldGuessScore)
        resetTerm
        void resetGuessScore
        mainLoop
    actionTypeCheckResult Specialized = do
        curGuess <- use guessScore
        updateDecreaseScore
        let mayFirstPartialllyGuess = curGuess ^? _Unguessed . re _Partially
        let updatePartialGuess partialGuess = do
                guessScore .= partialGuess
                scores %= cons (toScore partialGuess)
        let toNumOfSpecializedGuesses = maybe MultipleSpecialized (const FirstSpecialized)
        betterGuess <- toNumOfSpecializedGuesses <$> traverse updatePartialGuess mayFirstPartialllyGuess
        s <- use currentGuessScore
        putStrLnIO $ getSpecializedGuessOutput s betterGuess
        mainLoop
    actionTypeCheckResult Incorrect = do
        putStrLnIO "Incorrect!"
        guessStatus <- updateDecreaseScore
        when (guessStatus == WasLastChance) $ do
            oldTerm <- use term
            putStrLnIO "You did not manage to guess the correct type :("
            putStrLnIO "The correct type was : "
            putStrLnIO $ prettyTerm oldTerm
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

updateDecreaseScore :: (MonadState GameState m, MonadError () m) => m GuessStatus
updateDecreaseScore = do
    ns <- traceShow "HEEEI" use (to decGuessScore)
    case traceShowId ns of
        Just newStateWithDecreasedScore -> do
            traceShow "huuust" put newStateWithDecreasedScore
            pure MoreGuessesLeft
        Nothing -> do
            gs <- use guessScore
            traceShow gs resetTerm
            traceShow "nothing" resetGuessScore
            pure WasLastChance

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

data GuessStatus = WasLastChance | MoreGuessesLeft deriving (Eq)

data NumOfSpecializedGuesses = FirstSpecialized | MultipleSpecialized deriving (Eq)

getSpecializedGuessOutput :: (Show a, Num a) => a -> NumOfSpecializedGuesses -> String
getSpecializedGuessOutput s FirstSpecialized = "Partially correct, but not the most general type! +" ++ show s
getSpecializedGuessOutput s MultipleSpecialized = "Still not the most general type!"

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