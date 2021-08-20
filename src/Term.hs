{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Term where

import Control.Lens
import GHC.Natural (Natural)

data Term = Term
  { _name :: String,
    _context :: String,
    _termType :: String
  }
  deriving (Show, Eq)

makeLenses ''Term

data GuessScore = Unguessed {_getGuessScore :: Natural} | Partially {_getGuessScore :: Natural} deriving (Show, Eq)

toScore (Unguessed s) = s + 5
toScore (Partially s) = s

makeLenses ''GuessScore
makePrisms ''GuessScore

_partialGuess :: Prism' GuessScore GuessScore
_partialGuess = prism' id f
  where
    f (Unguessed s) = Just (Partially s)
    f _ = Nothing

data GameState = GameState
  { _scores :: [Natural],
    _allTerms :: [Term],
    _term :: Term,
    _guessScore :: GuessScore
  }
  deriving (Show, Eq)

makeLenses ''GameState

currentGuessScore :: Getting Natural GameState Natural
currentGuessScore = guessScore . to toScore

totalScore :: Getting Natural GameState Natural
totalScore = scores . to sum

getTotalScore :: GameState -> Natural
getTotalScore = foldOf (scores . to sum)

data StateChange = NewTerm | GuessedPartially deriving (Show, Eq)

newState :: StateChange -> GameState -> GameState
newState stateChange gameState@GameState {_scores, _term, _allTerms, _guessScore} =
  case stateChange of
    NewTerm ->
      gameState
        { _scores = toScore _guessScore : _scores,
          _term = head _allTerms,
          _allTerms = tail _allTerms,
          _guessScore = Unguessed 5
        }
    GuessedPartially ->
      gameState
        { _scores = toScore _guessScore : _scores,
          _guessScore = Partially $ _getGuessScore _guessScore
        }

predNatural :: Natural -> Maybe Natural
predNatural 1 = Nothing
predNatural x = Just $ pred x

decGuessScore :: GameState -> Maybe GameState
decGuessScore = traverseOf (guessScore . getGuessScore) predNatural