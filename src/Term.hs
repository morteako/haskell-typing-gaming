{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Term where

import Control.Lens
import Data.Maybe
import GHC.Natural

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

data GameState = GameState
  { _scores :: [Natural],
    _allTerms :: [Term],
    _term :: Term,
    _guessScore :: GuessScore
  }
  deriving (Show, Eq)

makeLenses ''GameState

totalScore :: Getting Natural GameState Natural
totalScore = scores . to sum

getTotalScore :: GameState -> Natural
getTotalScore = foldOf (scores . to sum)

data StateChange = NewTerm Term | GuessedPartially deriving (Show, Eq)

newState :: StateChange -> GameState -> GameState
newState stateChange gameState@GameState {_scores, _term, _allTerms, _guessScore} =
  case stateChange of
    NewTerm newTerm ->
      gameState
        { _scores = toScore _guessScore : _scores,
          _term = newTerm,
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

decGuessScore :: GameState -> GameState
decGuessScore = over (guessScore . getGuessScore) predNatural