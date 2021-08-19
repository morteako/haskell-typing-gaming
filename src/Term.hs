{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Term where

import Control.Lens
import GHC.Natural

data Term = Term
  { _name :: String,
    _context :: String,
    _termType :: String
  }
  deriving (Show, Eq)

makeLenses ''Term

data GameState = GameState
  { _scores :: [Natural],
    _allTerms :: [Term],
    _term :: Term,
    _guessScore :: Natural
  }
  deriving (Show, Eq)

makeLenses ''GameState

totalScore :: Getting Natural GameState Natural
totalScore = scores . to sum

getTotalScore :: GameState -> Natural
getTotalScore = foldOf (scores . to sum)

newState :: Term -> GameState -> GameState
newState newTerm GameState {_scores, _allTerms, _guessScore} =
  GameState {_scores = _guessScore : _scores, _term = newTerm, _allTerms, _guessScore = 10}

decGuessScore :: GameState -> GameState
decGuessScore = over guessScore pred