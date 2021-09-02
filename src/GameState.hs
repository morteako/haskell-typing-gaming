{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Control.Lens
import Data.Data
import GHC.Natural (Natural)
import Language.Haskell.Exts

data Term = Term
  { _name :: String
  , _termType :: Type ()
  }
  deriving (Show, Eq, Data)

instance Plated Term

makeLenses ''Term

prettyTerm :: Term -> String
prettyTerm Term{_name, _termType} = _name ++ " :: " ++ prettyPrint _termType

prettyTermtype :: Term -> String
prettyTermtype = foldOf (termType . to prettyPrint)

-- _partialGuess :: Prism' GuessScore GuessScore
-- _partialGuess = prism' id f
--  where
--   f (Unguessed s) = Just (Partially s)
--   f _ = Nothing

data GameState = GameState
  { _scores :: [Natural]
  , _allTerms :: [Term]
  , _term :: Term
  , _guessScore :: GuessScore
  }
  deriving (Show, Eq)
data GuessScore = Unguessed {_getGuessScore :: Natural} | Partially {_getGuessScore :: Natural} deriving (Show, Eq)

makeLenses ''GameState
makeLenses ''GuessScore
makePrisms ''GuessScore

currentGuessScore :: Getting Natural GameState Natural
currentGuessScore = guessScore . to toScore

totalScore :: Getting Natural GameState Natural
totalScore = scores . to sum

getTotalScore :: GameState -> Natural
getTotalScore = foldOf (scores . to sum)

getGuessesLeft :: Lens' GameState Natural
getGuessesLeft = guessScore . getGuessScore

getTermsLeft :: Fold GameState Int
getTermsLeft = allTerms . to length . to succ

decGuessScore :: GameState -> Maybe GameState
decGuessScore = traverseOf guessScore predNatural
 where
  predNatural :: GuessScore -> Maybe GuessScore
  predNatural (Partially 1) = Nothing
  predNatural (Unguessed 1) = Just (Partially 1)
  predNatural x = Just $ over getGuessScore pred x

toScore :: GuessScore -> Natural
toScore (Unguessed s) = s + 5
toScore (Partially s) = s

findContextHint :: Type () -> GuessScore -> ContextHint
findContextHint (TyForall () _ context _) guessScore =
  case guessScore of
    Unguessed 5 -> ContextHint ""
    _ -> ContextHint $ foldMap prettyPrint context ++ " "
findContextHint _ _ = ContextHint ""

newtype ContextHint = ContextHint {getContextHint :: String}
