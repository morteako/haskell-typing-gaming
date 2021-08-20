{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module App where

import Control.Applicative (Alternative)
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
  ( MonadState,
    StateT (StateT),
    execStateT,
  )
import Language.Haskell.Ghcid (Ghci, exec)
import Term

newtype App a = App {runApp :: StateT GameState (ReaderT Ghci IO) a}
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadReader Ghci, MonadIO, MonadState GameState)

class MonadState GameState m => GhciWithState m where
  execute :: String -> m [String]

instance GhciWithState App where
  execute s = do
    putStrIO "DEBUG : "
    putStrLnIO s
    ghci <- ask
    liftIO $ exec ghci s

printIO :: Show s => s -> App ()
printIO = liftIO . print

putStrIO :: String -> App ()
putStrIO = liftIO . putStr

putStrLnIO :: String -> App ()
putStrLnIO = liftIO . putStrLn

execApp :: Term -> [Term] -> Ghci -> App a -> IO GameState
execApp term terms ghci (App app) = runReaderT (execStateT app gameState) ghci
  where
    gameState = GameState {_scores = [], _term = term, _allTerms = terms, _guessScore = Unguessed 5}
