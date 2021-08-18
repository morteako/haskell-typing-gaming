{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App where

import Control.Applicative (Alternative)
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Language.Haskell.Ghcid (Ghci, exec)
import Term

newtype App a = App {runApp :: StateT GameState (ReaderT Ghci IO) a}
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadReader Ghci, MonadIO, MonadState GameState)

printIO :: Show s => s -> App ()
printIO = liftIO . print

putStrIO :: String -> App ()
putStrIO = liftIO . putStr

putStrLnIO :: String -> App ()
putStrLnIO = liftIO . putStrLn

execApp :: Term -> Ghci -> App a -> IO GameState
execApp t ghci (App app) = runReaderT (execStateT app gameState) ghci
  where
    gameState = GameState {_scores = [], _term = t, _guessScore = 10}

execute :: String -> App [String]
execute s = do
  ghci <- ask
  liftIO $ exec ghci s
