{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module App where

import Control.Applicative (Alternative)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (
  MonadState,
  StateT (StateT),
  execStateT,
 )
import Language.Haskell.Ghcid (Ghci, exec)
import Term

newtype App a = App {runApp :: ExceptT String (StateT GameState (ReaderT Ghci IO)) a}
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadReader Ghci, MonadIO, MonadState GameState, MonadError String)

type StateErrorGhci m = (GhciSession m, MonadState GameState m, MonadError String m)

class Monad m => GhciSession m where
  execute :: String -> m [String]

class Monad m => InputOutput m where
  readLine :: m String
  printStr :: Show s => s -> m ()
  printStrLn :: String -> m ()

-- printStr_ :: String -> m ()

instance InputOutput App where
  readLine = liftIO getLine
  printStr = liftIO . print
  printStrLn = liftIO . putStrLn

instance GhciSession App where
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

execApp :: [Term] -> Ghci -> App a -> IO GameState
execApp terms ghci (App app) = runReaderT (execStateT (runExceptT app) gameState) ghci
 where
  gameState = GameState{_scores = [], _term = head terms, _allTerms = tail terms, _guessScore = Unguessed 5}
