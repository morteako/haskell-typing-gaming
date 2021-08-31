{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Term (GameState)

newtype App a = App {runApp :: ExceptT () (StateT GameState (ReaderT Ghci IO)) a}
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadReader Ghci, MonadIO, MonadState GameState, MonadError ())

class Monad m => GhciSession m where
  execute :: String -> m [String]

class Monad m => InputOutput m where
  readLine :: m String
  printIO :: Show s => s -> m ()
  putStrLnIO :: String -> m ()
  putStrIO :: String -> m ()

instance InputOutput App where
  readLine = liftIO getLine
  printIO = liftIO . print
  putStrLnIO = liftIO . putStrLn
  putStrIO = liftIO . putStr

instance GhciSession App where
  execute s = do
    ghci <- ask
    liftIO $ exec ghci s
