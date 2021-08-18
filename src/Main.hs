{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Language.Haskell.Ghcid
import Control.Lens
import Control.Monad.Reader
import Control.Applicative
import Data.List

newtype App a = App {runApp :: ReaderT Ghci IO a}
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadReader Ghci, MonadIO)

printIO :: Show s => s -> App ()
printIO = liftIO . print

execApp :: Ghci -> App a -> IO a
execApp ghci (App app) = runReaderT app ghci

data Input = Skip | Quit | Blank | Guess String 
  deriving (Show,Eq)

data Term = Term {
  name :: String,
  context :: String,
  termType :: String
  }

t :: App ()
t = do
  liftIO $ print "app"

parseInput ::String -> Input
parseInput "" = Blank
parseInput "-s" = Skip
parseInput "-q" = Quit
parseInput g = Guess g

getRandomTerm :: IO Term
getRandomTerm = return $ Term {name = "lens", context = "Show a", termType = "a"}

checkGuess :: String -> String -> App Bool
checkGuess name g = do
  res <- execute (":t " ++ name ++ " :: " ++ g)
  printIO res
  pure $ name `isPrefixOf` concat res 

  

execute :: String -> App [String]
execute s = do
  ghci <- ask
  liftIO $ exec ghci s

mainLoop :: App ()
mainLoop = do
  printIO "INPUT :"
  inp <- parseInput <$> liftIO getLine
  printIO inp
  case inp of
    Skip -> mainLoop
    Quit -> guard False
    Blank -> mainLoop
    Guess g -> checkGuess "reverse " g >>= printIO >> mainLoop
  

main :: IO ()
main = do
  --mainLoop
  (ghci, _) <- startGhci "ghci" (Just ".") (\q s -> print q >> print s)
  -- executeStatement ":browse Data.List" >>= mapM print
  -- stopGhci ghci
  execApp ghci t

  execApp ghci mainLoop
  stopGhci ghci

