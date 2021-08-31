{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Args
import Control.Monad.Except (void)
import GHC.IO (unsafePerformIO)
import GHC.IO.Handle
import GHC.IO.Handle.FD (stdin, stdout)
import Game (runGame)
import Language.Haskell.Ghcid (exec, startGhci, stopGhci)
import Parse (groupTerms, parseBrowse)
import System.Random.Shuffle (shuffleM)

main :: IO ()
main = do
  Args{numQuestions, difficulty} <- execArgsParser
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  (ghci, _) <- startGhci "ghci" (Just "src") (\_ s -> print s)
  let moduleWithTerms = getModule difficulty
  exec ghci $ "import " ++ moduleWithTerms
  ls <- exec ghci $ ":browse " ++ moduleWithTerms
  terms <- shuffleM $ parseBrowse ls
  case take numQuestions terms of
    [] ->
      putStrLn "Internal error. Not enough terms in module. Exiting..."
    (startTerm : restOfTerms) ->
      void $ runGame startTerm restOfTerms ghci
  stopGhci ghci
