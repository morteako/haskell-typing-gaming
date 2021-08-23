{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Args where

import Options.Applicative

data Args = Args
  { numQuestions :: Int
  , difficulty :: Difficulty
  }
  deriving (Show)

data Difficulty = Easy | Medium | Hard deriving (Eq, Show)

getModule :: Difficulty -> String
getModule Easy = "Data.List"
getModule Medium = "Data.List" --TODO find suiting
getModule Hard = "Control.Lens"

pnumQuestions :: Parser Int
pnumQuestions = option auto (long "num-questions" <> value 10 <> help "Number of terms to guess")

pdifficulty :: Parser Difficulty
pdifficulty = option (eitherReader parse) (long "difficulty" <> value Easy <> help ("Level of diffuculty. " ++ options))
 where
  options = "one of : easy, medium or hard"

  parse "easy" = Right Easy
  parse "medium" = Right Medium
  parse "hard" = Right Hard
  parse _ = Left $ "needs to be " ++ options

parseArgs :: Parser Args
parseArgs = Args <$> pnumQuestions <*> pdifficulty

opts :: ParserInfo Args
opts =
  info
    (parseArgs <**> helper)
    ( fullDesc
        <> progDesc "Play The Term-typing game!"
        -- <> header "typing-game - a test for optparse-applicative"
    )

execArgsParser :: IO Args
execArgsParser = customExecParser (prefs showHelpOnError) opts