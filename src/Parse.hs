{-# LANGUAGE PartialTypeSignatures #-}

module Parse where

import Data.Functor (void)
import Data.List (groupBy, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Language.Haskell.Exts
import Term (Term (..))

parseToTerm :: String -> Maybe Term
parseToTerm str = case parseDecl str of
  ParseOk (TypeSig _ [name] type') -> Just $ Term{_name = prettyPrint name, _termType = void type'}
  _ -> Nothing

-- fix fully qualified aka remove
parseBrowse :: [String] -> [Term]
parseBrowse = mapMaybe parseToTerm . filter isNormalTerm . groupTerms
 where
  isNormalTerm x = all ($ x) [notNewline, notTypeAlias, notTypeClass]
  notNewline = not . isPrefixOf " "
  notTypeAlias = not . isPrefixOf "type "
  notTypeClass = not . isPrefixOf "class "

groupTerms :: [String] -> [String]
groupTerms = fmap concat . groupBy f
 where
  f _ (' ' : _) = True
  f _ _ = False

-- term _no -> true
-- _q _no -> true
-- _q term -> false
-- term term -> false