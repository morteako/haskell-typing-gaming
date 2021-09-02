{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Parse where

import Control.Lens
import Control.Monad
import Data.Data.Lens
import Data.Functor (void)
import Data.List (groupBy, isPrefixOf)
import Data.Maybe (mapMaybe)
import GameState (Term (..))
import Language.Haskell.Exts

parseToTerm :: String -> Maybe Term
parseToTerm str = do
  ParseOk (TypeSig _ [name] type') <- Just $ void <$> parseDecl str
  pure $ Term{_name = prettyPrint name, _termType = type'}

notModule :: Term -> Bool
notModule Term{_termType} = hasn't (template :: Traversal' (Type ()) (ModuleName ())) _termType

parseBrowse :: [String] -> [Term]
parseBrowse = filter notModule . mapMaybe parseToTerm . filter isNormalTerm . groupTerms
 where
  isNormalTerm x = all ($ x) [notTypeAlias, notTypeClass]
  notTypeAlias = not . isPrefixOf "type "
  notTypeClass = not . isPrefixOf "class "

groupTerms :: [String] -> [String]
groupTerms = fmap concat . groupBy f
 where
  f _ (' ' : _) = True
  f _ _ = False