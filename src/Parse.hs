module Parse where

import Data.List (groupBy, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Term (Term (..))
import qualified Utils

parseType :: String -> Maybe Term
parseType str =
  case splitOn " ::" str of
    termName : typeStr : _ -> Just $ Term{_name = termName, _context = "", _termType = typeStr}
    _ -> Nothing

-- fix fully qualified aka remove
parseBrowse :: [String] -> [Term]
parseBrowse = mapMaybe parseType . filter isNormalTerm . groupTerms
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