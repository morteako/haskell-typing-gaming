module Parse where

import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Term (Term (..))

parseType :: String -> Maybe Term
parseType str =
  case splitOn " ::" str of
    termName : typeStr : _ -> Just $ Term{_name = termName, _context = "", _termType = typeStr}
    _ -> Nothing

-- fix fully qualified aka remove
parseBrowse :: [String] -> [Term]
parseBrowse = mapMaybe parseType . filter isNormalTerm
 where
  isNormalTerm x = all ($ x) [notNewline, notTypeAlias, notTypeClass]
  notNewline = not . isPrefixOf " "
  notTypeAlias = not . isPrefixOf "type "
  notTypeClass = not . isPrefixOf "class "
