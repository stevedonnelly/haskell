module Text.Serialization.Tokens where
import Data.List as List
import Text.Scanner as Scanner

patterns = [
    "\\(",
    "\\)",
    "([A-Za-z0-9_]|(-)|(/))+",
    "\\\"[^\"]*\\\"",
    "[ \t\n]+"]

begin_tree = 0 :: Int
end_tree = 1 :: Int
identifier = 2 :: Int
string = 3 :: Int
space = 4 :: Int

scan = \input -> let
    (tokens, errors) = (Scanner.scan patterns input)
    in (List.filter (\x -> ((/=) (indexTokenId x) space)) tokens, errors)


