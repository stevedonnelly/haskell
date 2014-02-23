
module Text.Serialization.Grammar where
import Control.Exception.Base
import Data.List as List
import Data.Map as Map
import Text.Parser as Parser
import Text.Scanner as Scanner
import qualified Text.Serialization.Tokens as Tokens

begin_tree = (terminalParser Tokens.begin_tree)

tree = (sequenceParser [begin_tree, identifier, expressions, end_tree])

end_tree = (terminalParser Tokens.end_tree)

string = (terminalParser Tokens.string)

identifier = (terminalParser Tokens.identifier)

expression = (productionParser [
    tree,
    string,
    identifier])
tree_case = 0 :: Int
string_case = 1 :: Int
identifier_case = 2 :: Int

expressions = (listParser expression)

parse :: String -> (ParseTree, [ParseError])
parse = \input -> let
    (tokens, scan_errors) = (Tokens.scan input)
    in (assert (List.null scan_errors) (Parser.parse expressions tokens))


