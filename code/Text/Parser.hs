
module Text.Parser where
import Data.List as List
import Data.List.Extensions as ListExt
import Data.Tuple.Extensions as TupleExt
import Prelude.Extensions as PreludeExt
import Text.Scanner as Scanner


data ParseTree = Empty | Terminal Token | Sequence [ParseTree] | Production Int ParseTree deriving (Show, Eq)

isEmpty = \tree -> case tree of
    Empty -> True
    _ -> False
isTerminal = \tree -> case tree of
    (Terminal token) -> True
    _ -> False
isSequence = \tree -> case tree of
    (Sequence subtrees) -> True
    _ -> False
isProduction = \tree -> case tree of
    (Production id production) -> True
    _ -> False

terminal  = \(Terminal token) -> token
sequence = \(Sequence trees) -> trees
production = \(Production id tree) -> (id, tree)

numberOfTokens = \parse_tree -> case parse_tree of
    Empty -> 0
    (Terminal t) -> 1
    (Sequence s) -> (sum (List.map numberOfTokens s))
    (Production id p) -> (numberOfTokens p)


data ParseError = Error (Token, Int) | ProductionError [[ParseError]] deriving (Show, Eq)

isError = \error -> case error of
    (Error _) -> True
    _ -> False
isProductionError = \error -> case error of
    (ProductionError _) -> True
    _ -> False

error = \(Error (token, expected)) -> (token, expected)
productionError = \(ProductionError suberrors) -> suberrors


type ParseFunction = [Token] -> (ParseTree, [ParseError], [Token])

emptyParser :: ParseFunction
emptyParser = \tokens -> (Empty, [], tokens)

terminalParser :: Int -> ParseFunction
terminalParser = \token_id tokens -> let
    is_end = (List.null tokens)
    is_match = ((==) (tokenId (head tokens)) token_id)
    end_token = (-1, -1, "\\EOF")
    in (ifElse is_end (Empty, [Error (end_token, token_id)], [])
        (ifElse is_match (Terminal (head tokens), [], tail tokens)
            (Empty, [Error (head tokens, token_id)], tail tokens)))

sequenceParser :: [ParseFunction] -> ParseFunction
sequenceParser = \sequence tokens -> let
    parseNext = \(trees0, errors0, tokens0) parser -> let
        (tree1, errors1, tokens1) = (ifElse (List.null errors0) (parser tokens0) (Empty, [], tokens0))
        in ((:) tree1 trees0, (++) (reverse errors1) errors0, tokens1)
    (trees, errors, remaining) = (List.foldl parseNext ([], [], tokens) sequence)
    in (Sequence (reverse trees), reverse errors, remaining)

productionParser :: [ParseFunction] -> ParseFunction
productionParser = \productions tokens -> let
    parses = (List.map (\parser -> (parser tokens)) productions)
    selectProduction = \(id0, (tree0, errors0, tokens0)) (id1, (tree1, errors1, tokens1)) -> let
        size0 = (ifElse (List.null errors0) (numberOfTokens tree0) (-1))
        size1 = (ifElse (List.null errors1) (numberOfTokens tree1) (-1))
        in (ifElse ((>) size1 size0) (id1, (tree1, errors1, tokens1)) (id0, (tree0, errors0, tokens0)))
    result = (List.foldl selectProduction (0, head parses) (zipIndices0 parses))
    (id, (tree, errors, remaining)) = result
    has_success = (or (List.map ((.) List.null second3) parses))
    all_errors = [(ProductionError (List.map second3 parses))]
    in (ifElse has_success (Production id tree, [], remaining) (Empty, all_errors, tokens))

listParser :: ParseFunction -> ParseFunction
listParser = \parse_function -> let
    list_case = sequenceParser [parse_function, (listParser parse_function)]
    productions = [emptyParser, list_case]
    in (productionParser productions)

unwrapListParseTree = \tree -> let
    (id, subtree) = (production tree)
    [first, rest] = (sequence (subtree))
    in (ifElse ((==) id 0) [] ((:) first (unwrapListParseTree rest)))

parse :: ParseFunction -> [Token] -> (ParseTree, [ParseError])
parse = \parse_function tokens -> let
    (tree, errors, remaining) = (parse_function tokens)
    after_end = (List.map (\token -> Error (token, (-1))) remaining)
    in (tree, (reverse ((++) (reverse after_end) errors)))

