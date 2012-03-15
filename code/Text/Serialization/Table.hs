
module Text.Serialization.Table where
import Control.Exception.Base as Exception
import Data.List as List
import Data.List.Extensions as ListExt
import Prelude.Extensions as PreludeExt
import Text.Parser as Parser
import Text.Scanner as Scanner
import Text.Serialization.Grammar as Grammar
import Text.Serialization.Tokens as Tokens

data Table = Subtables String [Table] | Atom String | Identifier String deriving (Show, Eq)

isSubtables = \table -> case table of
    (Subtables _ _) -> True
    _ -> False
isAtom = \table -> case table of
    (Atom _) -> True
    _ -> False
isIdentifier = \table -> case table of
    (Identifier _) -> True
    _ -> False

subtables = \(Subtables name tables) -> (name, tables)
atom = \(Atom string) -> string
identifier = \(Identifier name) -> name

intAtom = ((.) readInt atom)
doubleAtom = ((.) readDouble atom)
rationalAtom = ((.) readRational atom)

indexedTable = \table index -> ((!!) (snd (subtables table)) index)
indexedAtom = (curry ((.) atom (uncurry indexedTable)))
indexedInt = (curry ((.) intAtom (uncurry indexedTable)))
indexedDouble = (curry ((.) doubleAtom (uncurry indexedTable)))
indexedRational = (curry ((.) rationalAtom (uncurry indexedTable)))

taggedTable = \table tag -> let
    matchesTag = \table -> ((&&) (isSubtables table) ((==) (fst (subtables table)) tag))
    matches = (List.filter matchesTag (snd (subtables table)))
    preconditions = ((==) (length matches) 1)
    in (assert preconditions (head matches))

lookup = \table tag -> let
    tags = (splitOn '/' tag)
    lookup = \table tags -> (ifElse (null tags) table (lookup (taggedTable table (head tags)) (tail tags)))
    in (lookup table tags)

lookupIndexedTable = \table tag index -> (indexedTable (Text.Serialization.Table.lookup table tag) index)
lookupIndexedAtom = (curry3 ((.) atom (uncurry3 lookupIndexedTable)))
lookupIndexedInt = (curry3 ((.) intAtom (uncurry3 lookupIndexedTable)))
lookupIndexedDouble = (curry3 ((.) doubleAtom (uncurry3 lookupIndexedTable)))
lookupIndexedRational = (curry3 ((.) rationalAtom (uncurry3 lookupIndexedTable)))

parseTreeList = \list_parse_tree -> let
    (id, production_tree) = (production list_parse_tree)
    sequence_trees = (Parser.sequence production_tree)
    first = (head sequence_trees)
    rest = (parseTreeList (last sequence_trees))
    in (ifElse (isEmpty production_tree) [] ((:) first rest))

expressionTable = \expression_parse_tree -> case expression_parse_tree of
    (Production 0 parse_tree) -> (treeExpressionTable parse_tree)
    (Production 1 parse_tree) -> (stringExpressionTable parse_tree)
    (Production 2 parse_tree) -> (identifierExpressionTable parse_tree)
    _ -> (Prelude.error ((++) "invalid serialization expression: " (show expression_parse_tree)))

treeExpressionTable = \tree_parse_tree -> let
    seq = (Parser.sequence tree_parse_tree)
    name = (Scanner.indexTokenText (terminal ((!!) seq 1)))
    subtables = (List.map expressionTable (parseTreeList ((!!) seq 2)))
    in (Subtables name subtables)

stringExpressionTable = \string_parse_tree -> let
    text = (Scanner.indexTokenText (terminal string_parse_tree))
    in (Atom (reverse (tail (reverse (tail text)))))

identifierExpressionTable = \string_parse_tree -> let
    in (Identifier (Scanner.indexTokenText (terminal string_parse_tree)))

rootTable = \root_parse_tree -> let
    subtables = (List.map expressionTable (parseTreeList root_parse_tree))
    in (resolveIdentifiers (Subtables "" subtables))

resolveIdentifiers = \root_table -> let
    resolveIdentifiers = \table -> case table of
        (Subtables name tables) -> (Subtables name (List.map resolveIdentifiers tables))
        (Atom string) -> (Atom string)
        (Identifier tag) -> (Text.Serialization.Table.lookup root_table tag)
    in (resolveIdentifiers root_table)

parseTable = \input -> let
    (parse_tree, errors) = (Grammar.parse input)
    in (assert (List.null errors) (rootTable parse_tree))

