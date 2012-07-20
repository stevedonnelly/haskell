
module Text.Serialization.Table where
import Control.Exception.Base as Exception
import Data.List as List
import Data.List.Extensions as ListExt
import Data.Map as Map
import Data.Map.Extensions as MapExt
import Data.Tuple.Extensions as TupleExt
import Prelude.Extensions as PreludeExt
import Text.Parser as Parser
import Text.Scanner as Scanner
import Text.Serialization.Grammar as Grammar
import Text.Serialization.Tokens as Tokens

data Table = Subtables String (Map Int Table) (Map String Table) 
    | Atom String 
    | Identifier String deriving (Show, Eq)

isSubtables = \table -> case table of
    (Subtables _ _ _) -> True
    _ -> False
isAtom = \table -> case table of
    (Atom _) -> True
    _ -> False
isIdentifier = \table -> case table of
    (Identifier _) -> True
    _ -> False

subtables = \(Subtables name by_index by_name) -> (name, by_index, by_name)
subtablesName = ((.) first3 subtables)
indexedSubtables = ((.) second3 subtables)
namedSubtables = ((.) third3 subtables)
atom = \(Atom string) -> string
identifier = \(Identifier name) -> name

intAtom = ((.) readInt atom)
doubleAtom = ((.) readDouble atom)
rationalAtom = ((.) readRational atom)

indexedTable = \table index -> ((!) (indexedSubtables table) index)
indexedAtom = (curry ((.) atom (uncurry indexedTable)))
indexedInt = (curry ((.) intAtom (uncurry indexedTable)))
indexedDouble = (curry ((.) doubleAtom (uncurry indexedTable)))
indexedRational = (curry ((.) rationalAtom (uncurry indexedTable)))

namedTable = \table name -> ((!) (namedSubtables table) name)

lookup = \table name -> let
    names = (splitOn '/' name)
    lookup = \table names -> let
        named_table = (namedTable table (head names))
        recurse = (lookup named_table (tail names))
        in (ifElse (List.null names) table recurse)
    in (lookup table names)

lookupIndexedTable = \table tag index -> (indexedTable (Text.Serialization.Table.lookup table tag) index)
lookupIndexedAtom = (curry3 ((.) atom (uncurry3 lookupIndexedTable)))
lookupIndexedInt = (curry3 ((.) intAtom (uncurry3 lookupIndexedTable)))
lookupIndexedDouble = (curry3 ((.) doubleAtom (uncurry3 lookupIndexedTable)))
lookupIndexedRational = (curry3 ((.) rationalAtom (uncurry3 lookupIndexedTable)))

parseTreeList = \list_parse_tree -> let
    (id, production_tree) = (Parser.production list_parse_tree)
    sequence_trees = (Parser.sequence production_tree)
    first = (head sequence_trees)
    rest = (parseTreeList (last sequence_trees))
    in (ifElse (Parser.isEmpty production_tree) [] ((:) first rest))

expressionTable = \expression_parse_tree -> case expression_parse_tree of
    (Production 0 parse_tree) -> (treeExpressionTable parse_tree)
    (Production 1 parse_tree) -> (stringExpressionTable parse_tree)
    (Production 2 parse_tree) -> (identifierExpressionTable parse_tree)
    _ -> (Prelude.error ((++) "invalid serialization expression: " (show expression_parse_tree)))

filterNamedSubtablesMap = \subtables -> let
    non_atoms = (List.filter isSubtables subtables)
    in (Map.fromList (List.map (\x -> (subtablesName x, x)) non_atoms))

treeExpressionTable = \tree_parse_tree -> let
    sequence = (Parser.sequence tree_parse_tree)
    name = (Scanner.indexTokenText (terminal ((!!) sequence 1)))
    subtables = (List.map expressionTable (parseTreeList ((!!) sequence 2)))
    by_index = (ListExt.toArray0 subtables)
    by_name = (filterNamedSubtablesMap subtables)
    in (Subtables name by_index by_name)

stringExpressionTable = \string_parse_tree -> let
    text = (Scanner.indexTokenText (terminal string_parse_tree))
    in (Atom (reverse (tail (reverse (tail text)))))

identifierExpressionTable = \string_parse_tree -> let
    in (Identifier (Scanner.indexTokenText (terminal string_parse_tree)))

rootTable = \root_parse_tree -> let
    subtables = (List.map expressionTable (parseTreeList root_parse_tree))
    by_index = (ListExt.toArray0 subtables)
    by_name = (filterNamedSubtablesMap subtables)
    in (resolveIdentifiers (Subtables "" by_index by_name))

resolveIdentifiers = \root_table -> let
    resolveIdentifiers = \table -> case table of
        (Subtables name by_index by_name) -> let
            resolveMap = (Map.map resolveIdentifiers)
            in (Subtables name (resolveMap by_index) (resolveMap by_name))
        (Atom string) -> (Atom string)
        (Identifier tag) -> (Text.Serialization.Table.lookup root_table tag)
    in (resolveIdentifiers root_table)

parseTable = \input -> let
    (parse_tree, errors) = (Grammar.parse input)
    in (assert (List.null errors) (rootTable parse_tree))

