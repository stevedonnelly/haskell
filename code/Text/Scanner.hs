
module Text.Scanner where
import Data.Char as Char
import Data.List as List
import Data.List.Extensions as ListExt
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Extensions as TupleExt
import Prelude.Extensions as PreludeExt
import Text.Regex.Posix


type Token = (Int, Int, String)
tokenType = first3
tokenPosition = second3
tokenText = third3
setTokenText = setThird3

matchFront :: String -> String -> (Bool, String)
matchFront = \input regex -> (input =~ ("\\`" ++ regex), input =~ ("\\`" ++ regex))

scan :: [String] -> String -> ([Token], [(Int, Char)])
scan = \patterns input -> let
    rules = (zipIndices0 patterns)
    selectMatch = \input previous rule -> let
        (is_match, text) = (matchFront input (snd rule))
        select_new = ((&&) is_match ((>) (length text) (length (snd previous))))
        in (ifElse select_new (fst rule, text) previous)
    scan = \input index -> let
        (match_type, match_text) = (List.foldl (selectMatch input) (-1, "") rules)
        match_length = (length match_text)
        after_match = (drop match_length input)
        (remaining, next_index, tokens, errors) = (ifElse ((/=) match_length 0) 
            (after_match, (+) index match_length, [(match_type, index, match_text)], []) 
            (tail input, (+) index 1, [], [(index, head input)]))
        (recurse_tokens, recurse_errors) = (scan remaining next_index)
        in (ifElse (List.null input) ([], []) ((++) tokens recurse_tokens, (++) errors recurse_errors))
    in (scan input 0)


