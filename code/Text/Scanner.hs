
module Text.Scanner where
import Data.Char as Char
import Data.List as List
import Data.List.Extensions as ListExt
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Extensions as TupleExt
import Prelude.Extensions as PreludeExt
import Text.Regex.Posix


type Token = (Int, String)
tokenId = fst
text = snd
type IndexToken = (Int, Token)
index = fst
indexTokenId = ((.) tokenId token)
indexTokenText = ((.) text token)
token = snd

matchFront :: String -> String -> (Bool, String)
matchFront = \input regex -> (input =~ ("\\`" ++ regex), input =~ ("\\`" ++ regex))

scan :: [String] -> String -> ([IndexToken], [(Int, Char)])
scan = \patterns input -> let
    rules = (zipIndices0 patterns)
    selectMatch = \input previous rule -> let
        (is_match, text) = (matchFront input (snd rule))
        select_new = ((&&) is_match ((>) (length text) (length (snd previous))))
        in (ifElse select_new (fst rule, text) previous)
    scan = \input index -> let
        token = (List.foldl (selectMatch input) (-1, "") rules)
        match_length = (length (text token))
        after_match = (drop match_length input)
        (remaining, next_index, tokens, errors) = (ifElse ((/=) match_length 0) 
            (after_match, (+) index match_length, [(index, token)], []) 
            (tail input, (+) index 1, [], [(index, head input)]))
        (recurse_tokens, recurse_errors) = (scan remaining next_index)
        in (ifElse (List.null input) ([], []) ((++) tokens recurse_tokens, (++) errors recurse_errors))
    in (scan input 0)



