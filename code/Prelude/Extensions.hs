
module Prelude.Extensions where
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Ratio as Ratio
import Data.Set as Set
import Debug.Trace as Trace
import System.IO.Unsafe as Unsafe
import System.Random as Random


ifElse = \boolean a b -> if boolean then a else b

while = \predicate -> (until ((.) not predicate))

maybeIf = \boolean value -> (ifElse boolean (Just value) Nothing)

splitMaybe = \maybe -> (isJust maybe, fromJust maybe)

cases = \pairs otherwise -> let
    (bool, value) = (head pairs)
    in (ifElse (List.null pairs) otherwise (ifElse bool value (cases (tail pairs) otherwise)))

curry3 :: ((a, b, c) -> d) -> (a -> b -> c -> d)
curry3 = \f -> (\a b c -> (f (a, b, c)))

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 = \f -> (\(a, b, c) -> (f a b c))

cond = \conditions default_value -> let
    current = (head conditions)
    recurse = (cond (tail conditions) default_value)
    in (ifElse (List.null conditions) default_value (ifElse (fst current) (snd current) recurse))

select2 = \p a b -> (ifElse (p a b) a b)

readInt :: String -> Int
readInt = read
readDouble :: String -> Double
readDouble = read
readRational :: String -> Rational
readRational = ((.) toRational readDouble)

random = \x -> (toRational ((%) (mod ((unsafePerformIO randomIO) :: Int) 999999999) 999999999))

trace = Trace.trace

