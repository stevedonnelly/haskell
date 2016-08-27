module Data.Either.Extensions where
import Data.Either as Either
import Data.List as List

left :: Either a b -> a
left = \either -> (List.head (lefts [either]))

right :: Either a b -> b
right = \either -> (List.head (rights [either]))


