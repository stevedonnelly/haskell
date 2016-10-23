module Data.Cache.LRU.Extensions where
import Data.Cache.LRU.Internal
import qualified Data.Map as Map
import Prelude hiding (last, lookup)

-- Fixed version of: https://github.com/chowells79/lrucache/blob/master/src/Data/Cache/LRU/Internal.hs#L120
-- Copyright Carl Howells 2010-2015
-- License: https://github.com/chowells79/lrucache/blob/master/LICENSE
-- Remove after fix is released.

-- | Same as 'insert', but also returns element which was dropped from
-- cache, if any.
insertInforming :: Ord key => key -> val -> LRU key val
                -> (LRU key val, Maybe (key, val))
insertInforming key val lru = maybe emptyCase nonEmptyCase $ first lru
    where
      contents = content lru
      full = maybe False (fromIntegral (Map.size contents) ==) $ maxSize lru
      present = key `Map.member` contents

      -- this is the case for adding to an empty LRU Cache
      emptyCase = (LRU fl fl (maxSize lru) m', Nothing)
          where
            fl = Just key
            lv = Link val Nothing Nothing
            m' = Map.insert key lv contents

      nonEmptyCase firstKey = if present then (hitSet, Nothing)
                              else add firstKey

      -- this updates the value stored with the key, then marks it as
      -- the most recently accessed
      hitSet = hit' key lru'
          where lru' = lru { content = contents' }
                contents' = adjust' (\v -> v {value = val}) key contents

      -- create a new LRU with a new first item, and
      -- conditionally dropping the last item
      add firstKey = if full then (lru'', Just (lastKey, value lastLV))
                     else (lru', Nothing)
          where
            -- add a new first item
            firstLV' = Link val Nothing $ Just firstKey
            contents' = Map.insert key firstLV' .
                        adjust' (\v -> v { prev = Just key }) firstKey $
                        contents
            lru' = lru { first = Just key, content = contents' }

            -- remove the last item
            Just lastKey = last lru'
            Just lastLV = Map.lookup lastKey contents'
            contents'' = Map.delete lastKey contents'
            lru'' = delete' lastKey lru' contents'' lastLV

