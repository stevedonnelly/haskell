module Data.Integer.Concurrent where
import Control.Applicative
import Control.Concurrent.MVar as MVar
import Control.Concurrent.MVar.Extensions as MVarExt

newAtomicInteger :: Integral a => a -> IO (MVar a)
newAtomicInteger = MVar.newMVar

getAndAdd :: Integral a => MVar a -> a -> IO a
getAndAdd = \mvar change -> ((liftA fst) (transformMVar mvar ((+) change)))

getAndIncrement :: Integral a => MVar a -> IO a
getAndIncrement = (flip getAndAdd 1)

getAndDecrement :: Integral a => MVar a -> IO a
getAndDecrement = (flip getAndAdd (-1))

addAndGet :: Integral a => MVar a -> a -> IO a
addAndGet = \mvar change -> ((liftA snd) (transformMVar mvar ((+) change)))

incrementAndGet :: Integral a => MVar a -> IO a
incrementAndGet = (flip addAndGet 1)

decrementAndGet :: Integral a => MVar a -> IO a
decrementAndGet = (flip addAndGet (-1))
