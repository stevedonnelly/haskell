module Control.Concurrent.MVar.Extensions where
import Control.Applicative
import Control.Concurrent.MVar as MVar

transformMVar :: MVar a -> (a -> a) -> IO (a, a)
transformMVar = \mvar transform -> do
    let wrap = \x -> let y = (transform x) in (y, (x, y))
    (modifyMVar mvar ((.) return wrap))

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


