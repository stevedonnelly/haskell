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


