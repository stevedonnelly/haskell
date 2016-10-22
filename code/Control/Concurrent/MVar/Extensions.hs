module Control.Concurrent.MVar.Extensions where
import Control.Concurrent.MVar as MVar
import Data.Maybe as Maybe
import Prelude.Extensions as PreludeExt


tryReadMaybeMVar :: MVar (Maybe a) -> IO (Maybe a)
tryReadMaybeMVar = \mvar -> do 
    success <- (MVar.tryReadMVar mvar)
    (return (ifElse (Maybe.isJust success) (Maybe.fromJust success) Nothing))

writeMVar :: MVar a -> a -> IO ()
writeMVar = \mvar value -> do 
    (MVar.tryTakeMVar mvar)
    success <- (MVar.tryPutMVar mvar value)
    let try_again = (writeMVar mvar value)
    (doIf (not success) try_again)

transformMVar :: MVar a -> (a -> a) -> IO (a, a)
transformMVar = \mvar transform -> do
    let wrap = \x -> let y = (transform x) in (y, (x, y))
    (modifyMVar mvar ((.) return wrap))

