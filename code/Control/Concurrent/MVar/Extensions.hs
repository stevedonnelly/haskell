module Control.Concurrent.MVar.Extensions where
import Control.Concurrent.MVar as MVar
import Prelude.Extensions as PreludeExt


transformMVar :: MVar a -> (a -> a) -> IO (a, a)
transformMVar = \mvar transform -> do
    let wrap = \x -> let y = (transform x) in (y, (x, y))
    (modifyMVar mvar ((.) return wrap))


