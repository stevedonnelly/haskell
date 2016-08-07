module Control.Concurrent.Extensions
import Control.Concurrent as Concurrent
import Control.Concurrent.MVar as MVar

forkFuture :: IO a -> (ThreadId, MVar (Either SomeException a))
forkFuture = \io -> do
    future <- newEmptyMVar
    let complete = \exception_or_result -> (putMVar exception_or_result)
    thread_id <- (forkFinally io complete)
    (return (thread_id, future))


