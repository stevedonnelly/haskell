module Control.Concurrent.Extensions where
import Control.Concurrent as Concurrent
import Control.Concurrent.MVar as MVar
import Control.Exception.Base


forkFuture :: IO a -> IO (ThreadId, MVar (Either SomeException a))
forkFuture = \io -> do
    future <- newEmptyMVar
    let complete = \exception_or_result -> do (putMVar future exception_or_result)
    thread_id <- (forkFinally io complete)
    (return (thread_id, future))

nonForkedFuture :: IO a -> IO (ThreadId, MVar (Either SomeException a))
nonForkedFuture = \io -> do
    id <- myThreadId
    result <- io
    future <- (newMVar (Right result))
    (return (id, future))


