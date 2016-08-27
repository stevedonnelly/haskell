module Control.Concurrent.Extensions where
import Control.Concurrent as Concurrent
import Control.Concurrent.MVar as MVar
import Control.DeepSeq as DeepSeq
import Control.Exception.Base
import Data.Either as Either
import Data.Either.Extensions as EitherExt
import Prelude.Extensions as PreludeExt

forkFuture :: IO a -> IO (ThreadId, MVar (Either SomeException a))
forkFuture = \io -> do
    future <- newEmptyMVar
    let complete = \exception_or_result -> (putMVar future exception_or_result)
    thread_id <- (forkFinally io complete)
    (return (thread_id, future))

forkStrictFuture :: NFData a => IO a -> IO (ThreadId, MVar (Either SomeException a))
forkStrictFuture = \io -> do
    future <- newEmptyMVar
    let {complete = \exception_or_result -> do 
        let strict_result = (DeepSeq.force (EitherExt.right exception_or_result))
        let strict_either = (ifElse (Either.isRight exception_or_result) (Right strict_result) exception_or_result)
        (putMVar future strict_either)}
    thread_id <- (forkFinally io complete)
    (return (thread_id, future))

nonForkedFuture :: IO a -> IO (ThreadId, MVar (Either SomeException a))
nonForkedFuture = \io -> do
    id <- myThreadId
    result <- io
    future <- (newMVar (Right result))
    (return (id, future))


