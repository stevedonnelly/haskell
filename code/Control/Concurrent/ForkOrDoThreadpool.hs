module Control.Concurrent.ForkOrDoPool where
import Control.Concurrent
import Control.Concurrent.MVar as MVar
import Data.List as List
import Data.Maybe as Maybe
import Prelude.Extensions as PreludeExt

type ForkOrDoPool = [(ThreadId, MVar (IO ()))]

createPool :: Int -> IO ForkOrDoPool
createPool = \number_of_threads -> do
    let work_lock = MVar.newEmptyMVar
    work_locks <- (mapM id (replicate number_of_threads work_lock))
    thread_ids <- (mapM ((.) forkIO workerThread) work_locks)
    (return (zip thread_ids work_locks))

workerThread :: (MVar (IO ())) -> IO ()
workerThread = \work_lock -> do
    work <- (MVar.readMVar work_lock)
    work
    (MVar.takeMVar work_lock)
    (workerThread work_lock)

submitTask :: ForkOrDoPool -> IO () -> IO Bool
submitTask = \threadpool task -> do
    let {submit_first = do
        let (thread_id, work_lock) = (List.head threadpool)
        (MVar.tryPutMVar work_lock task)}
    let {try_submit = do
        success <- submit_first
        (ifElse success (return True) (submitTask (List.tail threadpool) task))} 
    (ifElse (List.null threadpool) (return False) try_submit)

forkOrDo :: ForkOrDoPool -> IO () -> IO ()
forkOrDo = \threadpool task -> do
    success <- (submitTask threadpool task)
    (doIf (not success) task)


