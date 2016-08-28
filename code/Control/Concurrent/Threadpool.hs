module Control.Concurrent.Threadpool where
import Control.Concurrent
import Control.Concurrent.MVar as MVar
import Data.List as List
import Data.Maybe as Maybe
import Prelude.Extensions as PreludeExt

type Threadpool = [(ThreadId, MVar (MVar (IO ())))]

createThreadpool :: Int -> IO Threadpool
createThreadpool = \number_of_threads -> do
    let work_lock = MVar.newEmptyMVar
    work_locks <- (mapM id (replicate number_of_threads work_lock))
    thread_ids <- (mapM ((.) forkIO workerThread) work_locks)
    (return (zip thread_ids work_locks))

workerThread :: (MVar (MVar (IO ()))) -> IO ()
workerThread = \work_lock -> do
    work_holder <- MVar.newEmptyMVar
    (MVar.putMVar work_lock work_holder)
    work <- (MVar.takeMVar work_holder)
    work
    (workerThread work_lock)

submitTask :: Threadpool -> IO () -> IO Bool
submitTask = \threadpool task -> do
    let {submit_first = do
        let (thread_id, work_lock) = (List.head threadpool)
        work_holder <- (MVar.tryTakeMVar work_lock)
        let {available = do
            (MVar.putMVar (fromJust work_holder) task)
            (return True)}
        (ifElse (isJust work_holder) available (return False))}
    let {try_submit = do
        success <- submit_first
        (ifElse success (return True) (submitTask (List.tail threadpool) task))} 
    (ifElse (List.null threadpool) (return False) try_submit)


