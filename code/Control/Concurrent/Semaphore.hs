module Control.Concurrent.Semaphore where
import Control.Concurrent.MVar as MVar
import Control.Concurrent.MVar.Extensions as MVarExt
import Control.Monad
import Data.Integer.Concurrent as AtomicInt
import Prelude.Extensions as PreludeExt


type Semaphore = (MVar Int, MVar (), MVar ())

newSemaphore :: Int -> IO Semaphore
newSemaphore = \leases -> do
    counter <- (newAtomicInteger leases)
    lock <- (MVar.newMVar ())
    signal <- (ifElse ((>) leases 0) (MVar.newMVar ()) (MVar.newEmptyMVar))
    (return (counter, lock, signal)) 

currentLeases = \(counter, lock, signal) -> (MVar.readMVar counter)

-- takes will complete in FIFO order whether or not there are enough leases 
-- available for a smaller to take to complete ahead of a larger one
takeLeases :: Semaphore -> Int -> IO ()
takeLeases = \(counter, lock, signal) leases -> do
    (MVar.takeMVar lock)
    let {takeOrRetry = do
        (MVar.takeMVar signal)
        current <- (MVar.readMVar counter)
        let {take = do 
            remaining <- (addAndGet counter (negate leases))
            (doIf ((>) remaining 0) (MVar.tryPutMVar signal ()))}
        (ifElse ((>=) current leases) take takeOrRetry)}
    takeOrRetry
    (MVar.putMVar lock ())

takeLease = (flip takeLeases 1)

-- defaults to FIFO if enough leases are available, otherwise large takes go to the 
-- back of the queue if there are not enough leases available when it gets its turn
takeLeasesGenerously :: Semaphore -> Int -> IO ()
takeLeasesGenerously = \(counter, lock, signal) leases -> do
    (MVar.takeMVar lock)
    (MVar.takeMVar signal)
    current <- (MVar.readMVar counter)
    let available = ((>=) current leases) 
    remaining <- (doIfElse available (addAndGet counter (negate leases)) current)
    (doIf available (MVar.tryPutMVar signal ()))
    (MVar.putMVar lock ())
    (doIf (not available) (takeLeasesGenerously (counter, lock, signal) leases))

putLeases :: Semaphore -> Int -> IO ()
putLeases = \(counter, lock, signal) leases -> do
    remaining <- (addAndGet counter leases)
    (void (MVar.tryPutMVar signal ()))

putLease = (flip putLeases 1)

