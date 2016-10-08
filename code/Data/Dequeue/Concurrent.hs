module Data.Dequeue.Concurrent where
import Control.Concurrent.ReadWriteLock as RWLock
import qualified Data.Dequeue as Pure
import qualified Data.Dequeue.Extensions as PureExt
import Data.IORef as IORef
import Data.Maybe as Maybe
import Prelude.Extensions as PreludeExt


type Dequeue a = (RWLock, IORef (PureExt.Dequeue a))

empty :: IO (Dequeue a)
empty = do 
    lock <- RWLock.new
    ioref <- IORef.newIORef Pure.empty
    (return (lock, ioref))

transform :: (PureExt.Dequeue a -> b) -> Dequeue a -> IO b
transform = \f (lock, ioref) -> do 
    (RWLock.acquireRead lock)
    dequeue <- (IORef.readIORef ioref)
    let result = (f dequeue)
    (RWLock.releaseRead lock)
    (return result)

null = (transform Pure.null)
size = (transform Pure.length)

takeExtreme :: (PureExt.Dequeue a -> (a, PureExt.Dequeue a)) -> Dequeue a -> IO a
takeExtreme = \taker (lock, ioref) -> do
    (RWLock.acquireWrite lock)
    dequeue <- (IORef.readIORef ioref)
    let (value, remaining) = (taker dequeue)
    (IORef.writeIORef ioref remaining)
    (RWLock.releaseWrite lock)
    (return value)

takeFirst = (takeExtreme PureExt.takeFirst)
takeLast = (takeExtreme PureExt.takeLast)

insertExtreme :: (a -> PureExt.Dequeue a -> PureExt.Dequeue a) -> a -> Dequeue a -> IO ()
insertExtreme = \inserter value (lock, ioref) -> do 
    (RWLock.acquireWrite lock)
    dequeue <- (IORef.readIORef ioref)
    let result = (inserter value dequeue)
    (IORef.writeIORef ioref result)
    (RWLock.releaseWrite lock)

insertFirst = (insertExtreme PureExt.insertFirst)
insertLast = (insertExtreme PureExt.insertLast)


