module Data.Dequeue.Blocking where
import Control.Concurrent.MVar as MVar
import qualified Data.Dequeue as Pure
import qualified Data.Dequeue.Extensions as PureExt
import Data.Maybe as Maybe
import Prelude.Extensions as PreludeExt


type Dequeue a = (MVar (), MVar (), MVar (PureExt.Dequeue a))

empty :: IO (Dequeue a)
empty = do 
    lock <- (MVar.newMVar ())
    signal <- MVar.newEmptyMVar 
    queue <- (MVar.newMVar Pure.empty)
    (return (lock, signal, queue))

transform :: (PureExt.Dequeue a -> b) -> Dequeue a -> IO b
transform = \f (lock, signal, state) -> do 
    queue <- (MVar.takeMVar state)
    let result = (f queue)
    (MVar.putMVar state queue)
    (return result)

null = (transform Pure.null)
size = (transform Pure.length)

takeExtreme :: (PureExt.Dequeue a -> (a, PureExt.Dequeue a)) -> Dequeue a -> IO a
takeExtreme = \taker (lock, signal, state) -> do
    (MVar.takeMVar lock)
    (MVar.takeMVar signal)
    queue <- (MVar.takeMVar state)
    let (value, remaining) = (taker queue)
    (MVar.putMVar state remaining)
    (doIf (not (Pure.null remaining)) (MVar.tryPutMVar signal ()))
    (MVar.putMVar lock ())
    (return value)

takeFirst = (takeExtreme PureExt.takeFirst)
takeLast = (takeExtreme PureExt.takeLast)

tryTakeExtreme :: (PureExt.Dequeue a -> (a, PureExt.Dequeue a)) -> Dequeue a -> IO (Maybe a)
tryTakeExtreme = \taker (lock, signal, state) -> do
    locked <- (MVar.tryTakeMVar lock)
    let {locked_case = do 
        signaled <- (MVar.tryTakeMVar signal)
        let {nonempty_case = do 
            queue <- (MVar.takeMVar state)
            let (value, remaining) = (taker queue)
            (MVar.putMVar state remaining)
            (doIf (not (Pure.null remaining)) (MVar.tryPutMVar signal ()))
            (return (Just value))}
        result <- (doIfElse (isJust signaled) nonempty_case Nothing)
        (MVar.putMVar lock ())
        (return result)}
    (doIfElse (isJust locked) locked_case Nothing)

tryTakeFirst = (tryTakeExtreme PureExt.takeFirst)
tryTakeLast = (tryTakeExtreme PureExt.takeLast)

insertExtreme :: (a -> PureExt.Dequeue a -> PureExt.Dequeue a) -> a -> Dequeue a -> IO ()
insertExtreme = \inserter value (lock, signal, state) -> do 
    queue <- (MVar.takeMVar state)
    let result = (inserter value queue)
    (MVar.tryPutMVar signal ())    
    (MVar.putMVar state result)

insertFirst = (insertExtreme PureExt.insertFirst)
insertLast = (insertExtreme PureExt.insertLast)


