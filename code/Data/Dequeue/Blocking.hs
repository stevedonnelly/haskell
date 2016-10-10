module Data.Dequeue.Blocking where
import Control.Concurrent.MVar as MVar
import Control.Monad
import qualified Data.Dequeue as Pure
import qualified Data.Dequeue.Extensions as PureExt
import Data.Maybe as Maybe
import Prelude.Extensions as PreludeExt


type Dequeue a = (Maybe Int, MVar (), MVar (), MVar (), MVar (), MVar (PureExt.Dequeue a))

empty :: Maybe Int -> IO (Dequeue a)
empty = \capacity -> do 
    let queue = Pure.empty
    take_lock <- (MVar.newMVar ())
    take_signal <- MVar.newEmptyMVar 
    insert_lock <- (MVar.newMVar ())
    insert_signal <- (ifElse (underCapacity queue capacity) (MVar.newMVar ()) MVar.newEmptyMVar)
    state <- (MVar.newMVar queue)
    (return (capacity, take_lock, take_signal, insert_lock, insert_signal, state))

transform :: (PureExt.Dequeue a -> b) -> Dequeue a -> IO b
transform = \f (capacity, take_lock, take_signal, insert_lock, insert_signal, state) -> do 
    queue <- (MVar.takeMVar state)
    let result = (f queue)
    (MVar.putMVar state queue)
    (return result)

null = (transform Pure.null)
size = (transform Pure.length)

takeExtreme :: (PureExt.Dequeue a -> (a, PureExt.Dequeue a)) -> Dequeue a -> IO a
takeExtreme = \taker (capacity, take_lock, take_signal, insert_lock, insert_signal, state) -> do
    (MVar.takeMVar take_lock)
    (MVar.takeMVar take_signal)
    queue <- (MVar.takeMVar state)
    let (value, remaining) = (taker queue)
    (MVar.tryPutMVar insert_signal ())
    (doIf (not (Pure.null remaining)) (MVar.tryPutMVar take_signal ()))
    (MVar.putMVar state remaining)
    (MVar.putMVar take_lock ())
    (return value)

takeFirst = (takeExtreme PureExt.takeFirst)
takeLast = (takeExtreme PureExt.takeLast)

tryTakeExtreme :: (PureExt.Dequeue a -> (a, PureExt.Dequeue a)) -> Dequeue a -> IO (Maybe a)
tryTakeExtreme = \taker (capacity, take_lock, take_signal, insert_lock, insert_signal, state) -> do
    locked <- (MVar.tryTakeMVar take_lock)
    let {locked_case = do 
        signaled <- (MVar.tryTakeMVar take_signal)
        let {nonempty_case = do 
            queue <- (MVar.takeMVar state)
            let (value, remaining) = (taker queue)
            (MVar.tryPutMVar insert_signal ())
            (doIf (not (Pure.null remaining)) (MVar.tryPutMVar take_signal ()))
            (MVar.putMVar state remaining)
            (return (Just value))}
        result <- (doIfElse (Maybe.isJust signaled) nonempty_case Nothing)
        (MVar.putMVar take_lock ())
        (return result)}
    (doIfElse (Maybe.isJust locked) locked_case Nothing)

tryTakeFirst = (tryTakeExtreme PureExt.takeFirst)
tryTakeLast = (tryTakeExtreme PureExt.takeLast)

underCapacity :: PureExt.Dequeue a -> Maybe Int -> Bool
underCapacity = \queue capacity -> ((||) (Maybe.isNothing capacity) ((<) (Pure.length queue) (Maybe.fromJust capacity)))

insertExtreme :: (a -> PureExt.Dequeue a -> PureExt.Dequeue a) -> a -> Dequeue a -> IO ()
insertExtreme = \inserter value (capacity, take_lock, take_signal, insert_lock, insert_signal, state) -> do
    (MVar.takeMVar insert_lock)
    (MVar.takeMVar insert_signal)
    queue <- (MVar.takeMVar state)
    let result = (inserter value queue)
    (MVar.tryPutMVar take_signal ())
    (doIf (underCapacity result capacity) (MVar.tryPutMVar insert_signal ()))   
    (MVar.putMVar state result)
    (MVar.putMVar insert_lock ())

insertFirst = (insertExtreme PureExt.insertFirst)
insertLast = (insertExtreme PureExt.insertLast)

tryInsertExtreme :: (a -> PureExt.Dequeue a -> PureExt.Dequeue a) -> a -> Dequeue a -> IO Bool
tryInsertExtreme = \inserter value (capacity, take_lock, take_signal, insert_lock, insert_signal, state) -> do
    locked <- (MVar.tryTakeMVar insert_lock)
    let {locked_case = do 
        signaled <- (MVar.tryTakeMVar insert_signal)
        let {has_capacity_case = do 
            queue <- (MVar.takeMVar state)
            let result = (inserter value queue)
            (MVar.tryPutMVar take_signal ())
            (doIf (underCapacity result capacity) (MVar.tryPutMVar insert_signal ()))
            (MVar.putMVar state result)
            (return True)}
        result <- (doIfElse (Maybe.isJust signaled) has_capacity_case False)
        (MVar.putMVar insert_lock ())
        (return result)}
    (doIfElse (Maybe.isJust locked) locked_case False)

tryInsertFirst = (tryInsertExtreme PureExt.insertFirst)
tryInsertLast = (tryInsertExtreme PureExt.insertLast)


