module Data.Dequeue.Extensions where
import qualified Data.Dequeue as Dequeue
import Data.Maybe as Maybe
import Prelude.Extensions as PreludeExt

type Dequeue a = Dequeue.BankersDequeue a

takeFirst :: Dequeue a -> (a, Dequeue a)
takeFirst = ((.) fromJust Dequeue.popFront)

takeLast :: Dequeue a -> (a, Dequeue a)
takeLast = ((.) fromJust Dequeue.popBack)

insertFirst :: a -> Dequeue a -> Dequeue a
insertFirst = (flip Dequeue.pushFront)

insertLast :: a -> Dequeue a -> Dequeue a
insertLast = (flip Dequeue.pushBack)


