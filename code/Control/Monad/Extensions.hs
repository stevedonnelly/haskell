module Control.Monad.Extensions where
import Control.Applicative
import Prelude.Extensions as PreludeExt

untilM :: Monad m => (a -> m Bool) -> (a -> m a) -> a -> m a 
untilM = \condition transform initial -> do
    stop <- (condition initial)
    let {recurse = do
        next <- (transform initial)
        (untilM condition transform next)}
    (ifElse stop (return initial) recurse)

whileM :: Monad m => Applicative m => (a -> m Bool) -> (a -> m a) -> a -> m a 
whileM = \condition -> (untilM ((.) (liftA not) condition))
