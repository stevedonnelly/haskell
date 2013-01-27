module Graphics.UI.SDL.Extensions where
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Set.Extensions as SetExt
import qualified Graphics.UI.SDL as SDL
import Prelude.Extensions as PreludeExt


isNoEvent = \event -> case event of
    SDL.NoEvent -> True
    _ -> False
    
isKeyDownEvent = \event -> case event of
    SDL.KeyDown _ -> True
    _ -> False

isKeyUpEvent = \event -> case event of
    SDL.KeyUp _ -> True
    _ -> False

isKeyEvent = \event -> ((||) (isKeyDownEvent event) (isKeyUpEvent event))

isQuitEvent = \event -> case event of
    SDL.Quit -> True
    _ -> False

normalizeKeyEvent = \event -> case event of
    (SDL.KeyDown (SDL.Keysym key_id modifiers unicode)) -> (key_id, True)
    (SDL.KeyUp (SDL.Keysym key_id modifiers unicode)) -> (key_id, False)

normalizedKeyEvents = ((.) (List.map normalizeKeyEvent) (List.filter isKeyEvent))

updateKeySet = \keys events -> let
    in (List.foldl (flip (uncurry SetExt.insertOrDelete)) keys (normalizedKeyEvents events))

takeEvents = do
    event <- SDL.pollEvent
    let no_events = (isNoEvent event)
    remaining <- (ifElse no_events (return []) takeEvents)
    return (ifElse no_events [] ((:) event remaining))


