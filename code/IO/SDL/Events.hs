
module IO.SDL.Events where
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Graphics.UI.SDL.Events as SE
import Graphics.UI.SDL.Keysym as Keysym
import IO.Buttons as Buttons
import Prelude.Extensions as PreludeExt


isNoEvent = \event -> case event of
    SE.NoEvent -> True
    _ -> False
    
isKeyDownEvent = \event -> case event of
    SE.KeyDown _ -> True
    _ -> False

isKeyUpEvent = \event -> case event of
    SE.KeyUp _ -> True
    _ -> False

isQuitEvent = \event -> case event of
    SE.Quit -> True
    _ -> False

normalizeSDLKeyEvents = \key_events -> let
    normalizeEvent = \event -> case event of
        (KeyDown (Keysym key_id modifiers unicode)) -> (True, key_id)
        (KeyUp (Keysym key_id modifiers unicode)) -> (False, key_id)
    in (List.map normalizeEvent key_events)

processSDLKeyEvents = \key_events -> (processButtonEvents (normalizeSDLKeyEvents key_events))

takeEvents = do
    event <- SE.pollEvent
    let no_events = (isNoEvent event)
    remaining <- (ifElse no_events (return []) takeEvents)
    return (ifElse no_events [] ((:) event remaining))


