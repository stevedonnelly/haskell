
module IO.Buttons where
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Prelude.Extensions as PreludeExt


processButtonEvent :: Ord k => (k -> Set k -> Set k) -> k -> (Set k) -> (Map k Int) -> (Set k, Map k Int)
processButtonEvent = \updatePressed button_id pressed event_counters -> let
    result_pressed = (updatePressed button_id pressed)
    result_counters = (Map.insertWith (\new old -> ((+) old 1)) button_id 1 event_counters)
    in (result_pressed, result_counters)

processButtonDownEvent :: Ord k => k -> (Set k) -> (Map k Int) -> (Set k, Map k Int)
processButtonDownEvent = (processButtonEvent Set.insert)

processButtonUpEvent :: Ord k => k -> (Set k) -> (Map k Int) -> (Set k, Map k Int)
processButtonUpEvent = (processButtonEvent Set.delete)

processButtonEvents :: Ord k => [(Bool, k)] -> (Set k) -> (Map k Int) -> (Map k Int) -> (Set k, Map k Int, Map k Int)
processButtonEvents = \button_events pressed down_counters up_counters -> let
    processEvent = \(pressed, down_counters, up_counters) (is_down, button_id) -> let
        (down_pressed, down_events) = (processButtonDownEvent button_id pressed down_counters)
        (up_pressed, up_events) = (processButtonUpEvent button_id pressed up_counters)
        in (ifElse is_down (down_pressed, down_events, up_counters) (up_pressed, down_counters, up_events))
    in (List.foldl processEvent (pressed, down_counters, up_counters) button_events)


