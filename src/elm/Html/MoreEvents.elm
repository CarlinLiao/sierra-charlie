module Html.MoreEvents exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (defaultOptions, on)
import Json.Decode as Json exposing (Decoder, bool, field, map, map4)
import Types exposing (..)

-- establishes a consistent format for modifier key inputs
type alias ModifierKeys =
    { shiftKey : Bool
    , ctrlKey : Bool
    , altKey : Bool
    , metaKey : Bool
    }

-- translates inputs into a json-like elm object
modifierDecoder : Decoder ModifierKeys
modifierDecoder =
    map4 ModifierKeys
        (field "altKey" bool)
        (field "ctrlKey" bool)
        (field "shiftKey" bool)
        (field "metaKey" bool)

-- based on what key we're told is pressed, return a different element
convertToMsg : msg -> msg -> msg -> msg -> msg -> ModifierKeys -> msg
convertToMsg plainMsg shiftMsg ctrlMsg altMsg metaMsg modifierKeys =
    let
        finalMsg =
            if modifierKeys.shiftKey then
                shiftMsg
            else if modifierKeys.ctrlKey then
                ctrlMsg
            else if modifierKeys.altKey then
                altMsg
            else if modifierKeys.metaKey then
                metaMsg
            else
                plainMsg
    in
    finalMsg

-- on click, return a different element based on which key is pressed
-- used in the UI elements where we select hour of traffic data and what OSM features to display
onClickWithModifiers : msg -> msg -> msg -> msg -> msg -> Attribute msg
onClickWithModifiers plainMsg shiftMsg ctrlMsg altMsg metaMsg =
    on "click" (map (convertToMsg plainMsg shiftMsg ctrlMsg altMsg metaMsg) modifierDecoder)
