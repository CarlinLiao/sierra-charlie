module UI exposing (..)

import Html exposing (Html)
import Platform.Cmd as Cmd exposing (Cmd, none)
import Ports exposing (incomingMessage, outgoingMessage)
import Render exposing (renderUI)
-- import Special
-- import Task exposing (Task, andThen)
import Types exposing (..)

-- define default variable values to mutate on changes
defaultState : State
defaultState =
    { mode = Nothing
    , loadingProgress = 0
    , highlightedFeature = Nothing
    , selectedFeature = Nothing
    , routes = []
    , adjustment = Nothing
    , viewGroups = []
    , activeViews = []
    , viewInfoVisible = False
    , modelGroups = []
    , activeModel = Nothing
    , modelInfoVisible = False
    }

-- change state fields depending 
update : Msg -> State -> ( State, Cmd Msg )
update action state =
    case action of
        Idle ->
            ( state, none )

        Receive (UpdateMode mode) ->
            ( { state | mode = mode }, none )

        Receive (UpdateLoadingProgress loadingProgress) ->
            ( { state | loadingProgress = loadingProgress }, none )

        Receive (UpdateHighlightedFeature feature) ->
            ( { state | highlightedFeature = feature }, none )

        Receive (UpdateSelectedFeature feature) ->
            ( { state | selectedFeature = feature }, none )

        Receive (UpdateRoutes routes) ->
            ( { state | routes = routes }, none )

        Receive (UpdateAdjustment adjustment) ->
            ( { state | adjustment = adjustment }, none )

        Receive (UpdateViewGroups viewGroups) ->
            ( { state | viewGroups = viewGroups }, none )

        Receive (UpdateActiveViews activeViews) ->
            ( { state | activeViews = activeViews }, none )

        Receive (UpdateModelGroups modelGroups) ->
            ( { state | modelGroups = modelGroups }, none )

        Receive (UpdateActiveModel activeModel) ->
            ( { state | activeModel = activeModel }, none )

        Send message ->
            -- ( state, send message ) -- TODO: check if this is necessary
            ( state, none )

        SendSpecial tag ->
            ( state, sendSpecial tag )

        ToggleViewInfo ->
            ( { state | viewInfoVisible = not state.viewInfoVisible }, none )

        ToggleModelInfo ->
            ( { state | modelInfoVisible = not state.modelInfoVisible }, none )

-- translate a string command ('mode' field) from an incoming packet from JS into an Elm message
decodeMode : Maybe String -> Maybe Mode
decodeMode maybeEncoded =
    case maybeEncoded of
        Nothing ->
            Nothing

        Just encoded ->
            case encoded of
                "GetRoute" ->
                    Just GetRoute

                "GetRouteFromGoogle" ->
                    Just GetRouteFromGoogle

                _ ->
                    Debug.crash ("Invalid mode: " ++ toString encoded)

-- translate a string command ('message' field) from incoming packet from JS into an Elm message
decodeIncomingMessage : Maybe EncodedIncomingMessage -> Msg
decodeIncomingMessage maybeEncoded =
    case maybeEncoded of
        Nothing ->
            Idle

        Just encoded ->
            case encoded.tag of
                "UpdateMode" ->
                    Receive (UpdateMode (decodeMode encoded.mode))

                "UpdateLoadingProgress" ->
                    Receive (UpdateLoadingProgress encoded.loadingProgress)

                "UpdateHighlightedFeature" ->
                    Receive (UpdateHighlightedFeature encoded.feature)

                "UpdateSelectedFeature" ->
                    Receive (UpdateSelectedFeature encoded.feature)

                "UpdateRoutes" ->
                    Receive (UpdateRoutes encoded.routes)

                "UpdateAdjustment" ->
                    Receive (UpdateAdjustment encoded.adjustment)

                "UpdateViewGroups" ->
                    Receive (UpdateViewGroups encoded.viewGroups)

                "UpdateActiveViews" ->
                    Receive (UpdateActiveViews encoded.activeViews)

                "UpdateModelGroups" ->
                    Receive (UpdateModelGroups encoded.modelGroups)

                "UpdateActiveModel" ->
                    Receive (UpdateActiveModel encoded.activeModel)

                _ ->
                    Debug.crash ("Invalid incoming message: " ++ toString encoded)



-- -- Elm 0.16
-- port incomingMessage : Signal (Maybe EncodedIncomingMessage)
-- incomingAction : Signal Action
-- incomingAction =
--   Signal.map decodeIncomingMessage incomingMessage
-- -- Elm 0.18
-- TODO: refactor this for Elm 0.18. necessary for user-initiated changes to the map display.

-- on an incoming action, decode the action's message for Elm use
incomingAction : Sub Msg
incomingAction =
    incomingMessage decodeIncomingMessage

-- on subscription update, run incomingAction
handleSubs : State -> Sub Msg
handleSubs model =
    incomingAction

-- translates a mode type into a string, if there was a mode in the input
encodeMode : Maybe Mode -> Maybe String
encodeMode maybeMode =
    case maybeMode of
        Nothing ->
            Nothing

        Just mode ->
            case mode of
                GetRoute ->
                    Just "GetRoute"

                GetRouteFromGoogle ->
                    Just "GetRouteFromGoogle"

-- turns a string into an elm-object representing an outgoing message by pairing it with a tag object
encodeMessage : String -> EncodedOutgoingMessage
encodeMessage tag =
    { tag = tag
    , strings = []
    }

-- turns a list of strings into a particular elm-object representing an outgoing message
encodeStringsMessage : String -> List String -> EncodedOutgoingMessage
encodeStringsMessage tag strings =
    let
        base =
            encodeMessage tag
    in
    { base | strings = strings }

-- turns a string into a length-1 list of strings and encodes it
encodeStringMessage : String -> Maybe String -> EncodedOutgoingMessage
encodeStringMessage tag maybeString =
    case maybeString of
        Nothing ->
            encodeStringsMessage tag []

        Just string ->
            encodeStringsMessage tag [ string ]

-- prepares and encodes a string identified as a mode-type message
encodeModeMessage : String -> Maybe Mode -> EncodedOutgoingMessage
encodeModeMessage tag mode =
    encodeStringMessage tag (encodeMode mode)

-- encodes Elm messages into particular formats depending on input type
encodeOutgoingMessage : OutgoingMessage -> EncodedOutgoingMessage
encodeOutgoingMessage message =
    case message of
        SetMode mode ->
            encodeModeMessage "SetMode" mode

        HighlightFeatureByTOID toid ->
            encodeStringMessage "HighlightFeatureByTOID" toid

        SelectFeatureByTOID toid ->
            encodeStringMessage "SelectFeatureByTOID" toid

        DeleteSelectedFeature ->
            encodeMessage "DeleteSelectedFeature"

        UndeleteSelectedFeature ->
            encodeMessage "UndeleteSelectedFeature"

        ClearRoutes ->
            encodeMessage "ClearRoutes"

        ClearAdjustment ->
            encodeMessage "ClearAdjustment"

        ChooseViews names ->
            encodeStringsMessage "ChooseViews" names

        ChooseModel name ->
            encodeStringMessage "ChooseModel" (Just name)



-- -- packages the elm command and sends it back to JS
-- outgoingMessageMailbox : Mailbox (Maybe EncodedOutgoingMessage)
-- outgoingMessageMailbox =
--     Signal.mailbox Nothing
-- send : OutgoingMessage -> Cmd Msg
-- send message =
--     let
--         maybeEncoded =
--             Just (encodeOutgoingMessage message)
--     in
--     Cmd.task
--         (Signal.send outgoingMessageMailbox.address maybeEncoded
--             |> andThen (\_ -> Task.succeed Idle)
--         )

-- translates two particular messages into strings
encodeSpecialOutgoingMessage : SpecialOutgoingMessage -> String
encodeSpecialOutgoingMessage message =
    case message of
        SaveRoutesAsJSON ->
            "SaveRoutesAsJSON"

        SaveAdjustmentAsJSON ->
            "SaveAdjustmentAsJSON"


sendSpecial : SpecialOutgoingMessage -> Cmd Msg
sendSpecial message =
    -- let
    --     encoded =
    --         encodeSpecialOutgoingMessage message
    -- in
    -- Cmd.task
    --     (Special.send encoded
    --         |> andThen (\_ -> Task.succeed Idle)
    --     )
    Cmd.none

-- initializes the state of the Elm session
init : ( State, Cmd Msg )
init =
    -- ( defaultState, Idle )
    ( defaultState, none )



-- -- Elm 0.16
-- ui : App State
-- ui =
--     StartApp.start
--         { init = init
--         , update = update
--         , view = renderUI
--         , inputs = [ incomingAction ]
--         }
--
--
-- port tasks : Task Never () -> Cmd msg
--
--
-- main : Signal Html
-- main =
--     ui.html
-- -- Elm 0.18
-- -- the new format of the init/main commands seems to make this section unnecessary

-- starts the Elm session
main : Program Never State Msg
main =
    Html.program
        { init = init
        , update = update
        , view = renderUI
        , subscriptions = handleSubs
        }
