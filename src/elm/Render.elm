module Render exposing (..)

import Html exposing (Html, a, code, div, hr, pre, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Html.MoreEvents exposing (..)
import List.Extra as List
import Types exposing (..)

-- translate current entire state into an HTML div element with all UI submodules
renderUI : State -> Html Msg
renderUI state =
    div []
        [ div []
            [ lazy renderLoadingProgress state.loadingProgress
            ]
        , div [ id "ui-top-left" ]
            [ lazy3 renderViewsWindow state.viewGroups state.activeViews state.viewInfoVisible
            , lazy2 renderViewInfoWindow state.activeViews state.viewInfoVisible
            ]
        , div [ id "ui-top-right" ]
            [ lazy3 renderModelsWindow state.modelGroups state.activeModel state.modelInfoVisible
            , lazy2 renderModelInfoWindow state.activeModel state.modelInfoVisible
            , lazy renderAdjustmentWindow state.adjustment
            ]
        , div [ id "ui-bottom-left" ]
            [ lazy2 (renderFeature "highlighted") state.mode state.highlightedFeature
            ]
        , div [ id "ui-bottom-right" ]
            [ lazy2 (renderFeature "selected") state.mode state.selectedFeature
            , lazy renderRoutesWindow state.routes
            ]
        ]

-- translate current loading progress into a div element loading bar
renderLoadingProgress : Float -> Html Msg
renderLoadingProgress loadingProgress =
    let
        opacity =
            if loadingProgress == 100 then
                [ style [ ( "opacity", "0" ) ] ]
            else
                []
    in
    div ([ id "ui-loading-progress-track" ] ++ opacity)
        [ div
            [ id "ui-loading-progress-bar"
            , style [ ( "width", toString loadingProgress ++ "%" ) ]
            ]
            []
        ]

-- apply the relevant conditions (highlighted/is set to display) and 
-- calls the relevant method to render a road/road node/road link/route
renderFeature : String -> Maybe Mode -> Maybe Feature -> Html Msg
renderFeature featureKind maybeMode maybeFeature =
    let
        featureId =
            if featureKind == "highlighted" then
                "ui-highlighted-feature"
            else
                "ui-selected-feature"

        titlePrefix =
            if featureKind == "highlighted" then
                "Highlighted"
            else
                "Selected"

        display =
            if maybeFeature == Nothing then
                [ style [ ( "display", "none" ) ] ]
            else
                []

        contents =
            case maybeFeature of
                Nothing ->
                    []

                Just feature ->
                    case ( feature.tag, feature.roadNode, feature.roadLink, feature.road, feature.route ) of
                        ( "roadNode", Just roadNode, Nothing, Nothing, Nothing ) ->
                            renderRoadNode maybeMode titlePrefix roadNode

                        ( "roadLink", Nothing, Just roadLink, Nothing, Nothing ) ->
                            renderRoadLink titlePrefix roadLink

                        ( "road", Nothing, Nothing, Just road, Nothing ) ->
                            renderRoad titlePrefix road

                        ( "route", Nothing, Nothing, Nothing, Just route ) ->
                            renderRoute titlePrefix route

                        _ ->
                            []
    in
    div ([ id featureId, class "ui-window" ] ++ display) contents

-- render road node into the div and changes the UI elements (title, info display, UI options) as
-- needed depending on if the current node is highlighted, deleted, undeletable, or none mentioned 
renderRoadNode : Maybe Mode -> String -> RoadNode -> List (Html Msg)
renderRoadNode maybeMode titlePrefix roadNode =
    let
        title =
            [ renderWindowTitle (titlePrefix ++ " Node") ]

        description =
            case roadNode.address of
                Nothing ->
                    []

                Just address ->
                    [ renderWindowSubtitle address ]

        buttons =
            case ( roadNode.isDeleted, roadNode.isUndeletable ) of
                ( False, _ ) ->
                    renderButtons
                        [ renderToggle2 "Get Route"
                            (maybeMode == Just GetRoute)
                            (Send (SetMode (Just GetRoute)))
                            (Send (SetMode Nothing))
                        , renderAction "Delete" (Send DeleteSelectedFeature)
                        ]
                        ++ renderButtons
                            [ renderToggle2 "Get Route from Google"
                                (maybeMode == Just GetRouteFromGoogle)
                                (Send (SetMode (Just GetRouteFromGoogle)))
                                (Send (SetMode Nothing))
                            ]

                ( True, True ) ->
                    renderButtons
                        [ renderAction "Undelete" (Send UndeleteSelectedFeature)
                        ]

                _ ->
                    []

        location =
            case roadNode.point of
                ( x, y ) ->
                    renderLabeled "Location" [ div [] [ text (toString (round x) ++ " " ++ toString (round y)) ] ]

        toid =
            renderLabeled "Unique ID" [ renderTOID roadNode.toid ]

        roadLinks =
            renderLabeledList "Links" renderTOIDItem roadNode.roadLinkTOIDs
    in
    title ++ description ++ buttons ++ location ++ toid ++ roadLinks

-- render road link into the div and changes the UI elements (title, info display, UI options) as
-- needed depending on if the current link is highlighted, deleted, undeletable, or none mentioned 
renderRoadLink : String -> RoadLink -> List (Html Msg)
renderRoadLink titlePrefix roadLink =
    let
        title =
            [ renderWindowTitle (titlePrefix ++ " Link") ]

        description =
            [ renderWindowSubtitle (renderRoadLinkDescription roadLink) ]

        buttons =
            case ( roadLink.isDeleted, roadLink.isUndeletable ) of
                ( False, _ ) ->
                    renderButtons
                        [ renderAction "Delete" (Send DeleteSelectedFeature)
                        ]

                ( True, True ) ->
                    renderButtons
                        [ renderAction "Undelete" (Send UndeleteSelectedFeature)
                        ]

                _ ->
                    []

        cost =
            renderLabeled "Cost" [ div [] [ text (toString (round roadLink.length) ++ " × " ++ toString roadLink.penalty) ] ]

        toid =
            renderLabeled "Unique ID" [ renderTOID roadLink.toid ]

        roadNodes =
            case ( roadLink.negativeNodeTOID, roadLink.positiveNodeTOID ) of
                ( Nothing, Nothing ) ->
                    []

                ( Just negativeNodeTOID, Nothing ) ->
                    renderLabeled "Nodes"
                        [ div [] [ text "− ", renderTOID negativeNodeTOID ]
                        ]

                ( Nothing, Just positiveNodeTOID ) ->
                    renderLabeled "Nodes"
                        [ div [] [ text "+ ", renderTOID positiveNodeTOID ]
                        ]

                ( Just negativeNodeTOID, Just positiveNodeTOID ) ->
                    renderLabeled "Nodes"
                        [ div [] [ text "− ", renderTOID negativeNodeTOID ]
                        , div [] [ text "+ ", renderTOID positiveNodeTOID ]
                        ]

        roads =
            renderLabeledList "Link Grouping" renderRoadItem roadLink.roads
    in
    title ++ description ++ buttons ++ cost ++ toid ++ roadNodes ++ roads

-- formats the string describing a road link
renderRoadLinkDescription : RoadLink -> String
renderRoadLinkDescription roadLink =
    roadLink.term ++ ", " ++ roadLink.nature

-- create div with road ID and description
renderRoadItem : Road -> Html Msg
renderRoadItem road =
    let
        toid =
            [ renderTOIDItem road.toid ]

        description =
            [ div [] [ text (renderRoadDescription road) ] ]
    in
    div [] (toid ++ description)

-- render road link into the div and changes the UI elements (title, info display, UI options)
-- as needed depending on if the current link is highlighted, deleted, or neither 
renderRoad : String -> Road -> List (Html Msg)
renderRoad titlePrefix road =
    let
        title =
            [ renderWindowTitle (titlePrefix ++ " Road") ]

        description =
            [ renderWindowSubtitle (renderRoadDescription road) ]

        buttons =
            case road.isDeleted of
                False ->
                    renderButtons
                        [ renderAction "Delete" (Send DeleteSelectedFeature)
                        ]

                True ->
                    renderButtons
                        [ renderAction "Undelete" (Send UndeleteSelectedFeature)
                        ]

        toid =
            renderLabeled "Unique ID" [ renderTOID road.toid ]

        roadLinks =
            renderLabeledList "Road Links" renderTOIDItem road.roadLinkTOIDs
    in
    title ++ description ++ buttons ++ toid ++ roadLinks

-- render route and associated nodes and links into the div and changes the UI elements 
-- (title, info display, UI options) as needed depending on if the current link is 
-- highlighted, deleted, or neither 
renderRoute : String -> Route -> List (Html Msg)
renderRoute titlePrefix route =
    let
        title =
            [ renderWindowTitle (titlePrefix ++ " Route") ]

        buttons =
            renderButtons
                [ renderAction "Delete" (Send DeleteSelectedFeature)
                ]

        toid =
            renderLabeled "Unique ID" [ renderTOID route.toid ]

        roadNodes =
            renderLabeled "Nodes"
                [ div [] [ text "< ", renderTOID route.startNodeTOID ]
                , div [] [ text "> ", renderTOID route.endNodeTOID ]
                ]

        roadLinks =
            renderLabeledList "Links" renderTOIDItem route.roadLinkTOIDs
    in
    title ++ buttons ++ toid ++ roadNodes ++ roadLinks

-- formats the string describing a road depending on if it has an associated term or not
renderRoadDescription : Road -> String
renderRoadDescription road =
    case road.term of
        Nothing ->
            road.name ++ ", " ++ road.group

        Just term ->
            road.name ++ ", " ++ road.group ++ ", " ++ term

-- return the active components in the Views UI menu as a div
renderViewsWindow : List ViewGroup -> List View -> Bool -> Html Msg
renderViewsWindow viewGroups activeViews viewInfoVisible =
    let
        allViews =
            List.concatMap .views viewGroups

        findActiveIndex =
            List.findIndex (\view -> List.member view activeViews)

        maybeStart =
            findActiveIndex allViews

        maybeEnd =
            findActiveIndex (List.reverse allViews)

        extendChoice index view =
            case ( maybeStart, maybeEnd ) of
                ( Just start, Just end ) ->
                    let
                        targetStart =
                            min index start

                        reverseEnd =
                            List.length allViews - end - 1

                        targetEnd =
                            max index reverseEnd

                        targetCount =
                            targetEnd - targetStart + 1
                    in
                    List.map .name (List.take targetCount (List.drop targetStart allViews))

                _ ->
                    [ view.name ]

        renderViewGroupItem groupIndex index view =
            let
                isActive =
                    List.member view activeViews

                choose names =
                    Send (ChooseViews names)

                alone =
                    choose [ view.name ]

                extended =
                    choose (extendChoice (groupIndex + index) view)

                toggled =
                    if isActive then
                        choose (List.filter ((/=) view.name) (List.map .name activeViews))
                    else
                        choose (view.name :: List.map .name activeViews)

                handler =
                    onClickWithModifiers alone extended toggled alone toggled

                active =
                    if isActive then
                        [ class "ui-active" ]
                    else
                        []
            in
            a ([ handler ] ++ active) [ text view.name ]

        renderViewGroup viewGroup =
            case viewGroup.views of
                view0 :: _ ->
                    case List.findIndex ((==) view0) allViews of
                        Just groupIndex ->
                            renderLabeledChoices viewGroup.name
                                (List.indexedMap (renderViewGroupItem groupIndex) viewGroup.views)

                        _ ->
                            []

                _ ->
                    []

        contents =
            [ renderWindowTitle "Views" ]
                ++ renderButtons
                    [ renderToggle "Show Info" viewInfoVisible ToggleViewInfo
                    ]
                ++ List.concatMap renderViewGroup viewGroups
    in
    div [ class "ui-window" ] contents

-- return the active components in the Models UI menu as a div
renderModelsWindow : List ModelGroup -> Maybe Model -> Bool -> Html Msg
renderModelsWindow modelGroups activeModel modelInfoVisible =
    let
        renderModelGroupItem model =
            let
                isActive =
                    activeModel == Just model

                active =
                    if isActive then
                        [ class "ui-active" ]
                    else
                        []
            in
            a ([ onClick (Send (ChooseModel model.name)) ] ++ active) [ text model.name ]

        renderModelGroup modelGroup =
            renderLabeledChoices modelGroup.name
                (List.map renderModelGroupItem modelGroup.models)

        contents =
            [ renderWindowTitle "Models" ]
                ++ renderButtons
                    [ renderToggle "Show Info" modelInfoVisible ToggleModelInfo
                    ]
                ++ List.concatMap renderModelGroup modelGroups
    in
    div [ class "ui-window" ] contents

-- wrap definitions into a list of list of list of html messages
renderDefinition : String -> List (Html Msg)
renderDefinition lambda =
    renderLabeled "Definition"
        [ pre [] [ code [] [ text lambda ] ] ]

-- return the active components in the View Info UI menu as a div
renderViewInfoWindow : List View -> Bool -> Html Msg
renderViewInfoWindow activeViews visible =
    let
        renderViewInfo view =
            [ renderWindowSubtitle view.name ]
                ++ renderDefinition view.lambda

        display =
            if activeViews == [] || not visible then
                [ style [ ( "display", "none" ) ] ]
            else
                []

        contents =
            [ renderWindowTitle "View Info" ]
                ++ List.concatMap renderViewInfo activeViews
    in
    div ([ class "ui-window wide" ] ++ display) contents

-- translate an rgba code into a div for the UI with styling
renderColor : Maybe RGBA -> String -> List (Html Msg)
renderColor maybeRGBA name =
    case maybeRGBA of
        Just ( r, g, b, a ) ->
            let
                rgba =
                    "rgba("
                        ++ toString r
                        ++ ","
                        ++ toString g
                        ++ ","
                        ++ toString b
                        ++ ","
                        ++ toString a
                        ++ ")"
            in
            [ div []
                [ div [ class ("ui-color " ++ rgba), style [ ( "background-color", rgba ) ] ] []
                , text name
                ]
            ]

        _ ->
            []

-- create a legend based on the colors rendered
renderModelColors : Maybe ModelColors -> List (Html Msg)
renderModelColors maybeColors =
    case maybeColors of
        Just colors ->
            renderLabeled "Legend"
                (renderColor colors.min "min"
                    ++ renderColor colors.max "max"
                    ++ renderColor colors.out "out"
                )

        _ ->
            []

-- create a text div displaying the range of the model
renderModelRange : Maybe ModelRange -> List (Html Msg)
renderModelRange maybeRange =
    case maybeRange of
        Just range ->
            renderLabeled "Range"
                [ div [] [ text (toString range.min ++ " " ++ toString range.max) ] ]

        _ ->
            []

-- render div with model info, displaying it or not depending on input param
renderModelInfoWindow : Maybe Model -> Bool -> Html Msg
renderModelInfoWindow activeModel visible =
    let
        renderModelInfo =
            case activeModel of
                Just model ->
                    [ renderWindowSubtitle model.name ]
                        ++ renderDefinition model.lambda
                        ++ renderModelRange model.range
                        ++ renderModelColors model.colors

                _ ->
                    []

        display =
            if activeModel == Nothing || not visible then
                [ style [ ( "display", "none" ) ] ]
            else
                []

        contents =
            [ renderWindowTitle "Model Info" ]
                ++ renderModelInfo
    in
    div ([ class "ui-window wide" ] ++ display) contents

-- if there are any routes, render a UI menu listing valid and invalid routes
-- giving the option to clear or save the routes 
renderRoutesWindow : List Route -> Html Msg
renderRoutesWindow routes =
    let
        display =
            if routes == [] then
                [ style [ ( "display", "none" ) ] ]
            else
                []

        contents =
            if routes == [] then
                []
            else
                let
                    validRoutes =
                        case List.filter (\route -> route.roadLinkTOIDs /= []) routes of
                            [] ->
                                []

                            validList ->
                                renderRoutes "Valid" validList

                    invalidRoutes =
                        case List.filter (\route -> route.roadLinkTOIDs == []) routes of
                            [] ->
                                []

                            invalidList ->
                                renderRoutes "Invalid" invalidList
                in
                [ renderWindowTitle "Routes" ]
                    ++ renderButtons
                        [ renderAction "Clear" (Send ClearRoutes)
                        , renderAction "Save as JSON" (SendSpecial SaveRoutesAsJSON)
                        ]
                    ++ validRoutes
                    ++ invalidRoutes
    in
    div ([ class "ui-window" ] ++ display) contents

-- bundle the routes into a list, render its TOIDs, and label it  
renderRoutes : String -> List Route -> List (Html Msg)
renderRoutes label routes =
    renderLabeledList label renderTOIDItem (List.map .toid routes)

-- if adjusting, render a UI menu listing deleted nodes, links, and roads,
-- giving the option to clear or save as JSON
renderAdjustmentWindow : Maybe Adjustment -> Html Msg
renderAdjustmentWindow maybeAdjustment =
    let
        isEmpty =
            case maybeAdjustment of
                Nothing ->
                    True

                Just adjustment ->
                    if adjustment.itemCount == 0 then
                        True
                    else
                        False

        display =
            if isEmpty then
                [ style [ ( "display", "none" ) ] ]
            else
                []

        contents =
            case maybeAdjustment of
                Nothing ->
                    []

                Just adjustment ->
                    [ renderWindowTitle "Adjustment" ]
                        ++ renderButtons
                            [ renderAction "Clear" (Send ClearAdjustment)
                            , renderAction "Save as JSON" (SendSpecial SaveAdjustmentAsJSON)
                            ]
                        ++ [ div []
                                (renderLabeledList "Deleted Nodes" renderTOIDItem adjustment.deletedFeatures.roadNodeTOIDs
                                    ++ renderLabeledList "Deleted Links" renderTOIDItem adjustment.deletedFeatures.roadLinkTOIDs
                                    ++ renderLabeledList "Deleted Roads" renderTOIDItem adjustment.deletedFeatures.roadTOIDs
                                )
                           ]
    in
    div ([ class "ui-window" ] ++ display) contents

-- wrap passed param into a div that will title the ui window
renderWindowTitle : String -> Html Msg
renderWindowTitle title =
    div [ class "ui-window-title" ] [ text title ]

-- wrap passed param into a div that will subtitle the ui window
renderWindowSubtitle : String -> Html Msg
renderWindowSubtitle subtitle =
    div [ class "ui-window-subtitle" ] [ text subtitle ]

-- wapped passed param into a div that will have a rendered TOID
renderTOIDItem : String -> Html Msg
renderTOIDItem toid =
    div [] [ renderTOID toid ]

-- attach functions to an html msg that will select feature by ID on click
-- and highlight feature on mousein and unhighlight mouseout
renderTOID : String -> Html Msg
renderTOID toid =
    a
        [ onClick (Send (SelectFeatureByTOID (Just toid)))
        , onMouseEnter (Send (HighlightFeatureByTOID (Just toid)))
        , onMouseLeave (Send (HighlightFeatureByTOID Nothing))
        ]
        [ text toid ]

-- translate a list of strings into several label divs
renderLabeledList : String -> (a -> Html Msg) -> List a -> List (Html Msg)
renderLabeledList label render items =
    let
        itemCount =
            List.length items

        fullLabel =
            if itemCount > 2 then
                label ++ " (" ++ toString itemCount ++ ")"
            else
                label
    in
    case items of
        [] ->
            []

        _ ->
            renderLabeled fullLabel [ div [] (List.map render items) ]

-- renders a list of labels
renderLabeled : String -> List (Html Msg) -> List (Html Msg)
renderLabeled label contents =
    [ renderLabel label ] ++ contents

-- render a single label into ui-label div
renderLabel : String -> Html Msg
renderLabel label =
    div [ class "ui-label" ] [ text label ]

-- pair label and choice divs after creating them
renderLabeledChoices : String -> List (Html Msg) -> List (Html Msg)
renderLabeledChoices label actions =
    case actions of
        [] ->
            []

        _ ->
            renderLabeled label (renderChoices actions)

-- create a list of divs out of a list of ui-choices 
renderChoices : List (Html Msg) -> List (Html Msg)
renderChoices actions =
    [ div [ class "ui-choices" ] (List.map (\action -> action) actions) ]

-- create a list of divs out of a list of ui-buttons
renderButtons : List (Html Msg) -> List (Html Msg)
renderButtons actions =
    [ div [ class "ui-buttons" ] (List.map (\action -> action) actions) ]

-- put a label and an action handler into an elm html msg
renderAction : String -> Msg -> Html Msg
renderAction label action =
    a [ onClick action ] [ text label ]

-- returns an onClick function with/without a class depending on the isActive boolean
renderToggle : String -> Bool -> Msg -> Html Msg
renderToggle label isActive action =
    let
        attrs =
            if isActive then
                [ onClick action, class "ui-active" ]
            else
                [ onClick action ]
    in
    a attrs [ text label ]

-- returns one onClick function or another with/without a class
-- depending on if the given boolean param isActive
renderToggle2 : String -> Bool -> Msg -> Msg -> Html Msg
renderToggle2 label isActive action activeAction =
    let
        attrs =
            if isActive then
                [ onClick activeAction, class "ui-active" ]
            else
                [ onClick action ]
    in
    a attrs [ text label ]
