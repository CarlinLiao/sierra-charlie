module Render where

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Lazy exposing (lazy, lazy2)
import List.Extra as List

import Html.MoreEvents exposing (..)
import Types exposing (..)


type alias Trigger =
    Signal.Address Action


renderUI : Trigger -> State -> Html
renderUI trigger state =
    div []
      [ div []
          [ lazy renderLoadingProgress state.loadingProgress
          , lazy2 (renderFeature trigger "highlighted") state.mode state.highlightedFeature
          , lazy2 (renderFeature trigger "selected") state.mode state.selectedFeature
          ]
      , div [id "ui-windows-top-left"]
          [ lazy2 (renderViewsWindow trigger) state.viewGroups state.activeViews
          ]
      , div [id "ui-windows-top-right"]
          [ lazy (renderAdjustmentWindow trigger) state.adjustment
          , lazy (renderRoutesWindow trigger) state.routes
          ]
      ]


renderLoadingProgress : Float -> Html
renderLoadingProgress loadingProgress =
    let
      opacity =
        if loadingProgress == 100
          then [style [("opacity", "0")]]
          else []
    in
      div ([id "ui-loading-progress-track"] ++ opacity)
        [ div
            [ id "ui-loading-progress-bar"
            , style [("width", toString loadingProgress ++ "%")]
            ]
            []
        ]


renderFeature : Trigger -> String -> Maybe Mode -> Maybe Feature -> Html
renderFeature trigger featureKind maybeMode maybeFeature =
    let
      featureId =
        if featureKind == "highlighted"
          then "ui-highlighted-feature"
          else "ui-selected-feature"
      titlePrefix =
        if featureKind == "highlighted"
          then "Highlighted"
          else "Selected"
      display =
        if maybeFeature == Nothing
          then [style [("display", "none")]]
          else []
      contents =
        case maybeFeature of
          Nothing ->
            []
          Just feature ->
            case (feature.tag, feature.roadNode, feature.roadLink, feature.road, feature.route) of
              ("roadNode", Just roadNode, Nothing, Nothing, Nothing) ->
                renderRoadNode trigger maybeMode titlePrefix roadNode
              ("roadLink", Nothing, Just roadLink, Nothing, Nothing) ->
                renderRoadLink trigger titlePrefix roadLink
              ("road", Nothing, Nothing, Just road, Nothing) ->
                renderRoad trigger titlePrefix road
              ("route", Nothing, Nothing, Nothing, Just route) ->
                renderRoute trigger titlePrefix route
              _ ->
                []
    in
      div ([id featureId, class "ui-window"] ++ display) contents


renderRoadNode : Trigger -> Maybe Mode -> String -> RoadNode -> List Html
renderRoadNode trigger maybeMode titlePrefix roadNode =
    let
      title =
        [renderWindowTitle (titlePrefix ++ " Road Node")]
      description =
        case roadNode.address of
          Nothing ->
            []
          Just address ->
            [div [] [text address]]
      buttons =
        case (roadNode.isDeleted, roadNode.isUndeletable) of
          (False, _) ->
            ( renderButtons trigger
                [ renderToggle2 "Get Route" (maybeMode == Just GetRoute)
                    (Send (SetMode (Just GetRoute)))
                    (Send (SetMode Nothing))
                , renderAction "Delete" (Send DeleteSelectedFeature)
                ] ++
              renderButtons trigger
                [ renderToggle2 "Get Route from Google" (maybeMode == Just GetRouteFromGoogle)
                    (Send (SetMode (Just GetRouteFromGoogle)))
                    (Send (SetMode Nothing))
                ]
            )
          (True, True) ->
            renderButtons trigger
              [ renderAction "Undelete" (Send UndeleteSelectedFeature)
              ]
          _ ->
            []
      location =
        case roadNode.point of
          (x, y) ->
            renderLabeled "Location" [text (toString (round x) ++ " " ++ toString (round y))]
      toid =
        renderLabeled "TOID" [renderTOID trigger roadNode.toid]
      roadLinks =
        renderLabeledList "Road Links" (renderTOIDItem trigger) roadNode.roadLinkTOIDs
    in
      title ++ description ++ buttons ++ location ++ toid ++ roadLinks


renderRoadLink : Trigger -> String -> RoadLink -> List Html
renderRoadLink trigger titlePrefix roadLink =
    let
      title =
        [renderWindowTitle (titlePrefix ++ " Road Link")]
      description =
        [div [] [renderRoadLinkDescription roadLink]]
      buttons =
        case (roadLink.isDeleted, roadLink.isUndeletable) of
          (False, _) ->
            renderButtons trigger
              [ renderAction "Delete" (Send DeleteSelectedFeature)
              ]
          (True, True) ->
            renderButtons trigger
              [ renderAction "Undelete" (Send UndeleteSelectedFeature)
              ]
          _ ->
            []
      cost =
        renderLabeled "Cost" [text (toString (round roadLink.length) ++ " × " ++ toString roadLink.penalty)]
      toid =
        renderLabeled "TOID" [renderTOID trigger roadLink.toid]
      roadNodes =
        case (roadLink.negativeNodeTOID, roadLink.positiveNodeTOID) of
          (Nothing, Nothing) ->
            []
          (Just negativeNodeTOID, Nothing) ->
            renderLabeled "Road Nodes"
              [ div [] [text "− ", renderTOID trigger negativeNodeTOID]
              ]
          (Nothing, Just positiveNodeTOID) ->
            renderLabeled "Road Nodes"
              [ div [] [text "+ ", renderTOID trigger positiveNodeTOID]
              ]
          (Just negativeNodeTOID, Just positiveNodeTOID) ->
            renderLabeled "Road Nodes"
              [ div [] [text "− ", renderTOID trigger negativeNodeTOID]
              , div [] [text "+ ", renderTOID trigger positiveNodeTOID]
              ]
      roads =
        renderLabeledList "Roads" (renderRoadItem trigger) roadLink.roads
    in
      title ++ description ++ buttons ++ cost ++ toid ++ roadNodes ++ roads


renderRoadLinkDescription : RoadLink -> Html
renderRoadLinkDescription roadLink =
    text (roadLink.term ++ ", " ++ roadLink.nature)


renderRoadItem : Trigger -> Road -> Html
renderRoadItem trigger road =
    let
      toid =
        [renderTOIDItem trigger road.toid]
      description =
        [div [] [renderRoadDescription road]]
    in
      div [] (toid ++ description)


renderRoad : Trigger -> String -> Road -> List Html
renderRoad trigger titlePrefix road =
    let
      title =
        [renderWindowTitle (titlePrefix ++ " Road")]
      description =
        [div [] [renderRoadDescription road]]
      buttons =
        case road.isDeleted of
          False ->
            renderButtons trigger
              [ renderAction "Delete" (Send DeleteSelectedFeature)
              ]
          True ->
            renderButtons trigger
              [ renderAction "Undelete" (Send UndeleteSelectedFeature)
              ]
      toid =
        renderLabeled "TOID" [renderTOID trigger road.toid]
      roadLinks =
        renderLabeledList "Road Links" (renderTOIDItem trigger) road.roadLinkTOIDs
    in
      title ++ description ++ buttons ++ toid ++ roadLinks


renderRoute : Trigger -> String -> Route -> List Html
renderRoute trigger titlePrefix route =
    let
      title =
        [renderWindowTitle (titlePrefix ++ " Route")]
      buttons =
        renderButtons trigger
          [ renderAction "Delete" (Send DeleteSelectedFeature)
          ]
      toid =
        renderLabeled "TOID" [renderTOID trigger route.toid]
      roadNodes =
        renderLabeled "Road Nodes"
          [ div [] [text "< ", renderTOID trigger route.startNodeTOID]
          , div [] [text "> ", renderTOID trigger route.endNodeTOID]
          ]
      roadLinks =
        renderLabeledList "Road Links" (renderTOIDItem trigger) route.roadLinkTOIDs
    in
      title ++ buttons ++ toid ++ roadNodes ++ roadLinks


renderRoadDescription : Road -> Html
renderRoadDescription road =
    case road.term of
      Nothing ->
        text (road.name ++ ", " ++ road.group)
      Just term ->
        text (road.name ++ ", " ++ road.group ++ ", " ++ term)


renderViewsWindow : Trigger -> List ViewGroup -> List String -> Html
renderViewsWindow trigger viewGroups activeViews =
    let
      contents =
        [renderWindowTitle "Views"] ++
        (List.concatMap (renderViewGroup trigger activeViews) viewGroups)
    in
      div ([class "ui-window"]) contents


renderViewGroup : Trigger -> List String -> ViewGroup -> List Html
renderViewGroup trigger activeViews viewGroup =
    let
      activeGroupViews = List.filter (\view -> List.member view viewGroup.views) activeViews
      findActiveIndex = List.findIndex (\view -> List.member view activeViews)
      maybeStart = findActiveIndex viewGroup.views
      maybeEnd = findActiveIndex (List.reverse viewGroup.views)
      extendGroupChoice index view =
          case (maybeStart, maybeEnd) of
            (Just rawStart, Just reverseRawEnd) ->
              let
                start = min index rawStart
                rawEnd = List.length viewGroup.views - reverseRawEnd - 1
                end = max index rawEnd
                count = end - start + 1
              in
                List.take count (List.drop start viewGroup.views)
            _ ->
              [view]
      renderViewGroupItem index view trigger =
          let
            isActive = List.member view activeViews
            choose views = Send (ChooseViews views)
            alone = choose [view]
            extended = choose (extendGroupChoice index view)
            toggled =
                if isActive
                  then choose (List.filter ((/=) view) activeGroupViews)
                  else choose (view :: activeGroupViews)
            handler = onClickWithModifiers trigger alone extended toggled alone toggled
            active =
              if isActive
                then [class "ui-active"]
                else []
          in
            a ([handler] ++ active) [text view]
    in
      renderLabeledChoices trigger viewGroup.name
        (List.indexedMap renderViewGroupItem viewGroup.views)


renderRoutesWindow : Trigger -> List Route -> Html
renderRoutesWindow trigger routes =
    let
      display =
        if routes == []
          then [style [("display", "none")]]
          else []
      contents =
        if routes == []
          then []
          else
            let
              validRoutes =
                case List.filter (\route -> route.roadLinkTOIDs /= []) routes of
                  [] ->
                    []
                  validList ->
                    renderRoutes trigger "Valid" validList
              invalidRoutes =
                case List.filter (\route -> route.roadLinkTOIDs == []) routes of
                  [] ->
                    []
                  invalidList ->
                    renderRoutes trigger "Invalid" invalidList
            in
              [renderWindowTitle "Routes"] ++
              renderButtons trigger
                [ renderAction "Clear" (Send ClearRoutes)
                , renderAction "Save as JSON" (SendSpecial SaveRoutesAsJSON)
                ] ++
              validRoutes ++
              invalidRoutes
    in
      div ([class "ui-window"] ++ display) contents


renderRoutes : Trigger -> String -> List Route -> List Html
renderRoutes trigger label routes =
    renderLabeledList label (renderTOIDItem trigger) (List.map .toid routes)


renderAdjustmentWindow : Trigger -> Maybe Adjustment -> Html
renderAdjustmentWindow trigger maybeAdjustment =
    let
      isEmpty =
        case maybeAdjustment of
          Nothing ->
            True
          Just adjustment ->
            if adjustment.itemCount == 0
              then True
              else False
      display =
        if isEmpty
          then [style [("display", "none")]]
          else []
      contents =
        case maybeAdjustment of
          Nothing ->
            []
          Just adjustment ->
            [renderWindowTitle "Adjustment"] ++
            renderButtons trigger
              [ renderAction "Clear" (Send ClearAdjustment)
              , renderAction "Save as JSON" (SendSpecial SaveAdjustmentAsJSON)
              ] ++
            [ div []
                ( renderLabeledList "Deleted Nodes" (renderTOIDItem trigger) adjustment.deletedFeatures.roadNodeTOIDs ++
                  renderLabeledList "Deleted Links" (renderTOIDItem trigger) adjustment.deletedFeatures.roadLinkTOIDs ++
                  renderLabeledList "Deleted Roads" (renderTOIDItem trigger) adjustment.deletedFeatures.roadTOIDs
                )
            ]
    in
      div ([class "ui-window"] ++ display) contents


renderWindowTitle : String -> Html
renderWindowTitle title =
    div [class "ui-window-title"] [text title]


renderTOIDItem : Trigger -> String -> Html
renderTOIDItem trigger toid =
    div [] [renderTOID trigger toid]


renderTOID : Trigger -> String -> Html
renderTOID trigger toid =
    a
      [ onClick trigger (Send (SelectFeatureByTOID (Just toid)))
      , onMouseEnter trigger (Send (HighlightFeatureByTOID (Just toid)))
      , onMouseLeave trigger (Send (HighlightFeatureByTOID Nothing))
      ]
      [text toid]


renderLabeledList : String -> (a -> Html) -> List a -> List Html
renderLabeledList label render items =
    let
      itemCount = List.length items
      fullLabel =
        if itemCount > 2
          then label ++ " (" ++ toString (itemCount) ++ ")"
          else label
    in
      case items of
        [] ->
          []
        _ ->
          renderLabeled fullLabel (List.map render items)


renderLabeled : String -> List Html -> List Html
renderLabeled label contents =
    [div [] ([renderLabel label] ++ contents)]


renderLabel : String -> Html
renderLabel label =
    div [class "ui-label"] [text label]


renderLabeledChoices : Trigger -> String -> List (Trigger -> Html) -> List Html
renderLabeledChoices trigger label actions =
    case actions of
      [] ->
        []
      _ ->
        renderLabeled label (renderChoices trigger actions)


renderChoices : Trigger -> List (Trigger -> Html) -> List Html
renderChoices trigger actions =
    [div [class "ui-choices"] (List.map (\action -> action trigger) actions)]


renderButtons : Trigger -> List (Trigger -> Html) -> List Html
renderButtons trigger actions =
    [div [class "ui-buttons"] (List.map (\action -> action trigger) actions)]


renderAction : String -> Action -> Trigger -> Html
renderAction label action trigger =
    a [onClick trigger action] [text label]


renderToggle : String -> Bool -> Action -> Trigger -> Html
renderToggle label isActive action trigger =
    let
      attrs =
        if isActive
          then [onClick trigger action, class "ui-active"]
          else [onClick trigger action]
    in
      a attrs [text label]


renderToggle2 : String -> Bool -> Action -> Action -> Trigger -> Html
renderToggle2 label isActive action activeAction trigger =
    let
      attrs =
        if isActive
          then [onClick trigger activeAction, class "ui-active"]
          else [onClick trigger action]
    in
      a attrs [text label]
