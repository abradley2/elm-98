module DesktopElement exposing (..)

import ComponentResult exposing (ComponentResult, withCmd, withExternalMsg, withModel)
import ComponentResult.Effect exposing (withEffect, resolveEffects)
import Css exposing (..)
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json
import Json.Decode as D
import Theme
import UUID exposing (UUID)
import Window


type Effect
    = EffCmd (Cmd Msg)
    | EffBatch (List Effect)


runEffect : Effect -> Cmd Msg
runEffect eff =
    case eff of
        EffCmd cmd ->
            cmd

        EffBatch effects ->
            Cmd.batch <| List.map runEffect effects


type ExtMsg
    = FolderDoubleClicked DesktopElement


type Msg
    = NoOp
    | ElementMouseDown Json.ClickPosition
    | ElementDragStarted DesktopElement Json.ClickPosition
    | ElementDragEnded DesktopElement Json.ClickPosition
    | WindowMsg Window.Msg
    | ExtMsg ExtMsg


type DesktopElementData
    = Folder
    | File
    | Window ( Int, Int )


type alias DesktopElement =
    { name : String
    , id : UUID
    , position : ( Int, Int )
    , dataType : DesktopElementData
    , clickPosition : Maybe Json.ClickPosition
    }


update : Msg -> DesktopElement -> ComponentResult DesktopElement Msg ExtMsg Never
update msg model =
    update_ msg model |> resolveEffects runEffect 

update_ : Msg -> DesktopElement -> ComponentResult ( DesktopElement, Effect ) Msg ExtMsg Never
update_ msg model =
    case ( msg, model.dataType ) of
        ( ExtMsg extMsg, _ ) ->
            model
                |> withModel
                |> withExternalMsg extMsg
                |> withEffect (EffCmd Cmd.none)

        ( ElementMouseDown clickPosition, _ ) ->
            { model | clickPosition = Just clickPosition }
                |> withModel
                |> withEffect (EffCmd Cmd.none)

        ( ElementDragStarted element clickPosition, _ ) ->
            model
                |> withModel
                |> withEffect (EffCmd Cmd.none)

        ( ElementDragEnded element clickPosition, _ ) ->
            let
                ( withOffsetX, withOffsetY ) =
                    model.clickPosition
                        |> Maybe.map
                            (\offsetPos ->
                                ( clickPosition.clientX - offsetPos.offsetX
                                , clickPosition.clientY - offsetPos.offsetY
                                )
                            )
                        |> Maybe.withDefault ( clickPosition.clientX, clickPosition.clientY )

                nextElement =
                    { element
                        | position = ( withOffsetX, withOffsetY )
                    }
            in
            nextElement
                |> withModel
                |> withEffect (EffCmd Cmd.none)

        _ ->
            model
                |> withModel
                |> withEffect (EffCmd Cmd.none)


view : (Msg -> a) -> DesktopElement -> H.Html a
view toMsg element =
    H.map toMsg <|
        case element.dataType of
            Folder ->
                folderIconView element

            File ->
                fileIconView element

            Window ( windowWidth, windowHeight ) ->
                Window.view WindowMsg
                    { windowWidth = windowWidth
                    , windowHeight = windowHeight
                    }


folderIconView : DesktopElement -> H.Html Msg
folderIconView element =
    let
        ( xPos, yPos ) =
            element.position
    in
    H.div
        (draggableAttributes element
            [ A.css
                [ backgroundColor Theme.manilla
                , width (px 80)
                , height (px 80)
                , position absolute
                , top (px <| toFloat yPos)
                , left (px <| toFloat xPos)
                ]
            , A.id (UUID.toString element.id)
            , E.stopPropagationOn "contextmenu" (D.succeed ( NoOp, True ))
            , E.onDoubleClick (ExtMsg <| FolderDoubleClicked element)
            , E.on "mousedown" (D.map ElementMouseDown Json.decodeClickPosition)
            ]
        )
        []


fileIconView : DesktopElement -> H.Html Msg
fileIconView element =
    let
        ( xPos, yPos ) =
            element.position
    in
    H.div
        (draggableAttributes element
            [ A.css
                [ backgroundColor Theme.manilla
                , width (px 80)
                , height (px 80)
                , position absolute
                , top (px <| toFloat yPos)
                , left (px <| toFloat xPos)
                ]
            , A.id (UUID.toString element.id)
            , E.stopPropagationOn "contextmenu" (D.succeed ( NoOp, True ))
            ]
        )
        []


draggableAttributes : DesktopElement -> List (H.Attribute Msg) -> List (H.Attribute Msg)
draggableAttributes element =
    (++)
        [ A.draggable "true"
        , E.on "dragstart" <|
            D.map
                (ElementDragStarted element)
                Json.decodeClickPosition
        , E.on "dragend" <|
            D.map
                (ElementDragEnded element)
                Json.decodeClickPosition
        ]
