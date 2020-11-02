module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onResize)
import ComponentResult exposing (ComponentResult, resolve, withCmd, withExternalMsg, withModel)
import ComponentResult.Effect exposing (resolveAll, resolveEffects, withEffect)
import Css exposing (..)
import Html
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as D
import Task
import Theme


type Effect
    = EffectNone
    | QueryViewport
    | Batch (List Effect)


runEffect : Effect -> Cmd Msg
runEffect effect =
    case effect of
        QueryViewport ->
            Task.perform RecievedViewport getViewport

        EffectNone ->
            Cmd.none

        Batch effectList ->
            List.map runEffect effectList
                |> Cmd.batch


type alias Flags =
    {}


type alias Model =
    { viewport : Maybe Viewport
    , contextMenuPosition : Maybe ClickPosition
    }


type Msg
    = NoOp
    | OnResize Int Int
    | RecievedViewport Viewport
    | DesktopClicked
    | ContextMenuClicked ClickPosition


type alias ClickPosition =
    { clientX : Int
    , clientY : Int
    }


decodeClickPosition =
    D.map2
        ClickPosition
        (D.at [ "clientX" ] D.int)
        (D.at [ "clientY" ] D.int)


view : Model -> H.Html Msg
view model =
    H.div
        [ A.css
            [ height (vh 100)
            , width (vw 100)
            , backgroundColor (hex "#008080")
            , position relative
            ]
        , A.attribute "data-test" "desktop-background"
        , E.preventDefaultOn "contextmenu" (D.map (\e -> ( ContextMenuClicked e, True )) decodeClickPosition)
        , E.onClick DesktopClicked
        ]
        [ bottomMenuBar model
        , case model.contextMenuPosition of
            Just menuPosition ->
                H.div
                    [ A.css
                        [ width (px 80)
                        , backgroundColor Theme.silver
                        , Theme.buttonBoxShadow
                        , position absolute
                        , top (px <| toFloat menuPosition.clientY)
                        , left (px <| toFloat menuPosition.clientX)
                        ]
                    , E.stopPropagationOn "click" (D.succeed ( NoOp, True ))
                    ]
                    [ desktopContextMenuView model
                    ]

            Nothing ->
                H.text ""
        ]


bottomMenuBar : Model -> H.Html Msg
bottomMenuBar model =
    H.div
        [ A.css
            [ backgroundColor Theme.silver
            , minHeight (px 40)
            , position absolute
            , bottom (px 0)
            , left (px 0)
            , width (vw 100)
            , displayFlex
            , Theme.buttonBoxShadow
            ]
        ]
        []


init_ : Flags -> ComponentResult ( Model, Effect ) Msg Never Never
init_ flags =
    { viewport = Nothing
    , contextMenuPosition = Nothing
    }
        |> withModel
        |> withEffect EffectNone


init : Flags -> ( Model, Cmd Msg )
init flags =
    init_ flags |> resolveAll runEffect


update_ : Msg -> Model -> ComponentResult ( Model, Effect ) Msg Never Never
update_ msg model =
    case msg of
        NoOp ->
            model
                |> withModel
                |> withEffect EffectNone

        OnResize _ _ ->
            model
                |> withModel
                |> withEffect QueryViewport

        DesktopClicked ->
            { model | contextMenuPosition = Nothing }
                |> withModel
                |> withEffect EffectNone

        ContextMenuClicked position ->
            { model | contextMenuPosition = Just position }
                |> withModel
                |> withEffect EffectNone

        RecievedViewport viewport ->
            { model | viewport = Just viewport }
                |> withModel
                |> withEffect EffectNone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    update_ msg model |> resolveAll runEffect


subscriptions : Model -> Sub Msg
subscriptions model =
    onResize OnResize


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> H.toUnstyled
        , subscriptions = subscriptions
        }


desktopContextMenuView : Model -> H.Html Msg
desktopContextMenuView model =
    H.div
        [ A.css
            [ displayFlex
            , flexDirection column
            , alignItems stretch
            , padding2 (px 2) (px 4)
            ]
        ]
        (List.map
            desktopContextMenuItemView
            [ "Create File"
            , "Create Folder"
            ]
        )


desktopContextMenuItemView : String -> H.Html Msg
desktopContextMenuItemView itemName =
    H.button
        [ A.css
            [ borderStyle none
            , border (px 0)
            , boxShadow none
            , boxSizing borderBox
            , minWidth (px 0)
            , padding2 (px 2) (px 4)
            , cursor pointer
            , hover 
                [ backgroundColor Theme.darkSilver
                ]
            ]
        ]
        [ H.text itemName
        ]
