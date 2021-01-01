module Main exposing (..)

import Browser
import Browser.Dom as Dom exposing (Element, Error, Viewport, getElement, getViewport)
import Browser.Events exposing (onResize)
import ComponentResult exposing (ComponentResult, applyExternalMsg, mapModel, mapMsg, resolve, withCmd, withExternalMsg, withModel)
import ComponentResult.Effect exposing (resolveAll, resolveEffects, withEffect)
import Css exposing (..)
import DesktopElement exposing (DesktopElement, DesktopElementData(..))
import Dict exposing (Dict)
import Html
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json
import Json.Decode as D
import Process
import Random
import Task
import Theme
import UUID exposing (UUID)


type Effect
    = EffCmd (Cmd Msg)
    | EffQueryViewport
    | EffBatch (List Effect)


runEffect : Effect -> Cmd Msg
runEffect effect =
    case effect of
        EffQueryViewport ->
            Task.perform RecievedViewport getViewport

        EffCmd cmd ->
            cmd

        EffBatch effectList ->
            List.map runEffect effectList
                |> Cmd.batch


type alias Flags =
    { seeds : UUID.Seeds
    }


fallbackFlags : Flags
fallbackFlags =
    { seeds =
        UUID.Seeds
            (Random.initialSeed 0)
            (Random.initialSeed 0)
            (Random.initialSeed 0)
            (Random.initialSeed 0)
    }


type alias Model =
    { initialized : Result D.Error ()
    , viewport : Maybe Viewport
    , contextMenuPosition : Maybe Json.ClickPosition
    , flags : Flags
    , elements : Dict String DesktopElement
    }


getUUID : Model -> ( UUID.UUID, Model )
getUUID model =
    let
        ( uuid, nextSeeds ) =
            UUID.step model.flags.seeds

        flags =
            model.flags

        nextFlags =
            { flags | seeds = nextSeeds }
    in
    ( uuid, { model | flags = nextFlags } )


type Msg
    = NoOp
    | OnResize Int Int
    | RecievedViewport Viewport
    | DesktopClicked
    | ContextMenuClicked Json.ClickPosition
    | RequestCreateFile Json.ClickPosition
    | RequestCreateFolder Json.ClickPosition
    | DesktopElementMsg UUID DesktopElement.Msg


flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.map Flags (D.at [ "seeds" ] seedDecoder)


seedDecoder : D.Decoder UUID.Seeds
seedDecoder =
    D.map4 UUID.Seeds
        (D.at [ "0" ] D.int |> D.map Random.initialSeed)
        (D.at [ "1" ] D.int |> D.map Random.initialSeed)
        (D.at [ "2" ] D.int |> D.map Random.initialSeed)
        (D.at [ "3" ] D.int |> D.map Random.initialSeed)


view : Model -> H.Html Msg
view model =
    case model.initialized of
        Ok _ ->
            view_ model

        Err err ->
            H.pre
                [ A.css
                    [ color Theme.black
                    ]
                ]
                [ H.text <| D.errorToString err ]


view_ : Model -> H.Html Msg
view_ model =
    H.div
        [ A.css
            [ height (vh 100)
            , width (vw 100)
            , backgroundColor (hex "#008080")
            , position relative
            , overflow hidden
            ]
        , A.attribute "data-test" "desktop-background"
        , E.preventDefaultOn "contextmenu" (D.map (\e -> ( ContextMenuClicked e, True )) Json.decodeClickPosition)
        , E.onClick DesktopClicked
        ]
    <|
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
                    [ desktopContextMenuView model menuPosition
                    ]

            Nothing ->
                H.text ""
        ]
            ++ List.map
                (\element ->
                    DesktopElement.view (DesktopElementMsg element.id) element
                )
                (Dict.values model.elements)


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


init_ : D.Value -> ComponentResult ( Model, Effect ) Msg Never Never
init_ flagsJS =
    let
        decodeResult =
            D.decodeValue flagsDecoder flagsJS
    in
    update_ (OnResize 0 0)
        { viewport = Nothing
        , contextMenuPosition = Nothing
        , elements = Dict.empty
        , flags = decodeResult |> Result.withDefault fallbackFlags
        , initialized = decodeResult |> Result.map (always ())
        }


init : D.Value -> ( Model, Cmd Msg )
init flagsJS =
    init_ flagsJS |> resolveAll runEffect


desktopElementExtMsg : DesktopElement.ExtMsg -> ComponentResult Model Msg Never Never -> ComponentResult Model Msg Never Never
desktopElementExtMsg extMsg result =
    case extMsg of
        DesktopElement.FolderDoubleClicked element ->
            result
                |> mapModel
                    (\model ->
                        let
                            windowWidth =
                                400

                            xPos =
                                model.viewport
                                    |> Maybe.map (.viewport >> .width)
                                    |> Maybe.map (\v -> (v / 2) - (windowWidth / 2))
                                    |> Maybe.withDefault 0

                            ( nextId, nextModel ) =
                                getUUID model

                            newElements =
                                nextModel.elements
                                    |> Dict.insert (UUID.toString nextId)
                                        { id = nextId
                                        , dataType = Window ( windowWidth, 250 )
                                        , name = ""
                                        , position = ( Basics.round xPos, Basics.round 100 )
                                        , clickPosition = Nothing
                                        }
                        in
                        { nextModel | elements = newElements }
                    )


update_ : Msg -> Model -> ComponentResult ( Model, Effect ) Msg Never Never
update_ msg model =
    case msg of
        DesktopElementMsg uuid desktopElementMsg ->
            case Dict.get (UUID.toString uuid) model.elements of
                Just element ->
                    DesktopElement.update desktopElementMsg element
                        |> mapModel (\nextElement -> Dict.insert (UUID.toString uuid) nextElement model.elements)
                        |> mapModel (\elements -> { model | elements = elements })
                        |> mapMsg (DesktopElementMsg element.id)
                        |> applyExternalMsg desktopElementExtMsg
                        |> withEffect (EffCmd Cmd.none)

                Nothing ->
                    model
                        |> withModel
                        |> withEffect (EffCmd Cmd.none)

        RequestCreateFolder clickPosition ->
            let
                ( uuid, nextModel ) =
                    getUUID model

                newFolder =
                    { id = uuid
                    , name = ""
                    , position = ( clickPosition.clientX, clickPosition.clientY )
                    , dataType = Folder
                    , clickPosition = Nothing
                    }

                nextElements =
                    Dict.insert (UUID.toString uuid) newFolder model.elements
            in
            { nextModel
                | elements = nextElements
                , contextMenuPosition = Nothing
            }
                |> withModel
                |> withEffect (EffCmd Cmd.none)

        RequestCreateFile clickPosition ->
            let
                ( uuid, nextModel ) =
                    getUUID model

                newFile =
                    { id = uuid
                    , name = ""
                    , position = ( clickPosition.clientX, clickPosition.clientY )
                    , dataType = File
                    , clickPosition = Nothing
                    }

                nextElements =
                    Dict.insert (UUID.toString uuid) newFile model.elements
            in
            { nextModel
                | elements = nextElements
                , contextMenuPosition = Nothing
            }
                |> withModel
                |> withEffect (EffCmd Cmd.none)

        NoOp ->
            model
                |> withModel
                |> withEffect (EffCmd Cmd.none)

        OnResize _ _ ->
            model
                |> withModel
                |> withEffect EffQueryViewport

        DesktopClicked ->
            { model | contextMenuPosition = Nothing }
                |> withModel
                |> withEffect (EffCmd Cmd.none)

        ContextMenuClicked pos ->
            { model | contextMenuPosition = Just pos }
                |> withModel
                |> withEffect (EffCmd Cmd.none)

        RecievedViewport viewport ->
            { model | viewport = Just viewport }
                |> withModel
                |> withEffect (EffCmd Cmd.none)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    update_ msg model |> resolveAll runEffect


subscriptions : Model -> Sub Msg
subscriptions model =
    onResize OnResize


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> H.toUnstyled
        , subscriptions = subscriptions
        }


desktopContextMenuView : Model -> Json.ClickPosition -> H.Html Msg
desktopContextMenuView model clickPosition =
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
            [ ( "Create File", RequestCreateFile clickPosition )
            , ( "Create Folder", RequestCreateFolder clickPosition )
            ]
        )


desktopContextMenuItemView : ( String, Msg ) -> H.Html Msg
desktopContextMenuItemView ( itemName, onClick ) =
    H.button
        [ A.css
            [ borderStyle none
            , border (px 0)
            , boxShadow none
            , boxSizing borderBox
            , minWidth (px 0)
            , padding2 (px 2) (px 4)
            , cursor pointer
            , color Theme.black
            , Css.pseudoClass "not(:disabled):active"
                [ border (px 0)
                , padding2 (px 2) (px 4)
                ]
            , active
                [ border (px 0)
                , padding2 (px 2) (px 4)
                ]
            , hover
                [ backgroundColor Theme.darkSilver
                ]
            ]
        , E.onClick onClick
        ]
        [ H.text itemName
        ]
