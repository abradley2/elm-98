module Main exposing (..)

import Browser
import Browser.Dom as Dom exposing (Element, Error, Viewport, getElement, getViewport)
import Browser.Events exposing (onResize)
import ComponentResult exposing (ComponentResult, mapModel, resolve, withCmd, withExternalMsg, withModel)
import ComponentResult.Effect exposing (resolveAll, resolveEffects, withEffect)
import Css exposing (..)
import Dict exposing (Dict)
import Html
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as D
import Process
import Random
import Task
import Theme
import UUID exposing (UUID)


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
    { seeds : UUID.Seeds
    }


type DesktopElementData
    = Folder
    | File
    | Window ( Int, Int )


type alias DesktopElement =
    { name : String
    , id : UUID
    , position : ( Int, Int )
    , dataType : DesktopElementData
    }


type alias FolderData =
    { name : String
    , id : UUID
    , position : ( Int, Int )
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
    , contextMenuPosition : Maybe ClickPosition
    , flags : Flags
    , elements : Dict String DesktopElement
    , dragOffset : Maybe ClickPosition
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
    | ContextMenuClicked ClickPosition
    | RequestCreateFile ClickPosition
    | RequestCreateFolder ClickPosition
    | ElementDragStarted DesktopElement ClickPosition
    | ElementDragEnded DesktopElement ClickPosition
    | FolderDoubleClicked DesktopElement


type alias ClickPosition =
    { clientX : Int
    , clientY : Int
    , offsetX : Int
    , offsetY : Int
    }


decodeClickPosition : D.Decoder ClickPosition
decodeClickPosition =
    D.map4
        ClickPosition
        (D.at [ "clientX" ] D.int)
        (D.at [ "clientY" ] D.int)
        (D.at [ "offsetX" ] D.int)
        (D.at [ "offsetY" ] D.int)


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
        , E.preventDefaultOn "contextmenu" (D.map (\e -> ( ContextMenuClicked e, True )) decodeClickPosition)
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
                desktopElementView
                (Dict.values model.elements)


desktopElementView : DesktopElement -> H.Html Msg
desktopElementView element =
    case element.dataType of
        Folder ->
            folderIconView element

        File ->
            fileIconView element

        Window widthHeight ->
            windowView element widthHeight


windowView : DesktopElement -> ( Int, Int ) -> H.Html Msg
windowView element ( windowWidth, windowHeight ) =
    let
        ( xPos, yPos ) =
            element.position
    in
    H.div
        (draggableAttributes element
            [ A.class "window"
            , A.css
                [ position absolute
                , top (px <| toFloat yPos)
                , left (px <| toFloat xPos)
                , Theme.zIndexWindow
                ]
            ]
        )
        [ H.div
            [ A.css
                []
            , A.class "title-bar"
            ]
            [ H.div
                [ A.class "title-bar-text"
                ]
                [ H.text "File Explorer"
                ]
            , H.div 
                [ A.class "title-bar-controls"]
                [ H.button 
                    [ A.attribute "aria-label" "Minimize"] 
                    []
                , H.button 
                    [ A.attribute "aria-label" "Maximize"] 
                    []
                , H.button 
                    [ A.attribute "aria-label" "Close"] 
                    []
                ]
            ]
        , H.div
            [ A.class "window-body"
            , A.css
                [ height (px <| toFloat windowHeight)
                , width (px <| toFloat windowWidth)
                ]
            ]
            []
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
        , dragOffset = Nothing
        , initialized = decodeResult |> Result.map (always ())
        }


init : D.Value -> ( Model, Cmd Msg )
init flagsJS =
    init_ flagsJS |> resolveAll runEffect


update_ : Msg -> Model -> ComponentResult ( Model, Effect ) Msg Never Never
update_ msg model =
    case msg of
        FolderDoubleClicked element ->
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
                            }
            in
            { nextModel | elements = newElements }
                |> withModel
                |> withEffect EffectNone

        ElementDragStarted element clickPosition ->
            { model | dragOffset = Just clickPosition }
                |> withModel
                |> withEffect EffectNone

        ElementDragEnded element clickPosition ->
            let
                ( withOffsetX, withOffsetY ) =
                    model.dragOffset
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

                nextElements =
                    Dict.insert
                        (UUID.toString element.id)
                        nextElement
                        model.elements
            in
            { model | elements = nextElements }
                |> withModel
                |> withEffect EffectNone

        RequestCreateFolder clickPosition ->
            let
                ( uuid, nextModel ) =
                    getUUID model

                newFolder =
                    { id = uuid
                    , name = ""
                    , position = ( clickPosition.clientX, clickPosition.clientY )
                    , dataType = Folder
                    }

                nextElements =
                    Dict.insert (UUID.toString uuid) newFolder model.elements
            in
            { nextModel
                | elements = nextElements
                , contextMenuPosition = Nothing
            }
                |> withModel
                |> withEffect EffectNone

        RequestCreateFile clickPosition ->
            let
                ( uuid, nextModel ) =
                    getUUID model

                newFile =
                    { id = uuid
                    , name = ""
                    , position = ( clickPosition.clientX, clickPosition.clientY )
                    , dataType = File
                    }

                nextElements =
                    Dict.insert (UUID.toString uuid) newFile model.elements
            in
            { nextModel
                | elements = nextElements
                , contextMenuPosition = Nothing
            }
                |> withModel
                |> withEffect EffectNone

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

        ContextMenuClicked pos ->
            { model | contextMenuPosition = Just pos }
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


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> H.toUnstyled
        , subscriptions = subscriptions
        }


desktopContextMenuView : Model -> ClickPosition -> H.Html Msg
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


draggableAttributes : DesktopElement -> List (H.Attribute Msg) -> List (H.Attribute Msg)
draggableAttributes element =
    (++)
        [ A.draggable "true"
        , E.on "dragstart" <|
            D.map
                (ElementDragStarted element)
                decodeClickPosition
        , E.on "dragend" <|
            D.map
                (ElementDragEnded element)
                decodeClickPosition
        ]


fileIconView : DesktopElement -> H.Html Msg
fileIconView element =
    let
        ( xPos, yPos ) =
            element.position
    in
    H.div
        (draggableAttributes element
            [ A.css
                [ backgroundColor Theme.white
                , width (px 80)
                , height (px 80)
                , position absolute
                , top (px <| toFloat yPos)
                , left (px <| toFloat xPos)
                ]
            ]
        )
        []


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
            , E.onDoubleClick (FolderDoubleClicked element)
            ]
        )
        []
