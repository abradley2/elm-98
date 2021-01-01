module Window exposing (..)

import Css exposing (..)
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as A
import Html.Styled.Lazy as L
import Theme


type Msg
    = NoOp
    | Resize


type alias Model =
    { windowWidth : Int
    , windowHeight : Int
    }


view : (Msg -> a) -> Model -> H.Html a
view toMsg model =
    H.div
        [ A.class "window"
        , A.css
            [ position relative
            ]
        ]
    <|
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
                [ A.class "title-bar-controls" ]
                [ H.button
                    [ A.attribute "aria-label" "Minimize" ]
                    []
                , H.button
                    [ A.attribute "aria-label" "Maximize" ]
                    []
                , H.button
                    [ A.attribute "aria-label" "Close" ]
                    []
                ]
            ]
        , H.div
            [ A.class "window-body"
            , A.css
                [ height (px <| toFloat model.windowHeight)
                , width (px <| toFloat model.windowWidth)
                ]
            ]
            []
        ]
            ++ resizeBars


resizeBars : List (H.Html a)
resizeBars =
    [ H.div
        [ A.css
            [ backgroundColor Theme.manilla
            , position absolute
            , left (px 0)
            , width (px 12)
            ]
        ]
        []
    , H.div
        []
        []
    , H.div
        []
        []
    ]
