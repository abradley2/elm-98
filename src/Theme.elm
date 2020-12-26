module Theme exposing (..)

import Css exposing (..)


white_ : String
white_ =
    "#ffffff"


white : Color
white =
    hex white_


manilla_ : String
manilla_ =
    "#f1d592"


manilla : Color
manilla =
    hex manilla_


black_ : String
black_ =
    "#000000"


black : Color
black =
    hex black_


silver_ : String
silver_ =
    "#c0c0c0"


silver : Color
silver =
    hex silver_


darkSilver_ : String
darkSilver_ =
    "#747474"


darkSilver : Color
darkSilver =
    hex darkSilver_


buttonBoxShadow : Style
buttonBoxShadow =
    property "box-shadow" "inset -1px -1px #0a0a0a, inset 1px 1px #fff, inset -2px -2px grey, inset 2px 2px #dfdfdf"


buttonReset : Style
buttonReset =
    Css.batch
        [ borderStyle none
        , border (px 0)
        , boxShadow none
        , boxSizing borderBox
        , minWidth (px 0)
        , padding2 (px 2) (px 4)
        , cursor pointer
        , Css.pseudoClass "not(:disabled):active"
            [ border (px 0)
            , padding2 (px 2) (px 4)
            ]
        ]
