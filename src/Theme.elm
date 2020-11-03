module Theme exposing (..)

import Css exposing (..)


white_ =
    "#ffffff"


white =
    hex white_


manilla_ =
    "#f1d592"


manilla =
    hex manilla_


black_ =
    "#000000"


black =
    hex black_


silver_ =
    "#c0c0c0"


silver =
    hex silver_


darkSilver_ =
    "#747474"


darkSilver =
    hex darkSilver_


buttonBoxShadow =
    property "box-shadow" "inset -1px -1px #0a0a0a, inset 1px 1px #fff, inset -2px -2px grey, inset 2px 2px #dfdfdf"


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
