module Json exposing (..)

import Json.Decode as D

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