module Mouse exposing (..)

import Browser.Events
import Json.Decode as Decode exposing (Decoder)

-- Emulating elm-lang/mouse

type alias Position =
    { x : Int
    , y : Int
    }

position : Decoder Position
position =
    Decode.map2 Position
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)

clicks : (Position -> msg) -> Sub msg
clicks func =
    Browser.Events.onClick
        (Decode.map func position)

moves : (Position -> msg) -> Sub msg
moves func =
    Browser.Events.onMouseMove
        (Decode.map func position)

downs : (Position -> msg) -> Sub msg
downs func =
    Browser.Events.onMouseDown
        (Decode.map func position)

ups : (Position -> msg) -> Sub msg
ups func =
    Browser.Events.onMouseUp
        (Decode.map func position)
