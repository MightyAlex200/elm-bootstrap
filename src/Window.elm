module Window exposing (..)

import Browser.Events
import Browser.Dom
import Task exposing (Task)

-- Emulating elm-lang/window

type alias Size =
    { width : Int
    , height : Int
    }

size : Task x Size
size =
    Browser.Dom.getViewport
        |> Task.map (\viewport -> 
            Size
                (round viewport.viewport.width)
                (round viewport.viewport.height)
        )

width : Task x Int
width =
    Browser.Dom.getViewport
        |> Task.map (\viewport -> round viewport.viewport.width)

height : Task x Int
height =
    Browser.Dom.getViewport
        |> Task.map (\viewport -> round viewport.viewport.height)

resizes : (Size -> msg) -> Sub msg
resizes func =
    let
        newFunc =
            (\x y -> func (Size x y))
    in
        Browser.Events.onResize
            newFunc
