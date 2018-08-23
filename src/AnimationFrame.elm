module AnimationFrame exposing (..)

import Time exposing (Posix)
import Browser.Events

-- Emulating elm-lang/animation-frame

times : (Posix -> msg) -> Sub msg
times =
    Browser.Events.onAnimationFrame

diffs : (Float -> msg) -> Sub msg
diffs =
    Browser.Events.onAnimationFrameDelta
