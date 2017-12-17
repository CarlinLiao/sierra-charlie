module Special exposing (..)

import Task exposing (Task)
import Native.Special

-- alias for a built-in
send : String -> Task x ()
send =
    Native.Special.send
