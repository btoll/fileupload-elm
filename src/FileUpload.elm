port module FileUpload exposing (..)

type alias FileData =
    { contents : String
    , filename : String
    }

port fileSelected : String -> Cmd msg
port fileContentRead : ( List FileData -> msg) -> Sub msg

