port module FileUpload exposing (..)

type alias FileData =
    { contents : String
    , filename : String
    }

port fileSelected : String -> Cmd msg
port fileContentRead : (FileData -> msg) -> Sub msg

