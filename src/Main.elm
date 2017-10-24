module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (action, for, id, title, type_, value)
import Html.Events exposing (on)
import Json.Decode as Decode
import FileUpload exposing (FileData, fileSelected, fileContentRead)


type Msg
    = FileSelected
    | FileRead FileData

type alias File =
    { contents : String
    , filename : String
    }

type alias Model =
    { id : String
    , file : Maybe File
    }

main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

init : ( Model, Cmd Msg )
init =
    ( { id = "foo"
      , file = Nothing
      }
    , Cmd.none
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileSelected ->
            ( model, fileSelected model.id )

        FileRead data ->
            let
                newFile =
                    { contents = data.contents
                    , filename = data.filename
                    }
            in
                ( { model | file = Just newFile } , Cmd.none )

view : Model -> Html Msg
view model =
    form
    [ action "http://localhost:8000/nmg" ]
    ( List.concat [
        [ div [] [ select [] ( List.map optionsList [ "-- Choose Sport --", "Baseball", "Basketball", "Golf", "Soccer", "Tennis" ] ) ] ]
        , ( List.map fileUploadField [ "Banner", "Logo", "Icon" ] )
        , [ div [] [ input [ type_ "submit"] [ text "Upload" ] ] ]
    ] )

fileUploadField : String -> Html Msg
fileUploadField name =
    div []
    [
        label [ for name ] [ text name ]
        , input [ type_ "file" , id name , on "change" ( Decode.succeed FileSelected ) ] []
    ]

optionsList : String -> Html Msg
optionsList name =
    option [ value name ] [ text name ]

subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead FileRead

