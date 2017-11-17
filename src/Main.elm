module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (disabled, for, id, multiple, type_, value)
import Html.Events exposing (on, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Ports exposing (FileData, fileSelected, fileContentRead)


type Msg
    = FileSelected
    | FileRead ( List FileData )
    | SelectEntity String
    | SelectEntityId String
    | SubmitFile String
    | PostFile ( Result Http.Error String )

type alias Model =
    { id : String
    , disabled : Bool
    , entity : String
    , entityId : String
    , files : List FileData
    }

-- https://github.com/rtfeldman/elm-spa-example/blob/master/src/Util.elm#L8
(=>) : a -> b -> ( a, b )
(=>) =
    (,)

{-| infixl 0 means the (=>) operator has the same precedence as (<|) and (|>),
meaning you can use it at the end of a pipeline and have the precedence work out.
-}
infixl 0 =>

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
    ( { id = "fuploads"
      , disabled = False
      , entity = ""
      , entityId = ""
      , files = []
      }
    , Cmd.none
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileSelected ->
            ( model, fileSelected model.id )

        FileRead data ->
              ( { model | files = data }, Cmd.none )

        PostFile ( Ok _ ) ->
            clearForm model

        PostFile ( Err _ ) ->
            clearForm model

        SelectEntity entity ->
            ( { model | entity = entity }, Cmd.none )

        SelectEntityId id ->
            ( { model | entityId = id }, Cmd.none )

        SubmitFile url ->
            ( { model | disabled = True }, makeRequest model url model.files )

view : Model -> Html Msg
view model =
    form [ onSubmit ( SubmitFile "http://localhost:8080/nmg/image" ) ]
    ( List.concat [
        [ div [] [ select [ onInput SelectEntity ] ( List.map optionsList [ "-- Choose Entity --", "event", "sport", "team" ] ) ] ]
        , [ div [] [ select [ onInput SelectEntityId ] ( List.map optionsList ( "-- Choose ID --" :: List.map toString ( List.range 1 20 ) ) ) ] ]
        , [ fileUploadField ]
        , [ div [] [ input [ type_ "submit", disabled model.disabled ] [ text "Upload" ] ] ]
    ] )

subscriptions : Model -> Sub Msg
subscriptions data =
    fileContentRead FileRead

clearForm : Model -> ( Model, Cmd Msg )
clearForm model =
    ( { model | disabled = False }, Cmd.none )

makeRequest : Model -> String -> List FileData -> Cmd Msg
makeRequest model url files =
    let
        endpoint =
            url ++ "/" ++ model.entity ++ "/" ++ model.entityId

        json =
            List.map ( \file ->
                Encode.object
                   [ "filename" => Encode.string file.filename
                    , "contents" => Encode.string file.contents
                    ] )
            files

        body =
            Encode.list json
                |> Http.jsonBody

        request =
            Http.post endpoint body Decode.string

    in
        Http.send PostFile request

fileUploadField : Html Msg
fileUploadField =
    div []
    [
        label [ for "fuploads" ] [ text "Choose Entity Uploads" ]
        , input [ multiple True, type_ "file" , id "fuploads" , on "change" ( Decode.succeed FileSelected ) ] []
    ]

optionsList : String -> Html Msg
optionsList name =
    option [ value name ] [ text name ]

