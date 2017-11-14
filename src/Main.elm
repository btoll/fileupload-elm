module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (for, id, multiple, type_, value)
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

type alias File =
    { contents : String
    , filename : String
    }

type alias Model =
    { id : String
    , entity : String
    , entityId : String
    , files : List File
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

        -- TODO
        PostFile ( Ok _ ) ->
            ( model, Cmd.none )

        -- TODO
        PostFile ( Err _ ) ->
            ( model, Cmd.none )

        SelectEntity entity ->
            ( { model | entity = entity }, Cmd.none )

        SelectEntityId id ->
            ( { model | entityId = id }, Cmd.none )

        SubmitFile url ->
            ( model, makeRequest model url model.files )

makeRequest : Model -> String -> List File -> Cmd Msg
makeRequest model url objectfiles =
    let
        endpoint =
            url ++ "/" ++ model.entity ++ "/" ++ model.entityId

        json =
            List.map ( \file ->
                Encode.object
                   [ "filename" => Encode.string file.filename
                    , "contents" => Encode.string file.contents
                    ] )
            objectfiles

        body =
            Encode.list json
                |> Http.jsonBody

        request =
            Http.post endpoint body Decode.string


    in
        Http.send PostFile request

view : Model -> Html Msg
view model =
    form
    [ onSubmit ( SubmitFile "http://localhost:8080/nmg/image" ) ]
    ( List.concat [
        [ div [] [ select [ onInput SelectEntity ] ( List.map optionsList [ "-- Choose Entity --", "event", "sport", "team" ] ) ] ]
        , [ div [] [ select [ onInput SelectEntityId ] ( List.map optionsList ( "-- Choose ID --" :: List.map toString ( List.range 1 20 ) ) ) ] ]
        , [ fileUploadField ]
        , [ div [] [ input [ type_ "submit" ] [ text "Upload" ] ] ]
    ] )

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

subscriptions : Model -> Sub Msg
subscriptions data =
    fileContentRead FileRead

