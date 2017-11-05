module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (action, enctype, for, id, title, type_, value)
import Html.Events exposing (on, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import FileUpload exposing (FileData, fileSelected, fileContentRead)


type Msg
    = FileSelected
    | FileRead FileData
    | SubmitFile String
    | PostFile ( Result Http.Error String )

type alias File =
    { contents : String
    , filename : String
    }

type alias Model =
    { id : String
    , file : Maybe File
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
    ( { id = "Banner"
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

        SubmitFile url ->
            ( model, makeRequest url model.file )

        -- TODO
        PostFile ( Ok _ ) ->
            ( model, Cmd.none )

        -- TODO
        PostFile ( Err _ ) ->
            ( model, Cmd.none )

makeRequest : String -> Maybe File -> Cmd Msg
makeRequest url file =
    let
        objectfile =
            Maybe.withDefault { contents = "", filename = "" } file

        json =
            Encode.object
                [ "filename" => Encode.string objectfile.filename
                , "contents" => Encode.string objectfile.contents
                ]

        body =
            Encode.list [ json ]
                |> Http.jsonBody

        request =
            Http.post url body Decode.string


    in
        Http.send PostFile request

view : Model -> Html Msg
view model =
    form
--    [ action "http://localhost:8080/nmg/image/team/5", onSubmit SubmitFile ]
    [ onSubmit ( SubmitFile "http://localhost:8080/nmg/image/team/5" ) ]
    ( List.concat [
        [ div [] [ select [] ( List.map optionsList [ "-- Choose Sport --", "Baseball", "Basketball", "Golf", "Soccer", "Tennis" ] ) ] ]
        , ( List.map fileUploadField [ "Banner", "Logo", "Icon" ] )
        , [ div [] [ input [ type_ "submit" ] [ text "Upload" ] ] ]
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

