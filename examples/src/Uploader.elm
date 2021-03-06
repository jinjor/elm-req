module Uploaded exposing (main)

import Browser
import File exposing (File)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Requests



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Waiting
    | Uploading Float
    | Done
    | Fail


init : () -> ( Model, Cmd Msg )
init _ =
    ( Waiting
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotFiles (List File)
    | GotProgress Http.Progress
    | Uploaded (Result Http.Error ())
    | Cancel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFiles files ->
            ( Uploading 0
            , Requests.upload "upload" Uploaded files
            )

        GotProgress progress ->
            case progress of
                Http.Sending p ->
                    ( Uploading (Http.fractionSent p), Cmd.none )

                Http.Receiving _ ->
                    ( model, Cmd.none )

        Uploaded result ->
            case result of
                Ok _ ->
                    ( Done, Cmd.none )

                Err _ ->
                    ( Fail, Cmd.none )

        Cancel ->
            ( Waiting, Http.cancel "upload" )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Http.track "upload" GotProgress



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Waiting ->
            input
                [ type_ "file"
                , multiple True
                , on "change" (D.map GotFiles filesDecoder)
                ]
                []

        Uploading fraction ->
            div []
                [ progress
                    [ value (String.fromInt (round (100 * fraction)))
                    , A.max "100"
                    , style "display" "block"
                    ]
                    []
                , button [ onClick Cancel ] [ text "Cancel" ]
                ]

        Done ->
            h1 [] [ text "DONE" ]

        Fail ->
            h1 [] [ text "FAIL" ]


filesDecoder : D.Decoder (List File)
filesDecoder =
    D.at [ "target", "files" ] (D.list File.decoder)
