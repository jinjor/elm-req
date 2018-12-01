module Main exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Req
import Requests exposing (ErrorInfo, Repo, User)
import Task



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- FLAGS


type alias Flags =
    ()



-- MODEL


type alias Model =
    { state : State
    }


type State
    = Init
    | Waiting
    | Loaded ( User, Repo )
    | Error (Req.Error ErrorInfo)


init : () -> ( Model, Cmd Msg )
init flags =
    ( { state = Init }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Send
    | SendWith404
    | SendWithDecodeError
    | Received (Result (Req.Error ErrorInfo) ( User, Repo ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Send ->
            ( { state = Waiting }
            , Task.map2 Tuple.pair
                (Requests.getUser "jinjor")
                (Requests.getRepo "jinjor" "elm-req")
                |> Task.attempt Received
            )

        SendWith404 ->
            ( { state = Waiting }
            , Task.map2 Tuple.pair
                (Requests.getUser "jinjor")
                (Requests.getRepo "jinjor" "this-repo-doesnt-exist")
                |> Task.attempt Received
            )

        SendWithDecodeError ->
            ( { state = Waiting }
            , Task.map2 Tuple.pair
                (Requests.getUser "jinjor")
                (Requests.getRepoWithDecodeError "jinjor" "elm-req")
                |> Task.attempt Received
            )

        Received (Ok userAndRepo) ->
            ( { model | state = Loaded userAndRepo }, Cmd.none )

        Received (Err error) ->
            ( { model | state = Error error }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ div [] [ input [ type_ "button", onClick Send, value "Send" ] [] ]
        , div [] [ input [ type_ "button", onClick SendWith404, value "Send (404)" ] [] ]
        , div [] [ input [ type_ "button", onClick SendWithDecodeError, value "Send (Decode Error)" ] [] ]
        , hr [] []
        , viewState model
        ]


viewState : Model -> Html Msg
viewState model =
    case model.state of
        Init ->
            text ""

        Waiting ->
            text "waiting"

        Loaded ( user, repo ) ->
            text "success"

        Error e ->
            div [] <|
                case e of
                    Req.BadUrl url ->
                        [ text ("Bad url: " ++ url) ]

                    Req.Timeout ->
                        [ text "Timeout" ]

                    Req.NetworkError ->
                        [ text "Network error" ]

                    Req.BadStatus meta info ->
                        [ div [] [ text ("Bad status: " ++ String.fromInt meta.statusCode ++ " " ++ meta.statusText) ]
                        , div [] [ text info.message ]
                        , table []
                            (meta.headers
                                |> Dict.toList
                                |> List.map
                                    (\( k, v ) ->
                                        tr [] [ th [] [ text k ], td [] [ text v ] ]
                                    )
                            )
                        ]

                    Req.BadBody meta message ->
                        [ div [] [ text "Bad body" ]
                        , pre [] [ text message ]
                        ]
