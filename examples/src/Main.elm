module Main exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Req
import Requests exposing (Repo, User)
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
    = Waiting
    | Loaded ( User, Repo )
    | Error Requests.Error


init : () -> ( Model, Cmd Msg )
init flags =
    ( { state = Waiting }
    , Task.map2 Tuple.pair
        (Requests.getUser "jinjor")
        (Requests.getRepo "jinjor" "elm-req")
        |> Task.attempt Received
    )



-- UPDATE


type Msg
    = Received (Result Requests.Error ( User, Repo ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
    case model.state of
        Waiting ->
            text "waiting"

        Loaded ( user, repo ) ->
            text "success"

        Error e ->
            div []
                [ h2 [] [ text ("Error at " ++ e.method ++ " " ++ e.url) ]
                , p [] <|
                    case e.details of
                        Requests.BadUrl url ->
                            [ text ("Bad url: " ++ url) ]

                        Requests.Timeout ->
                            [ text "Timeout" ]

                        Requests.NetworkError ->
                            [ text "Network error" ]

                        Requests.BadStatus code message headers info ->
                            [ div [] [ text ("Bad status: " ++ String.fromInt code ++ " " ++ message) ]
                            , div [] [ text info.message ]
                            , table []
                                (headers
                                    |> Dict.toList
                                    |> List.map
                                        (\( k, v ) ->
                                            tr [] [ th [] [ text k ], td [] [ text v ] ]
                                        )
                                )
                            ]

                        Requests.BadBody message ->
                            [ div [] [ text "Bad body" ]
                            , pre [] [ text message ]
                            ]

                        Requests.BadErrorBody message ->
                            [ div [] [ text "Bad Error Body" ]
                            , pre [] [ text message ]
                            ]

                        Requests.Bug message ->
                            [ div [] [ text "Bug" ]
                            , pre [] [ text message ]
                            ]
                ]
