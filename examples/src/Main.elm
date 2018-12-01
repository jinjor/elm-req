module Main exposing (main)

import Browser
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
    | Error Req.StringReqError


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
    = Received (Result Req.StringReqError ( User, Repo ))


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
                [ h1 [] [ text "Error!" ]
                , h2 [] [ text "Request" ]
                , table []
                    [ tr [] [ th [] [ text "URL" ], td [] [ text e.req.url ] ]
                    , tr [] [ th [] [ text "Method" ], td [] [ text e.req.method ] ]
                    , tr [] [ th [] [ text "Headers" ], td [] [ text (Debug.toString e.req.headers) ] ]
                    , tr [] [ th [] [ text "Body" ], td [] [ text (Debug.toString e.req.body) ] ]
                    ]
                , h2 [] [ text "Response" ]
                , p [] [ text (Debug.toString e.res) ]
                , h2 [] [ text "Decode Error" ]
                , p [] [ text (Debug.toString e.decodeError) ]
                ]
