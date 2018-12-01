module Requests exposing
    ( Error
    , ErrorDetails(..)
    , Issue
    , Repo
    , User
    , getIssues
    , getRepo
    , getRepos
    , getUser
    )

import Dict exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Req
import Task exposing (Task)


getUser : String -> Task Error User
getUser userName =
    Req.get ("https://api.github.com/users/" ++ userName)
        |> Req.jsonTask userDecoder
        |> Task.mapError toError


getRepo : String -> String -> Task Error Repo
getRepo userName repoName =
    Req.get ("https://api.github.com/repos/" ++ userName ++ "/" ++ repoName)
        |> Req.jsonTask repoDecoder
        |> Task.mapError toError


getRepos : String -> Task Error (List Repo)
getRepos userName =
    Req.get ("https://api.github.com/users/" ++ userName ++ "/repos")
        |> Req.jsonTask (D.list repoDecoder)
        |> Task.mapError toError


getIssues : String -> String -> Task Error (List Issue)
getIssues userName repoName =
    Req.get ("https://api.github.com/repos/" ++ userName ++ "/" ++ repoName ++ "/issues")
        |> Req.jsonTask (D.list issueDecoder)
        |> Task.mapError toError


toError : Req.StringReqError -> Error
toError { req, res, decodeError } =
    { method = req.method
    , url = req.url
    , details =
        case decodeError of
            Just e ->
                BadBody (D.errorToString e)

            Nothing ->
                case res of
                    Http.BadUrl_ url ->
                        BadUrl url

                    Http.Timeout_ ->
                        Timeout

                    Http.NetworkError_ ->
                        NetworkError

                    Http.BadStatus_ metadata body ->
                        case D.decodeString (errorDecoder metadata.statusCode) body of
                            Ok info ->
                                BadStatus metadata.statusCode metadata.statusText metadata.headers info

                            Err e ->
                                BadErrorBody (D.errorToString e)

                    Http.GoodStatus_ _ body ->
                        Bug "Unexpected GoodStatus"
    }


type alias Repo =
    { name : String
    , description : Maybe String
    , language : Maybe String
    , owner : String
    , fork : Int
    , star : Int
    , watch : Int
    }


type alias Issue =
    { number : Int
    , title : String
    , state : String
    }


type alias User =
    { login : String
    , avatarUrl : String
    }


type alias Error =
    { method : String
    , url : String
    , details : ErrorDetails
    }


type ErrorDetails
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int String (Dict String String) ErrorInfo
    | BadBody String
    | BadErrorBody String
    | Bug String


type alias ErrorInfo =
    { message : String
    }


userDecoder : Decoder User
userDecoder =
    D.map2 User
        (D.field "login" D.string)
        (D.field "avatar_url" D.string)


repoDecoder : Decoder Repo
repoDecoder =
    D.map7 Repo
        (D.field "name" D.string)
        (D.maybe (D.field "description" D.string))
        (D.maybe (D.field "language" D.string))
        (D.at [ "owner", "login" ] D.string)
        (D.field "forks_count" D.int)
        (D.field "stargazers_count" D.int)
        (D.field "watchers_count" D.int)


issueDecoder : Decoder Issue
issueDecoder =
    D.map3 Issue
        (D.field "number" D.int)
        (D.field "title" D.string)
        (D.field "state" D.string)


errorDecoder : Int -> Decoder ErrorInfo
errorDecoder status =
    D.map ErrorInfo
        (D.field "message" D.string)
