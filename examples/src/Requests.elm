module Requests exposing
    ( Error
    , ErrorDetails(..)
    , ErrorInfo
    , Issue
    , Repo
    , User
    , getRepo
    , getRepoSimple
    , getRepoWithDecodeError
    , getUser
    , getUserSimple
    )

import Dict exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Req
import Task exposing (Task)


getUserSimple : String -> Task Http.Error User
getUserSimple userName =
    Req.get ("https://api.github.com/users/" ++ userName)
        |> Req.stringTask (Req.simplyResolveJson userDecoder)


getRepoSimple : String -> String -> Task Http.Error Repo
getRepoSimple userName repoName =
    Req.get ("https://api.github.com/repos/" ++ userName ++ "/" ++ repoName)
        |> Req.stringTask (Req.simplyResolveJson repoDecoder)


getUser : String -> Task Error User
getUser userName =
    Req.get ("https://api.github.com/users/" ++ userName)
        |> Req.stringTask (resolve userDecoder)


getRepo : String -> String -> Task Error Repo
getRepo userName repoName =
    Req.get ("https://api.github.com/repos/" ++ userName ++ "/" ++ repoName)
        |> Req.stringTask (resolve repoDecoder)


getRepoWithDecodeError : String -> String -> Task Error Repo
getRepoWithDecodeError userName repoName =
    Req.get ("https://api.github.com/repos/" ++ userName ++ "/" ++ repoName)
        |> Req.stringTask (resolve buggyRepoDecoder)


resolve : Decoder a -> Req.Req -> Http.Response String -> Result Error a
resolve decoder req res =
    case res of
        Http.BadUrl_ url ->
            err req (BadUrl url)

        Http.Timeout_ ->
            err req Timeout

        Http.NetworkError_ ->
            err req NetworkError

        Http.BadStatus_ metadata body ->
            case D.decodeString (errorDecoder metadata.statusCode) body of
                Ok info ->
                    err req (BadStatus metadata.statusCode metadata.statusText metadata.headers info)

                Err e ->
                    err req (BadErrorBody (D.errorToString e))

        Http.GoodStatus_ _ body ->
            case D.decodeString decoder body of
                Ok a ->
                    Ok a

                Err e ->
                    err req (BadBody (D.errorToString e))


err : Req.Req -> ErrorDetails -> Result Error a
err req details =
    Err
        { method = req.method
        , url = req.url
        , details = details
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


buggyRepoDecoder : Decoder Repo
buggyRepoDecoder =
    D.map7 Repo
        (D.field "name" D.string)
        (D.maybe (D.field "description" D.string))
        (D.maybe (D.field "language" D.string))
        (D.at [ "owner", "login" ] D.string)
        -- wrong field name
        (D.field "forks_number" D.int)
        (D.field "stargazers_count" D.int)
        (D.field "watchers_count" D.int)


errorDecoder : Int -> Decoder ErrorInfo
errorDecoder status =
    D.map ErrorInfo
        (D.field "message" D.string)
