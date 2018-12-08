module Next exposing
    ( ErrorInfo
    , Issue
    , Repo
    , User
    , getUser
    )

import Dict exposing (..)
import File exposing (File)
import Http
import Json.Decode as D exposing (Decoder)
import Next.Req as Req
import Task exposing (Task)


github : Req.Init
github =
    Req.from "https://api.github.com"


getUser : String -> Task (Req.Error String) User
getUser userName =
    github.get [ "users", userName ]
        |> Req.expectJson userDecoder
        |> Req.toTask


getRepoBuilder : String -> String -> Req.Builder (Req.Error String) Repo
getRepoBuilder userName repoName =
    github.get [ "repos", userName, repoName ]
        |> Req.expectJson repoDecoder


getRepo : String -> String -> Task (Req.Error String) Repo
getRepo userName repoName =
    getRepoBuilder userName repoName
        |> Req.toTask


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


errorDecoder : Http.Metadata -> Decoder ErrorInfo
errorDecoder _ =
    D.map ErrorInfo
        (D.field "message" D.string)
