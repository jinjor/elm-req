module Requests exposing
    ( ErrorInfo
    , Issue
    , Repo
    , User
    , getRepo
    , getRepoCompatible
    , getRepoWithDecodeError
    , getUser
    , getUserCompatible
    , upload
    )

import Dict exposing (..)
import File exposing (File)
import Http
import Json.Decode as D exposing (Decoder)
import Req
import Task exposing (Task)


upload : String -> (Result Http.Error () -> msg) -> List File -> Cmd msg
upload tracker toMsg files =
    Req.post "/"
        |> Req.withMultipartBody (List.map (Req.filePart "files[]") files)
        |> Req.trackWhatever tracker toMsg


getUserCompatible : String -> Task Http.Error User
getUserCompatible userName =
    Req.get ("https://api.github.com/users/" ++ userName)
        |> Req.jsonTaskCompatible userDecoder


getRepoCompatible : String -> String -> Task Http.Error Repo
getRepoCompatible userName repoName =
    Req.get ("https://api.github.com/repos/" ++ userName ++ "/" ++ repoName)
        |> Req.jsonTaskCompatible repoDecoder


getUser : String -> Task (Req.Error ErrorInfo) User
getUser userName =
    Req.get ("https://api.github.com/users/" ++ userName)
        |> Req.jsonTaskWithError
            { decoder = userDecoder, errorDecoder = errorDecoder }


getRepo : String -> String -> Task (Req.Error ErrorInfo) Repo
getRepo userName repoName =
    Req.get ("https://api.github.com/repos/" ++ userName ++ "/" ++ repoName)
        |> Req.jsonTaskWithError
            { decoder = repoDecoder, errorDecoder = errorDecoder }


getRepoWithDecodeError : String -> String -> Task (Req.Error ErrorInfo) Repo
getRepoWithDecodeError userName repoName =
    Req.get ("https://api.github.com/repos/" ++ userName ++ "/" ++ repoName)
        |> Req.jsonTaskWithError
            { decoder = buggyRepoDecoder, errorDecoder = errorDecoder }


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
