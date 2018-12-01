module Requests exposing (Issue, Repo, User, getIssues, getRepo, getRepos, getUser)

import Json.Decode as D exposing (Decoder)
import Req
import Task exposing (Task)


getUser : String -> Task Req.StringReqError User
getUser userName =
    Req.get ("https://api.github.com/users/" ++ userName)
        |> Req.jsonTask userDecoder


getRepo : String -> String -> Task Req.StringReqError Repo
getRepo userName repoName =
    Req.get ("https://api.github.com/repo/" ++ userName ++ "/" ++ repoName)
        |> Req.jsonTask repoDecoder


getRepos : String -> Task Req.StringReqError (List Repo)
getRepos userName =
    Req.get ("https://api.github.com/users/" ++ userName ++ "/repos")
        |> Req.jsonTask (D.list repoDecoder)


getIssues : String -> String -> Task Req.StringReqError (List Issue)
getIssues userName repoName =
    Req.get ("https://api.github.com/repos/" ++ userName ++ "/" ++ repoName ++ "/issues")
        |> Req.jsonTask (D.list issueDecoder)


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
