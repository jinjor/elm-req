elm-req
====

An experimental alternative for [elm/http](https://github.com/elm/http).

**Warning: Not fully tested yet**


## Motivation

- Use `put` `patch` and `delete`.
- Build options one by one.
- Get full information in requests/responses to describe errors.
- Get full information in requests to describe errors.
- Switch from `Cmd` to `Task` gradually (and vice versa).
- Switch from `Http.Error` to custom error gradually.
- Explicitly say what is risky.


## Examples

Create a task that works just like `get`.

```elm
getUserSimple : String -> Task Http.Error User
getUserSimple userName =
    Req.get ("https://api.github.com/users/" ++ userName)
        |> Req.stringTask (Req.simplyResolveJson userDecoder)
```

Work with custom error that enables

- Full access to `Req` / `Http.Response String`.
- Have decoded error JSON.

```elm
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
```