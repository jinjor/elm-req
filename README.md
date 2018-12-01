elm-req
====

An experimental alternative for [elm/http](https://github.com/elm/http).

**Warning: Not fully tested yet**


## Motivation

- Use Tasks more.
- Use `put` `patch` and `delete`.
- Build options one by one.
- Get full information in requests/responses to describe errors.
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

Use `Req.Error` instead of `Http.Error` to get more information.

```elm
type Error a
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata a
    | BadBody Http.Metadata String


getRepo : String -> String -> Task (Req.Error ErrorInfo) Repo
getRepo userName repoName =
    Req.get ("https://api.github.com/repos/" ++ userName ++ "/" ++ repoName)
        |> Req.stringTask
            (Req.resolveJson
                { decoder = repoDecoder, errorDecoder = errorDecoder }
            )
```

Use `Req.ReqWithError` to get full information of the request.

```elm
type Error a
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata a
    | BadBody Http.Metadata String


getRepo : String -> String -> Task (Req.ReqWithError ErrorInfo) Repo
getRepo userName repoName =
    Req.get ("https://api.github.com/repos/" ++ userName ++ "/" ++ repoName)
        |> Req.stringTask
            (Req.resolveJsonWithReq
                { decoder = repoDecoder, errorDecoder = errorDecoder }
            )
```