elm-req
====

An experimental alternative for [elm/http](https://github.com/elm/http).

**Warning: Not fully tested yet, and a big change coming soon!**


## Motivation

- Use Tasks more.
- Use `put` `patch` and `delete`.
- Build options one by one.
- Get full information in requests/responses to describe errors.
- Switch from `Cmd` to `Task` gradually (and vice versa).
- Switch from `Http.Error` to custom error gradually.
- Explicitly say what is risky.


## Examples

Create a task that can return `Http.Error`.

```elm
getRepoCompatible : String -> String -> Task Http.Error Repo
getRepoCompatible userName repoName =
    Req.get ("https://api.github.com/repos/" ++ userName ++ "/" ++ repoName)
        |> Req.jsonCompatible repoDecoder
```

Create a task that can return `Req.Error` which contains the full information.

```elm
getRepo : String -> String -> Task (Req.Error String) Repo
getRepo userName repoName =
    Req.get ("https://api.github.com/repos/" ++ userName ++ "/" ++ repoName)
        |> Req.json repoDecoder
```

```elm
type alias Error a =
    { request : Req
    , error : Problem a
    }

type Problem a
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata a
    | BadBody Http.Metadata String
```

You can also decode error body using `*WithError` family.

```elm
getRepo : String -> String -> Task (Req.Error ErrorInfo) Repo
getRepo userName repoName =
    Req.get ("https://api.github.com/repos/" ++ userName ++ "/" ++ repoName)
        |> Req.jsonWithError
            { decoder = repoDecoder, errorDecoder = errorDecoder }

errorDecoder : Http.Metadata -> Decoder ErrorInfo
errorDecoder meta =
    D.map ErrorInfo
        (D.field "message" D.string)
```