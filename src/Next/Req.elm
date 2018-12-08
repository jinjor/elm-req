module Next.Req exposing (Body(..), Builder, Error, Expect(..), Init, Part(..), Problem(..), Request, absolute, bytesPart, delete, expectBytes, expectJson, filePart, from, get, init, map, mapError, mapProblem, patch, post, put, relative, send, setExpect, stringPart, toHttpBody, toHttpExpect, toHttpHeaders, toHttpPart, toResolver, toTask, withBytesBody, withCredentials, withFileBody, withHeader, withJsonBody, withMultipartBody, withStringBody, withTimeout)

{-| WIP
-}

import Bytes exposing (Bytes)
import Bytes.Decode
import File exposing (File)
import Http
import Json.Decode
import Json.Encode
import Task exposing (Task)
import Url.Builder exposing (QueryParameter, Root)



-- TYPES


{-| Request
-}
type alias Request =
    { method : String
    , url : String
    , headers : List ( String, String )
    , body : String
    , timeout : Maybe Float
    , credentials : Bool
    , tracker : Maybe String
    }


{-| Builder
-}
type alias Builder e a =
    { method : String
    , root : Root
    , path : List String
    , query : List QueryParameter
    , headers : List ( String, String )
    , body : Body
    , timeout : Maybe Float
    , credentials : Bool
    , tracker : Maybe String
    , expect : Expect e a
    }


{-| Body
-}
type Body
    = EmptyBody
    | StringBody String String
    | JsonBody Json.Encode.Value
    | FileBody File
    | BytesBody String Bytes
    | MultipartBody (List Part)


{-| Part
-}
type Part
    = StringPart String String
    | FilePart String File
    | BytesPart String String Bytes


{-| Similar to `Http.Error` but have more informarion
-}
type Problem a
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata a
    | BadBody Http.Metadata String


{-| Request with Problem
-}
type alias Error a =
    { request : Request
    , problem : Problem a
    }


type Expect e a
    = ExpectString (Request -> Http.Response String -> Result e a)
    | ExpectBytes (Request -> Http.Response Bytes -> Result e a)



-- INIT


{-| -}
type alias Init =
    { get : List String -> Builder (Error String) String
    , post : List String -> Builder (Error String) String
    , put : List String -> Builder (Error String) String
    , patch : List String -> Builder (Error String) String
    , delete : List String -> Builder (Error String) String
    , method : String -> List String -> Builder (Error String) String
    }


{-| -}
from : String -> Init
from root =
    { get = get (Url.Builder.CrossOrigin root)
    , post = post (Url.Builder.CrossOrigin root)
    , put = put (Url.Builder.CrossOrigin root)
    , patch = patch (Url.Builder.CrossOrigin root)
    , delete = delete (Url.Builder.CrossOrigin root)
    , method = init (Url.Builder.CrossOrigin root)
    }


{-| -}
absolute : Init
absolute =
    { get = get Url.Builder.Absolute
    , post = post Url.Builder.Absolute
    , put = put Url.Builder.Absolute
    , patch = patch Url.Builder.Absolute
    , delete = delete Url.Builder.Absolute
    , method = init Url.Builder.Absolute
    }


{-| -}
relative : Init
relative =
    { get = get Url.Builder.Relative
    , post = post Url.Builder.Relative
    , put = put Url.Builder.Relative
    , patch = patch Url.Builder.Relative
    , delete = delete Url.Builder.Relative
    , method = init Url.Builder.Relative
    }



-- METHOD


get : Root -> List String -> Builder (Error String) String
get root path =
    init root "GET" path


post : Root -> List String -> Builder (Error String) String
post root path =
    init root "POST" path


put : Root -> List String -> Builder (Error String) String
put root path =
    init root "PUT" path


patch : Root -> List String -> Builder (Error String) String
patch root path =
    init root "PATCH" path


delete : Root -> List String -> Builder (Error String) String
delete root path =
    init root "DELETE" path


init : Root -> String -> List String -> Builder (Error String) String
init root method path =
    { method = method
    , root = root
    , path = path
    , query = []
    , headers = []
    , body = EmptyBody
    , timeout = Nothing
    , credentials = False
    , tracker = Nothing
    , expect =
        ExpectString
            (\req res ->
                Result.mapError (Error req) <|
                    case res of
                        Http.BadUrl_ url ->
                            Err (BadUrl url)

                        Http.Timeout_ ->
                            Err Timeout

                        Http.NetworkError_ ->
                            Err NetworkError

                        Http.BadStatus_ metadata body ->
                            Err (BadStatus metadata body)

                        Http.GoodStatus_ metadata body ->
                            Ok body
            )
    }



-- BODY


{-| Add text with mime type.
-}
withStringBody : String -> String -> Builder x a -> Builder x a
withStringBody mime body builder =
    { builder
        | body = StringBody mime body
    }


{-| Add JSON for POST etc.
-}
withJsonBody : Json.Encode.Value -> Builder x a -> Builder x a
withJsonBody body builder =
    { builder
        | body = JsonBody body
    }


{-| Add file to upload
-}
withFileBody : File -> Builder x a -> Builder x a
withFileBody file builder =
    { builder
        | body = FileBody file
    }


{-| Add bytes with mime type.
-}
withBytesBody : String -> Bytes -> Builder x a -> Builder x a
withBytesBody mime bytes_ builder =
    { builder
        | body = BytesBody mime bytes_
    }


{-| Add multipart body.
-}
withMultipartBody : List Part -> Builder x a -> Builder x a
withMultipartBody parts builder =
    { builder
        | body = MultipartBody parts
    }



-- PARTS


{-| String part (key and value)
-}
stringPart : String -> String -> Part
stringPart key value =
    StringPart key value


{-| String part (key and file)
-}
filePart : String -> File -> Part
filePart key file =
    FilePart key file


{-| Bytes part (key and mime and bytes)
-}
bytesPart : String -> String -> Bytes -> Part
bytesPart key mime bytes_ =
    BytesPart key mime bytes_



-- OPTIONS


{-| Add header
-}
withHeader : String -> String -> Builder x a -> Builder x a
withHeader key value builder =
    { builder | headers = ( key, value ) :: builder.headers }


{-| Add timeout
-}
withTimeout : Float -> Builder x a -> Builder x a
withTimeout timeout builder =
    { builder | timeout = Just timeout }


{-| Allow cookies from other domains.
-}
withCredentials : Builder e a -> Builder e a
withCredentials builder =
    { builder | credentials = True }



-- EXPECT


setExpect : Expect x a -> Builder y b -> Builder x a
setExpect expect builder =
    { method = builder.method
    , root = builder.root
    , path = builder.path
    , query = builder.query
    , headers = builder.headers
    , body = builder.body
    , timeout = builder.timeout
    , credentials = builder.credentials
    , tracker = builder.tracker
    , expect = expect
    }


{-| -}
expectJson : Json.Decode.Decoder a -> Builder x z -> Builder (Error String) a
expectJson decoder builder =
    setExpect
        (ExpectString
            (\req res ->
                Result.mapError (Error req) <|
                    case res of
                        Http.BadUrl_ url ->
                            Err (BadUrl url)

                        Http.Timeout_ ->
                            Err Timeout

                        Http.NetworkError_ ->
                            Err NetworkError

                        Http.BadStatus_ meta body ->
                            Err (BadStatus meta body)

                        Http.GoodStatus_ meta body ->
                            case Json.Decode.decodeString decoder body of
                                Ok a ->
                                    Ok a

                                Err e ->
                                    Err (BadBody meta (Json.Decode.errorToString e))
            )
        )
        builder


{-| -}
expectBytes : Bytes.Decode.Decoder a -> Builder x z -> Builder (Error Bytes) a
expectBytes decoder builder =
    setExpect
        (ExpectBytes
            (\req res ->
                Result.mapError (Error req) <|
                    case res of
                        Http.BadUrl_ url ->
                            Err (BadUrl url)

                        Http.Timeout_ ->
                            Err Timeout

                        Http.NetworkError_ ->
                            Err NetworkError

                        Http.BadStatus_ meta body ->
                            Err (BadStatus meta body)

                        Http.GoodStatus_ meta body ->
                            case Bytes.Decode.decode decoder body of
                                Just a ->
                                    Ok a

                                Nothing ->
                                    Err (BadBody meta "unexpected bytes")
            )
        )
        builder


{-| -}
map : (a -> b) -> Builder x a -> Builder x b
map f builder =
    setExpect
        (case builder.expect of
            ExpectString fromString ->
                ExpectString (\req res -> fromString req res |> Result.map f)

            ExpectBytes fromBytes ->
                ExpectBytes (\req res -> fromBytes req res |> Result.map f)
        )
        builder


{-| -}
mapError : (x -> y) -> Builder x a -> Builder y a
mapError f builder =
    setExpect
        (case builder.expect of
            ExpectString fromString ->
                ExpectString (\req res -> fromString req res |> Result.mapError f)

            ExpectBytes fromBytes ->
                ExpectBytes (\req res -> fromBytes req res |> Result.mapError f)
        )
        builder



-- CONVERT


toHttpHeaders : List ( String, String ) -> List Http.Header
toHttpHeaders headers =
    List.map (\( k, v ) -> Http.header k v) headers


toHttpBody : Body -> Http.Body
toHttpBody body =
    case body of
        EmptyBody ->
            Http.emptyBody

        StringBody mime str ->
            Http.stringBody mime str

        JsonBody value ->
            Http.stringBody "application/json" (Json.Encode.encode 0 value)

        FileBody file ->
            Http.fileBody file

        BytesBody mime bytes_ ->
            Http.bytesBody mime bytes_

        MultipartBody parts ->
            Http.multipartBody (List.map toHttpPart parts)


toHttpPart : Part -> Http.Part
toHttpPart part =
    case part of
        StringPart key value ->
            Http.stringPart key value

        FilePart key file ->
            Http.filePart key file

        BytesPart key mime bytes_ ->
            Http.bytesPart key mime bytes_


toResolver : Request -> Expect x a -> Http.Resolver x a
toResolver req expect =
    case expect of
        ExpectString f ->
            Http.stringResolver (f req)

        ExpectBytes f ->
            Http.bytesResolver (f req)


toHttpExpect : Request -> (Result x a -> b) -> Expect x a -> Http.Expect b
toHttpExpect req g expect =
    case expect of
        ExpectString f ->
            Http.expectStringResponse g (f req)

        ExpectBytes f ->
            Http.expectBytesResponse g (f req)



-- TASK


{-| -}
toTask : Builder x a -> Task x a
toTask builder =
    let
        req : Request
        req =
            { method = builder.method
            , url = Url.Builder.custom builder.root builder.path builder.query Nothing
            , headers = builder.headers
            , body =
                case builder.body of
                    JsonBody json ->
                        Json.Encode.encode 0 json

                    _ ->
                        Debug.todo ""
            , timeout = builder.timeout
            , credentials = builder.credentials
            , tracker = builder.tracker
            }
    in
    (if req.credentials then
        Http.riskyTask

     else
        Http.task
    )
        { method = builder.method
        , headers = toHttpHeaders builder.headers
        , url = req.url
        , body = toHttpBody builder.body
        , resolver = toResolver req builder.expect
        , timeout = builder.timeout
        }


{-| -}
send : (Result x a -> msg) -> Builder x a -> Cmd msg
send toMsg builder =
    let
        req =
            { method = builder.method
            , url = Url.Builder.custom builder.root builder.path builder.query Nothing
            , headers = builder.headers
            , body =
                case builder.body of
                    JsonBody json ->
                        Json.Encode.encode 0 json

                    _ ->
                        Debug.todo ""
            , timeout = builder.timeout
            , credentials = builder.credentials
            , tracker = builder.tracker
            }
    in
    (if req.credentials then
        Http.riskyRequest

     else
        Http.request
    )
        { method = req.method
        , headers = toHttpHeaders req.headers
        , url = req.url
        , body = toHttpBody builder.body
        , expect = toHttpExpect req toMsg builder.expect
        , timeout = req.timeout
        , tracker = builder.tracker
        }
