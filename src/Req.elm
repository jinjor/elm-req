module Req exposing
    ( Req, Body(..), Part(..), Error(..), ReqWithError
    , init, get, post, put, patch, delete
    , withStringBody, withJsonBody, withFileBody, withBytesBody, withMultipartBody
    , stringPart, filePart, bytesPart
    , withHeader, withTimeout, allowCookiesFromOtherDomains
    , stringTask, bytesTask, whateverTask, toTask
    , simplyResolveJson, resolveJson, resolveJsonWithReq
    , trackString, trackBytes, trackWhatever, track
    )

{-| An experimental alternative for [elm/http](https://github.com/elm/http).

See more details in [elm/http](https://package.elm-lang.org/packages/elm/http/latest/Http).


# Types

@docs Req, Body, Part, Error, ReqWithError


# Methods

@docs init, get, post, put, patch, delete


# Body

@docs withStringBody, withJsonBody, withFileBody, withBytesBody, withMultipartBody


# Parts

@docs stringPart, filePart, bytesPart


# Options

@docs withHeader, withTimeout, allowCookiesFromOtherDomains


# Task

@docs stringTask, bytesTask, whateverTask, toTask


# Resolver

@docs simplyResolveJson, resolveJson, resolveJsonWithReq


# Tracking

@docs trackString, trackBytes, trackWhatever, track

-}

import Bytes exposing (Bytes)
import Bytes.Decode
import File exposing (File)
import Http
import Json.Decode
import Json.Encode
import Task exposing (Task)



-- TYPES


{-| Request
-}
type alias Req =
    { method : String
    , url : String
    , headers : List ( String, String )
    , body : Body
    , timeout : Maybe Float
    , allowCookiesFromOtherDomains : Bool
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
type Error a
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata a
    | BadBody Http.Metadata String


{-| Req with Error
-}
type alias ReqWithError a =
    { request : Req
    , error : Error a
    }



-- METHOD


{-| GET
-}
get : String -> Req
get url =
    init "GET" url


{-| POST
-}
post : String -> Req
post url =
    init "POST" url


{-| PUT
-}
put : String -> Req
put url =
    init "PUT" url


{-| PATCH
-}
patch : String -> Req
patch url =
    init "PATCH" url


{-| DELETE
-}
delete : String -> Req
delete url =
    init "DELETE" url


{-| Arbitrary method
-}
init : String -> String -> Req
init method url =
    { method = method
    , url = url
    , headers = []
    , body = EmptyBody
    , timeout = Nothing
    , allowCookiesFromOtherDomains = False
    }



-- BODY


{-| Add text with mime type.
-}
withStringBody : String -> String -> Req -> Req
withStringBody mime body req =
    { req
        | body = StringBody mime body
    }


{-| Add JSON for POST etc.
-}
withJsonBody : Json.Encode.Value -> Req -> Req
withJsonBody body req =
    { req
        | body = JsonBody body
    }


{-| Add file to upload
-}
withFileBody : File -> Req -> Req
withFileBody file req =
    { req
        | body = FileBody file
    }


{-| Add bytes with mime type.
-}
withBytesBody : String -> Bytes -> Req -> Req
withBytesBody mime bytes req =
    { req
        | body = BytesBody mime bytes
    }


{-| Add multipart body.
-}
withMultipartBody : List Part -> Req -> Req
withMultipartBody parts req =
    { req
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
bytesPart key mime bytes =
    BytesPart key mime bytes



-- OPTIONS


{-| Add header
-}
withHeader : String -> String -> Req -> Req
withHeader key value req =
    { req | headers = ( key, value ) :: req.headers }


{-| Add timeout
-}
withTimeout : Float -> Req -> Req
withTimeout timeout req =
    { req | timeout = Just timeout }


{-| Allow cookies from other domains.
-}
allowCookiesFromOtherDomains : Bool -> Req -> Req
allowCookiesFromOtherDomains allowCookiesFromOtherDomains_ req =
    { req | allowCookiesFromOtherDomains = allowCookiesFromOtherDomains_ }



-- TASK


{-| Create a task to parse string body.
-}
stringTask :
    (Req -> Http.Response String -> Result x a)
    -> Req
    -> Task x a
stringTask resolve req =
    toTask (Http.stringResolver (resolve req)) req


{-| Create a task to parse bytes body.
-}
bytesTask :
    (Req -> Http.Response Bytes -> Result x a)
    -> Req
    -> Task x a
bytesTask resolve req =
    toTask (Http.bytesResolver (resolve req)) req


{-| Ignore the result.
-}
whateverTask :
    msg
    -> Req
    -> Task x msg
whateverTask msg req =
    toTask (Http.bytesResolver (\res -> Ok msg)) req


{-| Create a task with existing `Http.Resolver`.
-}
toTask : Http.Resolver x a -> Req -> Task x a
toTask resolver req =
    (if req.allowCookiesFromOtherDomains then
        Http.riskyTask

     else
        Http.task
    )
        { method = req.method
        , headers = List.map (\( k, v ) -> Http.header k v) req.headers
        , url = req.url
        , body = toHttpBody req.body
        , resolver = resolver
        , timeout = req.timeout
        }



-- RESOLVER


{-| Make a resolver function that returns `Http.Error`.

    getUserSimple : String -> Task Http.Error User
    getUserSimple userName =
        Req.get ("https://api.github.com/users/" ++ userName)
            |> Req.stringTask (Req.simplyResolveJson userDecoder)

-}
simplyResolveJson : Json.Decode.Decoder a -> Req -> Http.Response String -> Result Http.Error a
simplyResolveJson decoder _ res =
    case res of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata body ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Ok a ->
                    Ok a

                Err e ->
                    Err (Http.BadBody (Json.Decode.errorToString e))


{-| Make a resolver function that returns `Req.Error`.

    getUserSimple : String -> Task Http.Error User
    getUserSimple userName =
        Req.get ("https://api.github.com/users/" ++ userName)
            |> Req.stringTask
                (Req.resolveJson
                    { decoder = userDecoder
                    , errorDecoder = errorDecoder
                    }
                )

    - First decoder is used for good body
    - First decoder is used for bad body
    - Decoding errors of both go to BadBody

-}
resolveJson :
    { decoder : Json.Decode.Decoder a
    , errorDecoder : Http.Metadata -> Json.Decode.Decoder e
    }
    -> Req
    -> Http.Response String
    -> Result (Error e) a
resolveJson { decoder, errorDecoder } req res =
    case res of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            case Json.Decode.decodeString (errorDecoder metadata) body of
                Ok a ->
                    Err (BadStatus metadata a)

                Err e ->
                    Err (BadBody metadata (Json.Decode.errorToString e))

        Http.GoodStatus_ metadata body ->
            case Json.Decode.decodeString decoder body of
                Ok a ->
                    Ok a

                Err e ->
                    Err (BadBody metadata (Json.Decode.errorToString e))


{-| resolve Json and return Error with Req.
-}
resolveJsonWithReq :
    { decoder : Json.Decode.Decoder a
    , errorDecoder : Http.Metadata -> Json.Decode.Decoder e
    }
    -> Req
    -> Http.Response String
    -> Result (ReqWithError e) a
resolveJsonWithReq options req res =
    resolveJson options req res
        |> Result.mapError (ReqWithError req)



-- TRACKING


{-| Track progress of responses with string body.
-}
trackString :
    String
    -> (Result x a -> msg)
    -> (Req -> Http.Response String -> Result x a)
    -> Req
    -> Cmd msg
trackString tracker toMsg resolve req =
    track
        tracker
        (Http.expectStringResponse toMsg (resolve req))
        req


{-| Track progress of responses with bytes body.
-}
trackBytes :
    String
    -> (Result x a -> msg)
    -> (Req -> Http.Response Bytes -> Result x a)
    -> Req
    -> Cmd msg
trackBytes tracker toMsg resolve req =
    track
        tracker
        (Http.expectBytesResponse toMsg (resolve req))
        req


{-| Track something but ignore the result.
-}
trackWhatever :
    String
    -> (Result Never () -> msg)
    -> Req
    -> Cmd msg
trackWhatever tracker toMsg req =
    track
        tracker
        (Http.expectBytesResponse toMsg (\res -> Ok ()))
        req


{-| Make a traking Cmd using existing `Http.Expect`.
-}
track :
    String
    -> Http.Expect msg
    -> Req
    -> Cmd msg
track tracker expect req =
    (if req.allowCookiesFromOtherDomains then
        Http.riskyRequest

     else
        Http.request
    )
        { method = req.method
        , headers = toHttpHeaders req.headers
        , url = req.url
        , body = toHttpBody req.body
        , expect = expect
        , timeout = req.timeout
        , tracker = Just tracker
        }



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

        JsonBody json ->
            Http.stringBody "application/json" (Json.Encode.encode 0 json)

        FileBody file ->
            Http.fileBody file

        BytesBody mime bytes ->
            Http.bytesBody mime bytes

        MultipartBody parts ->
            Http.multipartBody (List.map toHttpPart parts)


toHttpPart : Part -> Http.Part
toHttpPart part =
    case part of
        StringPart key value ->
            Http.stringPart key value

        FilePart key file ->
            Http.filePart key file

        BytesPart key mime bytes ->
            Http.bytesPart key mime bytes
