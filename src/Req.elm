module Req exposing
    ( Req, Body(..), Part(..)
    , get, post, put, patch, delete
    , withStringBody, withJsonBody, withFileBody, withBytesBody, withMultipartBody
    , withHeader, withTimeout, allowCookiesFromOtherDomains
    , stringPart, filePart, bytesPart
    , stringTask, bytesTask, whateverTask, toTask
    , simplyResolveJson
    , trackString, trackBytes, trackWhatever, track
    )

{-|


# Types

@docs Req, Body, Part, StringReqError, BytesReqError


# Methods

@docs get, post, put, patch, delete


# Body

@docs withStringBody, withJsonBody, withFileBody, withBytesBody, withMultipartBody


# Othres

@docs withHeader, withTimeout, allowCookiesFromOtherDomains


# Parts

@docs stringPart, filePart, bytesPart


# Task

@docs stringTask, bytesTask, whateverTask, toTask


# Resolver

@docs simplyResolveJson


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


{-| -}
type alias Req =
    { method : String
    , url : String
    , headers : List ( String, String )
    , body : Body
    , timeout : Maybe Float
    , allowCookiesFromOtherDomains : Bool
    }


{-| -}
type Body
    = EmptyBody
    | StringBody String String
    | JsonBody Json.Encode.Value
    | FileBody File
    | BytesBody String Bytes
    | MultipartBody (List Part)


{-| -}
type Part
    = StringPart String String
    | FilePart String File
    | BytesPart String String Bytes



-- METHOD


{-| -}
get : String -> Req
get url =
    init "GET" url


{-| -}
post : String -> Req
post url =
    init "POST" url


{-| -}
put : String -> Req
put url =
    init "PUT" url


{-| -}
patch : String -> Req
patch url =
    init "PATCH" url


{-| -}
delete : String -> Req
delete url =
    init "DELETE" url


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


{-| -}
withStringBody : String -> String -> Req -> Req
withStringBody mime body req =
    { req
        | body = StringBody mime body
    }


{-| -}
withJsonBody : Json.Encode.Value -> Req -> Req
withJsonBody body req =
    { req
        | body = JsonBody body
    }


{-| -}
withFileBody : File -> Req -> Req
withFileBody file req =
    { req
        | body = FileBody file
    }


{-| -}
withBytesBody : String -> Bytes -> Req -> Req
withBytesBody mime bytes req =
    { req
        | body = BytesBody mime bytes
    }


{-| -}
withMultipartBody : List Part -> Req -> Req
withMultipartBody parts req =
    { req
        | body = MultipartBody parts
    }



-- OTHERS


{-| -}
withHeader : String -> String -> Req -> Req
withHeader key value req =
    { req | headers = ( key, value ) :: req.headers }


{-| -}
withTimeout : Float -> Req -> Req
withTimeout timeout req =
    { req | timeout = Just timeout }


{-| -}
allowCookiesFromOtherDomains : Bool -> Req -> Req
allowCookiesFromOtherDomains allowCookiesFromOtherDomains_ req =
    { req | allowCookiesFromOtherDomains = allowCookiesFromOtherDomains_ }



-- PARTS


{-| -}
stringPart : String -> String -> Part
stringPart key value =
    StringPart key value


{-| -}
filePart : String -> File -> Part
filePart key file =
    FilePart key file


{-| -}
bytesPart : String -> String -> Bytes -> Part
bytesPart key mime bytes =
    BytesPart key mime bytes



-- TASK


{-| -}
stringTask :
    (Req -> Http.Response String -> Result x a)
    -> Req
    -> Task x a
stringTask resolve req =
    toTask (Http.stringResolver (resolve req)) req


{-| -}
bytesTask :
    (Req -> Http.Response Bytes -> Result x a)
    -> Req
    -> Task x a
bytesTask resolve req =
    toTask (Http.bytesResolver (resolve req)) req


{-| -}
whateverTask :
    msg
    -> Req
    -> Task x msg
whateverTask msg req =
    toTask (Http.bytesResolver (\res -> Ok msg)) req


{-| -}
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


{-| -}
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



-- TRACKING


{-| -}
trackString :
    String
    -> (Result x a -> msg)
    -> (Req -> Http.Response String -> Result x a)
    -> Req
    -> Cmd msg
trackString tracker toMsg resolve req =
    track
        (Http.expectStringResponse toMsg (resolve req))
        tracker
        req


{-| -}
trackBytes :
    String
    -> (Result x a -> msg)
    -> (Req -> Http.Response Bytes -> Result x a)
    -> Req
    -> Cmd msg
trackBytes tracker toMsg resolve req =
    track
        (Http.expectBytesResponse toMsg (resolve req))
        tracker
        req


{-| -}
trackWhatever :
    String
    -> (Result Never () -> msg)
    -> Req
    -> Cmd msg
trackWhatever tracker toMsg req =
    track
        (Http.expectBytesResponse toMsg (\res -> Ok ()))
        tracker
        req


{-| -}
track :
    Http.Expect msg
    -> String
    -> Req
    -> Cmd msg
track expect tracker req =
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
