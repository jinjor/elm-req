module Req exposing
    ( Req, Body(..), Part(..), StringReqError, BytesReqError
    , get, post, put, patch, delete
    , withStringBody, withJsonBody, withFileBody, withBytesBody, withMultipartBody
    , withHeader, withTimeout, allowCookiesFromOtherDomains
    , stringPart, filePart, bytesPart
    , stringTask, jsonTask, bytesTask, whateverTask, toTask
    , trackString, trackJson, trackBytes, trackWhatever, track
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

@docs stringTask, jsonTask, bytesTask, whateverTask, toTask


# Tracking

@docs trackString, trackJson, trackBytes, trackWhatever, track

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


{-| -}
type alias StringReqError =
    { req : Req
    , res : Http.Response String
    , decodeError : Maybe Json.Decode.Error
    }


{-| -}
type alias BytesReqError =
    { req : Req
    , res : Http.Response Bytes
    , decodeError : Bool
    }



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
    Req
    -> Task StringReqError String
stringTask req =
    toTask
        (Http.stringResolver
            (\res ->
                case res of
                    Http.GoodStatus_ meta body ->
                        Ok body

                    _ ->
                        Err { req = req, res = res, decodeError = Nothing }
            )
        )
        req


{-| -}
jsonTask :
    Json.Decode.Decoder a
    -> Req
    -> Task StringReqError a
jsonTask decoder req =
    toTask
        (Http.stringResolver
            (\res ->
                case res of
                    Http.GoodStatus_ meta body ->
                        Result.mapError
                            (\err -> { req = req, res = res, decodeError = Just err })
                            (Json.Decode.decodeString decoder body)

                    _ ->
                        Err { req = req, res = res, decodeError = Nothing }
            )
        )
        req


{-| -}
bytesTask :
    Bytes.Decode.Decoder a
    -> Req
    -> Task BytesReqError a
bytesTask decoder req =
    toTask
        (Http.bytesResolver
            (\res ->
                case res of
                    Http.GoodStatus_ meta body ->
                        case Bytes.Decode.decode decoder body of
                            Just a ->
                                Ok a

                            Nothing ->
                                Err { req = req, res = res, decodeError = True }

                    _ ->
                        Err { req = req, res = res, decodeError = False }
            )
        )
        req


{-| -}
whateverTask :
    msg
    -> Req
    -> Task BytesReqError msg
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



-- TRACKING


{-| -}
trackString :
    String
    -> (Result StringReqError String -> msg)
    -> Req
    -> Cmd msg
trackString tracker toMsg req =
    track
        (Http.expectStringResponse
            toMsg
            (\res ->
                case res of
                    Http.GoodStatus_ meta body ->
                        Ok body

                    _ ->
                        Err { req = req, res = res, decodeError = Nothing }
            )
        )
        tracker
        req


{-| -}
trackJson :
    String
    -> (Result StringReqError a -> msg)
    -> Json.Decode.Decoder a
    -> Req
    -> Cmd msg
trackJson tracker toMsg decoder req =
    track
        (Http.expectStringResponse
            toMsg
            (\res ->
                case res of
                    Http.GoodStatus_ meta body ->
                        Result.mapError
                            (\err -> { req = req, res = res, decodeError = Just err })
                            (Json.Decode.decodeString decoder body)

                    _ ->
                        Err { req = req, res = res, decodeError = Nothing }
            )
        )
        tracker
        req


{-| -}
trackBytes :
    String
    -> (Result BytesReqError a -> msg)
    -> Bytes.Decode.Decoder a
    -> Req
    -> Cmd msg
trackBytes tracker toMsg decoder req =
    track
        (Http.expectBytesResponse
            toMsg
            (\res ->
                case res of
                    Http.GoodStatus_ meta body ->
                        case Bytes.Decode.decode decoder body of
                            Just a ->
                                Ok a

                            Nothing ->
                                Err { req = req, res = res, decodeError = True }

                    _ ->
                        Err { req = req, res = res, decodeError = False }
            )
        )
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
