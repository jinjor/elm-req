module Req exposing
    ( Req, Body(..), Part(..), Error, Problem(..)
    , init, get, post, put, patch, delete
    , withStringBody, withJsonBody, withFileBody, withBytesBody, withMultipartBody
    , stringPart, filePart, bytesPart
    , withHeader, withTimeout, allowCookiesFromOtherDomains
    , stringCompatible, string, stringWithError
    , jsonCompatible, json, jsonWithError
    , bytesCompatible, bytes, bytesWithError
    , whatever, toTask
    , trackStringCompatible, trackString, trackStringWithError
    , trackJsonCompatible, trackJson, trackJsonWithError
    , trackBytesCompatible, trackBytes, trackBytesWithError
    , trackWhatever, track
    )

{-| An experimental alternative for [elm/http](https://github.com/elm/http).

See more details in [elm/http](https://package.elm-lang.org/packages/elm/http/latest/Http).


# Types

@docs Req, Body, Part, Error, Problem


# Methods

@docs init, get, post, put, patch, delete


# Body

@docs withStringBody, withJsonBody, withFileBody, withBytesBody, withMultipartBody
@docs stringPart, filePart, bytesPart


# Options

@docs withHeader, withTimeout, allowCookiesFromOtherDomains


# Task

@docs stringCompatible, string, stringWithError
@docs jsonCompatible, json, jsonWithError
@docs bytesCompatible, bytes, bytesWithError
@docs whatever, toTask


# Tracking

@docs trackStringCompatible, trackString, trackStringWithError
@docs trackJsonCompatible, trackJson, trackJsonWithError
@docs trackBytesCompatible, trackBytes, trackBytesWithError
@docs trackWhatever, track

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
type Problem a
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata a
    | BadBody Http.Metadata String


{-| Req with Error
-}
type alias Error a =
    { request : Req
    , error : Problem a
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
withBytesBody mime bytes_ req =
    { req
        | body = BytesBody mime bytes_
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
bytesPart key mime bytes_ =
    BytesPart key mime bytes_



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



-- TASK


{-| Make a task that returns a string or `Http.Error`
-}
stringCompatible : Req -> Task Http.Error String
stringCompatible req =
    toTask
        (Http.stringResolver (resolveStringCompatible req))
        req


{-| Make a task that returns a string or `Req.Error`
-}
string : Req -> Task (Error String) String
string req =
    toTask
        (Http.stringResolver
            (resolveString
                (\meta body -> Json.Decode.succeed body)
                req
            )
        )
        req


{-| Make a task that returns a string or `Req.Error` with custom error.
-}
stringWithError :
    (Http.Metadata -> Json.Decode.Decoder e)
    -> Req
    -> Task (Error e) String
stringWithError errorDecoder req =
    toTask
        (Http.stringResolver
            (resolveString
                (\meta body -> errorDecoder meta)
                req
            )
        )
        req


{-| Make a task that returns an arbitrary data or `Http.Error`
-}
jsonCompatible :
    Json.Decode.Decoder a
    -> Req
    -> Task Http.Error a
jsonCompatible decoder req =
    toTask
        (Http.stringResolver (resolveJsonCompatible decoder req))
        req


{-| Make a task that returns an arbitrary data or `Req.Error`
-}
json :
    Json.Decode.Decoder a
    -> Req
    -> Task (Error String) a
json decoder req =
    toTask
        (Http.stringResolver
            (resolveJson
                { decoder = decoder
                , errorDecoder = \meta body -> Json.Decode.succeed body
                }
                req
            )
        )
        req


{-| Make a task that returns an arbitrary data or `Req.Error` with custom error.
-}
jsonWithError :
    { decoder : Json.Decode.Decoder a
    , errorDecoder : Http.Metadata -> Json.Decode.Decoder e
    }
    -> Req
    -> Task (Error e) a
jsonWithError decoders req =
    toTask
        (Http.stringResolver
            (resolveJson
                { decoder = decoders.decoder
                , errorDecoder = \meta body -> decoders.errorDecoder meta
                }
                req
            )
        )
        req


{-| Make a task that returns an arbitrary data or `Http.Error`.
-}
bytesCompatible :
    Bytes.Decode.Decoder a
    -> Req
    -> Task Http.Error a
bytesCompatible decoder req =
    toTask
        (Http.bytesResolver (resolveBytesCompatible decoder req))
        req


{-| Make a task that returns an arbitrary data or `Req.Error`.
-}
bytes :
    Bytes.Decode.Decoder a
    -> Req
    -> Task (Error String) a
bytes decoder req =
    toTask
        (Http.bytesResolver
            (resolveBytes
                { decoder = decoder
                , errorDecoder = \_ -> Bytes.Decode.succeed ""
                }
                req
            )
        )
        req


{-| Make a task that returns an arbitrary data or `Req.Error` with custom error.
-}
bytesWithError :
    { decoder : Bytes.Decode.Decoder a
    , errorDecoder : Http.Metadata -> Bytes.Decode.Decoder e
    }
    -> Req
    -> Task (Error e) a
bytesWithError decoders req =
    toTask
        (Http.bytesResolver
            (resolveBytes
                { decoder = decoders.decoder
                , errorDecoder = \meta -> decoders.errorDecoder meta
                }
                req
            )
        )
        req


{-| Ignore the result.
-}
whatever :
    msg
    -> Req
    -> Task x msg
whatever msg req =
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
        , headers = toHttpHeaders req.headers
        , url = req.url
        , body = toHttpBody req.body
        , resolver = resolver
        , timeout = req.timeout
        }



-- TRACKING


{-| Send a request for tracking, expecting a string or `Http.Error`.
-}
trackStringCompatible :
    String
    -> (Result Http.Error String -> msg)
    -> Req
    -> Cmd msg
trackStringCompatible tracker toMsg req =
    trackString_
        tracker
        toMsg
        resolveStringCompatible
        req


{-| Send a request for tracking, expecting a string or `Req.Error`.
-}
trackString :
    String
    -> (Result (Error String) String -> msg)
    -> Req
    -> Cmd msg
trackString tracker toMsg req =
    trackString_
        tracker
        toMsg
        (resolveString (\meta body -> Json.Decode.succeed body))
        req


{-| Send a request for tracking, expecting a string or `Req.Error` with custom error.
-}
trackStringWithError :
    String
    -> (Result (Error e) String -> msg)
    -> (Http.Metadata -> Json.Decode.Decoder e)
    -> Req
    -> Cmd msg
trackStringWithError tracker toMsg errorDecoder req =
    trackString_
        tracker
        toMsg
        (resolveString (\meta body -> errorDecoder meta))
        req


{-| Send a request for tracking, expecting an arbitrary data or `Http.Error`.
-}
trackJsonCompatible :
    String
    -> (Result Http.Error a -> msg)
    -> Json.Decode.Decoder a
    -> Req
    -> Cmd msg
trackJsonCompatible tracker toMsg decoder req =
    trackString_
        tracker
        toMsg
        (resolveJsonCompatible decoder)
        req


{-| Send a request for tracking, expecting an arbitrary data or `Req.Error`.
-}
trackJson :
    String
    -> (Result (Error String) a -> msg)
    -> Json.Decode.Decoder a
    -> Req
    -> Cmd msg
trackJson tracker toMsg decoder req =
    trackString_
        tracker
        toMsg
        (resolveJson
            { decoder = decoder
            , errorDecoder = \meta body -> Json.Decode.succeed body
            }
        )
        req


{-| Send a request for tracking, expecting an arbitrary data or `Req.Error` with custom error.
-}
trackJsonWithError :
    String
    -> (Result (Error e) a -> msg)
    ->
        { decoder : Json.Decode.Decoder a
        , errorDecoder : Http.Metadata -> Json.Decode.Decoder e
        }
    -> Req
    -> Cmd msg
trackJsonWithError tracker toMsg decoders req =
    trackString_
        tracker
        toMsg
        (resolveJson
            { decoder = decoders.decoder
            , errorDecoder = \meta body -> decoders.errorDecoder meta
            }
        )
        req


trackString_ :
    String
    -> (Result e a -> msg)
    -> Resolve String e a
    -> Req
    -> Cmd msg
trackString_ tracker toMsg resolve req =
    track
        tracker
        (Http.expectStringResponse toMsg (resolve req))
        req


{-| Send a request for tracking, expecting an arbitrary data or `Http.Error`.
-}
trackBytesCompatible :
    String
    -> (Result Http.Error a -> msg)
    -> Bytes.Decode.Decoder a
    -> Req
    -> Cmd msg
trackBytesCompatible tracker toMsg decoder req =
    trackBytes_
        tracker
        toMsg
        (resolveBytesCompatible decoder)
        req


{-| Send a request for tracking, expecting an arbitrary data or `Req.Error`.
-}
trackBytes :
    String
    -> (Result (Error String) a -> msg)
    -> Bytes.Decode.Decoder a
    -> Req
    -> Cmd msg
trackBytes tracker toMsg decoder req =
    trackBytes_
        tracker
        toMsg
        (resolveBytes
            { decoder = decoder
            , errorDecoder = \meta -> Bytes.Decode.succeed ""
            }
        )
        req


{-| Send a request for tracking, expecting an arbitrary data or `Req.Error` with custom error.
-}
trackBytesWithError :
    String
    -> (Result (Error e) a -> msg)
    ->
        { decoder : Bytes.Decode.Decoder a
        , errorDecoder : Http.Metadata -> Bytes.Decode.Decoder e
        }
    -> Req
    -> Cmd msg
trackBytesWithError tracker toMsg decoders req =
    trackBytes_
        tracker
        toMsg
        (resolveBytes
            { decoder = decoders.decoder
            , errorDecoder = \meta -> decoders.errorDecoder meta
            }
        )
        req


trackBytes_ :
    String
    -> (Result e a -> msg)
    -> Resolve Bytes e a
    -> Req
    -> Cmd msg
trackBytes_ tracker toMsg resolve req =
    track
        tracker
        (Http.expectBytesResponse toMsg (resolve req))
        req


{-| Track something but ignore the result.
-}
trackWhatever :
    String
    -> (Result x () -> msg)
    -> Req
    -> Cmd msg
trackWhatever tracker toMsg req =
    track
        tracker
        (Http.expectBytesResponse toMsg (\res -> Ok ()))
        req


{-| Make a Cmd with tracker using existing `Http.Expect`.
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



-- RESOLVE


type alias Resolve src x a =
    Req -> Http.Response src -> Result x a


resolveStringCompatible : Resolve String Http.Error String
resolveStringCompatible =
    \req res ->
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
                Ok body


resolveString :
    (Http.Metadata -> String -> Json.Decode.Decoder e)
    -> Resolve String (Error e) String
resolveString errorDecoder req res =
    Result.mapError (Error req) <|
        case res of
            Http.BadUrl_ url ->
                Err (BadUrl url)

            Http.Timeout_ ->
                Err Timeout

            Http.NetworkError_ ->
                Err NetworkError

            Http.BadStatus_ metadata body ->
                case Json.Decode.decodeString (errorDecoder metadata body) body of
                    Ok a ->
                        Err (BadStatus metadata a)

                    Err e ->
                        Err (BadBody metadata (Json.Decode.errorToString e))

            Http.GoodStatus_ metadata body ->
                Ok body


resolveJsonCompatible : Json.Decode.Decoder a -> Resolve String Http.Error a
resolveJsonCompatible decoder =
    \req res ->
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


resolveJson :
    { decoder : Json.Decode.Decoder a
    , errorDecoder : Http.Metadata -> String -> Json.Decode.Decoder e
    }
    -> Resolve String (Error e) a
resolveJson { decoder, errorDecoder } req res =
    Result.mapError (Error req) <|
        case res of
            Http.BadUrl_ url ->
                Err (BadUrl url)

            Http.Timeout_ ->
                Err Timeout

            Http.NetworkError_ ->
                Err NetworkError

            Http.BadStatus_ metadata body ->
                case Json.Decode.decodeString (errorDecoder metadata body) body of
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


resolveBytesCompatible : Bytes.Decode.Decoder a -> Resolve Bytes Http.Error a
resolveBytesCompatible decoder =
    \req res ->
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
                case Bytes.Decode.decode decoder body of
                    Just a ->
                        Ok a

                    Nothing ->
                        Err (Http.BadBody "unexpected bytes")


resolveBytes :
    { decoder : Bytes.Decode.Decoder a
    , errorDecoder : Http.Metadata -> Bytes.Decode.Decoder e
    }
    -> Resolve Bytes (Error e) a
resolveBytes { decoder, errorDecoder } req res =
    Result.mapError (Error req) <|
        case res of
            Http.BadUrl_ url ->
                Err (BadUrl url)

            Http.Timeout_ ->
                Err Timeout

            Http.NetworkError_ ->
                Err NetworkError

            Http.BadStatus_ metadata body ->
                case Bytes.Decode.decode (errorDecoder metadata) body of
                    Just a ->
                        Err (BadStatus metadata a)

                    Nothing ->
                        Err (BadBody metadata "unexpected bytes")

            Http.GoodStatus_ metadata body ->
                case Bytes.Decode.decode decoder body of
                    Just a ->
                        Ok a

                    Nothing ->
                        Err (BadBody metadata "unexpected bytes")
