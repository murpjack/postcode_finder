module PostcodeIo exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, Error(..), at, decodeString, dict, field, int, list, map, map2, map3, string)
import RemoteData exposing (WebData)
import Types exposing (Msg(..), Postcode, PostcodeDetails)


baseUrl : String
baseUrl =
    "https://postcodes.io/postcodes/"


getPostcode : Postcode -> Cmd Msg
getPostcode code =
    Http.get
        { url = baseUrl ++ code
        , expect = Http.expectJson (RemoteData.fromResult >> SinglePostcodeResponse) postcodeDecoder
        }


postcodeDecoder : Decoder PostcodeDetails
postcodeDecoder =
    field "result"
        (map3 PostcodeDetails
            (field "postcode" Decode.string)
            (field "country" Decode.string)
            (field "region" Decode.string)
        )


getNearestPostcodes : Postcode -> Cmd Msg
getNearestPostcodes code =
    Http.get
        { url = baseUrl ++ code ++ "/nearest"
        , expect =
            Http.expectJson (RemoteData.fromResult >> ListNearestPostcodesResponse) nearestDecoder
        }


nearestDecoder : Decoder (List PostcodeDetails)
nearestDecoder =
    field "result"
        (list <|
            map3 PostcodeDetails
                (field "postcode" Decode.string)
                (field "country" Decode.string)
                (field "region" Decode.string)
        )
