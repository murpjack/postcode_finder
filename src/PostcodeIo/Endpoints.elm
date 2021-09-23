module PostcodeIo.Endpoints exposing (..)

import Models exposing (Postcode)


baseUrl : String
baseUrl =
    "https://postcodes.io/postcodes/"


getPostcode : Postcode -> String
getPostcode code =
    baseUrl ++ code


getNearestPostcode : Postcode -> String
getNearestPostcode code =
    baseUrl ++ code ++ "/nearest"
