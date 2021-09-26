module Types exposing (..)

import Browser
import RemoteData exposing (WebData)
import Url exposing (Url)


type Msg
    = SinglePostcodeResponse (WebData PostcodeDetails)
    | ListNearestPostcodesResponse (WebData (List PostcodeDetails))
    | UrlRequest Browser.UrlRequest
    | UrlChange Url
    | UpdatePostcode Postcode
    | SubmitForm
    | ResetForm



---- MODEL ----


type alias Model =
    { postcodeResults : WebData PostcodeDetails
    , nearestPostcodesResults : WebData (List PostcodeDetails)
    , currentRoute : Route
    , searchPostcodeform : SearchPostcodeFom
    }


type alias Postcode =
    String


type Route
    = Root
    | Postcode Postcode
    | NotFoundRoute


type alias SearchPostcodeFom =
    { postcode : Postcode }



-- JM - In this app only 3 fields are used,
-- This is an exhaustive list of values of PostcodeDetails


type alias PostcodeDetails =
    { postcode : String

    -- , quality : Int
    -- , eastings : Int
    -- , northings : Int
    , country : String

    -- , nhsHa : String
    -- , longitude : Float
    -- , latitude : Float
    -- , europeanElectoralRegion : String
    -- , primaryCareTrust : String
    , region : String

    -- , lsoa : String
    -- , msoa : String
    -- , incode : String
    -- , outcode : String
    -- , parliamentaryConstituency : String
    -- , adminDistrict : String
    -- , parish : String
    -- , adminCounty : ()
    -- , adminWard : String
    -- , ced : ()
    -- , ccg : String
    -- , nuts : String
    -- , codes : Codes
    }



-- type alias Codes =
--     { adminDistrict : String
--     , adminCounty : String
--     , adminWard : String
--     , parish : String
--     , parliamentaryConstituency : String
--     , ccg : String
--     , ccgID : String
--     , ced : String
--     , nuts : String
--     , lsoa : String
--     , msoa : String
--     , lau2 : String
--     }
