module PostcodeIo.Types exposing (..)


type alias PostcodeDetails =
    { postcode : String
    , quality : Int
    , eastings : Int
    , northings : Int
    , country : String
    , nhsHa : String
    , longitude : Float
    , latitude : Float
    , europeanElectoralRegion : String
    , primaryCareTrust : String
    , region : String
    , lsoa : String
    , msoa : String
    , incode : String
    , outcode : String
    , parliamentaryConstituency : String
    , adminDistrict : String
    , parish : String
    , adminCounty : ()
    , adminWard : String
    , ced : ()
    , ccg : String
    , nuts : String
    , codes : Codes
    }


type alias Codes =
    { adminDistrict : String
    , adminCounty : String
    , adminWard : String
    , parish : String
    , parliamentaryConstituency : String
    , ccg : String
    , ccgID : String
    , ced : String
    , nuts : String
    , lsoa : String
    , msoa : String
    , lau2 : String
    }
