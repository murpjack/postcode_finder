module Constants exposing (..)

import Types exposing (PostcodeDetails)


featureSpace : PostcodeDetails
featureSpace =
    { postcode = "CB40GF"
    , country = "England"
    , region = "East of England"
    }



-- JM - According to Google, the shortest/longest postcode in UK are 5 and 7 characters respoectively


maxLength : Int
maxLength =
    7


minLength : Int
minLength =
    5
