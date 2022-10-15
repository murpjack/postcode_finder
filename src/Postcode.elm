module Postcode exposing (Postcode, toString, fromString, dummyCode)

import Parser.Advanced exposing ((|.), (|=), Parser, DeadEnd) 
import String
import Char 

{-- 
    Postcode handling API
    
    You love them. I love them. Postcodes.
    A postcode is made of values, which indicate a specific location.

    The shortest/longest UK geographical postcodes are 5 and 7 characters.
-}




type alias Postcode = 
        { area : String
        , district : String
        , subdistrict : String
        , sector : String
        , unit : String
        }

dummyCode : Postcode 
dummyCode =   
        { area = "CV"
        , district = "36"
        , subdistrict = ""
        , sector = "4"
        , unit = "FA"
        }

type alias PostcodeParser a =
   Parser.Advanced.Parser Never InvalidPostcode a

--type Context
--  = Definition String
--  | List
--  | Record



type InvalidPostcode
    = 
    --TooShort
    --| TooLong
    --| UnknownChar
    --| BadArea
    BadDistrict String
    --| BadSubdistrict
    --| BadSector
    --| BadUnit
    | UglyRear String

-- invalidPostcodeToErrorMessage : InvalidPostcode -> String
-- invalidPostcodeToErrorMessage problem = 
--    case problem of 
--        TooShort -> 
--            "This is too short"
--
--        UnknownChar -> 
--            "This is too unknown"
--
--        TooLong -> 
--            "This is too long"

toString : Postcode -> String
toString  { area, district, subdistrict, sector, unit } = 
        (area ++ district ++ subdistrict ++ " " ++ sector ++ unit) 
            |> String.toUpper


fromString : String -> Result (List (DeadEnd Never InvalidPostcode)) Postcode
fromString =
     Parser.Advanced.run parsePostcode << String.reverse 


parsePostcode : PostcodeParser Postcode
parsePostcode = 
    Parser.Advanced.succeed 
        (\unit sector subdistrict district area -> 
            { area = area
            , district = district
            , subdistrict = subdistrict
            , sector = sector
            , unit = unit
            }
        )
        |= chompUnit
        |= chompSector
        |. Parser.Advanced.spaces 
        |= chompSubdistrict  
        |= chompDistrict
        |= chompArea
        |. Parser.Advanced.end (UglyRear "end schade")
        


chompArea : Parser Never InvalidPostcode String
chompArea = 
    (Parser.Advanced.chompWhile Char.isAlpha 
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.map String.reverse
    )

chompSubdistrict: Parser Never InvalidPostcode String
chompSubdistrict = 
    (Parser.Advanced.chompWhile Char.isAlpha 
        |> Parser.Advanced.getChompedString)

chompDistrict : Parser Never InvalidPostcode String
chompDistrict = 
    (Parser.Advanced.chompWhile Char.isDigit 
        |> Parser.Advanced.getChompedString 
        |> Parser.Advanced.map String.reverse
    )

chompSector : Parser Never InvalidPostcode String
chompSector = 
    (Parser.Advanced.chompIf Char.isDigit (BadDistrict "district schade")
        |> Parser.Advanced.getChompedString 
    )

chompUnit : Parser Never InvalidPostcode String
chompUnit = 
    (Parser.Advanced.chompWhile Char.isAlpha 
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.map String.reverse
    )


 








