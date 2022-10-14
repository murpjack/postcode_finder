module Postcode exposing (Postcode, toString, fromString, dummyCode)

import Parser exposing ((|.), (|=), Parser) 
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
        

toString : Postcode -> String
toString  { area, district, subdistrict, sector, unit } = 
        (area ++ district ++ subdistrict ++ " " ++ sector ++ unit) 
            |> String.toUpper


fromString : String -> Result (List Parser.DeadEnd) Postcode
fromString =
     Parser.run (Parser.succeed 
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
        |. Parser.spaces 
        |= chompSubdistrict  
        |= chompDistrict
        |= chompArea
        |. Parser.end
        ) << String.reverse


chompArea : Parser String
chompArea = 
    (Parser.chompWhile Char.isAlpha 
        |> Parser.getChompedString
        |> Parser.map String.reverse
    )

chompSubdistrict: Parser String
chompSubdistrict = 
    (Parser.chompWhile Char.isAlpha 
        |> Parser.getChompedString)

chompDistrict : Parser String
chompDistrict = 
    (Parser.chompWhile Char.isDigit 
        |> Parser.getChompedString 
        |> Parser.map String.reverse
    )

chompSector : Parser String
chompSector = 
    (Parser.chompIf Char.isDigit 
        |> Parser.getChompedString 
    )

chompUnit : Parser String
chompUnit = 
    (Parser.chompWhile Char.isAlpha 
        |> Parser.getChompedString
        |> Parser.map String.reverse
    )


-- type alias PostcodeParser a =
--   Parser Context InvalidPostcode a

-- type Context
--  = Definition String
--  | List
--  | Record


--type InvalidPostcode
--    = TooShort
--    | TooLong
--    | UnknownChar
    --| BadArea
    --| BadDistrict
    --| BadSubdistrict
    --| BadSector
    --| BadUnit

 








