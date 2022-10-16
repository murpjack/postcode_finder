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
   Parser.Advanced.Parser Context InvalidPostcode a

type Context
  = List



type InvalidPostcode
    = BadArea String
    | BadDistrict String
    | BadSubdistrict String 
    | BadSector String
    | BadUnit String


toString : Postcode -> String
toString  { area, district, subdistrict, sector, unit } = 
        (area ++ district ++ subdistrict ++ " " ++ sector ++ unit) 
            |> String.toUpper


fromString : String -> Result (List (DeadEnd Context InvalidPostcode)) Postcode
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
        |. Parser.Advanced.spaces 
        |= chompUnit
        |= chompSector
        |. Parser.Advanced.spaces 
        |= chompSubdistrict  
        |= chompDistrict
        |= chompArea
        |. Parser.Advanced.spaces 
               


chompArea : Parser Context InvalidPostcode String
chompArea = 
    (Parser.Advanced.chompWhile Char.isAlpha 
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.andThen 
            (\str -> 
            if String.length str <= 2 then
                Parser.Advanced.succeed str
            else
             Parser.Advanced.problem 
                (BadArea "A UK postcode area value must have no more than 2 uppercase letters, and nothing else.")
            )
        |> Parser.Advanced.map String.reverse
    )

chompSubdistrict: Parser Context InvalidPostcode String
chompSubdistrict = 
    (Parser.Advanced.chompWhile Char.isAlpha 
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.andThen 
            (\str -> 
            if String.length str <= 1 then
                Parser.Advanced.succeed str
            else
             Parser.Advanced.problem 
                (BadSubdistrict "A UK postcode may have a single uppercase letter as a subdistrict, but most do not.")
            )
        )

chompDistrict : Parser Context InvalidPostcode String
chompDistrict = 
    (Parser.Advanced.chompWhile Char.isDigit 
        |> Parser.Advanced.getChompedString 
        |> Parser.Advanced.andThen 
            (\str -> 
            if String.length str <= 2 then
                Parser.Advanced.succeed str
            else
             Parser.Advanced.problem 
                (BadDistrict "A UK postcode district value must have no more than 2 numbers, and nothing else.")
            )
        |> Parser.Advanced.map String.reverse
    )

chompSector : Parser Context InvalidPostcode String
chompSector =
    (Parser.Advanced.chompIf Char.isDigit 
            (BadSector "I am expecting a sector value here but found a different value instead. A sector is a number between 0 and 9.")
        |> Parser.Advanced.getChompedString 
    )

chompUnit : Parser Context InvalidPostcode String
chompUnit =
    (Parser.Advanced.chompWhile Char.isAlpha
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.andThen 
            (\str -> 
            if String.length str == 2 then
                Parser.Advanced.succeed str
            else
             Parser.Advanced.problem 
                (BadUnit "A UK postcode unit value must have 2 digits.")
            )
        |> Parser.Advanced.map String.reverse
    )

