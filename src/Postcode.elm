module Postcode exposing (Postcode, listErrors, partsFromString, toString)

import Char
import Parser.Advanced exposing ((|.), (|=), DeadEnd, Parser)
import String



{--
    Postcode handling API
    
    You love them. I love them. Postcodes.
    A postcode is made of values, which indicate a specific location.

    The shortest/longest UK geographical postcodes are 5 and 7 characters.

    The example below should list any problems found while parsing a piece of unstructured data.
    
    Example:
     
    myListOfHints : List String
    myListOfHints = 
        Postcode.partsFromString
            >> (\res ->
                case res of 
                    Ok _ ->
                        []

                    Err err ->
                        Postcode.listErrors err
                )
        
    ...

-}


type alias Postcode =
    { area : String
    , district : String
    , subdistrict : String
    , sector : String
    , unit : String
    }


type alias PostcodeParser a =
    Parser.Advanced.Parser Context InvalidPostcode a


type Context
    = List


type InvalidPostcode
    = BadArea
    | BadDistrict
    | BadSubdistrict
    | BadSector
    | BadUnit
    | ExpectingAlpha
    | ExpectingInt
    | Unknown


listErrors : List (DeadEnd Context InvalidPostcode) -> List String
listErrors =
    List.map ((\d -> ( d.problem, d.col )) >> invalidPostcodeToString)


invalidPostcodeToString : ( InvalidPostcode, Int ) -> String
invalidPostcodeToString ( msg, position ) =
    case msg of
        BadArea ->
            "A postal AREA must contain 1 or 2 letters. Something looks incorrect."

        BadDistrict ->
            "A postal DISTRICT must have 1 or 2 numbers. Something looks incorrect."

        BadSubdistrict ->
            "A postal SUBDISTRICT must be a single letter. Something looks incorrect."

        BadSector ->
            "A postal SECTOR must be a number between 0 and 9. Something looks incorrect."

        BadUnit ->
            "A postal UNIT must have only 2 letters. Something looks incorrect."

        ExpectingAlpha ->
            "The character at position " ++ String.fromInt position ++ " should be a letter."

        ExpectingInt ->
            "The character at position " ++ String.fromInt position ++ " should be a number."

        Unknown ->
            "Ooops!? That wasn't supposed to happen..."


toString : Postcode -> String
toString { area, district, subdistrict, sector, unit } =
    (area ++ district ++ subdistrict ++ " " ++ sector ++ unit)
        |> String.toUpper


partsFromString : String -> Result (List (DeadEnd Context InvalidPostcode)) Postcode
partsFromString =
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
        |. Parser.Advanced.end BadArea


chompArea : Parser Context InvalidPostcode String
chompArea =
    Parser.Advanced.andThen (String.reverse >> checkAreaLength) <|
        Parser.Advanced.getChompedString <|
            Parser.Advanced.oneOf
                [ Parser.Advanced.succeed ()
                    |. Parser.Advanced.chompIf Char.isAlpha ExpectingAlpha
                    |. Parser.Advanced.chompIf Char.isAlpha ExpectingAlpha
                , Parser.Advanced.chompIf Char.isDigit BadArea
                    |> Parser.Advanced.andThen
                        (\_ -> Parser.Advanced.problem BadArea)
                ]


checkAreaLength : String -> Parser Context InvalidPostcode String
checkAreaLength str =
    if String.length str < 3 && String.length str > 0 then
        Parser.Advanced.succeed str

    else
        Parser.Advanced.problem BadArea


chompSubdistrict : Parser Context InvalidPostcode String
chompSubdistrict =
    Parser.Advanced.chompWhile Char.isAlpha
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.andThen
            (\str ->
                if String.length str <= 1 then
                    Parser.Advanced.succeed str

                else
                    Parser.Advanced.problem BadSubdistrict
            )


chompDistrict : Parser Context InvalidPostcode String
chompDistrict =
    Parser.Advanced.andThen (String.reverse >> checkDistrictLength) <|
        Parser.Advanced.getChompedString <|
            Parser.Advanced.succeed ()
                |. Parser.Advanced.chompIf Char.isDigit ExpectingInt
                |. Parser.Advanced.chompIf Char.isDigit ExpectingInt


checkDistrictLength : String -> Parser Context InvalidPostcode String
checkDistrictLength str =
    if String.length str < 3 && String.length str > 0 then
        Parser.Advanced.succeed str

    else
        Parser.Advanced.problem BadDistrict


chompSector : Parser Context InvalidPostcode String
chompSector =
    Parser.Advanced.oneOf
        [ Parser.Advanced.chompIf Char.isDigit BadSector
            |> Parser.Advanced.getChompedString
        , Parser.Advanced.chompIf (Char.isDigit >> not) BadUnit
            |> Parser.Advanced.andThen
                (\_ -> Parser.Advanced.problem BadUnit)
        ]


chompUnit : Parser Context InvalidPostcode String
chompUnit =
    Parser.Advanced.andThen (String.reverse >> checkUnitLength) <|
        Parser.Advanced.getChompedString <|
            Parser.Advanced.succeed ()
                |. Parser.Advanced.chompIf Char.isAlpha ExpectingAlpha
                |. Parser.Advanced.chompIf Char.isAlpha ExpectingAlpha


checkUnitLength : String -> Parser Context InvalidPostcode String
checkUnitLength str =
    if String.length str == 2 then
        Parser.Advanced.succeed str

    else
        Parser.Advanced.problem BadUnit
