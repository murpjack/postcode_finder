module Postcode exposing (Postcode, listErrors, parsePostcode)

import Char
import Parser.Advanced as Parser exposing ((|.), (|=), DeadEnd, Parser)
import String



{--
    Postcode handling API
    
    You love them. I love them. Postcodes.
    A postcode is made of values, which indicate a specific location.

    The shortest/longest UK geographical postcodes are 5 and 7 characters.
    
-}


type alias Postcode =
    String


type alias PostcodeParser a =
    Parser Context InvalidPostcode a


type Context
    = List


type InvalidPostcode
    = ExpectingArea
    | ExpectingDistrict
    | ExpectingDistrictOrSector
    | ExpectingSubdistrict
    | ExpectingSector
    | ExpectingUnit


invalidPostcodeToString : ( InvalidPostcode, Int ) -> String
invalidPostcodeToString ( msg, position ) =
    case msg of
        ExpectingArea ->
            "A postal AREA must contain 1 or 2 letters. Something looks incorrect."

        ExpectingDistrict ->
            "A postal DISTRICT must have 1 or 2 numbers. Something looks incorrect."

        ExpectingDistrictOrSector ->
            "Expecting a number at position " ++ String.fromInt position ++ ". Something looks incorrect."

        ExpectingSubdistrict ->
            "A postal SUBDISTRICT must be a single letter. Something looks incorrect."

        ExpectingSector ->
            "A postal SECTOR must be a number between 0 and 9. Something looks incorrect."

        ExpectingUnit ->
            "A postal UNIT must have exactly 2 letters. Something looks incorrect."


parsePostcode : String -> List String
parsePostcode input =
    case Parser.run chompPostcode input of
        Ok _ ->
            []

        Err ls ->
            listErrors ls


listErrors : List (DeadEnd Context InvalidPostcode) -> List String
listErrors errors =
    errors
        |> List.head
        |> Maybe.map ((\d -> ( d.problem, d.col )) >> invalidPostcodeToString >> List.singleton)
        |> Maybe.withDefault []



{--
    Parse a postcode which conforms to any of the following formats, 
    where 'a' is a letter and '9' is a number 
        
    * a9_9aa
    * a99_9aa
    * a9a_9aa
    * aa9_9aa
    * aa99_9aa
    * aa9a_9aa

-}


chompPostcode : PostcodeParser String
chompPostcode =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.spaces
            -- Area - first char
            |. Parser.chompIf Char.isAlpha ExpectingArea
            |. Parser.oneOf
                [ -- Area - second char
                  Parser.chompIf Char.isAlpha ExpectingArea
                    -- District - first char
                    |. Parser.chompIf Char.isDigit ExpectingDistrict
                    |. Parser.oneOf
                        [ -- This digit could be a second district char or a sector
                          Parser.chompIf Char.isDigit ExpectingDistrictOrSector
                            |. Parser.oneOf
                                [ chompIncode
                                , chompUnit
                                ]
                        , chompSubdistrict
                            |. chompIncode
                        ]

                -- District - first char
                , Parser.chompIf Char.isDigit ExpectingDistrict
                    |. Parser.oneOf
                        [ -- This digit could be a second district char or a sector
                          Parser.chompIf Char.isDigit ExpectingDistrictOrSector
                            |. Parser.oneOf
                                [ chompIncode
                                , chompUnit
                                ]
                        , chompSubdistrict
                            |. chompIncode
                        ]
                ]
            |. Parser.end ExpectingUnit


chompIncode : PostcodeParser String
chompIncode =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.spaces
            |. chompSector
            |. chompUnit


chompSubdistrict : PostcodeParser ()
chompSubdistrict =
    Parser.chompIf Char.isAlpha ExpectingSubdistrict


chompSector : PostcodeParser ()
chompSector =
    Parser.chompIf Char.isDigit ExpectingSector


chompUnit : PostcodeParser String
chompUnit =
    Parser.andThen checkUnitLength <|
        Parser.getChompedString <|
            Parser.succeed ()
                |. Parser.chompIf Char.isAlpha ExpectingUnit
                |. Parser.chompIf Char.isAlpha ExpectingUnit


checkUnitLength : String -> Parser Context InvalidPostcode String
checkUnitLength str =
    if String.length str == 2 then
        Parser.succeed str

    else
        Parser.problem ExpectingUnit
