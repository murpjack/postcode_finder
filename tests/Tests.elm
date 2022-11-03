module Tests exposing (..)

import Expect
import Postcode exposing (InvalidPostcode(..))
import Test exposing (..)


all : Test
all =
    describe "Postcode validation rules"
        [ test "AREA should ONLY contain alpha characters." <|
            \_ ->
                errorFromBadPostcode "A$9 9AA" ExpectingArea
        , test "AREA should have 1 or 2 characters." <|
            \_ ->
                errorFromBadPostcode "9 9AA" ExpectingArea
        , test "DISTRICT should ONLY contain numeric characters." <|
            \_ ->
                errorFromBadPostcode "AA@9 9AA" ExpectingDistrict
        , test "Space between DISTRICT and SECTOR in AA9 9AA should be valid." <|
            \_ ->
                let
                    postcodeString =
                        "AA9 9AA"

                    noErrors =
                        []
                in
                Expect.equal noErrors (Postcode.parsePostcode postcodeString)
        , test "Space between DISTRICT and SECTOR in A9 9AA should be valid." <|
            \_ ->
                let
                    postcodeString =
                        "A9 9AA"

                    noErrors =
                        []
                in
                Expect.equal noErrors (Postcode.parsePostcode postcodeString)
        , test "DISTRICT should have 1 or 2 characters." <|
            \_ ->
                errorFromBadPostcode "AA 9AA" ExpectingDistrict
        , test "SECTOR must be a single digit." <|
            \_ ->
                errorFromBadPostcode "AA99 AAA" ExpectingSector
        , test "UNIT must be 2 alpha characters." <|
            \_ ->
                errorFromBadPostcode "AA99 9AAA" ExpectingUnit
        , test "UNIT must ONLY contain alpha characters." <|
            \_ ->
                errorFromBadPostcode "AA99 9A8" ExpectingUnit
        ]


errorFromBadPostcode : String -> InvalidPostcode -> Expect.Expectation
errorFromBadPostcode badPostcodeString invalidPostcode =
    let
        expectedMessage =
            [ Postcode.invalidPostcodeToString ( invalidPostcode, 1 ) ]
    in
    Expect.equal expectedMessage (Postcode.parsePostcode badPostcodeString)
