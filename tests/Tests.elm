module Tests exposing (..)

import Expect
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Postcode validation rules"
        [ test "AREA should ONLY contain alpha characters." <|
            \_ ->
                Expect.fail "failed as expected!"
        , test "AREA should have 1 or 2 characters." <|
            \_ ->
                Expect.fail "failed as expected!"
        , test "DISTRICT should ONLY contain numeric characters." <|
            \_ ->
                Expect.fail "failed as expected!"
        , test "DISTRICT should have 1 or 2 characters." <|
            \_ ->
                Expect.fail "failed as expected!"
        , test "SUBDISTRICT should be a single alpha character." <|
            \_ ->
                Expect.fail "failed as expected!"
        , test "SUBDISTRICT is an optional field." <|
            \_ ->
                Expect.fail "failed as expected!"
        , test "SECTOR must be a single digit." <|
            \_ ->
                Expect.fail "failed as expected!"
        , test "UNIT must be 2 alpha characters." <|
            \_ ->
                Expect.fail "failed as expected!"
        , test "UNIT must ONLY contain alpha characters." <|
            \_ ->
                Expect.fail "failed as expected!"
        ]
