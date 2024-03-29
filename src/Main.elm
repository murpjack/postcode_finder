module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Attribute, Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import Maybe.Extra as Maybe
import Postcode
import RemoteData exposing (RemoteData(..), WebData)
import Url exposing (Url)
import Url.Parser exposing (..)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( initialModel key
    , Cmd.none
    )


type alias PostcodeDetails =
    { postcode : String
    , country : String
    , region : String
    }


featureSpace : PostcodeDetails
featureSpace =
    { postcode = "CB40GF"
    , country = "England"
    , region = "East of England"
    }


type alias Model =
    { postcodesResponse : WebData (List PostcodeDetails)
    , searchTerm : Maybe String
    , searchHints : List String
    , key : Nav.Key
    }


initialModel : Nav.Key -> Model
initialModel key =
    { postcodesResponse = RemoteData.NotAsked
    , searchTerm = Nothing
    , searchHints = []
    , key = key
    }


type Msg
    = PostcodesResponse (WebData (List PostcodeDetails))
    | UpdateSearchTerm String
    | SubmitForm
    | UrlChange Url
    | UrlRequest Browser.UrlRequest



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PostcodesResponse response ->
            ( { model | postcodesResponse = response }
            , Cmd.none
            )

        UpdateSearchTerm searchTerm ->
            ( { model
                | searchTerm = Just searchTerm
                , searchHints = []
              }
            , Cmd.none
            )

        SubmitForm ->
            let
                hints =
                    Maybe.unwrap []
                        Postcode.parsePostcode
                        model.searchTerm
            in
            ( { model | searchHints = hints }
            , if List.isEmpty hints then
                model.searchTerm
                    |> Maybe.unwrap Cmd.none
                        (\term ->
                            Cmd.batch [ getNearestPostcodes term ]
                        )

              else
                Cmd.none
            )

        UrlChange _ ->
            ( model, Cmd.none )

        UrlRequest _ ->
            ( model, Cmd.none )



-- Data


baseUrl : String
baseUrl =
    "https://api.postcodes.io/postcodes/"


getNearestPostcodes : String -> Cmd Msg
getNearestPostcodes postCode =
    Http.get
        { url = baseUrl ++ postCode ++ "/nearest"
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> PostcodesResponse)
                (Decode.field "result" (Decode.list postcodeDecoder))
        }


postcodeDecoder : Decoder PostcodeDetails
postcodeDecoder =
    Decode.map3 PostcodeDetails
        (Decode.field "postcode" Decode.string)
        (Decode.field "country" Decode.string)
        (Decode.field "region" Decode.string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        searchHints =
            if List.isEmpty model.searchHints then
                []

            else
                [ Html.div
                    [ Attrs.class "hints" ]
                    (List.map Html.text model.searchHints)
                ]

        postcodeResults =
            case model.postcodesResponse of
                NotAsked ->
                    [ Html.text "..." ]

                Loading ->
                    [ Html.text "Searching for postcodes" ]

                Failure err ->
                    [ Html.text ("Its seems there is an error: " ++ errorToString err) ]

                Success response ->
                    case response of
                        [] ->
                            []

                        single :: nearby ->
                            List.concat
                                [ [ Html.h2 [] [ Html.text "Matching Postcode" ]
                                  , resultItem single
                                  ]
                                , if List.isEmpty nearby then
                                    []

                                  else
                                    [ Html.h2 [] [ Html.text "Other nearby Postcodes" ]
                                    , Html.div [] (List.map resultItem nearby)
                                    ]
                                ]
    in
    { title = "UK Postcode finder"
    , body =
        [ Html.div [ Attrs.class "header" ]
            [ Html.div [ Attrs.class "wrapper" ]
                [ Html.h1 [] [ Html.text "UK Postcode Finder" ]
                ]
            ]
        , Html.div [ Attrs.class "wrapper" ]
            [ postcodeRulesTable
            , Html.div [ Attrs.class "search" ]
                ([ Html.label
                    [ Attrs.for "postcode" ]
                    [ Html.text "Postcode" ]
                 , Html.input
                    ([ Attrs.name "postcode"
                     , Attrs.id "postcode"
                     , Attrs.type_ "search"
                     , Events.onInput UpdateSearchTerm
                     , onEnter SubmitForm
                     , Attrs.placeholder featureSpace.postcode
                     ]
                        ++ (case model.searchTerm of
                                Just term ->
                                    [ Attrs.value term ]

                                Nothing ->
                                    []
                           )
                    )
                    []
                 , Html.button
                    [ Attrs.disabled
                        (case model.postcodesResponse of
                            Loading ->
                                True

                            _ ->
                                False
                        )
                    , Events.onClick SubmitForm
                    ]
                    [ Html.text "Search" ]
                 ]
                    ++ searchHints
                )
            , Html.div [ Attrs.class "results" ] postcodeResults
            ]
        , Html.div [ Attrs.class "footer" ]
            [ Html.p []
                [ Html.text "Written elegantly using " ]
            , Html.img [ Attrs.class "footer__elm-logo", Attrs.src "/logo.svg" ] []
            ]
        ]
    }


postcodeRulesTable : Html msg
postcodeRulesTable =
    Html.div [ Attrs.style "margin-bottom" "30px" ]
        [ Html.p []
            [ Html.text "UK postcodes can assume one of the following patterns:" ]
        , Html.table
            []
            [ Html.thead []
                [ Html.tr []
                    [ Html.th [] [ Html.text "Postcode Format" ]
                    , Html.th [] [ Html.text "Outward Code" ]
                    , Html.th [] [ Html.text "Inward Code" ]
                    , Html.th [] [ Html.text "Postcode Area" ]
                    , Html.th [] [ Html.text "District Code" ]
                    , Html.th [] [ Html.text "Sub-District" ]
                    , Html.th [] [ Html.text "Sector" ]
                    , Html.th [] [ Html.text "Unit" ]
                    ]
                ]
            , Html.tbody []
                [ Html.tr []
                    [ Html.td [] [ Html.code [] [ Html.text "AA9A 9AA" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA9A" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "9AA" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA9" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA9A" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA9A 9" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA" ] ]
                    ]
                , Html.tr []
                    [ Html.td [] [ Html.code [] [ Html.text "A9A 9AA" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "A9A" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "9AA" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "A" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "A9" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "A9A" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "A9A 9" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA" ] ]
                    ]
                , Html.tr []
                    [ Html.td [] [ Html.code [] [ Html.text "A9 9AA" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "A9" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "9AA" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "A" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "A9" ] ]
                    , Html.td [] [ Html.strong [] [ Html.text "N/A" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "A9 9" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA" ] ]
                    ]
                , Html.tr []
                    [ Html.td [] [ Html.code [] [ Html.text "A99 9AA" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "A99" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "9AA" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "A" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "A99" ] ]
                    , Html.td [] [ Html.strong [] [ Html.text "N/A" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "A99 9" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA" ] ]
                    ]
                , Html.tr []
                    [ Html.td [] [ Html.code [] [ Html.text "AA9 9AA" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA9" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "9AA" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA9" ] ]
                    , Html.td [] [ Html.strong [] [ Html.text "N/A" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA9 9" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA" ] ]
                    ]
                , Html.tr []
                    [ Html.td [] [ Html.code [] [ Html.text "AA99 9AA" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA99" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "9AA" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA99" ] ]
                    , Html.td [] [ Html.strong [] [ Html.text "N/A" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA99 9" ] ]
                    , Html.td [] [ Html.code [] [ Html.text "AA" ] ]
                    ]
                ]
            ]
        ]


resultItem : PostcodeDetails -> Html Msg
resultItem item =
    Html.div [ Attrs.class "results__item" ]
        [ Html.h3 [] [ Html.text item.postcode ]
        , Html.p [] [ Html.text ("Country: " ++ item.country) ]
        , Html.p [] [ Html.text ("Region: " ++ item.region) ]
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg

            else
                Decode.fail "not ENTER"
    in
    Events.on "keydown" (Decode.andThen isEnter Events.keyCode)



-- This handles different possible Http error types.


errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Timeout ->
            "Unable to reach the server, try again"

        NetworkError ->
            "Unable to reach the server, check your network connection"

        BadStatus 500 ->
            "The server had a problem, try again later"

        BadStatus 400 ->
            "Verify your information and try again"

        BadStatus _ ->
            "Unknown error"

        BadBody errorMessage ->
            "Bad Body - " ++ errorMessage
