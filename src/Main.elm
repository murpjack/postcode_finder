module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import Maybe.Extra as Maybe
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
    , searchValidation : String
    , key : Nav.Key
    }


initialModel : Nav.Key -> Model
initialModel key =
    { postcodesResponse = RemoteData.NotAsked
    , searchTerm = Nothing
    , searchValidation = ""
    , key = key
    }


type Msg
    = PostcodesResponse (WebData (List PostcodeDetails))
    | UpdatePostcode String
    | SubmitForm
    | ResetForm
    | UrlChange Url
    | UrlRequest Browser.UrlRequest



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --        SinglePostcode response ->
        --            ( { model | postcodeResults = response }
        --            , Maybe.unwrap Cmd.none (Nav.pushUrl model.key << String.toLower) model.searchTerm
        --            )
        PostcodesResponse response ->
            ( { model | postcodesResponse = response }
            , Cmd.none
            )

        UpdatePostcode searchTerm ->
            ( { model
                | searchTerm = Just searchTerm
                , searchValidation = ""
              }
            , Cmd.none
            )

        SubmitForm ->
            ( model
            , model.searchTerm
                |> Maybe.unwrap Cmd.none
                    (\term ->
                        Cmd.batch [ getNearestPostcodes term ]
                    )
            )

        ResetForm ->
            ( { model
                | searchTerm = Nothing
                , searchValidation = ""
              }
            , Cmd.none
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
        postcodeResults =
            case model.postcodesResponse of
                NotAsked ->
                    [ Html.text "Initialising." ]

                Loading ->
                    [ Html.text "Loading." ]

                Failure err ->
                    [ Html.text ("Error: " ++ errorToString err) ]

                Success response ->
                    case response of
                        [] ->
                            []

                        single :: nearby ->
                            [ Html.h2 [] [ Html.text "Matching Postcode" ]
                            , resultItem single
                            , Html.h2 [] [ Html.text "Other nearby Postcodes" ]
                            , Html.div [] (List.map resultItem nearby)
                            ]
    in
    { title = "Postcode finder"
    , body =
        [ Html.div [ Attrs.class "header" ]
            [ Html.div [ Attrs.class "wrapper" ]
                [ Html.h1 [] [ Html.text "Postcode Finder" ]
                ]
            ]
        , Html.div [ Attrs.class "wrapper" ]
            [ Html.div [ Attrs.class "search" ]
                [ Html.div [ Attrs.class "search__field" ]
                    [ Html.label
                        [ Attrs.for "postcode" ]
                        [ Html.text "Postcode" ]
                    , Html.input
                        ([ Attrs.name "postcode"
                         , Attrs.id "postcode"
                         , Events.onInput UpdatePostcode
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
                    ]
                , Html.div [ Attrs.class "search__buttons" ]
                    [ Html.button [ Events.onClick ResetForm ] [ Html.text "Reset" ]
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
                ]
            , Html.div [ Attrs.class "results" ] postcodeResults
            ]
        , Html.div [ Attrs.class "footer" ]
            [ Html.p []
                [ Html.text "Written elegantly using " ]
            , Html.img [ Attrs.class "footer__elm-logo", Attrs.src "/logo.svg" ] []
            ]
        ]
    }


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    Html.input [ Attrs.type_ t, Attrs.placeholder p, Attrs.value v, Events.onInput toMsg ] []


resultItem : PostcodeDetails -> Html Msg
resultItem item =
    Html.div [ Attrs.class "results__item" ]
        [ Html.h3 [] [ Html.text item.postcode ]
        , Html.p [] [ Html.text ("Country: " ++ item.country) ]
        , Html.p [] [ Html.text ("Region: " ++ item.region) ]
        ]



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
