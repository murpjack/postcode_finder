module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (autocomplete, class, for, id, name, placeholder, src, value)
import PostcodeIo exposing (getNearestPostcodes, getPostcode)
import RemoteData
import Routing
import Types exposing (Model, Msg(..), PostcodeDetails, Route(..))
import Url exposing (Url)



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


default : PostcodeDetails
default =
    { postcode = "CB40GF"
    , country = "England"
    , region = "East of England"
    }



-- TODO: Move these api calls to the Update method.


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    ( initialModel (Routing.parseLocation url)
    , Cmd.batch
        [ getPostcode default.postcode, getNearestPostcodes default.postcode ]
    )


initialModel : Route -> Model
initialModel route =
    { postcodeResults = RemoteData.Loading
    , nearestPostcodesResults = RemoteData.Loading
    , currentRoute = route
    , searchPostcodeform = { postcode = default.postcode }
    }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SinglePostcodeResponse response ->
            ( { model | postcodeResults = response }
            , Cmd.none
            )

        ListNearestPostcodesResponse response ->
            ( { model | nearestPostcodesResults = response }, Cmd.none )

        --  TODO: Plumb in remaining Update conditions.
        UrlRequest _ ->
            ( model, Cmd.none )

        UrlChange _ ->
            ( model, Cmd.none )

        FormSubmitted ->
            ( model, Cmd.none )

        FormReset ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- VIEW ----
--  TODO: Handle data load states - ie. Loading, Not Asked, Fail, Success.
--  TODO: Add simple styles.


view : Model -> Browser.Document Msg
view model =
    { title = "Postcode finder"
    , body =
        [ div [ class "header" ]
            [ img [ src "/logo.svg" ] []
            , h1 [] [ text "Postcode Finder" ]
            ]
        , div [ class "wrapper" ]
            [ div [ class "search" ]
                [ form [ class "search__form", autocomplete True ]
                    [ label
                        [ for "postcode", class "search__label" ]
                        [ text "Postcode" ]
                    , input
                        [ name "postcode"
                        , id "postcode"
                        , class "search__input"
                        , value model.searchPostcodeform.postcode
                        , placeholder default.postcode
                        ]
                        []
                    , button [] [ text "Search" ]
                    ]
                ]
            , div [ class "results" ]
                [ resultItem default
                , resultItem default
                , resultItem default
                , resultItem default
                ]
            ]
        , div [ class "footer" ]
            [ p []
                [ text "Written elegantly using Elm."
                ]
            ]
        ]
    }


resultItem : PostcodeDetails -> Html Msg
resultItem item =
    div []
        [ h2 [] [ text item.postcode ]
        , p [] [ text item.country ]
        , p [] [ text item.region ]
        ]
