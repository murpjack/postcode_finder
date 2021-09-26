module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import PostcodeIo exposing (getNearestPostcodes, getPostcode)
import RemoteData exposing (..)
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



-- JM - According to Google, the shortest/longest postcode in UK are 5 and 7 characters respoectively


maxLength : Int
maxLength =
    7


minLength : Int
minLength =
    5



-- TODO: Move these api calls to the Update method.


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    ( initialModel (Routing.parseLocation url)
    , Cmd.batch []
    )


initialModel : Route -> Model
initialModel route =
    { postcodeResults = RemoteData.Loading
    , nearestPostcodesResults = RemoteData.Loading
    , currentRoute = route
    , searchPostcodeform = { postcode = "" }
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

        UpdatePostcode postcode ->
            ( { model
                | searchPostcodeform = { postcode = postcode }
              }
            , Cmd.none
            )

        SubmitForm ->
            ( model
            , Cmd.batch
                [ getPostcode model.searchPostcodeform.postcode
                , getNearestPostcodes model.searchPostcodeform.postcode
                ]
            )

        ResetForm ->
            ( { model | searchPostcodeform = { postcode = "" } }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- VIEW ----
--  TODO: Handle data load states - ie. Loading, Not Asked, Fail, Success.
--  TODO: Add simple styles.


view : Model -> Browser.Document Msg
view model =
    let
        postcodeResults =
            case model.postcodeResults of
                NotAsked ->
                    text "Initialising."

                Loading ->
                    text "Loading."

                Failure err ->
                    text ("Error: " ++ errorToString err)

                Success details ->
                    resultItem details

        nearestPostcodesResults =
            case model.nearestPostcodesResults of
                NotAsked ->
                    p [] [ text "Initialising." ]

                Loading ->
                    p [] [ text "Loading." ]

                Failure err ->
                    p [] [ text ("Error: " ++ errorToString err) ]

                Success listOfPostcodes ->
                    div [] (List.map resultItem listOfPostcodes)
    in
    { title = "Postcode finder"
    , body =
        [ div [ class "header" ]
            [ img [ src "/logo.svg" ] []
            , h1 [] [ text "Postcode Finder" ]
            ]
        , div [ class "wrapper" ]
            [ div [ class "search" ]
                [ label
                    [ for "postcode", class "search__label" ]
                    [ text "Postcode" ]
                , input
                    [ name "postcode"
                    , id "postcode"
                    , class "search__input"
                    , value model.searchPostcodeform.postcode
                    , onInput UpdatePostcode
                    , placeholder default.postcode
                    ]
                    []
                , button [ onClick ResetForm ] [ text "Reset" ]
                , button
                    [ disabled
                        (String.isEmpty model.searchPostcodeform.postcode
                            || (String.length model.searchPostcodeform.postcode < minLength)
                            || (String.length model.searchPostcodeform.postcode > maxLength)
                        )
                    , onClick SubmitForm
                    ]
                    [ text "Search" ]
                ]
            , div [ class "results" ]
                [ postcodeResults
                , nearestPostcodesResults
                ]
            ]
        , div [ class "footer" ]
            [ p []
                [ text "Written elegantly using Elm."
                ]
            ]
        ]
    }


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


resultItem : PostcodeDetails -> Html Msg
resultItem item =
    div []
        [ h2 [] [ text item.postcode ]
        , p [] [ text item.country ]
        , p [] [ text item.region ]
        ]



-- JM - This handles different possible Http error types.
-- TODO: Improve Error messages to be more helpful to app user.


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
            errorMessage
