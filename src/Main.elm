module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Constants exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import PostcodeIo exposing (getNearestPostcodes, getPostcode)
import RemoteData exposing (..)
import Types exposing (Model, Msg(..), PostcodeDetails)
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
init _ url key =
    ( initialModel key
    , Cmd.none
    )


initialModel : Nav.Key -> Model
initialModel key =
    { postcodeResults = RemoteData.NotAsked
    , nearestPostcodesResults = RemoteData.NotAsked
    , postcode = ""
    , key = key
    }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SinglePostcodeResponse response ->
            ( { model | postcodeResults = response }
            , Nav.pushUrl model.key (String.toLower model.postcode)
            )

        ListNearestPostcodesResponse response ->
            ( { model | nearestPostcodesResults = response }, Cmd.none )

        UrlRequest _ ->
            ( model, Cmd.none )

        UrlChange _ ->
            ( model, Cmd.none )

        UpdatePostcode postcode ->
            ( { model
                | postcode = postcode
              }
            , Cmd.none
            )

        SubmitForm ->
            ( model
            , Cmd.batch
                [ getPostcode model.postcode
                , getNearestPostcodes model.postcode
                ]
            )

        ResetForm ->
            ( { model | postcode = "" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- VIEW ----


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

                Success response ->
                    div []
                        [ h2 [] [ text "Matching Postcode" ]
                        , resultItem response
                        ]

        nearestPostcodesResults =
            case model.nearestPostcodesResults of
                NotAsked ->
                    p [] [ text "Initialising." ]

                Loading ->
                    p [] [ text "Loading." ]

                Failure err ->
                    p [] [ text ("Error: " ++ errorToString err) ]

                Success response ->
                    div []
                        [ h2 [] [ text "Nearby Postcodes" ]
                        , div []
                            (response
                                |> List.map resultItem
                            )
                        ]
    in
    { title = "Postcode finder"
    , body =
        [ div [ class "header" ]
            [ div [ class "wrapper" ]
                [ h1 [] [ text "Postcode Finder" ]
                ]
            ]
        , div [ class "wrapper" ]
            [ div [ class "search" ]
                [ div [ class "search__field" ]
                    [ label
                        [ for "postcode" ]
                        [ text "Postcode" ]
                    , input
                        [ name "postcode"
                        , id "postcode"
                        , value model.postcode
                        , onInput UpdatePostcode
                        , placeholder featureSpace.postcode
                        , maxlength 7
                        ]
                        []
                    ]
                , div [ class "search__buttons" ]
                    [ button [ onClick ResetForm ] [ text "Reset" ]
                    , button
                        [ disabled
                            (String.isEmpty model.postcode
                                || (String.length model.postcode < minLength)
                                || (String.length model.postcode > maxLength)
                            )
                        , onClick SubmitForm
                        ]
                        [ text "Search" ]
                    ]
                ]
            , div [ class "results" ]
                [ postcodeResults
                , nearestPostcodesResults
                ]
            ]
        , div [ class "footer" ]
            [ p []
                [ text "Written elegantly using " ]
            , img [ class "footer__elm-logo", src "/logo.svg" ] []
            ]
        ]
    }


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


resultItem : PostcodeDetails -> Html Msg
resultItem item =
    div [ class "results__item" ]
        [ h3 [] [ text item.postcode ]
        , p [] [ text ("Country: " ++ item.country) ]
        , p [] [ text ("Region: " ++ item.region) ]
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
            "Bad Body - " ++ errorMessage
