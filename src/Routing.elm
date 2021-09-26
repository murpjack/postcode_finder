module Routing exposing (..)

import Types exposing (Postcode, Route(..))
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, map, oneOf, s, string, top)



-- JM - parseUrl and urlFragmentToPath could possibly be one function
-- though this helps my brain process better what is happening.


parseUrl : Url -> Maybe Route
parseUrl url =
    UrlParser.parse parseRoute (urlFragmentToPath url)


parseRoute : Parser (Route -> a) a
parseRoute =
    oneOf
        [ map Root top
        , map Postcode (s "postcode" </> string)
        ]


urlFragmentToPath : Url -> Url
urlFragmentToPath url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }


parseLocation : Url -> Route
parseLocation url =
    case parseUrl url of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
