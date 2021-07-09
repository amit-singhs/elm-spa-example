module Pages.PointOfSale exposing (view)

import Element exposing (..)
import Html exposing (Html)
import View exposing (View)


view : View msg
view =
    { title = "Point of sale"
    , body =
        [ Element.layout [] <|
            Element.text "Welcome to point of sale page."
        ]
    }
