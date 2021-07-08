module Pages.PointOfSale exposing (view)

import Html exposing (Html)
import View exposing (View)


view : View msg
view =
    { title = "Point of sale"
    , body = [ Html.text "Welcome to point of sale page." ]
    }
