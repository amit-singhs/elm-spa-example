module Pages.Home_ exposing (view)

import Element
import Element.Font exposing (semiBold)
import View exposing (View)


view : View msg
view =
    { title = "Homepage"
    , body =
        [ Element.layout [] <|
            Element.column
                [ Element.padding 40, Element.spacing 30 ]
                [ Element.row []
                    [ Element.text "Hello, world! from mdgriffith." ]
                , Element.row [ semiBold ]
                    [ Element.link []
                        { url = "/calculator"
                        , label = Element.text "Click here for Calculator page."
                        }
                    ]
                , Element.row [ semiBold ]
                    [ Element.link []
                        { url = "/point-of-sale"
                        , label = Element.text "Click here for point of sale page."
                        }
                    ]
                ]
        ]
    }
