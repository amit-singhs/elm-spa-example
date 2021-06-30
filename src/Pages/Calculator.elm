module Pages.Calculator exposing (page)

import Browser
import Debug exposing (toString)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, button, del, div, input, p)
import List exposing (..)
import Page exposing (Page)
import Request exposing (Request)
import Shared
import String
import View exposing (View)


type Operation
    = Addition
    | Subtraction
    | Multiplication
    | Division


type NumberType
    = Integer Int
    | Decimal Int Int


type alias Model =
    { firstNumber : NumberType
    , secondNumber : NumberType
    , displayedNumber : NumberType
    , operationType : Maybe Operation
    , result : NumberType
    , decimalButtonIsOn : Bool
    }


init : Model
init =
    { firstNumber = Integer 0
    , secondNumber = Integer 0
    , displayedNumber = Integer 0
    , operationType = Nothing
    , result = Integer 0
    , decimalButtonIsOn = False
    }


type Msg
    = AddNumbers
    | SubtractNumbers
    | MultiplyNumbers
    | DivideNumbers
    | DivideByHundred
    | InsertDigit Int
    | AllClearTextField
    | EqualsTo
    | DecimalButtonPressed
    | DoNothing String


page : Shared.Model -> Request -> Page
page _ req =
    Page.sandbox
        { init = init
        , update = update
        , view = view req
        }


view : Request -> View msg
view req =
    { title = "Homepage"
    , body =
        [ Element.layout []
            (Element.text ("Hello, " ++ String.fromInt (Maybe.withDefault 80 req.url.port_) ++ "!!"))
        ]
    }
