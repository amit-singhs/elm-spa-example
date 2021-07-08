module Pages.Calculator exposing (Model, Msg, page)

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



-- UTILITIES FUNCTIONS


removeDecimal : Float -> Int
removeDecimal floatNumber =
    if (floatNumber - toFloat (floor floatNumber)) /= 0 then
        removeDecimal (10 * floatNumber)

    else
        round floatNumber


renderNumberTypetoFloat : NumberType -> Float
renderNumberTypetoFloat numType =
    case numType of
        Integer i ->
            toFloat i

        Decimal intNumber decimalPlaces ->
            toFloat intNumber / toFloat (10 ^ decimalPlaces)


renderFloatToNumberType : Float -> NumberType
renderFloatToNumberType floatNumber =
    if String.contains "." (String.fromFloat floatNumber) == False then
        Integer (ceiling floatNumber)

    else
        case List.head (List.reverse (String.split "." (String.fromFloat floatNumber))) of
            Just decimalPart ->
                Decimal (removeDecimal floatNumber) (String.length decimalPart)

            Nothing ->
                Decimal 0 0


renderNumberTypetoString : NumberType -> String
renderNumberTypetoString numType =
    case numType of
        Integer i ->
            String.fromInt i

        Decimal intNumber decimalPlace ->
            String.fromFloat (toFloat intNumber / toFloat (10 ^ decimalPlace))


appendPeriodToInt : Int -> String
appendPeriodToInt integerToBeAppended =
    String.fromFloat (toFloat integerToBeAppended) ++ "."


page : Shared.Model -> Request -> Page.With Model Msg
page _ _ =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddNumbers ->
            { model
                | operationType = Just Addition
                , displayedNumber = model.firstNumber
                , decimalButtonIsOn = False
            }

        SubtractNumbers ->
            { model
                | operationType = Just Subtraction
                , displayedNumber = model.firstNumber
                , decimalButtonIsOn = False
            }

        MultiplyNumbers ->
            { model
                | operationType = Just Multiplication
                , displayedNumber = model.firstNumber
                , decimalButtonIsOn = False
            }

        DivideNumbers ->
            { model
                | operationType = Just Division
                , displayedNumber = model.firstNumber
                , decimalButtonIsOn = False
            }

        InsertDigit digit ->
            let
                insertAtOnes : NumberType -> Int -> NumberType
                insertAtOnes x d =
                    case x of
                        Integer v ->
                            Integer ((v * 10) + d)

                        Decimal v decimalPlace ->
                            --Decimal (v + toFloat d / toFloat (10 ^ decimalPlace)) (decimalPlace + 1)
                            Decimal ((v * 10) + d) (decimalPlace + 1)
            in
            case model.operationType of
                Nothing ->
                    { model
                        | displayedNumber = insertAtOnes model.displayedNumber digit
                        , firstNumber = insertAtOnes model.displayedNumber digit
                    }

                Just _ ->
                    { model
                        | displayedNumber = insertAtOnes model.secondNumber digit
                        , secondNumber = insertAtOnes model.secondNumber digit
                    }

        AllClearTextField ->
            init

        DivideByHundred ->
            { model
                | displayedNumber =
                    case model.displayedNumber of
                        Integer x ->
                            renderFloatToNumberType (toFloat x / 100)

                        Decimal x y ->
                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal x y) / 100)
            }

        EqualsTo ->
            let
                calculateResult m =
                    case m.operationType of
                        Just Addition ->
                            --m.firstNumber + m.secondNumber
                            case m.firstNumber of
                                Integer a ->
                                    case m.secondNumber of
                                        Integer b ->
                                            Integer (a + b)

                                        Decimal c d ->
                                            renderFloatToNumberType (toFloat a + renderNumberTypetoFloat (Decimal c d))

                                Decimal e f ->
                                    case m.secondNumber of
                                        Integer g ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) + toFloat g)

                                        Decimal h i ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) + renderNumberTypetoFloat (Decimal h i))

                        --Integer 1
                        Just Subtraction ->
                            case m.firstNumber of
                                Integer a ->
                                    case m.secondNumber of
                                        Integer b ->
                                            Integer (a - b)

                                        Decimal c d ->
                                            renderFloatToNumberType (toFloat a - renderNumberTypetoFloat (Decimal c d))

                                Decimal e f ->
                                    case m.secondNumber of
                                        Integer g ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) - toFloat g)

                                        Decimal h i ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) - renderNumberTypetoFloat (Decimal h i))

                        Just Multiplication ->
                            case m.firstNumber of
                                Integer a ->
                                    case m.secondNumber of
                                        Integer b ->
                                            Integer (a * b)

                                        Decimal c d ->
                                            renderFloatToNumberType (toFloat a * renderNumberTypetoFloat (Decimal c d))

                                Decimal e f ->
                                    case m.secondNumber of
                                        Integer g ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) * toFloat g)

                                        Decimal h i ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) * renderNumberTypetoFloat (Decimal h i))

                        Just Division ->
                            --m.firstNumber + m.secondNumber
                            case m.firstNumber of
                                Integer a ->
                                    case m.secondNumber of
                                        Integer b ->
                                            Integer (a // b)

                                        Decimal c d ->
                                            renderFloatToNumberType (toFloat a / renderNumberTypetoFloat (Decimal c d))

                                Decimal e f ->
                                    case m.secondNumber of
                                        Integer g ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) / toFloat g)

                                        Decimal h i ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) / renderNumberTypetoFloat (Decimal h i))

                        --m.firstNumber - m.secondNumber
                        --Integer 3
                        Nothing ->
                            Integer 0
            in
            { model
                | result = calculateResult model
                , displayedNumber = calculateResult model
                , firstNumber = calculateResult model
            }

        DoNothing str ->
            model

        DecimalButtonPressed ->
            let
                convertToDecimal num =
                    case num of
                        Integer u ->
                            Decimal u 0

                        Decimal _ _ ->
                            num
            in
            case model.operationType of
                Nothing ->
                    { model
                        | firstNumber = convertToDecimal model.firstNumber
                        , displayedNumber = convertToDecimal model.displayedNumber
                        , result = convertToDecimal model.result
                        , decimalButtonIsOn = True
                    }

                _ ->
                    { model
                        | secondNumber = convertToDecimal model.secondNumber
                        , displayedNumber = convertToDecimal model.displayedNumber
                        , result = convertToDecimal model.result
                        , decimalButtonIsOn = True
                    }



--VIEW


view : Model -> View msg
view model =
    { title = "Homepage"
    , body =
        [ Element.layout []
            (Element.text "Hello,  Universe !!")
        ]
    }
