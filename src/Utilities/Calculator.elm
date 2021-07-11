module Utilities.Calculator exposing
    ( NumberType(..)
    , addTwoNumberTypes
    , createButton
    , renderFloatToNumberType
    , renderNumberTypetoFloat
    , renderNumberTypetoString
    , renderToElementList
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


type NumberType
    = Integer Int
    | Decimal Int Int


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


renderToElementList : List NumberType -> List (Element msg)
renderToElementList xs =
    let
        attributes =
            []
    in
    case xs of
        [] ->
            []

        numType :: tail ->
            case numType of
                Integer _ ->
                    Element.row attributes [ Element.text (renderNumberTypetoString numType) ]
                        :: renderToElementList tail

                Decimal _ _ ->
                    Element.row attributes [ Element.text (renderNumberTypetoString numType) ]
                        :: renderToElementList tail


addTwoNumberTypes : NumberType -> NumberType -> NumberType
addTwoNumberTypes numType1 numType2 =
    case numType1 of
        Integer i ->
            case numType2 of
                Integer j ->
                    Integer (i + j)

                Decimal _ _ ->
                    renderFloatToNumberType (toFloat i + renderNumberTypetoFloat numType2)

        Decimal _ _ ->
            case numType2 of
                Integer i ->
                    renderFloatToNumberType (toFloat i + renderNumberTypetoFloat numType1)

                Decimal _ _ ->
                    renderFloatToNumberType (renderNumberTypetoFloat numType1 + renderNumberTypetoFloat numType2)


createButton : String -> Int -> msg -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Element msg
createButton buttonLabel buttonlength buttonEvent tL tR bL bR r g b =
    Input.button
        [ Element.height (Element.px 60)
        , Element.width (Element.px buttonlength)
        , Border.width 1
        , Border.roundEach
            { topLeft = tL
            , topRight = tR
            , bottomLeft = bL
            , bottomRight = bR
            }
        , Border.color <| Element.rgb255 84 83 81
        , Font.size 25
        , Font.family
            [ Font.typeface "Helvetica"
            ]
        , Background.color <| Element.rgb255 r g b
        , Font.color <| Element.rgb255 228 228 228
        , Font.medium
        , Font.center
        , Element.mouseDown
            [ Background.color <| Element.rgb255 180 180 179
            , Border.color <| Element.rgb255 84 83 81
            ]
        ]
        { onPress = Just buttonEvent
        , label = Element.text buttonLabel
        }
