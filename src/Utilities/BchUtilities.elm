module Utilities.BchUtilities exposing (..)

import Element exposing (Element, text)
import Html exposing (Html)
import QRCode
import Svg.Attributes as SvgA
import Utilities.Calculator as Calculator exposing (NumberType(..))


type alias Address =
    String


type alias Mnemonic =
    String


type Operation
    = PressingAddButton


type alias Model =
    { displayedNumber : NumberType
    , inputNumber : NumberType
    , operationType : Maybe Operation
    , numbersList : List NumberType
    , total : NumberType
    , decimalButtonIsOn : Bool
    , wallet : Maybe Address
    , mnemonic : Maybe Mnemonic
    , showModal : Bool
    , bchUsdPrice : Maybe Float
    , addressBalance : Maybe Float
    }


buildCashDataURI : String -> NumberType -> Maybe Float -> String
buildCashDataURI address totalInUSD maybeBchUsdPrice =
    case maybeBchUsdPrice of
        Nothing ->
            address

        Just bchUsdPrice ->
            address
                ++ "?total="
                ++ String.fromFloat
                    (convertUsdToBch
                        (Calculator.renderNumberTypetoFloat totalInUSD)
                        bchUsdPrice
                    )


qrCodeView : Model -> Element msg
qrCodeView model =
    Element.el [] <|
        case model.wallet of
            Just address ->
                let
                    dataURI =
                        buildCashDataURI address model.total model.bchUsdPrice
                in
                Element.column []
                    [ Element.row []
                        [ Element.column [] [ text "Balance : " ]
                        , Element.column []
                            [ case model.addressBalance of
                                Nothing ->
                                    text "Unavailable"

                                Just balance ->
                                    text <| String.fromFloat balance
                            ]
                        ]
                    , Element.row [ Element.centerX ]
                        [ Element.html
                            (QRCode.fromStringWith QRCode.High dataURI
                                |> Result.map
                                    (QRCode.toSvg
                                        [ SvgA.width "500px"
                                        , SvgA.height "500px"
                                        ]
                                    )
                                |> Result.withDefault (Html.text "Error while encoding to QRCode.")
                            )
                        ]
                    , Element.row [] [ text dataURI ]
                    ]

            Nothing ->
                text "No address found"


convertUsdToBch : Float -> Float -> Float
convertUsdToBch usd bchUsdPrice =
    usd / bchUsdPrice
