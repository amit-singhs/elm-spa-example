module Pages.PointOfSale exposing (Model, Msg, page, view)

--imports

import BchPorts exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Page exposing (Page)
import QRCode
import Request exposing (Request)
import Shared exposing (subscriptions)
import Svg.Attributes as SvgA
import Utilities.Palette as Palette
import Utilities.UxComponents as UxComponents
import View exposing (View)



-- Utilities


convertUsdToBch : Float -> Float -> Float
convertUsdToBch usd bchUsdPrice =
    usd / bchUsdPrice


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


renderToElementList : List NumberType -> List (Element Msg)
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
                    row attributes [ Element.text (renderNumberTypetoString numType) ]
                        :: renderToElementList tail

                Decimal _ _ ->
                    row attributes [ Element.text (renderNumberTypetoString numType) ]
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
                        (renderNumberTypetoFloat totalInUSD)
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
                    , Element.row [ centerX ]
                        [ html
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


createButton : String -> Int -> Msg -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Element Msg
createButton buttonLabel buttonlength buttonEvent tL tR bL bR r g b =
    Input.button
        [ Element.height (px 60)
        , Element.width (px buttonlength)
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
        , mouseDown
            [ Background.color <| Element.rgb255 180 180 179
            , Border.color <| Element.rgb255 84 83 81
            ]
        ]
        { onPress = Just buttonEvent
        , label = Element.text buttonLabel
        }



-- page(equivalent of main)


page : Shared.Model -> Request -> Page.With Model Msg
page _ _ =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


type alias Address =
    String


type alias Mnemonic =
    String


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


init : ( Model, Cmd Msg )
init =
    ( { displayedNumber = Integer 0
      , inputNumber = Integer 0
      , numbersList = []
      , total = Integer 0
      , operationType = Nothing
      , decimalButtonIsOn = False
      , wallet = Nothing
      , showModal = False
      , bchUsdPrice = Nothing
      , mnemonic = Nothing
      , addressBalance = Nothing

      --   , itemValue = Nothing
      }
    , Cmd.batch
        [ getBchPrice ()
        , getWalletFromLocalStorage "wallet"
        ]
    )


type Operation
    = PressingAddButton


type NumberType
    = Integer Int
    | Decimal Int Int


type Msg
    = SayHello
    | DoNothing String
    | InsertDigit Int
    | DecimalButtonPressed
    | AddButtonPressed
    | CashAddressRecv String
    | BchUSDPriceRecv Float
    | MnemonicRecv Mnemonic
    | BalanceRecv Float
    | DisplayModal
    | HideModal



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SayHello ->
            ( { model | displayedNumber = model.inputNumber }
            , Cmd.none
            )

        DoNothing _ ->
            ( model
            , Cmd.none
            )

        InsertDigit digit ->
            let
                insertAtOnes : NumberType -> Int -> NumberType
                insertAtOnes x d =
                    case x of
                        Integer v ->
                            Integer ((v * 10) + d)

                        Decimal v decimalPlace ->
                            Decimal ((v * 10) + d) (decimalPlace + 1)
            in
            case model.operationType of
                Nothing ->
                    ( { model
                        | displayedNumber = insertAtOnes model.displayedNumber digit
                        , inputNumber = insertAtOnes model.displayedNumber digit
                      }
                    , Cmd.none
                    )

                Just _ ->
                    ( { model
                        | inputNumber = insertAtOnes model.inputNumber digit
                        , displayedNumber = insertAtOnes model.inputNumber digit
                      }
                    , Cmd.none
                    )

        DecimalButtonPressed ->
            let
                convertToDecimal num =
                    case num of
                        Integer u ->
                            Decimal u 0

                        Decimal _ _ ->
                            num
            in
            ( { model
                | displayedNumber = convertToDecimal model.displayedNumber
                , inputNumber = convertToDecimal model.inputNumber
              }
            , Cmd.none
            )

        AddButtonPressed ->
            ( { model
                | operationType = Just PressingAddButton
                , total = addTwoNumberTypes model.total model.inputNumber
                , displayedNumber = model.inputNumber
                , inputNumber = Integer 0
                , decimalButtonIsOn = False
                , numbersList = model.inputNumber :: model.numbersList
              }
            , Cmd.none
            )

        BchUSDPriceRecv bchUsdPriceRecvd ->
            ( { model | bchUsdPrice = Just bchUsdPriceRecvd }
            , Cmd.none
            )

        MnemonicRecv mnemonic ->
            ( { model | mnemonic = Just mnemonic }
            , getCashAddress mnemonic
            )

        CashAddressRecv address ->
            ( { model | wallet = Just address }
            , Cmd.none
            )

        BalanceRecv balance ->
            ( { model | addressBalance = Just balance }
            , Cmd.none
            )

        DisplayModal ->
            ( { model | showModal = True }
            , case model.wallet of
                Just address ->
                    fetchAddressBalance address

                Nothing ->
                    Cmd.none
            )

        HideModal ->
            ( { model | showModal = False }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ cashAddressReceiver CashAddressRecv
        , bchPriceReceiver BchUSDPriceRecv
        , mnemonicFromLocalStorageReceiver MnemonicRecv
        , addressBalanceReceiver BalanceRecv
        ]


view model =
    { title = "Point of sale"
    , body =
        [ Element.layout
            [ inFront <|
                UxComponents.viewModalPlaceholder model.showModal
                    { title = "QR Code for payment"
                    , body = qrCodeView model
                    , onClose = HideModal
                    }
            ]
          <|
            column
                [ padding 40
                , spacing 10
                , width fill
                ]
                [ row
                    [ spacing 30 ]
                    [ column [ Element.width <| fillPortion 1618 ]
                        [ row []
                            [ Input.text
                                [ Border.roundEach
                                    { topLeft = 12
                                    , topRight = 12
                                    , bottomLeft = 0
                                    , bottomRight = 0
                                    }
                                , Element.width (px 495)
                                , Border.color <| Element.rgb255 84 83 81
                                , padding 40
                                , Background.color <| Element.rgb255 174 245 189
                                , Font.light
                                , Font.alignRight
                                , Font.color <| Element.rgb255 0 0 0
                                ]
                                { label = Input.labelHidden "input text box"
                                , onChange = DoNothing
                                , placeholder = Nothing
                                , text = renderNumberTypetoString model.displayedNumber
                                }
                            ]
                        , row []
                            [ column [] [ createButton "1" 165 (InsertDigit 1) 0 0 0 0 106 166 119 ]
                            , column [] [ createButton "2" 165 (InsertDigit 2) 0 0 0 0 106 166 119 ]
                            , column [] [ createButton "3" 165 (InsertDigit 3) 0 0 0 0 106 166 119 ]
                            ]
                        , row []
                            [ column [] [ createButton "4" 165 (InsertDigit 4) 0 0 0 0 106 166 119 ]
                            , column [] [ createButton "5" 165 (InsertDigit 5) 0 0 0 0 106 166 119 ]
                            , column [] [ createButton "6" 165 (InsertDigit 6) 0 0 0 0 106 166 119 ]
                            ]
                        , row []
                            [ column [] [ createButton "7" 165 (InsertDigit 7) 0 0 0 0 106 166 119 ]
                            , column [] [ createButton "8" 165 (InsertDigit 8) 0 0 0 0 106 166 119 ]
                            , column [] [ createButton "9" 165 (InsertDigit 9) 0 0 0 0 106 166 119 ]
                            ]
                        , row []
                            [ column [] [ createButton "." 165 DecimalButtonPressed 0 0 0 0 106 166 119 ]
                            , column [] [ createButton "0" 165 (InsertDigit 0) 0 0 0 0 106 166 119 ]
                            , column [] [ createButton "Add" 165 AddButtonPressed 0 0 0 0 106 166 119 ]
                            ]
                        , row []
                            [ column [] [ createButton ("Pay: $ " ++ renderNumberTypetoString model.total) 495 DisplayModal 0 0 0 0 106 166 119 ]
                            ]
                        ]
                    , column [ spacing 10, width <| fillPortion 1000 ] <|
                        renderToElementList model.numbersList
                            ++ [ text ("Total : $ " ++ renderNumberTypetoString model.total)
                               ]
                    ]
                , Element.row [ Font.semiBold ]
                    [ Element.link []
                        { url = "/"
                        , label = Element.text "Goto Home"
                        }
                    ]
                , Element.row [ Font.semiBold ]
                    [ Element.link []
                        { url = "/calculator"
                        , label = Element.text "Click here to go to calculator"
                        }
                    ]
                ]
        ]
    }
