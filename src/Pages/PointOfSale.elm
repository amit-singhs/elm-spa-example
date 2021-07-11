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
import Request exposing (Request)
import Shared exposing (subscriptions)
import Utilities.BchUtilities as BchUtil exposing (Model, Operation(..))
import Utilities.Calculator as Calculator exposing (NumberType(..))
import Utilities.UxComponents as UxComponents
import View exposing (View)



--TODO
-- * Move all the utilities functions into a Calculator.elm
-- Utilities
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
                , total = Calculator.addTwoNumberTypes model.total model.inputNumber
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
                    , body = BchUtil.qrCodeView model
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
                                , text = Calculator.renderNumberTypetoString model.displayedNumber
                                }
                            ]
                        , row []
                            [ column [] [ Calculator.createButton "1" 165 (InsertDigit 1) 0 0 0 0 106 166 119 ]
                            , column [] [ Calculator.createButton "2" 165 (InsertDigit 2) 0 0 0 0 106 166 119 ]
                            , column [] [ Calculator.createButton "3" 165 (InsertDigit 3) 0 0 0 0 106 166 119 ]
                            ]
                        , row []
                            [ column [] [ Calculator.createButton "4" 165 (InsertDigit 4) 0 0 0 0 106 166 119 ]
                            , column [] [ Calculator.createButton "5" 165 (InsertDigit 5) 0 0 0 0 106 166 119 ]
                            , column [] [ Calculator.createButton "6" 165 (InsertDigit 6) 0 0 0 0 106 166 119 ]
                            ]
                        , row []
                            [ column [] [ Calculator.createButton "7" 165 (InsertDigit 7) 0 0 0 0 106 166 119 ]
                            , column [] [ Calculator.createButton "8" 165 (InsertDigit 8) 0 0 0 0 106 166 119 ]
                            , column [] [ Calculator.createButton "9" 165 (InsertDigit 9) 0 0 0 0 106 166 119 ]
                            ]
                        , row []
                            [ column [] [ Calculator.createButton "." 165 DecimalButtonPressed 0 0 0 0 106 166 119 ]
                            , column [] [ Calculator.createButton "0" 165 (InsertDigit 0) 0 0 0 0 106 166 119 ]
                            , column [] [ Calculator.createButton "Add" 165 AddButtonPressed 0 0 0 0 106 166 119 ]
                            ]
                        , row []
                            [ column [] [ Calculator.createButton ("Pay: $ " ++ Calculator.renderNumberTypetoString model.total) 495 DisplayModal 0 0 0 0 106 166 119 ]
                            ]
                        ]
                    , column [ spacing 10, width <| fillPortion 1000 ] <|
                        Calculator.renderToElementList model.numbersList
                            ++ [ text ("Total : $ " ++ Calculator.renderNumberTypetoString model.total)
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
