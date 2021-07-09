port module BchPorts exposing (..)

-- Ports


port getCashAddress : Mnemonic -> Cmd msg


port cashAddressReceiver : (String -> msg) -> Sub msg


port getBchPrice : () -> Cmd msg


port bchPriceReceiver : (Float -> msg) -> Sub msg


port getWalletFromLocalStorage : String -> Cmd msg


port mnemonicFromLocalStorageReceiver : (Mnemonic -> msg) -> Sub msg


port fetchAddressBalance : Address -> Cmd msg


port addressBalanceReceiver : (Float -> msg) -> Sub msg


type alias Address =
    String


type alias Mnemonic =
    String
