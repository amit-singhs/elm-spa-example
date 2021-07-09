module Utilities.UxComponents exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Heroicons.Solid as Heroicons
import Svg.Attributes as SvgA
import Utilities.Palette


type alias ModalConfig msg =
    { title : String
    , body : Element msg
    , onClose : msg
    }


edges =
    { left = 0
    , right = 0
    , top = 0
    , bottom = 0
    }


viewModalPlaceholder : Bool -> ModalConfig msg -> Element msg
viewModalPlaceholder showModal modalConfig =
    if showModal == False then
        Element.none

    else
        row
            [ width fill
            , height fill
            , Background.color Palette.overlayBGColor
            , Events.onClick modalConfig.onClose
            ]
            [ column
                [ Background.color Palette.white
                , centerX
                , centerY
                , padding 25
                , Border.rounded 10
                ]
                [ row
                    [ Region.heading 1
                    , Font.size 30
                    , width fill
                    , paddingXY 0 10
                    , Border.widthEach { edges | bottom = 1 }
                    ]
                    [ column
                        [ width <| fillPortion 40
                        , paddingEach { edges | right = 50 }
                        ]
                        [ text modalConfig.title ]
                    , column [ width <| fillPortion 1, Events.onClick modalConfig.onClose ]
                        [ Heroicons.x
                            [ SvgA.height "25px"
                            , SvgA.width "25px"
                            ]
                            |> html
                        ]
                    ]
                , row
                    [ Border.widthEach { edges | bottom = 1 }
                    , paddingXY 0 25
                    , width fill
                    ]
                    [ modalConfig.body ]
                , row
                    [ paddingXY 0 15
                    , alignRight
                    ]
                    [ Input.button []
                        { onPress = Just modalConfig.onClose
                        , label = text "Close"
                        }
                    ]
                ]
            ]
