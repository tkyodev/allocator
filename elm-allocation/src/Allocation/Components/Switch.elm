module Allocation.Components.Switch exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html.Attributes


view : msg -> Bool -> String -> Element msg
view msg checked label =
    Input.checkbox [ mouseOver [ Background.color <| rgba 0 0 0 0.1 ], padding 4 ] <|
        { checked = checked
        , icon =
            toggleCheckboxWidget
                { offColor = rgb255 187 187 187
                , onColor = rgb255 20 180 230
                , sliderColor = rgb255 255 255 255
                , toggleHeight = 20
                , toggleWidth = 40
                }
        , label = Input.labelRight [ centerY ] <| text label
        , onChange = \_ -> msg
        }


toggleCheckboxWidget :
    { offColor : Color, onColor : Color, sliderColor : Color, toggleHeight : Int, toggleWidth : Int }
    -> Bool
    -> Element msg
toggleCheckboxWidget { offColor, onColor, sliderColor, toggleHeight, toggleWidth } checked =
    -- From https://ellie-app.com/85HbWTjCGWha1
    let
        pad : number
        pad =
            3

        sliderSize : Int
        sliderSize =
            toggleHeight - 2 * pad
    in
    el
        [ Background.color <|
            if checked then
                onColor

            else
                offColor
        , width <| px <| toggleWidth
        , height <| px <| toggleHeight
        , Border.rounded 14
        , htmlAttribute <| Html.Attributes.style "box-shadow" "rgba(0, 0, 0, 0.5) 0px 3px 2px -1px inset"
        , inFront <|
            el [ height fill ] <|
                el
                    [ Background.color sliderColor
                    , Border.rounded <| sliderSize // 2
                    , width <| px <| sliderSize
                    , height <| px <| sliderSize
                    , centerY
                    , moveRight pad
                    , htmlAttribute <| Html.Attributes.style "transition" ".2s"
                    , htmlAttribute <| Html.Attributes.style "box-shadow" "rgba(0, 0, 0, 0.5) 0px 3px 2px -1px"
                    , htmlAttribute <|
                        if checked then
                            let
                                translation : String
                                translation =
                                    (toggleWidth - sliderSize - pad)
                                        |> String.fromInt
                            in
                            Html.Attributes.style "transform" <| "translateX(" ++ translation ++ "px)"

                        else
                            Html.Attributes.class ""
                    ]
                <|
                    text ""
        ]
    <|
        text ""
