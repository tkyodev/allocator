module Allocator.Components.Slider exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input


view :
    { inputFieldsWidth : Int
    , labelWidth : Int
    , label : String
    , max : Float
    , min : Float
    , step : Maybe Float
    , value : Float
    , msg : Float -> msg
    }
    -> Element msg
view { inputFieldsWidth, labelWidth, value, min, max, step, label, msg } =
    Input.slider
        [ width <| px inputFieldsWidth
        , height <| px 10
        , behindContent
            (el
                [ width fill
                , height (px 6)
                , centerY
                , Background.color <| rgba 0 0 0 0.2
                , Border.rounded 2
                ]
                none
            )
        ]
        { onChange = msg
        , label = Input.labelLeft [ width <| px labelWidth ] <| text <| label ++ " "
        , min = min
        , max = max
        , value = value
        , thumb = Input.defaultThumb
        , step = step
        }
