module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import CanvasColor as Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)


type alias Model =
    { count : Float }


init : () -> ( Model, Cmd Msg )
init () =
    ( { count = 0 }, Cmd.none )


type Msg
    = Frame Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( { model | count = model.count + 1 }, Cmd.none )


width =
    400


height =
    400


centerX =
    width / 2


centerY =
    height / 2


view : Model -> Html Msg
view { count } =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.element
            width
            height
            [ style "border" "10px solid rgba(0,0,0,0.1)" ]
            (Canvas.empty
                |> clearRect 0 0 width height
                |> fillStyle (Color.hsl (degrees (count / 4)) 0.3 0.7)
                |> render count
            )
        ]


render count cmds =
    let
        size =
            width / 3

        x =
            -(size / 2)

        y =
            -(size / 2)
    in
    cmds
        |> save
        |> translate centerX centerY
        |> rotate (degrees (count * 3))
        |> fillRect x y size size
        |> restore


subscriptions model =
    onAnimationFrameDelta Frame


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
