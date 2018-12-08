port module Example exposing (Model, Msg(..), Position(..), divStyle, init, isNothing, main, update, view, viewDiv)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop
import Json.Decode exposing (Value)


port dragstart : Value -> Cmd msg


type Position
    = Up
    | Middle
    | Down


type alias Model =
    { data : { count : Int, position : Position }
    , dragDrop : DragDrop.Model Int Position
    }


type Msg
    = DragDropMsg (DragDrop.Msg Int Position)


init () =
    ( { data = { count = 0, position = Up }
      , dragDrop = DragDrop.init
      }
    , Cmd.none
    )


update msg model =
    case msg of
        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop
            in
            ( { model
                | dragDrop = model_
                , data =
                    case result of
                        Nothing ->
                            model.data

                        Just ( count, position, _ ) ->
                            { count = count + 1, position = position }
              }
            , DragDrop.getDragstartEvent msg_
                |> Maybe.map (.event >> dragstart)
                |> Maybe.withDefault Cmd.none
            )


subscriptions model =
    Sub.none


divStyle =
    [ style "border" "1px solid black"
    , style "padding" "50px"
    , style "text-align" "center"
    ]


view model =
    let
        dropId =
            DragDrop.getDropId model.dragDrop

        position =
            DragDrop.getDroppablePosition model.dragDrop
    in
    div []
        [ viewDiv Up model.data dropId position
        , viewDiv Middle model.data dropId position
        , viewDiv Down model.data dropId position
        ]


isNothing maybe =
    case maybe of
        Just _ ->
            False

        Nothing ->
            True


viewDiv position data dropId droppablePosition =
    let
        highlight =
            if dropId |> Maybe.map ((==) position) |> Maybe.withDefault False then
                case droppablePosition of
                    Nothing ->
                        []

                    Just pos ->
                        if pos.y < pos.height // 2 then
                            [ style "background-color" "cyan" ]

                        else
                            [ style "background-color" "magenta" ]

            else
                []
    in
    div
        (divStyle
            ++ highlight
            ++ (if data.position /= position then
                    DragDrop.droppable DragDropMsg position

                else
                    []
               )
        )
        (if data.position == position then
            [ img (src "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg" :: width 100 :: DragDrop.draggable DragDropMsg data.count) []
            , text (String.fromInt data.count)
            ]

         else
            []
        )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
