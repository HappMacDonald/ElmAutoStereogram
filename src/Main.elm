module ElmAutoStereogram exposing (main)

{-| Web app that creates text-based MagicEye Autostereograms.

Todo:
BUGS:
> type Model = Model { tab: Int }
> Model { tab: 3 }
-- PARSE ERROR ------------------------------------------------------------- elm

Something went wrong while parsing a record in repl_value_1's definition.

5|   Model { tab: 3 }
                ^
I was expecting:

  - an equals sign (=) followed by an expression
  - a vertical bar (|) followed by the record fields you want to update


* Get onto my feet making this silly thing

Here's my original design-doc:

Now I just need to sit down and use some Elm to make the user interface way better.

Tabs to switch between output modes would be really helpful:

* viewing text — current method
* markdown code — text you can copy and paste into Reddit submissions or comments that will render as desired
* and viewing canvas — would allow folk to right click and save-as-png or copy-image to avoid needing to know how to use a snipping tool

Using sliders to position the text instead of making folk enter a column number, and rendering in real time upon changes would also be pretty hip. :B
-}

import Html
--     exposing
--         (   Html
--         ,   div
--         ,   p
--         ,   h1
--         ,   li
--         ,   ol
--         ,   section
-- --        , text
--         ,   ul
--         )
-- import Html.Attributes as Attr
-- import Html.Events exposing (onInput, onBlur)
-- import Html.Events.Extra exposing (onEnter)
-- import Dom exposing (focus)
import Task
-- import Random
import Browser


-- PRIMARY DECLARATION


main : Program String Model msg
main =
    Browser.program
        UrlChange
        { init =
            init

        , view =
            view

        , update =
            update

        , subscriptions =
            subscriptions

        }



-- HELPERS AND DEFINITIONS

type TabType =
    TabPlaintext
    | TabMarkdown
    | TabCanvas


-- MODEL


type Model =
    Model
    {   tab: TabType
    }



-- INIT


init : Browser.Location -> ( Model, Cmd Msg )
init location =
    Model
    { tab: TabPlaintext
    } ! ( )



-- UPDATE


type Msg
    =   UrlChange Browser.Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UrlChange location ->
      Model
      {   model
      |   tab =
              location.hash
      
      } ! ( )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ Html.text "" ]
