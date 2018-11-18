module ElmAutoStereogram exposing (main)

{-| Web app that creates text-based MagicEye Autostereograms.

Todo:
* Make the tab nav links work, per https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#link

Here's my original design-doc:

Now I just need to sit down and use some Elm to make the user interface way better.

Tabs to switch between output modes would be really helpful:

* viewing text — current method
* markdown code — text you can copy and paste into Reddit submissions or comments that will render as desired
* and viewing canvas — would allow folk to right click and save-as-png or copy-image to avoid needing to know how to use a snipping tool

Using sliders to position the text instead of making folk enter a column number, and rendering in real time upon changes would also be pretty hip. :B
-}

import Html
    exposing
        (   Html
        ,   div
--         ,   p
--         ,   h1
--         ,   li
--         ,   ol
--         ,   section
-- --        , text
--         ,   ul
        )
-- import Html.Attributes as Attr
-- import Html.Events exposing (onInput, onBlur)
-- import Html.Events.Extra exposing (onEnter)
-- import Dom exposing (focus)
import Task
-- import Random
import Browser
import Browser.Navigation as Nav
import Json.Decode as Decode
import Url
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Debug


-- PRIMARY DECLARATION


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init =
            init

        , view =
            view

        , update =
            update

        , subscriptions =
            subscriptions

        , onUrlChange =
            (\url -> UrlChange url)

        , onUrlRequest =
            (\_ -> Url.Url Url.Https "google.com" Nothing "/" Nothing Nothing |> UrlChange )

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

init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model -- key url (Keycloak.validate flags)
    { tab = TabPlaintext
    } 
  , Cmd.none
  )



-- UPDATE


type Msg
    =   UrlChange Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  -- case msg of
  --   UrlChange location ->
  --   ( Model
  --     {   model
  --     |   tab =
  --             location.hash
      
  --     }
  --   , Cmd.none
  --   )
  (model, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


tabColor : Element.Color
tabColor =
  Element.rgb 0.9 0.8 0.7


tabDivider : Element Msg
tabDivider =
  Element.el
  [ Element.width ( Element.fill |> Element.maximum 2 )
  ]
  ( Element.text "" )


tab : String -> String -> Element Msg
tab title _ =
  Element.text title
  |> Html.a []
  |>Element.el
    [ Border.roundEach
      { topLeft = 3
      , topRight = 3
      , bottomLeft = 0
      , bottomRight = 0
      }
    -- , Border.widthEach
    --   { bottom = 0
    --   , left = 2
    --   , right = 2
    --   , top = 2
    --   }
    , Border.color tabColor
    , Background.color tabColor
    , Element.fillPortion 1
      |>Element.width
    , Element.padding 5
    , Font.center
    ]

body : Model -> List ( Html Msg )
body model =
  [ Element.layout
    [ Element.width Element.fill
    , Element.padding 10
    -- , Element.explain Debug.todo
    ]
    <| Element.row
    [ Element.centerX
    , Element.width
      ( Element.fill
        |> Element.maximum 600
        |> Element.minimum 600
      )
    ]
    [ tab "Plain Text" "TabText"
    , tabDivider
    , tab "Reddit Markdown" "TabMarkdown"
    , tabDivider
    , tab "Downloadable Image" "TabImage"
    ]
  ]

view : Model -> Browser.Document Msg
view model =
  { title = "Title"
  , body = body model
  }