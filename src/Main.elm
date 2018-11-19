module ElmAutoStereogram exposing (main)

{-| Web app that creates text-based MagicEye Autostereograms.

Todo:
* Plaintext only portion rendering properly now, so..
* Build out (with test suite!) the machinery to render the ascii Autostereogram
  for some hard-coded phrase and placements. :)
* Testing the Dictionary module
* Have temporary method "testDictionary" at bottom of this file to speed up manual testing of that.
* That gets called with hard-coded random seed ints, and then appended to the label of "Plain Text" tab.
* So next thing is I need to figure out next step... ;P
** Probably reading the JS code to see how it begins?

Here's my original design-doc:

Now I just need to sit down and use some Elm to make the user interface way better.

Tabs to switch between output modes would be really helpful:

* viewing text — current method
* markdown code — text you can copy and paste into Reddit submissions or comments that will render as desired
* and viewing canvas — would allow folk to right click and save-as-png or copy-image to avoid needing to know how to use a snipping tool

Using movable text grabbies to position the text instead of making folk enter a column number, and rendering in real time upon changes would also be pretty hip. :B
Now if I use movable text grabbies, I wonder how challenging it would really be to feature multiple words per line? Hmm...
-}

import Dictionary

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
import Random


-- PRIMARY DECLARATION


main : Program Decode.Value Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- HELPERS AND DEFINITIONS

type TabType
  = TabPlaintext
  | TabMarkdown
  | TabImage


outputRows : Int
outputRows =
  23


outputColumns : Int
outputColumns =
  79


-- MODEL


type Model =
  Model
  { tab: TabType
  , ascii: List String
  }


tabChange : Model -> Url.Url -> ( Model, Cmd Msg )
tabChange ((Model modelRecord) as model) url =
  case url.fragment of
    Just "TabPlaintext" ->
      ( Model { modelRecord | tab = TabPlaintext }, Nav.load (Url.toString url) )

    Just "TabMarkdown" ->
      ( Model { modelRecord | tab = TabMarkdown }, Nav.load (Url.toString url) )

    Just "TabImage" ->
      ( Model { modelRecord | tab = TabImage }, Nav.load (Url.toString url) )

    _ ->
      ( model, Cmd.none )



-- INIT

init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
    x=Debug.log "update msg=" url
  in
    tabChange
    ( Model
      { tab = TabPlaintext
      , ascii = "@"
        |>String.repeat outputColumns
        |>List.repeat outputRows
      }
    ) url



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Model modelRecord) as model) =
  let
    x=Debug.log "update msg=" msg
  in
    case msg of
      LinkClicked urlRequest ->
        case urlRequest of
          Browser.Internal url ->
            tabChange model url

          Browser.External url ->
            ( model, Nav.load url )
      
      UrlChanged url ->
        ( model, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


tabColor : Element.Color
tabColor =
  Element.rgb 0.9 0.8 0.7


tabColorActive : Element.Color
tabColorActive =
  Element.rgb 0.8 0.7 0.6


tabBorderColor : Element.Color
tabBorderColor =
  Element.rgb 0.95 0.85 0.75


tabDivider : Element Msg
tabDivider =
  Element.el
  [ Element.width ( Element.fill |> Element.maximum 5 )
  ]
  ( Element.text "" )


tab : Model -> TabType -> String -> String -> Element Msg
tab (Model model) tabTarget title link =
  let
    backgroundColor = 
      if tabTarget == model.tab then
        tabColorActive
      else
        tabColor

  in
    Element.link
    [ Element.centerX
    , Element.width Element.fill
    ]
    { url = link
    , label = Element.text title
    }
    |>Element.el
      [ Border.roundEach
        { topLeft = 3
        , topRight = 3
        , bottomLeft = 0
        , bottomRight = 0
        }
      , Border.widthEach
        { bottom = 0
        , left = 2
        , right = 2
        , top = 2
        }
      , Border.color tabBorderColor
      , Background.color backgroundColor
      , Element.fillPortion 1
        |>Element.width
      , Element.padding 5
      , Font.center
      ]

body : Model -> List ( Html Msg )
body ( (Model modelRecord) as model) =
  [ Element.layout
    [ Element.width Element.fill
    , Element.padding 10
    -- , Element.explain Debug.todo
    ]
    <|Element.column
      [ Element.centerX
--      , Element.width
--        ( Element.fill
--          |> Element.maximum 600
--          |> Element.minimum 600
--        )
      ]
      [ Element.row
        [ Element.centerX
        , Element.width Element.fill
        ]
        [ tab model TabPlaintext ("Plain Text" ++ (testDictionary 7)) "#TabPlaintext"
        , tabDivider
        , tab model TabMarkdown "Reddit Markdown" "#TabMarkdown"
        , tabDivider
        , tab model TabImage "Downloadable Image" "#TabImage"
        ]
      , Element.column
        [ Border.width 2
        , Border.color (Element.rgb 0 0 0)
        , Background.color (Element.rgb 1 1 1)
        , Font.family [ Font.typeface "Courier" ]
        , Element.paddingXY 8 8
        ]
        ( modelRecord.ascii
          |>List.map
            (\string -> Element.text string)
        )
      ]
  ]

view : Model -> Browser.Document Msg
view model =
  { title = "Title"
  , body = body model
  }

testDictionary : Int -> String
testDictionary q =
  let
    seed = Random.initialSeed q
  in
    Dictionary.getOne seed
    |>Tuple.first 
