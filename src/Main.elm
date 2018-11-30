module ElmAutoStereogram exposing (main)

{-| Web app that creates text-based MagicEye Autostereograms.

Bugs:
* If a WordPlacement word gets too long .. I get an infinite loop. ;P

Todo:
* Have an initialPuzzle object to act as a hardcoded puzzle we'll begin working
  with.
* Trying to build out a puzzleRender function next to actually create an
  autostereogram from said puzzle data.

Here's my original design-doc:

Now I just need to sit down and use some Elm to make the user interface
way better.

Tabs to switch between output modes would be really helpful:

* viewing text — current method
* markdown code — text you can copy and paste into Reddit submissions
  or comments that will render as desired
* and viewing canvas — would allow folk to right click and save-as-png
  or copy-image to avoid needing to know how to use a snipping tool

Using movable text grabbies to position the text instead of making folk
enter a column number, and rendering in real time upon changes
would also be pretty hip. :B

Now if I use movable text grabbies, I wonder how challenging
it would really be to feature multiple words per line? Hmm...
-}

import Dictionary

import Html exposing (Html)
import Html.Attributes as Attr
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
import Element.Input as Input
import Debug
import Random
import Canvas
import CanvasColor

x = Debug.todo "Wait! Gotta run in Chrome, and fix the infinite loop problem. D:"


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
  = TabEdit
  | TabPlainText
  | TabImage


-- (outputRows, outputColumns) : (Int, Int)
-- (outputRows, outputColumns) =
--   (23, 79)


outputRows : Int
outputRows =
  23


outputColumns : Int
outputColumns =
  79  


{-| How many characters a background-layer pattern repeats
Eventually I'll make this configurable
-}

parallax : Int
parallax =
  15


{-| The width in glyphs of a space character.
Yeah obviously this is "one", I just hate magic numbers. :P
-}
spaceWidth : Int
spaceWidth =
  1

{-| When using more than one word to fill a space, make sure that
the second word gets to be at least this long because otherwise
we'll get too many tiny words everywhere. :/
-}
breathingRoom : Int
breathingRoom =
  3

{-| We won't allow single messages longer than this
Hardcoded to be 6 less than parallax for now.
I forgot where "6" came from, mor experimentation may be called for
to find out it's origins. :P
-}
maxMessageLength : Int
maxMessageLength =
  parallax - 6



type WordPlacement =
  WordPlacement
  { word : String -- what the word is
  , left : Int -- how far from left edge of output it should begin
  }


type Puzzle =
  Puzzle ( List ( List WordPlacement ) )


calcLeftRightLength : List PairList -> List PairList -> Int -> Random.Seed ->
  { leftLength : Int
  , rightLength : Int
  , leftWordPairs : PairList
  , rightWordPairs : PairList
  , seedA1: Random.Seed
  }
calcLeftRightLength leftPairs rightPairs wordLength seedC0 =
  let
    (leftLength, seedC1) =
      Random.step
        ( Random.int
            Dictionary.shortestWord
            ( maxMessageLength
            - wordLength
            + Dictionary.shortestWord
            )
        )
        seedC0

    rightLength =
      maxMessageLength - wordLength - leftLength + 2
      -- yeah.. magic number "2" here disappoints me.
      -- will need to research more what it's
      -- real genesis and/or meaning is. :(

    -- x = Debug.log "active"
    --     { leftLength = leftLength
    --     , wordLength = wordLength
    --     , rightLength = rightLength
    --     , sum = leftLength + wordLength + rightLength
    --     }
  in
    case
      Dictionary.listGetElement leftLength leftPairs
    of
      Nothing ->
        -- try again, new random seed
        calcLeftRightLength leftPairs rightPairs wordLength seedC1

      Just [] ->
        -- try again, new random seed
        calcLeftRightLength leftPairs rightPairs wordLength seedC1

      Just leftWordPairs ->
        case
          Dictionary.listGetElement
            rightLength rightPairs
        of
          Nothing ->
            -- try again, new random seed
            calcLeftRightLength leftPairs rightPairs wordLength seedC1

          Just [] ->
            -- try again, new random seed
            calcLeftRightLength leftPairs rightPairs wordLength seedC1
                
          Just rightWordPairs ->
            { leftLength = leftLength
            , rightLength = rightLength
            , leftWordPairs = leftWordPairs
            , rightWordPairs = rightWordPairs
            , seedA1 = seedC1
            }
            -- |>Debug.log ""



parallaxFill : (String, Random.Seed) -> (String, Random.Seed)
parallaxFill (stringSoFar, seed0) =
  let
    remainingSize =
      parallax - (String.length stringSoFar)

    (chunkSize, seed1) =
      Random.step (Random.int 1 (remainingSize - spaceWidth)) seed0

  in
    if
    ( (chunkSize < remainingSize - spaceWidth)
    &&(chunkSize > remainingSize - 2*spaceWidth - breathingRoom)
    )
    then parallaxFill (stringSoFar, seed1) -- try again, new random seed
    else
      case Dictionary.getOneByLength (chunkSize, seed1) of
        Nothing ->
          parallaxFill (stringSoFar, seed1) -- try again, new random seed

        Just (word, seed2) ->
          let
            newStringSoFar =
              stringSoFar ++ word ++ " "

          in
            if String.length newStringSoFar >= parallax
            then (newStringSoFar, seed2)
            else parallaxFill (newStringSoFar, seed2)



puzzleRender
  : (Puzzle, Random.Seed)
  ->(List String, Random.Seed)
puzzleRender (Puzzle puzzle, seed0) =
  let
    (leftPairs, rightPairs) =
      -- Debug.log "leftAndRightPairs" 
      leftAndRightPairs

    -- x=Debug.log
    --     "first fail leftPairs"
    --     Dictionary.listFirstIndex
    --       (\list -> (List.length list) < 1)
    --       Dictionary.shortestWord
    --       leftPairs

  in
    puzzle
    |>List.foldr
        (\puzzleRow ( accumulator, seedA0 ) ->
          let
            ( outputRowGenerator, seedB1 ) =
              case puzzleRow of
                [] ->
                  let
                    (pattern, seedA1) =
                      parallaxFill ("", seedA0)

                    (offset, seedA2) =
                      Random.step (Random.int 0 (parallax-spaceWidth) ) seedA1
                    -- x = Debug.log "passive" { sum = String.length pattern }
                  in
                    -- we're only using one non-changing pattern here,
                    -- as there is no word to encode on this line.
                    ( { offset = offset
                      , leftPattern = pattern
                      , rightPattern = pattern
                      , changeOver = 0
                      }
                    , seedA2
                    )

                ( WordPlacement {word, left} ) :: _ ->
                  let
                    wordLength =
                      String.length word

                    { leftLength
                      , rightLength
                      , leftWordPairs
                      , rightWordPairs
                      , seedA1
                      } =
                      calcLeftRightLength leftPairs rightPairs wordLength seedA0

                    ((leftWord0, leftWord1), seedA2) =
                      Dictionary.listGetOne leftWordPairs seedA1
                      |>Tuple.mapFirst
                          (Maybe.withDefault ("logicError", "logicError"))

                    ((rightWord0, rightWord1), seedA3) =
                      Dictionary.listGetOne rightWordPairs seedA2
                      |>Tuple.mapFirst
                          (Maybe.withDefault ("logicError", "logicError"))

                    leftPattern =
                      ( leftWord0 ++ " "
                      ++word ++ " "
                      ++rightWord0 ++ " "
                      )

                    rightPattern =
                      ( leftWord1 ++ " "
                      ++word ++ " "
                      ++rightWord1 ++ " "
                      )

                    -- x = Debug.log "active"
                    --     { sumLeft = String.length leftPattern
                    --     , sumRight = String.length rightPattern
                    --     }

                  in
                    ( { offset = leftLength - ( modBy parallax left ) + parallax
                      , leftPattern = leftPattern
                      , rightPattern = rightPattern
                      , changeOver = left - leftLength
                      }
                    , seedA3
                    )
                    -- |>Debug.log ""


            outputRow : String
            outputRow =
              -- Debug.todo "String.repeat \"2\" 10"
              List.range 0 outputColumns
              |>List.map
                  (\columnNumber ->
                    let
                      pattern =
                        -- if columnNumber == outputRowGenerator.changeOver
                        -- then "###############"
                        -- else
                          if columnNumber < outputRowGenerator.changeOver
                          then outputRowGenerator.rightPattern
                          else outputRowGenerator.leftPattern
                        -- |>Debug.log "pattern"

                      position =
                        columnNumber + outputRowGenerator.offset
                        |>modBy parallax

                    in
                      pattern
                      |>String.dropLeft
                          position
                      |>String.uncons
                      |>Maybe.withDefault (' ', "")
                      |>Tuple.first

                  )
              |>String.fromList

          in
            ( outputRow :: accumulator, seedB1 )
        )
        ( [], seed0 )

-- MODEL

type Model =
  Model
  { navKey: Nav.Key
  , currentUrl: Url.Url
  , tab: TabType
  , ascii: List String
  , puzzle: Puzzle
  , randomSeed: Random.Seed
  -- , leftPairs : List PairList
  -- , rightPairs : List PairList
  }


initialPuzzle : Puzzle
initialPuzzle =
  Puzzle
  [ []
  , []
  , []
  , []
  , []
  , []
  , [ WordPlacement
      { word = "stop", left = 34}
    ]
  , [ WordPlacement
      { word = "wasting", left = 35}
    ]
  , [ WordPlacement
      { word = "your", left = 37}
    ]
  , [ WordPlacement
      { word = "time", left = 38}
    ]
  , []
  , [ WordPlacement
      { word = "get", left = 60}
    ]
  , [ WordPlacement
      { word = "a", left = 62}
    ]
  , [ WordPlacement
      { word = "life", left = 63}
    ]
  , [ WordPlacement
      { word = "life", left = 63}
    ]
  , [ WordPlacement
      { word = "life", left = 63}
    ]
  , [ WordPlacement
      { word = "life", left = 63}
    ]
  , [ WordPlacement
      { word = "life", left = 63}
    ]
  , [ WordPlacement
      { word = "life", left = 63}
    ]
  , [ WordPlacement
      { word = "life", left = 63}
    ]
  , [ WordPlacement
      { word = "life", left = 63}
    ]
  , [ WordPlacement
      { word = "life", left = 63}
    ]
  , [ WordPlacement
      { word = "life", left = 63}
    ]
  -- , []
  -- , []
  -- , []
  -- , []
  -- , []
  -- , []
  -- , []
  -- , []
  -- , []
  ]



-- INIT


type alias PairList =
  List (String, String)


type alias LeftAndRightPairs =
  ( List PairList, List PairList )


emptyPairOfPairList : LeftAndRightPairs
emptyPairOfPairList =
  ([],[])


pairOfListConcat : (List a, List a) -> (List a, List a) -> (List a, List a)
pairOfListConcat
  (headFirst, headSecond) -- heads of first and second lists
  (tailFirst, tailSecond) -- tails of first and second lists
  = ( headFirst ++ tailFirst -- concat head to tail of first list
    , headSecond ++ tailSecond -- concat head to tail of second list
    )


leftAndRightPairs3 :
  List String -> List String -> ( PairList, PairList )
leftAndRightPairs3 shortList longList =
  shortList
  |>List.foldl
      (\shortWord shortAccumulator -> 
        let
          shortLength =
            String.length shortWord

        in
          longList
          |>List.foldl
              (\longWord longAccumulator ->
                let
                  appendToChosenListInAListPairList newItem listPairList =
                    let
                      oldListPairList =
                        Dictionary.listGetElement shortLength listPairList
                        |>Maybe.withDefault []

                      --x = Debug.log "?" (oldListPairList, shortLength, listPairList)

                      newListPairList =
                        newItem :: oldListPairList

                      --x = Debug.log "?" (newListPairList , newItem, oldListPairList)
                    in
                      -- ( Debug.log
                      --   ( Debug.toString
                      --     { shortlength = shortLength
                      --     , newListPairList = newListPairList
                      --     , nullList = []
                      --     , listPairList = listPairList
                      --     }
                      --   )
                      -- )
                      listPairList
                      |>Dictionary.listUpdateElement
                          shortLength
                          newListPairList
                          []

                in                
                  if (String.dropRight 1 longWord) == shortWord
                  then
                    longAccumulator
                    |>Tuple.mapFirst
                      -- ( appendToChosenListInAListPairList
                      --     (shortWord, longWord)
                      -- )
                      ((::) (shortWord, longWord))
                  else
                    if(String.dropLeft 1 longWord) == shortWord
                    then
                      longAccumulator
                      |>Tuple.mapSecond
                        -- ( appendToChosenListInAListPairList
                        --     (shortWord, longWord)
                        -- )
                        ((::) (longWord, shortWord))
                    else
                      longAccumulator
              )
              shortAccumulator
      )
      ([],[])



leftAndRightPairs2
  : List ( List String ) -> Int -> LeftAndRightPairs -> LeftAndRightPairs
leftAndRightPairs2 wordLists length accumulator =
  case wordLists of
    [] ->
      accumulator

    [_] ->
      accumulator

    [_,_] ->
      accumulator

    shortList :: longList :: remainder ->
      let
        newAccumulator =
          pairOfListConcat
            accumulator
            ( leftAndRightPairs3 shortList longList 
            |>Tuple.mapBoth List.singleton List.singleton
            )

        -- x=Debug.log "?"
        --   ( Debug.toString
        --     { accumulator = accumulator
        --     , l3 =
        --       ( leftAndRightPairs3 shortList longList 
        --       |>Tuple.mapBoth List.singleton List.singleton
        --       )
        --     , newAccumulator = newAccumulator
        --     }
        --   )
      in
        -- pairOfListConcat
        --   newAccumulator
        --   ( leftAndRightPairs2
        --       ( longList :: remainder )
        --       ( length+1 )
        --       newAccumulator
        --   )
        leftAndRightPairs2
          ( longList :: remainder )
          ( length+1 )
          newAccumulator

leftAndRightPairs : LeftAndRightPairs
leftAndRightPairs =
  leftAndRightPairs2 Dictionary.dictionarySplitList 0 emptyPairOfPairList


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
  let
    -- x=Debug.log "init msg=" url

    seed0 =
      Random.initialSeed 0

    (ascii, seed1) =
      -- ( List.repeat outputRows (String.repeat outputColumns "2"), seed0 )
      puzzleRender (initialPuzzle, seed0)

  in
    tabChange
    ( Model
      { navKey = navKey
      , currentUrl = url
      , randomSeed = seed1
      , tab = TabEdit
      , puzzle = initialPuzzle
      , ascii = ascii
      -- , leftPairs = leftPairs
      -- , rightPairs = rightPairs
      }
    ) url



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | WordSet Int Int Int String
  | Noop


tabFragmentMap : Maybe String -> TabType
tabFragmentMap fragment =
  case fragment of
    Just "TabImage" ->
      TabImage

    Just "TabPlainText" ->
      TabPlainText

    -- Just "TabEdit" ->
    _ ->  -- including Nothing and Just "TabEdit"
      TabEdit


tabChange : Model -> Url.Url -> ( Model, Cmd Msg )
tabChange ((Model modelRecord) as model) url =
  let
    command =
      if modelRecord.currentUrl == url then
        Cmd.none
      else
        Nav.load (Url.toString url)
  in
    ( Model
      { modelRecord
      | tab = tabFragmentMap url.fragment
      , currentUrl = url
      }
    , command
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Model modelRecord) as model) =
  let
    x=Debug.log "update msg=" msg
  in
    case msg of
      LinkClicked urlRequest ->
        case urlRequest of
          Browser.Internal url ->
            ( model
            , Nav.pushUrl modelRecord.navKey (Url.toString url)
            )

          Browser.External url ->
            ( model, Nav.load url )

      UrlChanged url ->
        tabChange model url

      WordSet row colRank left word ->
        let
          wordPlacement =
            WordPlacement { word = word, left = left }

          puzzleOld =
            case modelRecord.puzzle of
              Puzzle puzzle ->
                puzzle

          rowOld =
            Dictionary.listGetElement row puzzleOld
            |>Maybe.withDefault []

          rowNew =
            Dictionary.listUpdateElement
              colRank
              wordPlacement
              ( WordPlacement { word = "", left = 0 } )
              rowOld

          puzzleNew =
            Puzzle ( Dictionary.listUpdateElement row rowNew [] puzzleOld )

          (ascii, seed0) =
            -- ( List.repeat outputRows (String.repeat outputColumns "2"), seed0 )
            puzzleRender (puzzleNew, modelRecord.randomSeed)

        in
          ( Model
            { modelRecord
            | puzzle = puzzleNew
            , ascii = ascii
            , randomSeed = seed0
            }
          , Cmd.none
          )
      
      Noop ->
        ( model, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


eachDirection : Int -> { top: Int, left: Int, bottom: Int, right: Int }
eachDirection dist =
  { top = dist, left = dist, bottom = dist, right = dist }


tabColor : Element.Color
tabColor =
  Element.rgb 0.9 0.8 0.7


tabColorActive : Element.Color
tabColorActive =
  Element.rgb 0.8 0.7 0.6


tabBorderColor : Element.Color
tabBorderColor =
  Element.rgb 0.95 0.85 0.75


shadeColor : Element.Color
shadeColor =
  Element.rgba 0.2 0.2 0.2 0.5


shadeSubstance : Element.Color
shadeSubstance =
  Element.rgba 0 0 0 0


editBoxBorderColor : Element.Color
editBoxBorderColor =
  Element.rgb 0.5 0.5 0.5


mainForegroundColor : Element.Color
mainForegroundColor =
  Element.rgb 0 0 0


mainBackgroundColor : Element.Color
mainBackgroundColor =
  Element.rgb 1 1 1


selectEnable : List ( Element.Attribute Msg)
selectEnable =
  [ Element.htmlAttribute <| Attr.style "-moz-user-select" "text"
  , Element.htmlAttribute <| Attr.style "-webkit-user-select" "text"
  , Element.htmlAttribute <| Attr.style "-ms-user-select" "text"
  , Element.htmlAttribute <| Attr.style "user-select" "text"
  ]


selectDisable : List ( Element.Attribute Msg)
selectDisable =
  [ Element.htmlAttribute <| Attr.style "-moz-user-select" "none"
  , Element.htmlAttribute <| Attr.style "-webkit-user-select" "none"
  , Element.htmlAttribute <| Attr.style "-ms-user-select" "none"
  , Element.htmlAttribute <| Attr.style "user-select" "none"
  ]


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
    ( [ Element.centerX
      , Element.width Element.fill
      ]
      ++selectDisable
    )
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

plainTextPane
  : Model
  ->Maybe ( List ( Element.Attribute Msg ) )
  ->List ( Element Msg )
plainTextPane ( ( Model modelRecord) as model) maybeInFront =
  [ Element.column
    ( [
      ]
    ++( case maybeInFront of
          Nothing ->
            []

          Just inFront ->
            inFront
      
      )
    )
    ( modelRecord.ascii
      |>List.map
        (\string -> Element.text <| "    " ++ string)
    )
  ]


testExtract : Model -> String
testExtract ( ( Model modelRecord) as model ) =
  case modelRecord.puzzle of
    Puzzle puzzle ->
      case Dictionary.listGetElement 7 puzzle of
        Nothing ->
          ";P"

        Just [] ->
          ";P"

        Just (WordPlacement wordPlacement :: _) ->
          wordPlacement.word


editPane : Model -> List ( Element Msg )
editPane ( (Model modelRecord) as model ) =
  plainTextPane
    model
    ( Just
      ( [ Element.inFront
          ( Element.el
              ( [ Background.color shadeSubstance
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.inFront
                  ( Element.el
                      [ Background.color shadeSubstance
                      , Element.width Element.fill
                      , Element.height Element.fill
                      ]
                      ( Input.text
                          selectEnable
                          { onChange = WordSet 7 0 35
                          , text = testExtract model
                          , placeholder = Nothing
                          , label = Input.labelLeft [ ] (Element.text "|||")
                          }

                      )
                  )
                ]
                ++selectEnable
              )
              ( Element.text <| testExtract model )
          )
        ]
        ++selectDisable
      )
    )

  -- case modelRecord.puzzle of
  --   Puzzle puzzle ->
  --     puzzle 
  --     |>List.map
  --         (\wordPlacements ->
  --             case wordPlacements of
  --               [] ->
  --                 Element.text "\n"

  --               WordPlacement wordPlacement :: _ ->
  --                 Element.el
  --                   [ Border.color editBoxBorderColor
  --                   , Border.width 2
  --                   ]
  --                   ( Element.text wordPlacement.word )

  --         )

body : Model -> List ( Html Msg )
body ( (Model modelRecord) as model) =
  [ Element.layout
    [ Element.width Element.fill
    , Element.padding 10
    , Background.color mainBackgroundColor
    , Font.color mainForegroundColor
    -- , Element.explain Debug.todo
    ]
    <|Element.column -- Center Column Content
      [ Element.centerX
      ]
      [ Element.row -- Tabbed Navigation
          [ Element.centerX
          , Element.width Element.fill
          ]
          -- [ tab model TabPlainText ("Plain Text" ++ (testDictionary 8)) "#TabPlainText"
          [ tab model TabEdit "Edit" "#TabEdit"
          , tabDivider
          , tab model TabPlainText "Plain Text" "#TabPlainText"
          , tabDivider
          , tab model TabImage "Downloadable Image" "#TabImage"
          ]
      , Element.column -- Primary content
          ( [ Border.width 2
            , Border.color mainForegroundColor
            , Background.color
              ( if modelRecord.tab == TabEdit
                then shadeColor
                else mainBackgroundColor
              )
            , Font.color mainForegroundColor
            , Font.family [ Font.typeface "Courier" ]
            -- , Element.spaceEvenly
            , Element.width
              ( Element.fill
              |>Element.minimum 1028
              |>Element.maximum 1028
              )
            , Element.height
              ( Element.fill
              |>Element.minimum 480
              |>Element.maximum 480
              )
            , Element.paddingXY 8 8
            ]
          )
          ( case modelRecord.tab of
              TabEdit ->
                editPane model
              
              TabPlainText ->
                plainTextPane model Nothing
              
              TabImage ->
                -- [ Element.image [] { src = "https://i.imgur.com/2a2cUTH.jpg", description = "h4wt13" } ]
                [ Element.text "Canvas is hard D:"
                ]
          )
      ]
  ]


view : Model -> Browser.Document Msg
view model =
  { title = "Title"
  , body = body model
  }


-- test/temporary scaffoldings


testDictionary : Int -> String
testDictionary q =
  let
    seed = Random.initialSeed q
  in
    Dictionary.getOne seed
    |>Tuple.first 


