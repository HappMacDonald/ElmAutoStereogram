module ElmAutoStereogram exposing (main)

{-| Web app that creates text-based MagicEye Autostereograms.

Bugs:
* Sometimes the pattern it tries to pull from is too long (16 characters)
  for an as yet not understood reason. Probably a magic number is off
  somewhereplace? ;P

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

      Just rightWordPairs ->
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
                
          Just leftWordPairs ->
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



puzzleRender :
  List PairList
  -> List PairList
  -> (Puzzle, Random.Seed)
  -> (List String, Random.Seed)
puzzleRender leftPairs rightPairs (Puzzle puzzle, seed0) =
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
                  |>Debug.log ""


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
  , leftPairs : List PairList
  , rightPairs : List PairList
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
  , []
  , []
  , []
  , []
  , []
  , []
  , []
  , []
  , []
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
    (leftPairs, rightPairs) =
      -- Debug.log "leftAndRightPairs" 
      leftAndRightPairs

    -- x=Debug.log
    --     "first fail leftPairs"
    --     Dictionary.listFirstIndex
    --       (\list -> (List.length list) < 1)
    --       Dictionary.shortestWord
    --       leftPairs

    seed0 =
      Random.initialSeed 0

    (ascii, seed1) =
      -- ( List.repeat outputRows (String.repeat outputColumns "2"), seed0 )
      puzzleRender leftPairs rightPairs (initialPuzzle, seed0)

  in
    tabChange
    ( Model
      { navKey = navKey
      , currentUrl = url
      , randomSeed = seed1
      , tab = TabPlaintext
      , puzzle = initialPuzzle
      , ascii = ascii
      , leftPairs = leftPairs
      , rightPairs = rightPairs
      }
    ) url



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


tabFragmentMap : Maybe String -> TabType
tabFragmentMap fragment =
  case fragment of
    Just "TabMarkdown" ->
      TabMarkdown

    Just "TabImage" ->
      TabImage

    _ -> -- including Nothing and Just TabPlaintext
      TabPlaintext


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
        -- [ tab model TabPlaintext ("Plain Text" ++ (testDictionary 8)) "#TabPlaintext"
        [ tab model TabPlaintext "Plain Text" "#TabPlaintext"
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


-- test/temporary scaffoldings


testDictionary : Int -> String
testDictionary q =
  let
    seed = Random.initialSeed q
  in
    Dictionary.getOne seed
    |>Tuple.first 


