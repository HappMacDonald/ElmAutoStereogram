module ElmAutoStereogram exposing (main)

{-| Web app that creates text-based MagicEye Autostereograms.

Bugs:

  - OK, grabber is working 99% well
    \*\* Main problem just now is it "flashes" to the left margin
    \*\* This happens when code gets position relative to input instead of background
    \*\* having a hard time definitively telling which thing I'm dragging over. :P

Todo:

  - HiDPI testing from Tolana, and no Kukkutarma

Joachim said:

> For now, you can work around it, which is great, but involves some setup work.
> You can pass the devicePixelRatio as a Flag to the program,
> and then set the style width and height to the size you want and the attributes
> width and height to those dimensions multiplied by the pixel ratio.
> Something like
>
>     [ width (w * pixelRatio)
>     , height (h * pixelRatio)
>     , style "width" (String.fromInt w ++ "px")
>     , style "height" (String.fromInt h ++ "px")
>     ]
>
> As the attributes for the element.

So I should test doing that.

Here's my original design-doc:

Now I just need to sit down and use some Elm to make the user interface
way better.

Tabs to switch between output modes would be really helpful:

  - viewing text — current method
  - markdown code — text you can copy and paste into Reddit submissions
    or comments that will render as desired
  - and viewing canvas — would allow folk to right click and save-as-png
    or copy-image to avoid needing to know how to use a snipping tool

Using movable text grabbies to position the text instead of making folk
enter a column number, and rendering in real time upon changes
would also be pretty hip. :B

Now if I use movable text grabbies, I wonder how challenging
it would really be to feature multiple words per line? Hmm...

-}

-- import Html.Events exposing (onInput, onBlur)
-- import Html.Events.Extra exposing (onEnter)
-- import Dom exposing (focus)
-- import Random

import Browser
import Browser.Navigation as Nav
import Canvas
import CanvasColor
import Debug
import Dictionary
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attr
import Html5.DragDrop
import Json.Decode as Decode
import Random
import Task
import Url



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


defaultParallax : Int
defaultParallax =
    15


minParallax : Puzzle -> Int
minParallax puzzle =
    puzzleWordsMaxLength puzzle
        + 6
        |> Debug.log "minParallax"


maxParallax : Int
maxParallax =
    23



-- I'unno man, it seems to hang with any longer parallax
--, and I'm too lazy to figure out why.


outputRows : Int
outputRows =
    23


outputColumns : Int
outputColumns =
    79


{-| canvasOverSample forces Canvas image to render that many times larger
than the logical pixels in the parent object. This is a lazy way to deal with
monitors that have "logical" pixels bigger than their physical pixels, that
would otherwise cause all canvas contents to get icky and blurry and smudged.
-}
canvasOverSample : Float
canvasOverSample =
    10


contentMargin : Int
contentMargin =
    8


fontSize : Int
fontSize =
    20


fontWidth : Float
fontWidth =
    0.6 * toFloat fontSize


inputHeight : Int
inputHeight =
    fontSize // 2


tabFontSize : Int
tabFontSize =
    fontSize * 4 // 3



-- bump up roughly 133%


contentWidth : Int
contentWidth =
    52 * fontSize



-- arbitrarily tuned


contentHeight : Int
contentHeight =
    24 * fontSize



-- arbitrarily tuned


{-| The width in glyphs of a space character.
Yeah obviously this is "one", I just hate magic numbers. :P
-}
spaceWidth : Int
spaceWidth =
    1


markdownCodeIndent : Int
markdownCodeIndent =
    4


editPaneRightOffset : Int
editPaneRightOffset =
    43


editPaneDownOffset : Int
editPaneDownOffset =
    5


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
maxMessageLength : Int -> Int
maxMessageLength parallax =
    parallax - 6


type WordPlacement
    = WordPlacement
        { word : String -- what the word is
        , left : Int -- how far from left edge of output it should begin
        }


type Puzzle
    = Puzzle (List (List WordPlacement))


type Validity
    = Valid
    | Invalid


calcLeftRightLength :
    Int
    -> List PairList
    -> List PairList
    -> Int
    -> Random.Seed
    ->
        { leftLength : Int
        , rightLength : Int
        , leftWordPairs : PairList
        , rightWordPairs : PairList
        , seedA1 : Random.Seed
        }
calcLeftRightLength parallax leftPairs rightPairs wordLength seedC0 =
    let
        ( leftLength, seedC1 ) =
            Random.step
                (Random.int
                    Dictionary.shortestWord
                    (maxMessageLength parallax
                        - wordLength
                        + Dictionary.shortestWord
                    )
                )
                seedC0

        rightLength =
            maxMessageLength parallax - wordLength - leftLength + 2

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
            calcLeftRightLength parallax leftPairs rightPairs wordLength seedC1

        Just [] ->
            -- try again, new random seed
            calcLeftRightLength parallax leftPairs rightPairs wordLength seedC1

        Just leftWordPairs ->
            case
                Dictionary.listGetElement
                    rightLength
                    rightPairs
            of
                Nothing ->
                    -- try again, new random seed
                    calcLeftRightLength parallax leftPairs rightPairs wordLength seedC1

                Just [] ->
                    -- try again, new random seed
                    calcLeftRightLength parallax leftPairs rightPairs wordLength seedC1

                Just rightWordPairs ->
                    { leftLength = leftLength
                    , rightLength = rightLength
                    , leftWordPairs = leftWordPairs
                    , rightWordPairs = rightWordPairs
                    , seedA1 = seedC1
                    }



-- |>Debug.log ""


parallaxFill : Int -> ( String, Random.Seed ) -> ( String, Random.Seed )
parallaxFill parallax ( stringSoFar, seed0 ) =
    let
        remainingSize =
            parallax - String.length stringSoFar

        ( chunkSize, seed1 ) =
            Random.step (Random.int 1 (remainingSize - spaceWidth)) seed0
    in
    if
        (chunkSize < remainingSize - spaceWidth)
            && (chunkSize > remainingSize - 2 * spaceWidth - breathingRoom)
    then
        parallaxFill parallax ( stringSoFar, seed1 )
        -- try again, new random seed

    else
        case Dictionary.getOneByLength ( chunkSize, seed1 ) of
            Nothing ->
                parallaxFill parallax ( stringSoFar, seed1 )

            -- try again, new random seed
            Just ( word, seed2 ) ->
                let
                    newStringSoFar =
                        stringSoFar ++ word ++ " "
                in
                if String.length newStringSoFar >= parallax then
                    ( newStringSoFar, seed2 )

                else
                    parallaxFill parallax ( newStringSoFar, seed2 )


puzzleRender :
    Int
    -> ( Puzzle, Random.Seed )
    -> ( List String, Random.Seed )
puzzleRender parallax ( Puzzle puzzle, seed0 ) =
    let
        _ =
            Debug.log "parallax" parallax

        ( leftPairs, rightPairs ) =
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
        |> List.foldr
            (\puzzleRow ( accumulator, seedA0 ) ->
                let
                    wordPresentMaybe : Maybe WordPlacement
                    wordPresentMaybe =
                        case puzzleRow of
                            [] ->
                                Nothing

                            ((WordPlacement { word }) as wordPlacement) :: _ ->
                                if word == "" then
                                    Nothing

                                else
                                    Just wordPlacement

                    ( outputRowGenerator, seedB1 ) =
                        case wordPresentMaybe of
                            Nothing ->
                                let
                                    ( pattern, seedA1 ) =
                                        parallaxFill parallax ( "", seedA0 )

                                    ( offset, seedA2 ) =
                                        Random.step
                                            (Random.int
                                                0
                                                (parallax - spaceWidth)
                                            )
                                            seedA1

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

                            Just (WordPlacement { word, left }) ->
                                let
                                    wordLength =
                                        String.length word

                                    { leftLength, rightLength, leftWordPairs, rightWordPairs, seedA1 } =
                                        calcLeftRightLength
                                            parallax
                                            leftPairs
                                            rightPairs
                                            wordLength
                                            seedA0

                                    ( ( leftWord0, leftWord1 ), seedA2 ) =
                                        Dictionary.listGetOne
                                            leftWordPairs
                                            seedA1
                                            |> Tuple.mapFirst
                                                (Dictionary.maybeLazyDefault
                                                    (\() ->
                                                        ( -- Debug.log
                                                          -- ( Debug.toString ( "A", leftWordPairs, seedA1 ) )
                                                          "logicError"
                                                        , "logicError"
                                                        )
                                                    )
                                                )

                                    ( ( rightWord0, rightWord1 ), seedA3 ) =
                                        Dictionary.listGetOne
                                            rightWordPairs
                                            seedA2
                                            -- |>Debug.log "Nothing?"
                                            |> Tuple.mapFirst
                                                (Dictionary.maybeLazyDefault
                                                    (\() ->
                                                        ( -- Debug.log
                                                          -- ( Debug.toString ( "B", rightWordPairs, seedA2 ) )
                                                          "logicError"
                                                        , "logicError"
                                                        )
                                                    )
                                                )

                                    leftPattern =
                                        leftWord0
                                            ++ " "
                                            ++ word
                                            ++ " "
                                            ++ rightWord0
                                            ++ " "

                                    rightPattern =
                                        leftWord1
                                            ++ " "
                                            ++ word
                                            ++ " "
                                            ++ rightWord1
                                            ++ " "

                                    -- x = Debug.log "active"
                                    --     { sumLeft = String.length leftPattern
                                    --     , sumRight = String.length rightPattern
                                    --     }
                                in
                                ( { offset =
                                        leftLength
                                            - modBy parallax left
                                            + parallax
                                            + spaceWidth
                                  , leftPattern = leftPattern
                                  , rightPattern = rightPattern
                                  , changeOver = left - leftLength
                                  }
                                , seedA3
                                )

                    -- |>Debug.log ""
                    outputRow : String
                    outputRow =
                        List.range 0 outputColumns
                            |> List.map
                                (\columnNumber ->
                                    let
                                        pattern =
                                            if columnNumber < outputRowGenerator.changeOver then
                                                outputRowGenerator.rightPattern

                                            else
                                                outputRowGenerator.leftPattern

                                        -- |>Debug.log "pattern"
                                        position =
                                            columnNumber
                                                + outputRowGenerator.offset
                                                |> modBy parallax
                                    in
                                    pattern
                                        |> String.dropLeft
                                            position
                                        |> String.uncons
                                        |> Maybe.withDefault ( ' ', "" )
                                        |> Tuple.first
                                )
                            |> String.fromList
                in
                ( outputRow :: accumulator, seedB1 )
            )
            ( [], seed0 )



-- MODEL


type Model
    = Model
        { navKey : Nav.Key
        , currentUrl : Url.Url
        , tab : TabType
        , ascii : List String
        , puzzle : Puzzle
        , parallax : Int
        , randomSeed : Random.Seed
        , dragDropHandle : Html5.DragDrop.Model DragId DropId

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
                { word = "stop", left = 34 }
          ]
        , [ WordPlacement
                { word = "wasting", left = 35 }
          ]
        , [ WordPlacement
                { word = "your", left = 37 }
          ]
        , [ WordPlacement
                { word = "time", left = 38 }
          ]
        , []
        , [ WordPlacement
                { word = "get", left = 60 }
          ]
        , [ WordPlacement
                { word = "a", left = 62 }
          ]
        , [ WordPlacement
                { word = "life", left = 63 }
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
    List ( String, String )


type alias LeftAndRightPairs =
    ( List PairList, List PairList )


emptyPairOfPairList : LeftAndRightPairs
emptyPairOfPairList =
    ( [], [] )


emptyWordPlacement : WordPlacement
emptyWordPlacement =
    WordPlacement { word = "", left = 0 }


pairOfListConcat : ( List a, List a ) -> ( List a, List a ) -> ( List a, List a )
pairOfListConcat ( headFirst, headSecond )
    -- heads of first and second lists
    ( tailFirst, tailSecond )
    =
    -- tails of first and second lists
    ( headFirst ++ tailFirst
      -- concat head to tail of first list
    , headSecond ++ tailSecond
      -- concat head to tail of second list
    )


leftAndRightPairs3 : List String -> List String -> ( PairList, PairList )
leftAndRightPairs3 shortList longList =
    shortList
        |> List.foldl
            (\shortWord shortAccumulator ->
                let
                    shortLength =
                        String.length shortWord
                in
                longList
                    |> List.foldl
                        (\longWord longAccumulator ->
                            let
                                appendToChosenListInAListPairList newItem listPairList =
                                    let
                                        oldListPairList =
                                            Dictionary.listGetElement
                                                shortLength
                                                listPairList
                                                |> Maybe.withDefault []

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
                                        |> Dictionary.listUpdateElement
                                            shortLength
                                            newListPairList
                                            []
                            in
                            if String.dropRight 1 longWord == shortWord then
                                longAccumulator
                                    |> Tuple.mapFirst
                                        -- ( appendToChosenListInAListPairList
                                        --     (shortWord, longWord)
                                        -- )
                                        ((::) ( shortWord, longWord ))

                            else if String.dropLeft 1 longWord == shortWord then
                                longAccumulator
                                    |> Tuple.mapSecond
                                        -- ( appendToChosenListInAListPairList
                                        --     (shortWord, longWord)
                                        -- )
                                        ((::) ( longWord, shortWord ))

                            else
                                longAccumulator
                        )
                        shortAccumulator
            )
            ( [], [] )


leftAndRightPairs2 : List (List String) -> Int -> LeftAndRightPairs -> LeftAndRightPairs
leftAndRightPairs2 wordLists length accumulator =
    case wordLists of
        [] ->
            accumulator

        [ _ ] ->
            accumulator

        [ _, _ ] ->
            accumulator

        shortList :: longList :: remainder ->
            let
                newAccumulator =
                    pairOfListConcat
                        accumulator
                        (leftAndRightPairs3 shortList longList
                            |> Tuple.mapBoth List.singleton List.singleton
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
                (longList :: remainder)
                (length + 1)
                newAccumulator


leftAndRightPairs : LeftAndRightPairs
leftAndRightPairs =
    leftAndRightPairs2 Dictionary.dictionarySplitList
        0
        emptyPairOfPairList


extractWordPlacement : Model -> Maybe Int -> Int -> Maybe WordPlacement
extractWordPlacement ((Model modelRecord) as model) maybeRow colRank =
    let
        (Puzzle puzzle) =
            modelRecord.puzzle
    in
    case maybeRow of
        Nothing ->
            Nothing

        Just row ->
            case Dictionary.listGetElement row puzzle of
                Nothing ->
                    Nothing

                Just [] ->
                    Nothing

                Just (wordPlacement :: _) ->
                    Just wordPlacement


puzzleWordsMaxLength : Puzzle -> Int
puzzleWordsMaxLength (Puzzle puzzle) =
    List.foldr
        (\row accumulator1 ->
            List.foldr
                (\(WordPlacement { word }) accumulator2 ->
                    max (String.length word) accumulator2
                )
                accumulator1
                row
        )
        0
        puzzle


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        -- x=Debug.log "init msg=" url
        seed0 =
            Random.initialSeed 0

        ( ascii, seed1 ) =
            -- ( List.repeat outputRows (String.repeat outputColumns "2"), seed0 )
            puzzleRender defaultParallax ( initialPuzzle, seed0 )
    in
    tabChange
        (Model
            { navKey = navKey
            , currentUrl = url
            , randomSeed = seed1
            , tab = TabEdit
            , puzzle = initialPuzzle
            , parallax = defaultParallax
            , ascii = ascii
            , dragDropHandle = Html5.DragDrop.init
            }
        )
        url



-- UPDATE


type alias DragId =
    Int


type alias DropId =
    Int


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | WordSet Int Int String -- row, colRank, word
    | LeftSet Int Int Int -- row, colRank, left
    | DragDropMsg (Html5.DragDrop.Msg DragId DropId)
    | ParallaxChange Float
    | Noop


tabFragmentMap : Maybe String -> TabType
tabFragmentMap fragment =
    case fragment of
        Just "TabImage" ->
            TabImage

        Just "TabPlainText" ->
            TabPlainText

        -- Just "TabEdit" ->
        _ ->
            -- including Nothing and Just "TabEdit"
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


wordLeftClamp : Int -> String -> Int -> Int
wordLeftClamp parallax word left =
    Dictionary.intClampMinMax
        parallax
        (outputColumns - String.length word + 1)
        left


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Model modelRecord) as model) =
    let
        parallax =
            modelRecord.parallax

        -- x=Debug.log "update msg=" msg
        replaceWordPlacement : Int -> Int -> Maybe Int -> Maybe String -> Puzzle
        replaceWordPlacement row colRank leftMaybe wordMaybe =
            let
                puzzleOld =
                    case modelRecord.puzzle of
                        Puzzle puzzle ->
                            puzzle

                rowOld =
                    Dictionary.listGetElement row puzzleOld
                        |> Maybe.withDefault []

                wordPlacementOld =
                    Dictionary.listGetElement colRank rowOld
                        |> Maybe.withDefault emptyWordPlacement

                ( wordOld, leftOld ) =
                    case wordPlacementOld of
                        WordPlacement wordPlacementOldRecord ->
                            ( wordPlacementOldRecord.word
                            , wordPlacementOldRecord.left
                            )

                wordNew =
                    case wordMaybe of
                        Nothing ->
                            wordOld

                        Just word ->
                            if String.length word <= maxMessageLength parallax then
                                word

                            else
                                wordOld

                leftNew =
                    case leftMaybe of
                        Nothing ->
                            wordLeftClamp parallax wordNew leftOld

                        Just left ->
                            wordLeftClamp parallax wordNew left

                wordPlacementNew =
                    WordPlacement { word = wordNew, left = leftNew }

                rowNew =
                    Dictionary.listUpdateElement
                        colRank
                        wordPlacementNew
                        emptyWordPlacement
                        rowOld
            in
            Puzzle (Dictionary.listUpdateElement row rowNew [] puzzleOld)
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

        WordSet row colRank word ->
            let
                puzzleNew =
                    replaceWordPlacement row colRank Nothing (Just word)

                ( ascii, seed0 ) =
                    -- ( List.repeat outputRows (String.repeat outputColumns "2"), seed0 )
                    puzzleRender parallax ( puzzleNew, modelRecord.randomSeed )
            in
            ( Model
                { modelRecord
                    | puzzle = puzzleNew
                    , ascii = ascii
                    , randomSeed = seed0
                }
            , Cmd.none
            )

        LeftSet row colRank left ->
            let
                puzzleNew =
                    replaceWordPlacement row colRank (Just left) Nothing

                ( ascii, seed0 ) =
                    -- ( List.repeat outputRows (String.repeat outputColumns "2"), seed0 )
                    puzzleRender parallax ( puzzleNew, modelRecord.randomSeed )
            in
            ( Model
                { modelRecord
                    | puzzle = puzzleNew
                    , ascii = ascii
                    , randomSeed = seed0
                }
            , Cmd.none
            )

        DragDropMsg dragDropMsg ->
            let
                ( dragDropHandle, resultMaybe1 ) =
                    Html5.DragDrop.update dragDropMsg modelRecord.dragDropHandle

                -- |>Debug.log "dragdrop"
                dragIdMaybe =
                    Html5.DragDrop.getDragId dragDropHandle

                dropIdMaybe =
                    Html5.DragDrop.getDropId dragDropHandle

                positionMaybe =
                    case ( dragIdMaybe, dropIdMaybe ) of
                        ( Just dragId, Just dropId ) ->
                            if dropId == -1 then
                                Html5.DragDrop.getDroppablePosition dragDropHandle

                            else
                                Nothing

                        _ ->
                            Nothing

                -- x = Debug.log "" (dropIdMaybe, positionMaybe, resultMaybe1)
                wordPlacementMaybe =
                    extractWordPlacement model dragIdMaybe testColRank
            in
            case ( dragIdMaybe, wordPlacementMaybe ) of
                ( Just dragId, Just (WordPlacement { word, left }) ) ->
                    let
                        leftNew1 =
                            case positionMaybe of
                                Nothing ->
                                    left

                                Just position ->
                                    round
                                        (toFloat position.x / fontWidth - 7)
                                        |> wordLeftClamp parallax word

                        leftNew2 =
                            if abs (leftNew1 - left) > parallax then
                                left

                            else
                                leftNew1

                        -- x =
                        --   if leftNew<1
                        --   then Debug.log "zero?" "]"--(dragDropHandle, resultMaybe1)
                        --   else ""--(dragDropHandle, resultMaybe1)
                        moved =
                            leftNew2 /= left

                        puzzleNew =
                            if moved then
                                replaceWordPlacement
                                    dragId
                                    testColRank
                                    (Just leftNew2)
                                    Nothing

                            else
                                modelRecord.puzzle

                        ( asciiNew, seed0 ) =
                            if moved then
                                puzzleRender parallax ( puzzleNew, modelRecord.randomSeed )

                            else
                                ( modelRecord.ascii, modelRecord.randomSeed )
                    in
                    ( Model
                        { modelRecord
                            | dragDropHandle = dragDropHandle
                            , puzzle = puzzleNew
                            , ascii = asciiNew
                            , randomSeed = seed0
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ParallaxChange parallaxNewFloat ->
            let
                ( ascii, seed0 ) =
                    -- ( List.repeat outputRows (String.repeat outputColumns "2"), seed0 )
                    puzzleRender parallax ( modelRecord.puzzle, modelRecord.randomSeed )

                parallaxNew =
                    round parallaxNewFloat
            in
            ( Model
                { modelRecord
                    | parallax =
                        Dictionary.intClampMinMax
                            (minParallax modelRecord.puzzle)
                            maxParallax
                            parallaxNew
                    , ascii =
                        if parallax /= parallaxNew then
                            ascii

                        else
                            modelRecord.ascii
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


eachDirection : Int -> { top : Int, left : Int, bottom : Int, right : Int }
eachDirection dist =
    { top = dist, left = dist, bottom = dist, right = dist }


eachDirection0 : { top : Int, left : Int, bottom : Int, right : Int }
eachDirection0 =
    eachDirection 0


type alias RGB =
    { red : Float, green : Float, blue : Float }


type alias RGBA =
    { red : Float, green : Float, blue : Float, opacity : Float }


canvasRGB : RGB -> CanvasColor.Color
canvasRGB rgb =
    CanvasColor.rgb
        (round (rgb.red * 255))
        (round (rgb.green * 255))
        (round (rgb.blue * 255))


canvasRGBA : RGBA -> CanvasColor.Color
canvasRGBA rgba =
    CanvasColor.rgba
        (round (rgba.red * 255))
        (round (rgba.green * 255))
        (round (rgba.blue * 255))
        rgba.opacity


uiRGB : RGB -> Element.Color
uiRGB rgb =
    Element.rgb rgb.red rgb.green rgb.blue


uiRGBA : RGBA -> Element.Color
uiRGBA rgba =
    Element.rgba rgba.red rgba.green rgba.blue rgba.opacity


tabColor : RGB
tabColor =
    RGB 0.9 0.8 0.7


tabColorActive : RGB
tabColorActive =
    RGB 0.8 0.7 0.6


tabBorderColor : RGB
tabBorderColor =
    RGB 0.95 0.85 0.75


shadeColor : RGBA
shadeColor =
    RGBA 0.2 0.2 0.2 0.5


shadeSubstance : RGBA
shadeSubstance =
    RGBA 0 0 0 0


editBoxBorderColor : RGB
editBoxBorderColor =
    RGB 0.5 0.5 0.5


mainForegroundColor : RGB
mainForegroundColor =
    RGB 0 0 0


mainBackgroundColor : RGB
mainBackgroundColor =
    RGB 1 1 1


selectEnable : List (Element.Attribute Msg)
selectEnable =
    [ Element.htmlAttribute <| Attr.style "-moz-user-select" "text"
    , Element.htmlAttribute <| Attr.style "-webkit-user-select" "text"
    , Element.htmlAttribute <| Attr.style "-ms-user-select" "text"
    , Element.htmlAttribute <| Attr.style "user-select" "text"
    ]


selectDisable : List (Element.Attribute Msg)
selectDisable =
    [ Element.htmlAttribute <| Attr.style "-moz-user-select" "none"
    , Element.htmlAttribute <| Attr.style "-webkit-user-select" "none"
    , Element.htmlAttribute <| Attr.style "-ms-user-select" "none"
    , Element.htmlAttribute <| Attr.style "user-select" "none"
    ]


tabDivider : Element Msg
tabDivider =
    Element.el
        [ Element.width <| Element.maximum 5 Element.fill
        ]
        (Element.text "")


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
        ([ Element.centerX
         , Element.width Element.fill
         , Font.size tabFontSize
         ]
            ++ selectDisable
        )
        { url = link
        , label = Element.text title
        }
        |> Element.el
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
            , Border.color (uiRGB tabBorderColor)
            , Background.color (uiRGB backgroundColor)
            , Element.fillPortion 1
                |> Element.width
            , Element.padding 5
            , Font.center
            ]


plainTextPane :
    Model
    -> Maybe (List (Element.Attribute Msg))
    -> List (Element Msg)
plainTextPane ((Model modelRecord) as model) maybeInFront =
    [ Element.column
        ([ Element.paddingXY contentMargin contentMargin
         ]
            ++ (case maybeInFront of
                    Nothing ->
                        []

                    Just inFront ->
                        inFront
               )
        )
        (modelRecord.ascii
            |> List.map
                (\string ->
                    Element.text <|
                        String.repeat markdownCodeIndent " "
                            ++ string
                )
        )
    ]


imagePane : Model -> List (Element Msg)
imagePane ((Model modelRecord) as model) =
    let
        leftMargin =
            4 * fontWidth

        canvasWidth =
            (toFloat contentWidth
                - leftMargin
                -- * canvasOverSample
                - toFloat contentMargin
                - 3.8
            )
                |> round

        canvasHeight =
            toFloat contentHeight
                -- * canvasOverSample
                - 5
                |> round

        rowpx : Int -> Float
        rowpx row =
            toFloat (fontSize * row)
                + 5.1

        renderRows : List String -> Int -> Canvas.Commands -> Canvas.Commands
        renderRows asciiList index cmds =
            case asciiList of
                [] ->
                    cmds

                head :: remainingList ->
                    cmds
                        |> Canvas.fillText
                            head
                            7.9
                            (rowpx index)
                            Nothing
                        |> renderRows remainingList (index + 1)
    in
    [ Element.el
        [ Element.paddingEach
            { eachDirection0
                | left = round leftMargin
                , top = 1
            }
        ]
        (Canvas.element
            canvasWidth
            canvasHeight
            []
            (Canvas.empty
                -- |>Canvas.scale (1/canvasOverSample) (1/canvasOverSample)
                |> Canvas.fillStyle (canvasRGB mainBackgroundColor)
                |> Canvas.fillRect
                    0
                    0
                    (toFloat canvasWidth)
                    (toFloat canvasHeight)
                |> Canvas.fillStyle (canvasRGB mainForegroundColor)
                |> Canvas.font (String.fromInt fontSize ++ "px Courier")
                |> Canvas.textBaseline Canvas.Top
                |> Canvas.save
                |> renderRows modelRecord.ascii 0
                -- |>Canvas.fillText "Hello world" 0 ( rowpx  0 ) Nothing
                |> Canvas.restore
            )
            |> Element.html
        )
    ]


editPane : Model -> List (Element Msg)
editPane ((Model modelRecord) as model) =
    let
        parallax =
            modelRecord.parallax

        (Puzzle puzzle) =
            modelRecord.puzzle

        input : Int -> Element Msg
        input row =
            let
                wordPlacement =
                    extractWordPlacement model (Just row) testColRank
                        |> Maybe.withDefault emptyWordPlacement
            in
            case ( row >= List.length puzzle, wordPlacement ) of
                ( True, _ ) ->
                    Element.none

                -- return no data for this level, just forward next.
                ( False, WordPlacement { word, left } ) ->
                    let
                        leftClean =
                            wordLeftClamp parallax word left

                        offsetX =
                            toFloat editPaneRightOffset
                                + toFloat leftClean
                                * fontWidth
                                -- |>Debug.log "moveRight"
                                |> Element.moveRight

                        offsetY =
                            editPaneDownOffset
                                + (row * fontSize)
                                |> toFloat
                                |> Element.moveDown
                    in
                    Input.text
                        ([ Element.spacing 0
                         , Element.maximum inputHeight Element.fill
                            |> Element.height
                         , Element.maximum
                            (toFloat (2 + maxMessageLength parallax)
                                * fontWidth
                                + 10
                                |> round
                            )
                            Element.fill
                            |> Element.width

                         --  , Element.alpha 0.7
                         , offsetX
                         , offsetY
                         , row
                            + 1
                            |> input
                            |> Element.inFront
                         ]
                            ++ selectEnable
                            ++ (Html5.DragDrop.draggable
                                    DragDropMsg
                                    row
                                    |> List.map Element.htmlAttribute
                               )
                            ++ (Html5.DragDrop.droppable
                                    DragDropMsg
                                    row
                                    |> List.map Element.htmlAttribute
                               )
                        )
                        { onChange = WordSet row testColRank
                        , text = word
                        , placeholder = Nothing

                        -- , label = Input.labelLeft [] gripperElement
                        , label = Input.labelLeft [] Element.none
                        }

        shade : List (Element.Attribute Msg)
        shade =
            [ Element.inFront
                (Element.el
                    ([ Background.color (uiRGBA shadeSubstance)
                     , Element.width Element.fill
                     , Element.height Element.fill

                     -- , Element.paddingEach
                     --   { eachDirection0
                     --   | top = 5
                     --   , left = 7
                     --   }
                     ]
                        ++ (List.map
                                Element.htmlAttribute
                            <|
                                Html5.DragDrop.droppable DragDropMsg -1
                           )
                    )
                 <|
                    input 0
                )
            ]
                ++ selectDisable
    in
    plainTextPane
        model
        (Just shade)


body : Model -> List (Html Msg)
body ((Model modelRecord) as model) =
    [ Element.layout
        [ Element.width Element.fill
        , Element.padding 10
        , Background.color (uiRGB mainBackgroundColor)
        , Font.color (uiRGB mainForegroundColor)

        -- , Element.explain Debug.todo
        ]
      <|
        Element.column
            -- Center Column Content
            [ Element.centerX
            ]
            [ Element.row
                -- Tabbed Navigation
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
            , Element.column
                -- Primary content
                [ Border.width 2
                , Border.color (uiRGB mainForegroundColor)
                , Background.color
                    (if modelRecord.tab == TabEdit then
                        uiRGBA shadeColor

                     else
                        uiRGB mainBackgroundColor
                    )
                , Font.color (uiRGB mainForegroundColor)
                , Font.family [ Font.typeface "Courier" ]
                , Font.size fontSize

                -- , Element.spaceEvenly
                , Element.width
                    (Element.fill
                        |> Element.minimum contentWidth
                        |> Element.maximum contentWidth
                    )
                , Element.height
                    (Element.fill
                        |> Element.minimum contentHeight
                        |> Element.maximum contentHeight
                    )
                ]
                ((case modelRecord.tab of
                    TabEdit ->
                        editPane model

                    TabPlainText ->
                        plainTextPane model Nothing

                    TabImage ->
                        imagePane model
                 )
                    ++ [ Input.slider
                            [ Element.paddingXY 0 5
                            ]
                            { onChange = ParallaxChange
                            , label =
                                Element.text
                                    ("parallax: "
                                        ++ String.fromInt modelRecord.parallax
                                    )
                                    |> Input.labelAbove
                                        []
                            , min = toFloat (minParallax modelRecord.puzzle)
                            , max = toFloat maxParallax
                            , value = toFloat modelRecord.parallax
                            , thumb = Input.defaultThumb
                            , step = Just 1
                            }
                       ]
                )
            ]
    ]


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Ascii Autostereogram Creator"
    , body = body model
    }



-- test/temporary scaffoldings


testRow : Int
testRow =
    7


testColRank =
    0
