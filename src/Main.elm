port module Main exposing (..)

import Html exposing (div, button, text, h2, input, ul, li, i, span, program, select, option)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Time
import Date
import Random
import String
import Array
import Words
import Task
import Debug

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Player
    = Alice
    | Bob
    | Observer


type Team
    = Green
    | Neutral
    | Black


type alias Card =
    { aliceLabel : Team
    , bobLabel : Team
    , word : String
    , aliceGuessed : Bool
    , bobGuessed : Bool
    }


type alias Model =
    { viewAs : Player
    , hour : Int
    , cards : List Card
    , isSpymaster : Bool
    , history : List Player
    }


totalCards =
    25


labelKey =
    let
        greenCount =
            9

        blackCount =
            3

        neutralCount =
            totalCards - greenCount - blackCount
    in
        List.concat
            [ List.repeat blackCount Black
            , List.repeat greenCount Green
            , List.repeat neutralCount Neutral
            ]


shuffle seed pile =
    let
        randomValsInRange ( bottomEnd, topEnd ) ( acc, seed ) =
            let
                ( nextval, nextseed ) =
                    Random.step (Random.int 0 topEnd) seed
            in
                ( nextval :: acc, nextseed )

        swap ( i, r ) aa =
            aa
                |> Array.set i (Array.get r aa |> Maybe.withDefault Nothing)
                |> Array.set r (Array.get i aa |> Maybe.withDefault Nothing)
    in
        List.repeat (List.length pile - 1) (List.length pile - 1)
            |> List.indexedMap (,)
            |> List.foldl randomValsInRange ( [], seed )
            |> Tuple.first
            |> List.map2 (,) (List.range 0 (List.length pile - 1))
            |> List.foldl swap (Array.fromList pile |> Array.map Just)
            |> Array.toList
            |> List.filterMap identity


dealCards sessionSeed seed hour =
    let
        aliceLabelKey =
            shuffle seed labelKey

        -- TODO: DRY
        aliceGreens =
            List.indexedMap (,) aliceLabelKey
                |> List.filter ((==) Green << Tuple.second)
                |> shuffle (gameSeed hour)
                |> List.map Tuple.first

        aliceBlacks =
            List.indexedMap (,) aliceLabelKey
                |> List.filter ((==) Black << Tuple.second)
                |> shuffle (gameSeed hour)
                |> List.map Tuple.first

        aliceNeutrals =
            List.indexedMap (,) aliceLabelKey
                |> List.filter ((==) Neutral << Tuple.second)
                |> shuffle (gameSeed hour)
                |> List.map Tuple.first

        greensForBob =
            (aliceGreens |> List.take 3)
                ++ (aliceBlacks |> List.take 1)
                ++ (aliceNeutrals |> List.take 5)

        blacksForBob =
            (aliceBlacks |> List.drop 1 |> List.take 1)
                ++ (aliceGreens |> List.drop 3 |> List.take 1)
                ++ (aliceNeutrals |> List.drop 5 |> List.take 1)

        neutralsForBob =
            (aliceGreens |> List.drop 4)
                ++ (aliceBlacks |> List.drop 2)
                ++ (aliceNeutrals |> List.drop 1)

        bobLabelKey =
            List.range 0 totalCards
                |> List.map (\x -> if List.member x greensForBob then
                    Green
                else if List.member x blacksForBob then
                    Black
                else
                    Neutral)

        picks =
            Words.all
                |> shuffle sessionSeed
                |> List.drop (totalCards * (hoursIntoSession hour))
    in
        List.map3 (\a b p -> { aliceLabel = a, bobLabel = b, word = p, aliceGuessed = False, bobGuessed = False }) aliceLabelKey bobLabelKey picks


init =
    ( Model Alice 0 [] False []
    , Cmd.batch
        [ Time.now |> Task.perform Time.inHours |> Cmd.map (\x -> NewHour <| floor x) ]
    )



-- UPDATE


type Msg
    = ViewAs Player
    | NewHour Int
    | AliceGuess Card
    | BobGuess Card
    | ToggleSpymaster


aliceGuess word w =
    if w == word then
        { word | aliceGuessed = not word.aliceGuessed }
    else
        w


bobGuess word w =
    if w == word then
        { word | bobGuessed = not word.bobGuessed }
    else
        w


hoursPerSession =
    12


hoursIntoSession hour =
    (hour % hoursPerSession)


sessionSeed hour =
    let
        seed =
            (hour // hoursPerSession) |> Random.initialSeed

        ( _, seed_ ) =
            Random.step Random.bool seed
    in
        seed_


gameSeed hour =
    let
        seed =
            hour |> Random.initialSeed

        ( _, seed_ ) =
            Random.step Random.bool seed
    in
        seed_


update msg model =
    case msg of
        ToggleSpymaster ->
            ( { model | isSpymaster = not model.isSpymaster }, Cmd.none )

        AliceGuess word ->
            ( { model | cards = List.map (aliceGuess word) model.cards, history = Alice :: model.history }, Cmd.none )

        BobGuess word ->
            ( { model | cards = List.map (bobGuess word) model.cards, history = Bob :: model.history  }, Cmd.none )

        ViewAs player ->
            ( { model | viewAs = player, isSpymaster = False }, Cmd.none )

        NewHour hour ->
            ( { model | hour = hour, cards = dealCards (sessionSeed hour) (gameSeed hour) hour }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []



-- VIEW


card : Player -> Bool -> Card -> Html.Html Msg
card viewAsPlayer isSpymaster word =
    let
        aliceLabel =
            " team-" ++ (toString word.aliceLabel) ++ " "

        bobLabel =
            " team-" ++ (toString word.bobLabel) ++ " "

        guessedGreen =
            (word.aliceLabel == Green && word.bobGuessed) || (word.bobLabel == Green && word.aliceGuessed)

        guessedBlack =
            (word.aliceLabel == Black && word.bobGuessed) || (word.bobLabel == Black && word.aliceGuessed)

        playerGuessed =
            (viewAsPlayer == Alice && word.aliceGuessed || viewAsPlayer == Bob && word.bobGuessed)

        showBackground =
            guessedGreen || guessedBlack || isSpymaster || playerGuessed

        showText =
            if not isSpymaster then
                not guessedGreen && not guessedBlack && not playerGuessed
            else if viewAsPlayer == Bob then
                not word.aliceGuessed && not guessedGreen && not guessedBlack
            else
                not word.bobGuessed && not guessedGreen && not guessedBlack

        team =
            if guessedGreen then
                "team-Green"
            else if guessedBlack then
                "team-Black"
            else if word.aliceGuessed && word.bobGuessed && word.aliceLabel == word.bobLabel then
                "team-" ++ (toString word.aliceLabel)
            else if viewAsPlayer == Bob && isSpymaster || viewAsPlayer == Alice && not isSpymaster then
                bobLabel
            else
                aliceLabel
    in
        div
            [ class <|
                "word-card "
                    ++ team
                    ++ (if showBackground then
                            " background "
                        else
                            ""
                       )
                    ++ (if showText then
                            " foreground "
                        else
                            ""
                       )
            , onClick
                (if isSpymaster then
                    if viewAsPlayer == Bob then
                        (AliceGuess word)
                    else
                        (BobGuess word)
                 else if viewAsPlayer == Bob then
                    (BobGuess word)
                 else
                    (AliceGuess word)
                )
            ]
            [ text word.word ]


asGameTime h =
    let
        t =
            ((toFloat h) * 60 * 60 * 1000) |> Date.fromTime

        hour =
            Date.hour t

        day =
            Date.dayOfWeek t

        ( fh, fampm ) =
            (if hour == 12 then
                ( 12, "pm" )
             else if (hour < 12) then
                ( hour, "am" )
             else
                ( hour - 12, "pm" )
            )
    in
        (toString day) ++ " " ++ (toString fh) ++ fampm


view : Model -> Html.Html Msg
view model =
    div [ class "main" ]
        [ span [ class "controls" ]
            [ span [ class "which-game" ] [ (text <| asGameTime model.hour) ]
            , span [ class "reveal-board" ]
                [ i
                    [ class "fa fa-eye"
                    , attribute "aria-label" "true"
                    , onClick ToggleSpymaster
                    ]
                    []
                , select
                    [ class "player-select"
                    , onInput <|
                        \s ->
                            if s == "Alice" then
                                ViewAs Alice
                            else
                                ViewAs Bob
                    ]
                    [ option [ value "Alice" ] [ text "Alice" ]
                    , option [ value "Bob" ] [ text "Bob" ]
                    ]
                , span [class "turns"] [text <| "Turns: " ++ toString (
                        List.foldl (\player (turns, last) -> (if player == last then (turns, last) else (turns+1, player))) (0, Observer) model.history |> Tuple.first
                    )]
                ]
            , i
                [ class "fa fa-eye fa-arrow-circle-o-left prev-game"
                , attribute "aria-label" "true"
                , onClick <| NewHour (model.hour - 1)
                ]
                []
            , i
                [ class "fa fa-arrow-circle-o-right next-game"
                , attribute "aria-label" "true"
                , onClick <| NewHour (model.hour + 1)
                ]
                []
            ]
        , div [ class "board" ]
            (List.map (card model.viewAs model.isSpymaster) model.cards)
        ]
