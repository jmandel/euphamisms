port module Main exposing (..)

import Html exposing (div, button, text, h2, input, ul, li, i, span, program)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Time
import Date
import Random
import String
import Array
import Words
import Task


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
    | Evil


type alias Card =
    { aliceLabel: Team
    , bobLabel: Team
    , word : String
    , aliceGuessed: Bool
    , bobGuessed: Bool
    }


type alias Model =
    { viewAs : Player
    , hour : Int
    , cards : List Card
    }


deadCard =
    { word = Nothing, team = Neutral }


totalCards =
    25

labelKey =
    let
        count = totalCards // 3
        greenCount = count
        evilCount = 3
        neutralCount = totalCards - greenCount - evilCount

    in
        List.concat
            [ List.repeat evilCount Evil
            , List.repeat greenCount Green
            , List.repeat neutralCount Neutral
            ]

shuffle seed pile =
    let
        randomValsInRange (bottomEnd, topEnd) ( acc, seed ) =
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
        List.repeat(List.length pile - 1) (List.length pile - 1)
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
        bobLabelKey =
            shuffle (gameSeed <| hour*2+1) labelKey
        picks =
            Words.all
                |> shuffle sessionSeed
                |> List.drop (totalCards * (hoursIntoSession hour))
    in
        List.map3 (\a b p -> { aliceLabel = a, bobLabel = b, word = p, aliceGuessed = False, bobGuessed = False }) aliceLabelKey bobLabelKey picks

init =
    ( Model Observer 0 []
    , Cmd.batch
        [ Time.now |> Task.perform  Time.inHours |> Cmd.map (\x -> NewHour <| floor x) ]
    )



-- UPDATE


type Msg
    = ViewAs Player
    | NewHour Int
    | AliceGuess Card
    | BobGuess Card


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

        AliceGuess word ->
            ( { model | cards = List.map (aliceGuess word) model.cards }, Cmd.none )

        BobGuess word ->
            ( { model | cards = List.map (bobGuess word) model.cards }, Cmd.none )

        ViewAs player ->
            ( { model | viewAs = player }, Cmd.none )

        NewHour hour ->
            ( { model | hour = hour, cards = dealCards (sessionSeed hour) (gameSeed hour) hour }, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []



-- VIEW


card : Player -> Card ->  Html.Html Msg
card viewAsPlayer word =
    let
        aliceLabel =
            " team-" ++ (toString word.aliceLabel) ++ " "
        bobLabel =
            " team-" ++ (toString word.bobLabel) ++ " "
        guessedGreen = (word.aliceLabel == Green && word.bobGuessed) || (word.bobLabel == Green && word.aliceGuessed)
        guessedBlack = (word.aliceLabel == Evil && word.bobGuessed) || (word.bobLabel == Evil && word.aliceGuessed)
        showBackground =
            guessedGreen || guessedBlack || (viewAsPlayer /= Observer) || (word.aliceGuessed && word.bobGuessed)
        showText =
            if viewAsPlayer == Bob then
                not word.aliceGuessed && not guessedGreen && not guessedBlack
            else if viewAsPlayer == Alice then
                not word.bobGuessed && not guessedGreen && not guessedBlack
            else
                not (word.aliceGuessed && word.bobGuessed) && not (guessedGreen) && not (guessedBlack)
        team =
            if guessedGreen then
                "team-Green"
            else if guessedBlack then
                "team-Evil"
            else if word.aliceGuessed && word.bobGuessed && word.aliceLabel == word.bobLabel then
                "team-" ++ (toString word.aliceLabel)
            else
                if viewAsPlayer == Bob then
                    bobLabel
                else if viewAsPlayer == Alice then
                    aliceLabel
                else "team-"
    in
        div
            [ class <|
                "word-card " ++ team 
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

            ]
            [ (text word.word) 
            , div [ class <| "chip alice " ++ (
                    if word.bobGuessed then
                        ("guessed-" ++ (toString word.aliceLabel))
                    else ""
                    )
                  , onClick (BobGuess word)
            ] []
            , div [ class <| "chip bob " ++ (
                    if word.aliceGuessed then
                        "guessed-" ++ (toString word.bobLabel)
                    else ""
                    )
                  , onClick (AliceGuess word)
            ] []
            ]


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
            , i
                [ class "fa fa-eye reveal-board"
                , attribute "aria-label" "true"
                ]
                [ span [class (if model.viewAs == Alice then "viewas-active" else ""), onClick <| ViewAs Alice] [text "Alice"]
                , span [class (if model.viewAs == Observer then "viewas-active" else ""), onClick <| ViewAs Observer] [text "Observer"]
                , span [class (if model.viewAs == Bob then "viewas-active" else ""), onClick <| ViewAs Bob] [text "Bob"]]
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
            (List.map (card model.viewAs) model.cards)
        ]
