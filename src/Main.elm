port module Main exposing (..)

import Html exposing (div, button, text, h2, input, ul, li, i, span)
import Html.App exposing (program)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Time
import Date
import Random
import String
import Array
import Words
import Task


main : Program Never
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Team
    = Red
    | Blue
    | Neutral
    | Evil


type alias Card =
    { team : Team
    , word : String
    , revealed: Bool
    }


type alias Model =
    { isSpymaster : Bool
    , seed : Int
    , cards : List Card
    }


deadCard =
    { word = Nothing, team = Neutral }


totalCards =
    25

teamList =
    let
        count = totalCards // 3
    in
        List.concat
            [ [ Evil ]
            , List.repeat count Red
            , List.repeat (count + 1) Blue
            , List.repeat (totalCards - 2 * count - 2) Neutral
            ]

shuffle seed pile =
    let
        randomValsInRange topEnd ( acc, seed ) =
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
        [0..(List.length pile - 1)]
            |> List.reverse
            |> List.indexedMap (+)
            |> List.foldl randomValsInRange ( [], seed )
            |> fst
            |> List.map2 (,) [0..(List.length pile - 1)]
            |> List.foldl swap (Array.fromList pile |> Array.map Just)
            |> Array.toList
            |> List.filterMap identity


dealCards seed =
    let teams = shuffle seed teamList
        picks = shuffle seed <| Words.all
    in
        List.map2 (\t p -> {team=t, word=p, revealed=False}) teams picks


init =
    ( Model False 0 []
    , Cmd.batch
        [ Time.now |> Task.perform (always 0) Time.inHours |> Cmd.map (\x -> NewSeed <| floor x) ]
    )



-- UPDATE


type Msg
    = ToggleLabels
    | HideLabels
    | NewSeed Int
    | Reveal Card

reveal word w =
    if w == word then {word | revealed=True}
    else w

update msg model =
    case msg of
        ToggleLabels ->
            ( { model | isSpymaster = not model.isSpymaster }, Cmd.none )
        Reveal word ->
            ({ model | cards = List.map (reveal word) model.cards}, Cmd.none)

        HideLabels ->
            ( { model | isSpymaster = False }, Cmd.none )

        NewSeed seed ->
            ( { model | seed = seed, cards = dealCards <| Random.initialSeed seed }, Cmd.none )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []



-- VIEW


card showColor word =
    if showColor == True || word.revealed then
        div [ class <| "word-card team-" ++ (toString word.team) ]
            [ (text word.word)
            ]
    else
        div [ class <| "word-card",
              onClick (Reveal word)
            ]
            [ (text word.word)
            ]

asGameTime h =
    ( (toFloat h) * 60 * 60 * 1000)
    |> Date.fromTime
    |> \d -> (Date.dayOfWeek d, (
        if Date.hour d > 12
        then ((Date.hour d) - 12, "pm")
        else ((Date.hour d), "am") ))
    |> (\(day, (hour, ampm)) -> (toString day) ++ " " ++ (toString hour) ++ ampm)

view : Model -> Html.Html Msg
view model =
    div [class "main"] [
        span [class "controls"] [
            span [class "which-game"] [(text <| asGameTime model.seed)],
            i [ class "fa fa-eye reveal-board",
                attribute "aria-label" "true",
                onClick ToggleLabels] [],
            i [ class "fa fa-eye fa-arrow-circle-o-left prev-game",
                attribute "aria-label" "true",
                onClick <| NewSeed (model.seed - 1)] [],
            i [ class "fa fa-arrow-circle-o-right next-game",
                attribute "aria-label" "true",
                onClick <| NewSeed (model.seed + 1)] []
        ],
        div [ class "board" ]
             (List.map (card model.isSpymaster) model.cards)
    ]
