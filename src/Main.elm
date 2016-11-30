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
    , guessed: Bool
    }


type alias Model =
    { isSpymaster : Bool
    , hour : Int
    , cards : List Card
    , firstTeam: Team
    }


deadCard =
    { word = Nothing, team = Neutral }


totalCards =
    25

teamList hour =
    let
        count = totalCards // 3
        blueDelta = if (randomTeam hour) == Blue then 1 else 0
        redDelta = 1 - blueDelta
    in
        List.concat
            [ [ Evil ]
            , List.repeat (count + redDelta) Red
            , List.repeat (count + blueDelta) Blue
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


dealCards sessionSeed gameSeed hour =
    let teams = shuffle gameSeed (teamList gameSeed)
        picks = Words.all
            |> shuffle sessionSeed
            |> List.drop (totalCards * (hoursIntoSession hour))

    in
        List.map2 (\t p -> {team=t, word=p, guessed=False}) teams picks


init =
    ( Model False 0 [] Blue
    , Cmd.batch
        [ Time.now |> Task.perform (always 0) Time.inHours |> Cmd.map (\x -> NewHour <| floor x) ]
    )



-- UPDATE


type Msg
    = ToggleLabels
    | HideLabels
    | NewHour Int
    | Guess Card

guess word w =
    if w == word then {word | guessed= not word.guessed}
    else w


teamGenerator  = Random.map (\b -> if b then Blue else Red) Random.bool

randomTeam seed=
    let
        (team, seed') = Random.step teamGenerator seed
    in
        team


hoursPerSession = 12
hoursIntoSession hour = (hour % hoursPerSession)

sessionSeed hour =
    let seed = (hour // hoursPerSession) |> Random.initialSeed
        (_, seed') = Random.step Random.bool seed
    in seed'

gameSeed hour =
    let seed = hour |> Random.initialSeed
        (_, seed') = Random.step Random.bool seed
    in seed'

update msg model =
    case msg of
        ToggleLabels ->
            ( { model | isSpymaster = not model.isSpymaster }, Cmd.none )
        Guess word ->
            ({ model | cards = List.map (guess word) model.cards}, Cmd.none)
        HideLabels ->
            ( { model | isSpymaster = False }, Cmd.none )

        NewHour hour ->
            ( { model | hour = hour, cards = dealCards (sessionSeed hour) (gameSeed hour) hour, firstTeam = randomTeam (gameSeed hour)  }, Cmd.none )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []



-- VIEW


card isSpymaster word =
    let team = "team-" ++ (toString word.team)
        showBackground = isSpymaster || word.guessed
        showText = not word.guessed
    in
        div [ class <| "word-card " ++ team 
                                    ++ (if showBackground then " background " else "")
                                    ++ (if showText then " foreground " else "" ),
              onClick (Guess word)
            ]
            [ (text word.word) ]

asGameTime h =
    let t = ( (toFloat h) * 60 * 60 * 1000) |> Date.fromTime
        h = Date.hour t
        d = Date.dayOfWeek t
        (fh, fampm) = (
        if h == 12 then
            (12, "pm")
        else if (h < 12) then
            (h, "am")
        else (h-12, "pm")) in
    (toString d) ++ " " ++ (toString fh) ++ fampm

teamFrame model = if model.isSpymaster then "first-" ++ (toString model.firstTeam) else ""

view : Model -> Html.Html Msg
view model =
    div [class "main"] [
        div [class <| "frame " ++ (teamFrame model)][],
        span [class "controls"] [
            span [class "which-game"] [(text <| asGameTime model.hour)],
            i [ class "fa fa-eye reveal-board",
                attribute "aria-label" "true",
                onClick ToggleLabels] [],
            i [ class "fa fa-eye fa-arrow-circle-o-left prev-game",
                attribute "aria-label" "true",
                onClick <| NewHour (model.hour - 1)] [],
            i [ class "fa fa-arrow-circle-o-right next-game",
                attribute "aria-label" "true",
                onClick <| NewHour (model.hour + 1)] []
        ],
        div [ class "board" ]
             (List.map (card model.isSpymaster) model.cards)
    ]
