port module Main exposing (..)

import Html exposing (div, button, text, h2, input, ul, li)
import Html.App exposing (program)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Time
import Random
import String
import Array
import Words
import Task

initialSeed = Random.initialSeed 5

main : Program Never
main = program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type Team = Red | Blue | Neutral | Evil

type alias Card = {
  team: Team,
  word: Maybe String
}

type alias Model =
  {isSpymaster  : Bool
  , seed: Int
  , elapsed : Int
  , alertText : String
  , otherText : String
  , logs : List String
  , words: List Card
  , alts: List Int
  }

n = (List.length Words.all)

valToCard idx = {
        word = Words.all
            |> List.drop idx
            |> List.head,
        team = Neutral}

deadCard = {word=Nothing, team=Neutral}

totalCards = 25
teamList =
    let count = totalCards // 3
    in List.concat [
            [Evil],
            List.repeat count Red,
            List.repeat (count+1) Blue,
            List.repeat (totalCards-2*count-2) Neutral]

shuffle seed pile =
    let randomValsInRange topEnd (acc, seed) =
            let (nextval, nextseed) = Random.step (Random.int 0 topEnd) seed in
            (nextval :: acc, nextseed)
        doSwaps (i, r) aa =
            aa
            |> Array.set i (Array.get r aa |> Maybe.withDefault deadCard)
            |> Array.set r (Array.get i aa |> Maybe.withDefault deadCard)

    in [1..(List.length pile)]
    |> List.reverse
    |> List.foldl randomValsInRange ([], seed)
    |> fst
    |> List.map2 (\i r -> (i, r)) [0..(List.length pile)]
    |> List.foldl doSwaps (Array.fromList pile)
    |> Array.toList

randomSelectionGenerator = Random.int 0 n
 |> Random.map valToCard
 |> Random.list totalCards

assignTeam t c = {c | team=t}

allCards seed =
    shuffle seed
    <| List.map2 assignTeam teamList
    <| fst
    <| Random.step randomSelectionGenerator seed

init =
  (Model False 0 9 "It works!" "so" [] [] [], Cmd.batch [
    Time.now |> Task.perform (always 0) Time.inHours |> Cmd.map (\x -> NewSeed <| floor x),
    Random.generate Alternate (Random.list 25 (Random.int 0 (List.length Words.all))),
    Random.generate Alternate (Random.list 25 (Random.int 0 100)) ])


-- UPDATE


type Msg = ShowLabels | HideLabels | Tick | Alert | ChangeAlertText String | Log String | NewSeed Int | Selection (List Card) | Alternate (List Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Selection l -> ({model | words=l}, Cmd.none)
    Alternate l -> ({model | alts=l}, Cmd.none)
    ShowLabels ->
      ({ model | isSpymaster=True }, Cmd.none)

    HideLabels ->
      ({ model | isSpymaster=False }, Cmd.none)

    Tick ->
      ({ model | elapsed = model.elapsed + 1}, Cmd.none)

    Alert ->
      (model, Cmd.none)

    ChangeAlertText text ->
      ({ model | alertText = text }, Cmd.none)

    NewSeed seed -> ({model | seed=seed, words = allCards <| Random.initialSeed seed}, Cmd.none)

    Log text ->
      ({ model | logs = text :: model.logs }, Cmd.none)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [ ]


-- VIEW

card showColor word =
  if showColor == True
    then
        div [class  <| "word-card team-" ++ (toString word.team)] [
        (text (Maybe.withDefault "" word.word))
        ]
    else
        div [class  <| "word-card"] [
        (text (Maybe.withDefault "" word.word))
        ]


view : Model -> Html.Html Msg
view model =
  div [class "board"]
      (List.concat [
            (List.map (card model.isSpymaster) model.words), [
                if model.isSpymaster
                then
                    button [onClick (HideLabels)] [(text "Hide") ]
                else
                    button [onClick (ShowLabels)] [(text "Reveal") ],
                button [onClick (NewSeed (model.seed+1))] [(text "Next Game") ]
            ]
        ])
