{- Run this from the main WordGameSandbox directory, so it can import the Wrd1515444549915349 file. -}


module Main exposing (..)

import Delay exposing (..)

import GraphicSVG exposing(..)
import GraphicSVG.EllieApp exposing (..)
import Random


{-numDoors = 7-}
leftBound = -80
rightBound = 80
{-increment = (rightBound - leftBound) / (numDoors)-}
{-squareSize = (rightBound - leftBound) / (numDoors) - 1-}
tempDisplayTime = 1

type alias Model =
    { time : Float
        , winningDoor : Int
        , numberDoors: Int
        , chosenDoor : Int
        , autoPlayWillChoose : Int
        , autoPlayWillKeep : Bool
        , doorToNotReveal : Int --door to not reveal, if we've already chosen the winning door
        , doorStates : List (Int, DoorState)
        , keepWins : Int
        , keepLosses : Int
        , switchWins : Int
        , switchLosses : Int
        , resultMessage : String
        , tempDisplay : String
        , tempDisplayStart : Float
        , simulationState: SimulationState
        , isAutoPlay: Bool
    }

generateWinningDoor numDoors = Random.generate RandomDoor <|
  Random.pair
    (Random.int 0 (numDoors-1)) --first door is the winning door
    (Random.int 1 (numDoors-1)) --picks which door will be revealed

generateChoice numDoors = Random.generate RandomChoice <| (Random.int 0 (numDoors-1))

main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init = \ _ -> (init, generateWinningDoor 7)
        , update = update
        , view = \ model -> { title = "Monty Hall Simulator", body = view model }
        , subscriptions = \_ -> Sub.none
        }

view model = collage 192 128 (myShapes model)

myShapes model =
    [group (List.map (\(idx, doorState) -> (square ((rightBound - leftBound) / (model.numberDoors) - 1) |> filled (doorColour doorState) |> notifyTap (if doorState == WinningClosed || doorState == LosingClosed then ClickOnDoor idx else InvalidAction) |> move(leftBound + toFloat idx * ((rightBound - leftBound) / (model.numberDoors)) + ((rightBound - leftBound) / (model.numberDoors) - 1) / 2, 0))) model.doorStates)]
        ++
    [
        text "Monty Hall Simulator" |> centered |> filled black |> move (0,50)
        --,text (String.fromInt <| model.winningDoor) |> filled black |> move (80,50) -- THIS DISPLAYS THE WINNING DOOR INDEX, UNCOMMENT TO DEBUG
        ,getInstructions model.simulationState model |> centered |> filled orange |> move (0,40)
        ,maybeGetTempDisplay model.tempDisplay model.tempDisplayStart model.time
        ,maybeGetButtons model.simulationState |> move (0,30)
        ,text ("Keep- Wins: " ++ (String.fromInt <| model.keepWins) ++ " || Losses: " ++ (String.fromInt <| model.keepLosses)) |> centered |> filled yellow |> move(0,-30)
        ,text ("Switch- Wins: " ++ (String.fromInt <| model.switchWins) ++ " || Losses: " ++ (String.fromInt <| model.switchLosses)) |> centered |> filled yellow |> move(0,-40)
        ,getAutoPlayButton model.isAutoPlay |> move(0,-50)
        , group [
            rectangle 33 10 |> filled orange
            , text "Run With 3" |> centered |> size 3 |> filled white
        ] |> notifyTap (ChangeDoors) |> move(-70,-50)
    ]

getAutoPlayButton isAutoPlay =
    group[ group [
        rectangle 33 10 |> filled orange
        , text "Simulate once (keep)" |> centered |> size 3 |> filled white
    ] |> notifyTap (ToggleAutoplay True) |> move(-20,0)
    ,
    group [
        rectangle 33 10 |> filled orange
        , text "Simulate once (switch)" |> centered |> size 3 |> filled white
    ] |> notifyTap (ToggleAutoplay False) |> move(20,0)
    ]


maybeGetTempDisplay tempDisplay tempDisplayStart time =
    if time - tempDisplayStart < tempDisplayTime then
        text tempDisplay |> centered |> filled orange
    else
        text "" |> centered |> filled orange

maybeGetButtons currentSimulationState =
    if currentSimulationState == AwaitingKeepOrSwitch then
        getKeepOrSwitchButtons
    else if currentSimulationState == DisplayResult then
        getTryAgainButton
    else
        text "" |> centered |> filled yellow

getKeepOrSwitchButtons =
    group [
        group [
            rectangle 20 10 |> filled orange |> move (-25, 0)
            , text "Keep" |> centered |> size 3 |> filled white |> move (-25, 0)
        ] |> notifyTap (Keep)
        , group [
            rectangle 20 10 |> filled orange |> move (5, 0)
            , text "Switch" |> centered |> size 3 |> filled white |> move (5, 0)
        ] |> notifyTap (Switch)
    ]

getTryAgainButton =
    group [
        rectangle 20 10 |> filled orange
        , text "Play again" |> centered |> size 3 |> filled white
    ] |> notifyTap (TryAgain)

-- represent each door state as a colour
doorColour doorState =
  case doorState of
    WinningClosed -> blue
    LosingClosed -> blue
    WinningOpen -> green
    LosingOpen -> grey
    Chosen -> yellow
    LosingRevealed -> red

-- fetch instructions according to current simulation state
getInstructions simulationState model =
  case simulationState of
    AwaitingInitialSelection -> text "Please choose a door."
    AwaitingKeepOrSwitch -> text "Do you want to keep or switch?"
    DisplayResult -> text model.resultMessage

-- to know when a click happens, we need to define new messages
type Msg = Tick Float GetKeyState
         | RandomDoor (Int, Int)
         | ClickOnDoor Int
         | Keep
         | Switch
         | InvalidAction
         | TryAgain
         | ChangeDoors
         | ToggleAutoplay Bool
         | RandomChoice Int

type SimulationState = AwaitingInitialSelection | AwaitingKeepOrSwitch | DisplayResult

type DoorState = WinningClosed | LosingClosed | WinningOpen | LosingOpen | Chosen | LosingRevealed

initListWithWinner numDoors winningIdx = List.map (\idx -> (idx, if idx == winningIdx then WinningClosed else LosingClosed)) (List.range 0 (numDoors-1))

getNextSimulationState currentSimulationState =
    case currentSimulationState of
        AwaitingInitialSelection -> AwaitingKeepOrSwitch
        AwaitingKeepOrSwitch -> DisplayResult
        DisplayResult -> DisplayResult

revealDoor numDoors idx currentState winningDoor doorToNotReveal chosenDoor =
    if winningDoor == chosenDoor then
        if idx == (modBy numDoors (chosenDoor+doorToNotReveal)) then
            LosingClosed
        else
            LosingOpen
    else
        if winningDoor == idx then
            WinningClosed
        else
            LosingOpen

chooseDoor idx model =
    if model.simulationState == AwaitingInitialSelection then
            List.map (\(i, currentDoorState) -> (i, if i == idx then Chosen else revealDoor model.numberDoors i currentDoorState model.winningDoor model.doorToNotReveal idx)) model.doorStates
    else
        model.doorStates

revealLastDoor doorStates =
    List.map (\(i, currentDoorState) -> (i,
        if currentDoorState == LosingClosed then
            LosingRevealed
        else if currentDoorState == WinningClosed then
            WinningOpen
        else
            currentDoorState
    )) doorStates

hasWon model hasKept =
    if hasKept then
        if model.chosenDoor == model.winningDoor then
            True
        else
            False
    else
        if model.chosenDoor /= model.winningDoor then
            True
        else
            False

getWinOrLoseMessage: Model -> Bool -> String
getWinOrLoseMessage model won =
    if won then
        "You've won!"
    else
        "You've lost!"

autoUpdate model =
    if model.isAutoPlay then
        if model.simulationState == AwaitingInitialSelection then
            update (ClickOnDoor model.autoPlayWillChoose) model
        else if model.simulationState == AwaitingKeepOrSwitch then
            if model.autoPlayWillKeep then
                update Keep model
            else
                update Switch model
        else
            update TryAgain model
    else
       update InvalidAction model


update msg model = case msg of
                    Tick t _ -> ({ model | time = t}, Cmd.none)
                    RandomDoor (rd1, rd2) -> ({ model | winningDoor = rd1, simulationState = AwaitingInitialSelection, doorToNotReveal = rd2, doorStates = initListWithWinner model.numberDoors rd1}, Cmd.none)
                    ClickOnDoor idx -> { model | chosenDoor = idx
                            , simulationState = if model.simulationState == AwaitingInitialSelection then AwaitingKeepOrSwitch else model.simulationState
                            , doorStates = chooseDoor idx model
                        } |> autoUpdate
                    InvalidAction -> ({ model | tempDisplay = "", tempDisplayStart = model.time}, Cmd.none) --Does nothing
                    Keep -> { model |
                        simulationState = if model.simulationState == AwaitingKeepOrSwitch then DisplayResult else model.simulationState
                        , resultMessage = getWinOrLoseMessage model (hasWon model True)
                        , keepWins = if hasWon model True then model.keepWins + 1 else model.keepWins
                        , keepLosses = if not (hasWon model True) then model.keepLosses + 1 else model.keepLosses
                        , doorStates = revealLastDoor model.doorStates
                        }|> autoUpdate
                    Switch -> { model |
                        simulationState = if model.simulationState == AwaitingKeepOrSwitch then DisplayResult else model.simulationState
                        , resultMessage = getWinOrLoseMessage model (hasWon model False)
                        , switchWins = if hasWon model False then model.switchWins + 1 else model.switchWins
                        , switchLosses = if not (hasWon model False) then model.switchLosses + 1 else model.switchLosses
                        , doorStates = revealLastDoor model.doorStates
                        } |> autoUpdate
                    TryAgain -> ({model | isAutoPlay = False}, generateWinningDoor model.numberDoors)
                    ChangeDoors -> ({model | numberDoors = ceiling 3}, Cmd.none)
                    ToggleAutoplay willKeep -> ({model | isAutoPlay = True, autoPlayWillKeep = willKeep}, generateChoice model.numberDoors)
                    RandomChoice randomChoice -> {model | autoPlayWillChoose = randomChoice} |> autoUpdate

init = { time = 0
        , winningDoor = 0
        , numberDoors = 0
        , chosenDoor = 0
        , doorToNotReveal = 0
        , autoPlayWillChoose = 0
        , autoPlayWillKeep = True
        , doorStates = List.map (\idx -> (idx, LosingClosed)) (List.range 0 7)
        , keepWins = 0
        , keepLosses = 0
        , switchWins = 0
        , switchLosses = 0
        , tempDisplay = ""
        , resultMessage = ""
        , tempDisplayStart = 0
        , simulationState = AwaitingInitialSelection
        , isAutoPlay = False
        }

