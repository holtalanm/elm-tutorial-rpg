module Update exposing (..)

import Msgs exposing (Msg(..))
import Models exposing (Model, Player)
import Routing exposing (parseLocation)
import Commands exposing (savePlayerCmd)
import RemoteData

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.OnFetchPlayers response ->
            ({ model | players = response }, Cmd.none)
        Msgs.OnLocationChange location ->
            let newRoute = parseLocation location
            in ({ model | route = newRoute }, Cmd.none)
        Msgs.ChangeLevel player howMuch ->
            let updatedPlayer = {player | level = player.level + howMuch}
            in (model, savePlayerCmd updatedPlayer)
        Msgs.OnPlayerSave (Ok player) ->
            (updatePlayer model player, Cmd.none)
        Msgs.OnPlayerSave (Err error) ->
            (model, Cmd.none)

pickPlayer : Player -> (Player -> Player) 
pickPlayer updatedPlayer =
    \currentPlayer ->
        if updatedPlayer.id == currentPlayer.id then
            updatedPlayer
        else currentPlayer

updatePlayerList : Player -> (List Player -> List Player)
updatePlayerList updatedPlayer =
    \players -> List.map (pickPlayer updatedPlayer) players

updatePlayer : Model -> Player -> Model
updatePlayer model updatedPlayer =
    let updatedPlayers = RemoteData.map (updatePlayerList updatedPlayer) model.players
    in {model | players = updatedPlayers}