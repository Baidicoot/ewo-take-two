-- declares the gamestate state
module Game.Engine.State where
import qualified Data.Map.Strict as Map
import Control.Arrow
import Misc (firstElem, consMaybe, cons, (****))

import Game.Engine.Entity
import Game.Engine.Terrain
import Game.Engine.Common

entityLookSq :: Int
entityLookSq = 9

data State = State {
    regions :: Map.Map RegionIndex Region,
    entities :: [Entity],
    events :: [Event],
    spawners :: Map.Map Int (SpawnData -> Entity)
}

generateSight :: State -> Coord -> EntitySight
generateSight state coord = EntitySight (filter ((<=) entityLookSq . coordSqDist coord . loc) (entities state))

generateSights :: State -> [(Entity, EntitySight)]
generateSights state =
    let mapFn = generateSight state . loc in
            map (\e -> (e, mapFn e)) (entities state)

cull :: [Entity] -> [Entity]
cull = filter (not . dead)

mapUpdates :: DeltaTime -> [Event] -> [(Entity, EntitySight)] -> ([Entity], [Event])
mapUpdates dt evs = (\(a, b) -> (a, concat b)) . unzip . map (\(e, es) -> update e es dt evs)

sortInputs :: [Input] -> ([Event], [(RegionIndex, Region)], [Entity])
sortInputs = foldr (\x (ev, r, en) -> case x of
    Keypress sender char -> (KeyEvent sender char:ev, r, en)
    SpawnCommand t dat -> (SpawnEvent t dat:ev, r, en)
    Loaded index region entities -> (ev, (index, region):r, en ++ entities)) ([], [], [])

spawnEntities :: Map.Map Int (SpawnData -> Entity) -> [Event] -> [Entity]
spawnEntities table = foldr (\x e -> case x of
    SpawnEvent t dat -> case Map.lookup t table of
        Just fn -> (fn dat):e
        _ -> e
    _ -> e) []

tick :: DeltaTime -> [Input] -> State -> State
tick dt i state = state { regions = updatedRegions, entities = allEntities, events = newEvents }
    where
        (inputEvents, newRegions, loadedEntities) = sortInputs i
        (updatedEntities, entityEvents) = mapUpdates dt (events state) (generateSights state)
        newEvents = entityEvents ++ inputEvents
        updatedRegions = Map.union (Map.fromList newRegions) (regions state)
        newEntities = spawnEntities (spawners state) newEvents
        allEntities = updatedEntities ++ newEntities ++ loadedEntities

getPlayer :: SenderID -> State -> Maybe Entity
getPlayer name state = firstElem ((==) (Just name) . playerName) (entities state)