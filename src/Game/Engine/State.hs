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

step :: Direction -> Coord -> Map.Map RegionIndex Region -> Either Request (Char, Coord)
step dir start dat = case internal of
    Nothing -> Left (Load ri)
    Just c -> Right c
    where
        ri = regionIndex start
        internal = do
            region <- Map.lookup ri dat
            (ch, pd) <- indexTile start region
            return (ch, nextCoord dir start pd)

forwards :: Direction -> Coord -> Int -> Map.Map RegionIndex Region -> ([Char], Maybe Request)
forwards _ _ 0 _ = ([], Nothing)
forwards dir start len dat = case step dir start dat of
    Left req -> (replicate len ' ', Just req)
    Right (ch, start') -> (first (cons ch)) (forwards dir start' (len - 1) dat)

diagonal :: Quadrat -> Coord -> Int -> Map.Map RegionIndex Region -> ([Char], [Request])
diagonal _ _ 0 _ = ([], [])
diagonal quad start len dat = case step primaryDir start dat of
    Left req -> (replicate (len * (len - 1)) ' ', [req])
    Right (ch, fwdStart) ->
        let fwdMarch = forwards primaryDir fwdStart (len - 1) dat
            sideMarch = case step secondaryDir fwdStart dat of
                Left req -> (replicate (len ^ 2 - 3*len - 3) ' ', [req])
                Right (ch', sideStart) -> (first (cons ch')) (diagonal quad start (len-1) dat) in
                    ((++) **** consMaybe) fwdMarch sideMarch
    where
        (primaryDir, secondaryDir) = getDirections quad

{-
recursiveWalk :: Direction -> Coord -> Int -> Map.Map RegionIndex Region -> ([Char], Maybe Request)
recursiveWalk dir start len dat = case Map.lookup ri dat of
    Nothing -> (replicate len ' ', Just (Load ri))
    Just region -> case indexTile start region of
        Nothing -> (replicate len ' ', Just (Load ri))
        Just (ch, pd) -> if len > 0 then
                let start' = nextCoord dir start pd in (first (cons ch)) (recusiveWalk dir start' (len - 1) dat)
            else
                ([ch], Nothing)
    where
        ri = regionIndex start

recursiveView :: Coord -> Quadrat -> Int -> Map.Map RegionIndex Region -> ([Char], [Request])
recursiveView start dir len dat = case Map.lookup ri dat of
    Nothing -> (replicate (len * (len - 1)) ' ', Just (Load ri))
    Just region -> case indexTile start region of
        Nothing -> (replicate (len * (len - 1)) ' ', Just (Load ri))
        Just (ch, pd) ->
-}
--render :: Coord -> State -> (PlayerSight, [Request]) -- need to implement PlayerSight