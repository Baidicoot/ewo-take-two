-- Entity Object is an interface for a generic stateful entity
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
module Game.Engine.Entity where
import Game.Engine.Common
import Game.Engine.Terrain
import Game.Save.Saveable

data Event
    = Broadcast String String
    | AttackEvent Coord Int
    | SpawnEvent Int SpawnData
    | KeyEvent SenderID Char
    -- ...
    deriving(Show)

data Input
    = Keypress SenderID Char
    | Loaded RegionIndex Region [Entity]
    | SpawnCommand Int SpawnData

data Request
    = Load RegionIndex
    | Drop RegionIndex
    deriving(Show, Eq, Ord)

data SpawnData = SpawnData (Maybe SenderID) (Int, Int) deriving(Show)

data EntitySight = EntitySight [Entity]

data EntityData = EntityData Int Coord Bool Bool (Maybe SenderID)

data EntityS s = EntityS {
    _state :: s,
    _update :: EntitySight -> DeltaTime -> s -> [Event] -> (EntityS s, [Event]),
    _render :: [(Coord, Char)],
    _data :: EntityData
}

data Entity = forall s. (ExistsSaver s) => Entity (EntityS s)

update :: Entity -> EntitySight -> DeltaTime -> [Event] -> (Entity, [Event])
update (Entity e) s dt es = let (e', es') = _update e s dt (_state e) es in (Entity e', es')

getData :: Entity -> EntityData
getData (Entity e) = _data e

getType :: Entity -> Int
getType e = let (EntityData t _ _ _ _) = getData e in t

loc :: Entity -> Coord
loc e = let (EntityData _ c _ _ _) = getData e in c

dead :: Entity -> Bool
dead e = let (EntityData _ _ b _ _) = getData e in b

load :: Entity -> Bool
load e = let (EntityData _ _ _ b _) = getData e in b

playerName :: Entity -> Maybe SenderID
playerName e = let (EntityData _ _ _ _ s) = getData e in s

render :: Entity -> [(Coord, Char)]
render (Entity e) = _render e