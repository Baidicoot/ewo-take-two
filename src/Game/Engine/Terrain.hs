{-# LANGUAGE DeriveGeneric #-}
module Game.Engine.Terrain where
import qualified Data.Vector as V
import Game.Engine.Common

regionSize :: Int
regionSize = 256

type Tile = (Char, Maybe PortalData)
type PortalData = (Coord, Coord, Coord, Coord)
newtype Region = Region (V.Vector Tile)

indexOffsetTile :: Coord -> Region -> Maybe Tile -- takes normalized coord
indexOffsetTile (_, x, y) (Region vec) = vec V.!? index
    where
        index = y*regionSize + x

regionIndex :: Coord -> RegionIndex
regionIndex (lv, x, y) = (lv, x `div` regionSize, y `div` regionSize)

regionOffset :: Coord -> Coord
regionOffset c@(lv, x, y) = let (_, ix, iy) = regionIndex c in (lv, x - (regionSize*ix), y - (regionSize*iy))

indexTile :: Coord -> Region -> Maybe Tile
indexTile = indexOffsetTile . regionOffset -- takes global coord

data Direction = N | S | E | W deriving(Show)
data Quadrat =  NE | EN | ES | SE | SW | WS | WN | NW

nextCoord :: Direction -> Coord -> Maybe PortalData -> Coord
nextCoord N (lv, x, y) Nothing = (lv, x, y+1)
nextCoord S (lv, x, y) Nothing = (lv, x, y-1)
nextCoord E (lv, x, y) Nothing = (lv, x+1, y)
nextCoord W (lv, x, y) Nothing = (lv, x-1, y)

nextCoord N _ (Just (next, _, _, _)) = next
nextCoord S _ (Just (_, next, _, _)) = next
nextCoord E _ (Just (_, _, next, _)) = next
nextCoord W _ (Just (_, _, _, next)) = next

getDirections :: Quadrat -> (Direction, Direction)
getDirections NE = (N, E)
getDirections EN = (E, N)
getDirections ES = (E, S)
getDirections SE = (S, E)
getDirections SW = (S, W)
getDirections WS = (W, S)
getDirections WN = (W, N)
getDirections NW = (N, W)

getQuadrats :: Direction -> (Quadrat, Quadrat)
getQuadrats N = (NE, NW)
getQuadrats S = (SE, SW)
getQuadrats E = (EN, ES)
getQuadrats W = (WN, WS)