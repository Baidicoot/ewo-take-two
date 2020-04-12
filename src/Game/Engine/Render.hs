module Game.Engine.Render where
import Game.Engine.Terrain
import Game.Engine.Common
import Game.Engine.Entity

import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Arrow

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x, y) (a, b) = ceiling . sqrt . fromIntegral $ (x - a) ^ 2 + (y - b) ^ 2

isDiagonalTo :: (Int, Int) -> (Int, Int) -> Bool
(ax, ay) `isDiagonalTo` b = any (==b) [(ax + x, ay + y) | x <- [-1, 1], y <- [-1, 1]]

midpoint :: (Int, Int) -> (Int, Int) -> (Int, Int)
midpoint a@(ax, ay) b@(bx, by)
    | a `isDiagonalTo` b = (ax, by)
    | otherwise = 
        let (dx, dy) = ((bx - ax) `div` 2, (by - ay) `div` 2) in
            (ax + dx, ay + dy)

genPoints :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
genPoints a@(ax, ay) b@(bx, by)
    | a == b = []
    | dist a b <= 1 = [b]
    | otherwise =
        let mid = midpoint a b
            half1 = genPoints a mid
            half2 = genPoints mid b in
                half1 ++ half2

getDir :: (Int, Int) -> (Int, Int) -> Direction
getDir (ax, ay) b
    | (ax, ay + 1) == b = N
    | (ax, ay - 1) == b = S
    | (ax + 1, ay) == b = E
    | (ax - 1, ay) == b = W
    | otherwise = error "algorythm spat out the wrong data"

genMoves :: (Int, Int) -> (Int, Int) -> [Direction]
genMoves start end = fst $ foldl (\(dir, prev) next -> (getDir prev next:dir, next)) ([], start) (genPoints start end)

step :: Direction -> Coord -> Map.Map RegionIndex Region -> Either Request Coord
step dir start dat = case internal of
    Nothing -> Left (Load ri)
    Just c -> Right c
    where
        ri = regionIndex start
        internal = do
            region <- Map.lookup ri dat
            (_, pd) <- indexTile start region
            return (nextCoord dir start pd)

coordChar :: Coord -> Map.Map RegionIndex Region -> Either Request Char
coordChar start dat = case internal of
    Nothing -> Left (Load ri)
    Just c -> Right c
    where
        ri = regionIndex start
        internal = do
            region <- Map.lookup ri dat
            (ch, _) <- indexTile start region
            return ch

emptyChar :: Char
emptyChar = '.'

doMoves :: Coord -> Map.Map RegionIndex Region -> [Direction] -> (Char, Maybe Request)
doMoves start dat (m:ms) = case step m start dat of
    Left req -> (emptyChar, Just req)
    Right start' -> doMoves start' dat ms
doMoves start dat [] = case coordChar start dat of
    Left req -> (emptyChar, Just req)
    Right ch -> (ch, Nothing)

playerLookDist :: Int
playerLookDist = 10

playerLookDirections :: [[Direction]]
playerLookDirections = map (genMoves (0, 0)) [(x, y) | x <- range, y <- range]
    where
        range = [-playerLookDist..playerLookDist]

renderView :: Coord -> Map.Map RegionIndex Region -> ([Char], [Request])
renderView start dat = second catMaybes . unzip . map (doMoves start dat) $ playerLookDirections