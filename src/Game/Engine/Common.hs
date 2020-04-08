-- Common defines common items in-game, like coordinates

module Game.Engine.Common where
import qualified Data.Text as T

type Coord = (Int, Int, Int) -- level, x, y

coordSqDist :: Coord -> Coord -> Int
coordSqDist (_, x, y) (_, a, b) = (x - a) ^ 2 + (y - b) ^ 2

type RegionIndex = (Int, Int, Int) -- level, x, y

type DeltaTime = Float

type SenderID = T.Text