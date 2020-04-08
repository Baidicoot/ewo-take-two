module Main where
import Game.Engine.State
import Game.Engine.Terrain
import Game.Engine.Entity
import Game.Engine.Render
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.List (intercalate)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

center :: [Char]
center = take 65536 (concat (repeat "abcde"))

dat :: [(Char, Maybe PortalData)]
dat = map (\c -> (c, Nothing)) center

reg :: Region
reg = Region (V.fromList dat)

emptyState :: State
emptyState = State { regions = Map.empty, entities = [], events = [], spawners = Map.empty }

state :: State
state = tick 0.0 [Loaded (0, 0, 0) reg []] emptyState

view :: String
view = let (a, _) = renderView (0, 50, 50) (regions state) in a

main :: IO ()
main = do
    putStrLn "start"
    putStrLn $ intercalate "\n" (splitEvery 7 view)
    putStrLn "end"
