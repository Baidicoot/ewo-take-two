module Demo where
import Game.Engine.Terrain
import Game.Engine.Render
import Game.Engine.State
import Game.Engine.Common
import Game.Engine.Entity

import System.IO
import System.Process
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Vector as V

reg :: Region
reg = Region . V.fromList $ map (\c -> (c, Nothing)) $ take 65536 (concat (repeat "abcde"))

empty :: State
empty = State { regions = Map.empty, entities = [], events = [], spawners = Map.empty }

state :: State
state = tick 0.0 [Loaded (0, 0, 0) reg []] empty

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

viewSize :: Int
viewSize = playerLookDist * 2 + 1

draw :: State -> (Int, Int, Int) -> String
draw st co = let (s, _) = renderView co (regions st) in intercalate "\n" (splitEvery viewSize s)

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

moveDemo :: Direction -> Coord -> Coord
moveDemo dir coord = nextCoord dir coord Nothing

demo :: IO ()
demo = do
    coord <- newIORef (0, 0, 0)
    hSetEcho stdout False
    hSetBuffering stdin NoBuffering
    forever $ do
        clearScreen
        pos <- readIORef coord
        putStrLn $ draw state pos
        v <- stdin `ifReadyDo` getChar
        case v of
            Just x -> case x of
                    'w' -> modifyIORef' coord (moveDemo N)
                    'a' -> modifyIORef' coord (moveDemo W)
                    's' -> modifyIORef' coord (moveDemo S)
                    'd' -> modifyIORef' coord (moveDemo E)
                    _ -> pure ()
            Nothing -> pure ()
        threadDelay 10000
        