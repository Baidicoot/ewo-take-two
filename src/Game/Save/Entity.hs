module Game.Save.Entity where
import Game.Save.Saveable
import Game.Engine.Entity

import ByteString as B
import qualified Data.Map.Strict as Map

type EntityLoadTable = Map.Map Int (B.ByteString -> Maybe Entity)