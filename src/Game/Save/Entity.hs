{-# LANGUAGE MultiParamTypeClasses #-}
module Game.Save.Entity where
import Game.Save.Common
import Game.Engine.Entity

import Data.ByteString as B
import qualified Data.Map.Strict as Map

newtype EntityLoadTable = EntityLoadTable (Map.Map Int (B.ByteString -> Maybe Entity))

loadCodedEntity :: B.ByteString -> Int -> EntityLoadTable -> Maybe Entity
loadCodedEntity str i (EntityLoadTable tab) = do
    fn <- Map.lookup i tab
    fn str

loadEntity :: EntityLoadTable -> B.ByteString -> Maybe Entity
loadEntity table str = do
    (i, str') <- parseInt16 str
    loadCodedEntity str' i table

instance Saveable Entity () EntityLoadTable where
    save _ _ (Entity e) = saver () (_state e)
    load _ = loadEntity