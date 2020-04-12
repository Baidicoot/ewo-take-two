module Game.Save.Saveable where

import qualified Data.ByteString as B

class Saveable s where
    save :: s -> B.ByteString
    load :: B.ByteString -> Maybe s