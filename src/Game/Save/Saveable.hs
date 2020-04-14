{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Game.Save.Saveable where
import Data.ByteString (ByteString)

type Loader c v = c -> ByteString -> Maybe v -- loader function with config
type Saver c v = c -> v -> ByteString

class ExistsSaver s where
    saver :: Saver () s

class Saveable s i o | s -> i o where
    save :: o -> Saver i s
    load :: i -> Loader o s