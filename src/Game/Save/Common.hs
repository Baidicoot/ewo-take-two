{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Game.Save.Common where
import qualified Data.ByteString as B
-- import Data.ByteString.Internal (w2c, c2w) -- DEBUG STUFF
import Data.Word
import Data.Bits

import Game.Save.Saveable

parseInt16 :: B.ByteString -> Maybe (Int, B.ByteString)
parseInt16 str = do
    (w, str') <- B.uncons str
    (w', str'') <- B.uncons str'
    return $ ((shiftL (fromIntegral w) 8) + (fromIntegral w'), str'')

unparseInt16 :: Int -> (Word8, Word8)
unparseInt16 i = (fromIntegral (shiftR i 8), fromIntegral i)

loadMultiple :: Loader c v -> Loader c [v]
loadMultiple fn config str = do
    (len, str') <- parseInt16 str
    case (B.length str') of
        x | x > len -> do
            e <- fn config (B.take len str')
            es <- loadMultiple fn config (B.drop len str')
            return (e:es)
        x | x == len -> (fn config str') >>= (return . return)
        _ -> Nothing

saveMultiple :: Saver c v -> Saver c [v]
saveMultiple fn config (v:vs) =
    let str = fn config v
        (l0, l1) = unparseInt16 (B.length str)
        str0 = l0 `B.cons` l1 `B.cons` str
        str1 = saveMultiple fn config vs in
            str0 `B.append` str1
saveMultiple _ _ [] = B.empty

instance (Saveable a b c) => Saveable [a] b c where
    save _ = saveMultiple (save undefined)
    load _ = loadMultiple (load undefined)

loadPair :: Loader a b -> Loader c d -> Loader (a, c) (b, d)
loadPair f0 f1 (a, c) str = do
    (len, str') <- parseInt16 str
    b <- f0 a (B.take len str')
    d <- f1 c (B.drop len str')
    return (b, d)

savePair :: Saver a b -> Saver c d -> Saver (a, c) (b, d)
savePair f0 f1 (a, c) (b, d) =
    let str0 = f0 a b
        str1 = f1 c d
        (l0, l1) = unparseInt16 (B.length str0) in
            l0 `B.cons` l1 `B.cons` str0 `B.append` str1

instance (Saveable a b c, Saveable d e f) => Saveable (a, d) (b, e) (c, f) where
    save _ = savePair (save undefined) (save undefined)
    load _ = loadPair (load undefined) (load undefined)

{- DEBUG STUFF
saveChar :: Saver () Char
saveChar _ = B.singleton . c2w

loadChar :: Loader () Char
loadChar _ = Just . w2c . B.head

instance Saveable Char () () where
    save _ = saveChar
    load _ = loadChar
-}