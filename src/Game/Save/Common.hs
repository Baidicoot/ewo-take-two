{-# LANGUAGE MultiParamTypeClasses #-}
module Game.Save.Common where
import qualified Data.ByteString as B
import Data.ByteString.Internal (w2c, c2w)
import Data.Word
import Data.Bits

type Loader c v = c -> B.ByteString -> Maybe v -- loader function with config
type Saver c v = c -> v -> B.ByteString

class ExistsSaver s where
    saver :: Saver () s

class Saveable s i o where
    save :: o -> Saver i s
    load :: i -> Loader o s

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

{- DEBUG THINGS
saveString :: Saver () String
saveString _ = B.pack . map c2w

loadString :: Loader () String
loadString _ = Just . map w2c . B.unpack
-}