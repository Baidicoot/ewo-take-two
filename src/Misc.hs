module Misc where
import Control.Arrow

cons :: a -> [a] -> [a]
cons = (:)

-- miscelanious helper functions where Prelude does not suffice
firstElem :: (a -> Bool) -> [a] -> Maybe a
firstElem cond (a:as) = if cond a then Just a else firstElem cond as
firstElem _ [] = Nothing

dup :: (a -> b) -> (a, a) -> (b, b)
dup fn = fn *** fn

consMaybe :: Maybe a -> [a] -> [a]
consMaybe (Just x) = cons x
consMaybe Nothing = id

(****) :: (a -> b -> c) -> (d -> e -> f) -> (a, d) -> (b, e) -> (c, f)
a **** d = (uncurry (***)) . (a *** d)

(<<) :: (c -> d -> e) -> (a -> b -> c) -> a -> b -> d -> e -- composition for binary operators
(<<) fne fnc a b d = fne (fnc a b) d

(*****) :: (a -> b -> c -> d) -> (e -> f -> g -> h) -> (a, e) -> (b, f) -> (c, g) -> (d, h)
a ***** b = (uncurry (***)) << (a **** b)