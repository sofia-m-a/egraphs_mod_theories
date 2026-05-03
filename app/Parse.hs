module Parse where

import Lude

-- class Stringing p where
--     grab :: p Char
--     char :: Char -> p ()

--     succeed :: p ()
--     andThen :: p a -> p b -> p (a, b)

--     fail :: p Void
--     choice :: p a -> p b -> p (Either a b)

--     label :: Text -> p a -> p a
--     mapBi :: (a -> b) -> (b -> a) -> p a -> p b

--     mu :: (a -> b -> c -> b) -> p a -> p b -> p c -> p b

-- number :: Stringing p => p Natural
-- number = label "number" $ mu (\_ n d -> 10*n + d) succeed _ _