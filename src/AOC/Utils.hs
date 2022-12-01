module AOC.Utils (liftEither, invertOrd) where

liftEither :: String -> Either e a -> IO a
liftEither msg = either (error msg) pure

invertOrd :: Ord a => a -> a -> Ordering
invertOrd x y = case compare x y of
  LT -> GT
  GT -> LT
  EQ -> EQ
