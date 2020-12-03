module AdventOfCode.Numeric
  ( integralToBounded,
  )
where

integralToBounded :: forall a b. (Integral b, Integral a, Bounded a) => b -> Maybe a
integralToBounded (toInteger -> n)
  | n < toInteger (minBound @a) = Nothing
  | n > toInteger (maxBound @a) = Nothing
  | otherwise = Just $ fromIntegral n
{-# INLINE integralToBounded #-}
