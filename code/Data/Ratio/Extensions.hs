
module Data.Ratio.Extensions where
import Data.Ratio as Ratio


setPrecision = \precision rational -> (approxRational rational precision)
setPrecision10 = (setPrecision ((%) 1 10000000000))


