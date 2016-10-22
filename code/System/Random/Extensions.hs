module System.Random.Extensions where
import System.Random as Random


randomIORational :: IO Rational
randomIORational = do
    d <- randomIO :: IO Double
    (return (toRational d))


