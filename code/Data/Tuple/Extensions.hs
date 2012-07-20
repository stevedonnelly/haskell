
module Data.Tuple.Extensions where
import Data.Tuple as Tuple

setSubelement :: (a -> b -> a) -> (a -> b) -> (b -> c -> b) -> (a -> c -> a)
setSubelement = \setOuter getInner setInner tuple value -> (setOuter tuple (setInner (getInner tuple) value))

first2 (a, b) = a
second2 (a, b) = b
setFirst2 (a, b) x = (x, b)
setSecond2 (a, b) x = (a, x)

first3 (a, b, c) = a
second3 (a, b, c) = b
third3 (a, b, c) = c
setFirst3 (a, b, c) x = (x, b, c)
setSecond3 (a, b, c) x = (a, x, c)
setThird3 (a, b, c) x = (a, b, x)

first4 (a, b, c, d) = a
second4 (a, b, c, d) = b
third4 (a, b, c, d) = c
fourth4 (a, b, c, d) = d
setFirst4 (a, b, c, d) x = (x, b, c, d)
setSecond4 (a, b, c, d) x = (a, x, c, d)
setThird4 (a, b, c, d) x = (a, b, x, d)
setFourth4 (a, b, c, d) x = (a, b, c, x)

first5 (a, b, c, d, e) = a
second5 (a, b, c, d, e) = b
third5 (a, b, c, d, e) = c
fourth5 (a, b, c, d, e) = d
fifth5 (a, b, c, d, e) = e
setFirst5 (a, b, c, d, e) x = (x, b, c, d, e)
setSecond5 (a, b, c, d, e) x = (a, x, c, d, e)
setThird5 (a, b, c, d, e) x = (a, b, x, d, e)
setFourth5 (a, b, c, d, e) x = (a, b, c, x, e)
setFifth5 (a, b, c, d, e) x = (a, b, c, d, x)


