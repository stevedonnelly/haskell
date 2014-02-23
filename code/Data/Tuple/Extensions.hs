
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

first6 (a, b, c, d, e, f) = a
second6 (a, b, c, d, e, f) = b
third6 (a, b, c, d, e, f) = c
fourth6 (a, b, c, d, e, f) = d
fifth6 (a, b, c, d, e, f) = e
sixth6 (a, b, c, d, e, f) = f
setFirst6 (a, b, c, d, e, f) x = (x, b, c, d, e, f)
setSecond6 (a, b, c, d, e, f) x = (a, x, c, d, e, f)
setThird6 (a, b, c, d, e, f) x = (a, b, x, d, e, f)
setFourth6 (a, b, c, d, e, f) x = (a, b, c, x, e, f)
setFifth6 (a, b, c, d, e, f) x = (a, b, c, d, x, f)
setSixth6 (a, b, c, d, e, f) x = (a, b, c, d, e, x)

first7 (a, b, c, d, e, f, g) = a
second7 (a, b, c, d, e, f, g) = b
third7 (a, b, c, d, e, f, g) = c
fourth7 (a, b, c, d, e, f, g) = d
fifth7 (a, b, c, d, e, f, g) = e
sixth7 (a, b, c, d, e, f, g) = f
seventh7 (a, b, c, d, e, f, g) = g
setFirst7 (a, b, c, d, e, f, g) x = (x, b, c, d, e, f, g)
setSecond7 (a, b, c, d, e, f, g) x = (a, x, c, d, e, f, g)
setThird7 (a, b, c, d, e, f, g) x = (a, b, x, d, e, f, g)
setFourth7 (a, b, c, d, e, f, g) x = (a, b, c, x, e, f, g)
setFifth7 (a, b, c, d, e, f, g) x = (a, b, c, d, x, f, g)
setSixth7 (a, b, c, d, e, f, g) x = (a, b, c, d, e, x, g)
setSeventh7 (a, b, c, d, e, f, g) x = (a, b, c, d, e, f, x)

first8 (a, b, c, d, e, f, g, h) = a
second8 (a, b, c, d, e, f, g, h) = b
third8 (a, b, c, d, e, f, g, h) = c
fourth8 (a, b, c, d, e, f, g, h) = d
fifth8 (a, b, c, d, e, f, g, h) = e
sixth8 (a, b, c, d, e, f, g, h) = f
seventh8 (a, b, c, d, e, f, g, h) = g
eighth8 (a, b, c, d, e, f, g, h) = h
setFirst8 (a, b, c, d, e, f, g, h) x = (x, b, c, d, e, f, g, h)
setSecond8 (a, b, c, d, e, f, g, h) x = (a, x, c, d, e, f, g, h)
setThird8 (a, b, c, d, e, f, g, h) x = (a, b, x, d, e, f, g, h)
setFourth8 (a, b, c, d, e, f, g, h) x = (a, b, c, x, e, f, g, h)
setFifth8 (a, b, c, d, e, f, g, h) x = (a, b, c, d, x, f, g, h)
setSixth8 (a, b, c, d, e, f, g, h) x = (a, b, c, d, e, x, g, h)
setSeventh8 (a, b, c, d, e, f, g, h) x = (a, b, c, d, e, f, x, h)
setEighth8 (a, b, c, d, e, f, g, h) x = (a, b, c, d, e, f, g, x)


