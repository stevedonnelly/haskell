
module Data.Tuple.Extensions where
import Data.Tuple as Tuple

setSubelement :: (a -> b -> a) -> (a -> b) -> (b -> c -> b) -> (a -> c -> a)
setSubelement = \setOuter getInner setInner tuple value -> (setOuter tuple (setInner (getInner tuple) value))

first2 (a, b) = a
second2 (a, b) = b
setFirst2 (a, b) x = (x, b)
setSecond2 (a, b) x = (a, x)
toList2 = \(a, b) -> [a, b]
fromList2 = \[a, b] -> (a, b)

first3 (a, b, c) = a
second3 (a, b, c) = b
third3 (a, b, c) = c
setFirst3 (a, b, c) x = (x, b, c)
setSecond3 (a, b, c) x = (a, x, c)
setThird3 (a, b, c) x = (a, b, x)
toList3 = \(a, b, c) -> [a, b, c]
fromList3 = \[a, b, c] -> (a, b, c)

first4 (a, b, c, d) = a
second4 (a, b, c, d) = b
third4 (a, b, c, d) = c
fourth4 (a, b, c, d) = d
setFirst4 (a, b, c, d) x = (x, b, c, d)
setSecond4 (a, b, c, d) x = (a, x, c, d)
setThird4 (a, b, c, d) x = (a, b, x, d)
setFourth4 (a, b, c, d) x = (a, b, c, x)
toList4 = \(a, b, c, d) -> [a, b, c, d]
fromList4 = \[a, b, c, d] -> (a, b, c, d)

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
toList5 = \(a, b, c, d, e) -> [a, b, c, d, e]
fromList5 = \[a, b, c, d, e] -> (a, b, c, d, e)

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
toList6 = \(a, b, c, d, e, f) -> [a, b, c, d, e, f]
fromList6 = \[a, b, c, d, e, f] -> (a, b, c, d, e, f)

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
toList7 = \(a, b, c, d, e, f, g) -> [a, b, c, d, e, f, g]
fromList7 = \[a, b, c, d, e, f, g] -> (a, b, c, d, e, f, g)

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
toList8 = \(a, b, c, d, e, f, g, h) -> [a, b, c, d, e, f, g, h]
fromList8 = \[a, b, c, d, e, f, g, h] -> (a, b, c, d, e, f, g, h)

first9 (a, b, c, d, e, f, g, h, i) = a
second9 (a, b, c, d, e, f, g, h, i) = b
third9 (a, b, c, d, e, f, g, h, i) = c
fourth9 (a, b, c, d, e, f, g, h, i) = d
fifth9 (a, b, c, d, e, f, g, h, i) = e
sixth9 (a, b, c, d, e, f, g, h, i) = f
seventh9 (a, b, c, d, e, f, g, h, i) = g
eighth9 (a, b, c, d, e, f, g, h, i) = h
ninth9 (a, b, c, d, e, f, g, h, i) = i
setFirst9 (a, b, c, d, e, f, g, h, i) x = (x, b, c, d, e, f, g, h, i)
setSecond9 (a, b, c, d, e, f, g, h, i) x = (a, x, c, d, e, f, g, h, i)
setThird9 (a, b, c, d, e, f, g, h, i) x = (a, b, x, d, e, f, g, h, i)
setFourth9 (a, b, c, d, e, f, g, h, i) x = (a, b, c, x, e, f, g, h, i)
setFifth9 (a, b, c, d, e, f, g, h, i) x = (a, b, c, d, x, f, g, h, i)
setSixth9 (a, b, c, d, e, f, g, h, i) x = (a, b, c, d, e, x, g, h, i)
setSeventh9 (a, b, c, d, e, f, g, h, i) x = (a, b, c, d, e, f, x, h, i)
setEighth9 (a, b, c, d, e, f, g, h, i) x = (a, b, c, d, e, f, g, x, i)
setNinth9 (a, b, c, d, e, f, g, h, i) x = (a, b, c, d, e, f, g, h, x)
toList9 = \(a, b, c, d, e, f, g, h, i) -> [a, b, c, d, e, f, g, h, i]
fromList9 = \[a, b, c, d, e, f, g, h, i] -> (a, b, c, d, e, f, g, h, i)

first10 (a, b, c, d, e, f, g, h, i, j) = a
second10 (a, b, c, d, e, f, g, h, i, j) = b
third10 (a, b, c, d, e, f, g, h, i, j) = c
fourth10 (a, b, c, d, e, f, g, h, i, j) = d
fifth10 (a, b, c, d, e, f, g, h, i, j) = e
sixth10 (a, b, c, d, e, f, g, h, i, j) = f
seventh10 (a, b, c, d, e, f, g, h, i, j) = g
eighth10 (a, b, c, d, e, f, g, h, i, j) = h
ninth10 (a, b, c, d, e, f, g, h, i, j) = i
tenth10 (a, b, c, d, e, f, g, h, i, j) = j
setFirst10 (a, b, c, d, e, f, g, h, i, j) x = (x, b, c, d, e, f, g, h, i, j)
setSecond10 (a, b, c, d, e, f, g, h, i, j) x = (a, x, c, d, e, f, g, h, i, j)
setThird10 (a, b, c, d, e, f, g, h, i, j) x = (a, b, x, d, e, f, g, h, i, j)
setFourth10 (a, b, c, d, e, f, g, h, i, j) x = (a, b, c, x, e, f, g, h, i, j)
setFifth10 (a, b, c, d, e, f, g, h, i, j) x = (a, b, c, d, x, f, g, h, i, j)
setSixth10 (a, b, c, d, e, f, g, h, i, j) x = (a, b, c, d, e, x, g, h, i, j)
setSeventh10 (a, b, c, d, e, f, g, h, i, j) x = (a, b, c, d, e, f, x, h, i, j)
setEighth10 (a, b, c, d, e, f, g, h, i, j) x = (a, b, c, d, e, f, g, x, i, j)
setNinth10 (a, b, c, d, e, f, g, h, i, j) x = (a, b, c, d, e, f, g, h, x, j)
setTenth10 (a, b, c, d, e, f, g, h, i, j) x = (a, b, c, d, e, f, g, h, i, x)
toList10 = \(a, b, c, d, e, f, g, h, i, j) -> [a, b, c, d, e, f, g, h, i, j]
fromList10 = \[a, b, c, d, e, f, g, h, i, j] -> (a, b, c, d, e, f, g, h, i, j)

