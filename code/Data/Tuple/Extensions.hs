
module Data.Tuple.Extensions where
import Data.Tuple as Tuple

setFst (a, b) x = (x, b)
setSnd (a, b) x = (a, x)

fst3 (a, b, c) = a
snd3 (a, b, c) = b
third3 (a, b, c) = c
setFst3 (a, b, c) x = (x, b, c)
setSnd3 (a, b, c) x = (a, x, c)
setThird3 (a, b, c) x = (a, b, x)

fst4 (a, b, c, d) = a
snd4 (a, b, c, d) = b
third4 (a, b, c, d) = c
fourth4 (a, b, c, d) = d
setFst4 (a, b, c, d) x = (x, b, c, d)
setSnd4 (a, b, c, d) x = (a, x, c, d)
setThird4 (a, b, c, d) x = (a, b, x, d)
setFourth4 (a, b, c, d) x = (a, b, c, x)

fst5 (a, b, c, d, e) = a
snd5 (a, b, c, d, e) = b
third5 (a, b, c, d, e) = c
fourth5 (a, b, c, d, e) = d
fifth5 (a, b, c, d, e) = e
setFst5 (a, b, c, d, e) x = (x, b, c, d, e)
setSnd5 (a, b, c, d, e) x = (a, x, c, d, e)
setThird5 (a, b, c, d, e) x = (a, b, x, d, e)
setFourth5 (a, b, c, d, e) x = (a, b, c, x, e)
setFifth5 (a, b, c, d, e) x = (a, b, c, d, x)


