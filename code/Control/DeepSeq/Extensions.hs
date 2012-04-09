module Control.DeepSeq.Extensions where
import Control.DeepSeq as DeepSeq


strict1 :: NFData a => (a -> b) -> (a -> b)
strict1 = \f -> (\a -> (deepseq a (f a)))

strict2 :: NFData a => NFData b => (a -> b -> c) -> (a -> b -> c)
strict2 = \f -> (\a b -> (deepseq a (deepseq b (f a b))))

strict3 :: NFData a => NFData b => NFData c => (a -> b -> c -> d) -> (a -> b -> c -> d)
strict3 = \f -> (\a b c -> (deepseq a (deepseq b (deepseq c (f a b c)))))

strict4 :: NFData a => NFData b => NFData c => NFData d => (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> e)
strict4 = \f -> (\a b c d -> (deepseq a (deepseq b (deepseq c (deepseq d (f a b c d))))))

strict5 :: NFData a => NFData b => NFData c => NFData d => NFData e => (a -> b -> c -> d -> e -> f) -> (a -> b -> c -> d -> e -> f)
strict5 = \f -> (\a b c d e -> (deepseq a (deepseq b (deepseq c (deepseq d (deepseq e (f a b c d e)))))))


