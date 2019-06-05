module Constraints where


not :: (a ~ b) => a -> b -> a
not x _ = x
