import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Eq, Show)

-- Make the list a Functor
instance Functor List where
  fmap _ Empty = Empty
  fmap f (Value x xs) = Value (f x) $ fmap f xs

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists (Value x xs) b = Value x $ combineLists xs b
combineLists _ b = b

-- Make our list a Monoid
instance Semigroup (List a) where
  (<>) = combineLists

instance Monoid (List a) where
  mempty = Empty

-- Make our list an Applicative
instance Applicative List where
  pure x = Value x Empty
  Value f fs <*> xs = fmap f xs <> (fs <*> xs)
  Empty <*> _ = Empty

-- Make sure that the List obeys the laws for Applicative and Monoid
isApplicative =
  and [ (pure id <*> u) == u                               -- Applicative: Identity
      , (pure (.) <*> x <*> y <*> u) == (x <*> (y <*> u))  -- Applicative: Composition
      , (pure f <*> pure t) == (pure :: a -> List a) (f t) -- Applicatite: Homomorphism
      , (x <*> pure t) == (pure ($ t) <*> x)               -- Applicative: Interchange
      , u <> mempty == u                                   -- Monoid: Right identity
      , mempty <> u == u                                   -- Monoid: Left identity
      , u <> (v <> w) == (u <> v) <> w                     -- Semigroup: Associativity
      , mconcat [u, v, w] == foldr (<>) mempty [u, v, w] ] -- Monoid: Concatenation
  where
    f = negate
    t = 1
    u = Value 2 Empty
    v = Value 3 Empty
    w = Value 4 Empty
    x = Value succ Empty
    y = Value pred Empty

-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty
threeValueList = Value 10 $ Value 20 $ Value 30 Empty
fourValueList = threeValueList <> Value 40 Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)
useFmap = fmap plusTwo threeValueList
-- Value 12 (Value 22 (Value 32 Empty))

-- Use <$> and <*> on the lists with a binary function
useFmapApply = (*) <$> twoValueList <*> threeValueList
-- Value 100 (Value 200 (Value 300 (Value 200 (Value 400 (Value 600 Empty)))))

-- Create some lists of binary functions
binFuncList = Value (+) $ Value (*) $ Value (^) Empty

-- Use <*> on the binary functions list and the number lists
binFuncApply = binFuncList <*> threeValueList <*> fourValueList
-- Value 20 (Value 30 (Value 40 (Value 50 (Value 30 (Value 40 (Value 50
--   (Value 60 (Value 40 (Value 50 (Value 60 (Value 70 (Value 100 (Value 200
--   (Value 300 (Value 400 (Value 200 (Value 400 (Value 600 (Value 800 (Value 300
--   (Value 600 (Value 900 (Value 1200 (Value 10000000000 (Value 100000000000000000000
--   (Value 1000000000000000000000000000000 (Value 10000000000000000000000000000000000000000
--   (Value 10240000000000 (Value 104857600000000000000000000 (Value 1073741824000000000000000000000000000000
--   (Value 10995116277760000000000000000000000000000000000000000 (Value 590490000000000
--   (Value 348678440100000000000000000000 (Value 205891132094649000000000000000000000000000000
--   (Value 121576654590569288010000000000000000000000000000000000000000 Empty)))))))))))))))))))))))))))))))))))
