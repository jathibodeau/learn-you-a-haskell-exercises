{-
 - Create a type called Validation
 - The type constructor takes one parameter
 - There are two Values: 
 -   Success, which takes that parameter and
 -   Fail String, which represents a failure, and a reason for that failure
 -}
data Validation a
  = Success a
  | Fail String
  deriving (Eq, Show)

-- Make the Validation a Monad
instance Functor Validation where
  fmap f (Success a) = Success $ f a
  fmap _ (Fail x) = Fail x

instance Applicative Validation where
  pure = Success
  (<*>) (Success f) = fmap f
  (<*>) (Fail x) = const $ Fail x

instance Monad Validation where
  return = pure
  (>>=) (Success x) f = f x
  (>>=) (Fail x) _ = Fail x

{-
 - Create a function, positiveCheck, which takes a number and returns a successful Validation if it's positive, 
 - and a failed Validation with a String message if not.
 -}
positiveCheck :: (Num a, Ord a) => a -> Validation a
positiveCheck x
  | signum x == 1 = Success x
  | otherwise = Fail "Not a positive number!"

{-
 - Create a function, evenCheck, which returns a successful Validation if it's even,
 - and a failed Validation with a string message if it's odd
 -}
evenCheck :: (Integral a)  =>  a -> Validation a
evenCheck x
  | even x = Success x
  | otherwise = Fail "Not an even number!"

{-
 - Write a function which uses positiveCheck and evenCheck to make sure a number is both positive and even
 -}
positiveAndEvenCheck :: (Num a, Ord a, Integral a) => a -> Validation a
positiveAndEvenCheck x = positiveCheck x >>= evenCheck
