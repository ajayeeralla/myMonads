{-# LANGUAGE GADTs #-}

module Data.MyFunctors where
import Data.Nat
import Data.MyTypes

-- | Define Functor instance for MyMaybe
instance Functor MyMaybe where
  fmap _ Empty = Empty
  fmap f (Only x) = Only (f x)

-- | Functor instance for IList
instance Functor (IList n) where
  --fmap :: (a -> b) -> IList n a -> IList n b
  -- | Here fmap exactly like iMap
  fmap g Nil = Nil
  fmap g (Cons x xs) = Cons (g x) (fmap g xs)

  --class Functor f where
  --fmap :: (a->b) -> f a -> f b

  --instance Functor (Either a) where
  --  fmap :: (a -> b) -> Either a a -> Either a b
  --  fmap g (Left x) = Left x
  --  fmap g (Right y) = Right (g y)

instance Functor (MyEither a b) where
  --fmap :: (a->b) -> MyEither a b a -> MyEither a b b
  fmap g (MyLeft x) = MyLeft x
  fmap g (Middle x) = Middle x
  fmap g (MyRight x) = MyRight (g x)