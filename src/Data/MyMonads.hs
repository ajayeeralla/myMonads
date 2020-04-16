--{-# LANGUAGE Derive #-}
--{-# LANGUAGE DeriveFunctor #-}


module Data.MyMonads where
import Data.Nat
import Data.MyTypes
import Data.MyFunctors
import Data.MyApplicatives

-- | Define Monad instance for MyMaybe
instance Monad MyMaybe where
  return  = Only
  Empty >>= f = Empty
  Only x >>= f = f x
  fail _       = Empty

-- | Monad class
{-class Monad m where
  (>>=)  :: m a -> (  a -> m b) -> m b
  (>>)   :: m a ->  m b         -> m b
  return ::   a                 -> m a
  fail   :: String -> m a-}

-- | Monad instance for MyEither
instance Monad (MyEither e d) where
  --(>>=)  :: MyEithe e d a -> (a -> MyEithe e d b) -> MyEithe e d b
  --(>>)   :: MyEithe e d a ->  MyEithe e d b         -> MyEithe e d b
  --return ::   a                 -> MyEithe e d a
  --fail   :: String -> MyEithe e d a
  return = MyRight
  MyLeft x >>= f = MyLeft x
  Middle x >>= f = Middle x
  MyRight x >>= f =  f x
