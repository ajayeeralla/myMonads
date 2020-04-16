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
