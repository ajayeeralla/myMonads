--{-# LANGUAGE DeriveAnyClass #-}
--{-# LANGUAGE DeriveFunctor #-}
module Mondads
  ( MyMaybe (..)
  )
where

data MyMaybe a = Empty | Only a
  deriving ( Eq
           , Ord
           , Show
           )


instance Functor MyMaybe where
  fmap _ Empty = Empty
  fmap f (Only x) = Only (f x)

instance Applicative MyMaybe where
   pure = Only

   Only f  <*> m       = fmap f m
   Empty <*> _m      = Empty

   Only _m1 *> m2      = m2
   Empty  *> _m2     = Empty

instance Monad MyMaybe where
  return  = Only
  Empty >>= f = Empty
  Only x >>= f = f x
  fail _       = Empty
