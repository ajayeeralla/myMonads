--{-# LANGUAGE Derive #-}
--{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.MyMonads
  ( MyMaybe (..)
  )
where
import Data.Nat
-- | Monad MyMaybe

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



data IList n a where
  Cons :: a -> IList n a -> IList (S n) a
  Nil :: IList Z a



length :: IList n a -> Nat
length Nil = 0
length (Cons x xs) =  S (Data.MyMonads.length xs)

toList :: IList n a -> [a]
toList Nil = []
toList (Cons x xs) = x:toList xs


iMap :: (a -> b) -> IList n a -> IList n b
iMap f Nil = Nil
iMap f (Cons x xs) = Cons (f x) (iMap f xs)

instance Functor (IList n) where
  fmap = iMap


-- | Monad MyState
