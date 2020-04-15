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

-- | Define Functor instance for MyMaybe

instance Functor MyMaybe where
  fmap _ Empty = Empty
  fmap f (Only x) = Only (f x)

-- | Define Applicative instance for MyMaybe

instance Applicative MyMaybe where
   pure = Only

   Only f  <*> m       = fmap f m
   Empty <*> _m      = Empty

   Only _m1 *> m2      = m2
   Empty  *> _m2     = Empty

-- | Define Monad instance for MyMaybe

instance Monad MyMaybe where
  return  = Only
  Empty >>= f = Empty
  Only x >>= f = f x
  fail _       = Empty


-- | Define IList type

data IList n a where
  Cons :: a -> IList n a -> IList (S n) a
  Nil :: IList Z a

-- | Define iLen method

iLen :: IList n a -> Nat
iLen Nil = 0
iLen (Cons x xs) =  S (iLen xs)

-- | toList method
toList :: IList n a -> [a]
toList Nil = []
toList (Cons x xs) = x:toList xs

-- | toIList
--toIList:: [a] -> IList  a
--toIList [] = Nil 


-- | iMap method

iMap :: (a -> b) -> IList n a -> IList n b
iMap f Nil = Nil
iMap f (Cons x xs) = Cons (f x) (iMap f xs)

-- | Functor instance for IList

instance Functor (IList n) where
  fmap = iMap
