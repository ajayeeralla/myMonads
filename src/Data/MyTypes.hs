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

module Data.MyTypes
  ( MyMaybe (..)
  , IList (..)
  , iMap
  , MyEither(..)
  , MyState (..)
  )
where
import Data.Nat

-- | MyMaybe Type

data MyMaybe a = Empty | Only a
  deriving ( Eq
           , Ord
           , Show
           )

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


-- | MyEither
data MyEither a b c = MyLeft a | Middle b | MyRight c
  deriving ( Eq
           , Ord
           , Read
           , Show
           )
-- | State
newtype MyState s a = MyState { runMyState :: s -> (a,s) }
