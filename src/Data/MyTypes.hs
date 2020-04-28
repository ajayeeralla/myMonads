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
  , MyEither (..)
  , MyState (..)
  , MyWriter (..)
  , MyReader (..)
  , MyError (..)
  , MyMaybeT (..)
  , MyErrorT (..)
  , MyStateT (..)
  , MyReaderT (..)
  , MyWriterT (..)
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
 Cons :: a -> IList n a -> IList ('S n) a
 Nil :: IList 'Z a

-- | Define iLen method

iLen :: IList n a -> Nat
iLen Nil = 0
iLen (Cons _ xs) =  S (iLen xs)

-- | toList method
toList :: IList n a -> [a]
toList Nil = []
toList (Cons x xs) = x:toList xs

-- | toIList
--toIList:: [a] -> IList  a
--toIList [] = Nil


-- | iMap method

iMap :: (a -> b) -> IList n a -> IList n b
iMap _ Nil = Nil
iMap f (Cons x xs) = Cons (f x) (iMap f xs)


-- | MyEither
data MyEither a b c = MyLeft a | Middle b | MyRight c
  deriving ( Eq
           , Ord
           , Read
           , Show
           )

-- | MyState
newtype MyState s a = MyState { runMyState :: s -> (a,s) }

-- | MyReader
newtype MyReader e a = MyReader { runMyReader :: e -> a }

-- | MyWriter
newtype MyWriter w a = MyWriter { runMyWriter :: (a, w) }

-- | MyError
newtype MyError e a c = MyError { runMyError :: MyEither e a c}

-- | Types related to transformers
newtype MyMaybeT m a = MyMaybeT { runMyMaybeT :: m (MyMaybe a) }

-- | MyWriterT
newtype MyWriterT w m a = MyWriterT { runMyWriterT :: m (a, w) }

-- | MyReaderT
newtype MyReaderT e m a = MyReaderT { runMyReaderT :: e -> m a }

-- | MyStateT
newtype MyStateT s m a = MyStateT { runMyStateT :: s -> m (a, s) }

-- | MyErrorT
newtype  MyErrorT e a m c = MyErrorT { runMyErrorT :: m (MyEither e a c) }
