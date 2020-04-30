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
{-# LANGUAGE UndecidableInstances  #-}

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
import Control.Monad (liftM,ap)

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

-- | MyMaybe Type

data MyMaybe a = Empty | Only a
  deriving ( Eq
           , Ord
           , Show
           )

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

-- | My Functors
-- | Functor instances of the types
instance Functor MyMaybe where
  --fmap :: (a-> b) -> MyMaybe a -> MyMaybe b
  fmap _ Empty = Empty
  fmap f (Only x) = Only (f x)

instance Functor (MyEither a b) where
  --fmap :: (a->b) -> MyEither a b a -> MyEither a b b
  fmap g (MyLeft x) = MyLeft x
  fmap g (Middle x) = Middle x
  fmap g (MyRight x) = MyRight (g x)

instance Functor (MyState s) where
  --fmap :: (a-> b) -> MyState s a -> MyState s b
  fmap g (MyState h) = MyState $ \s0 -> let (a0, s1)= h s0
                                         in  (g a0, s1)

instance Functor (MyWriter w) where
  fmap g (MyWriter x) = MyWriter $ let (a0, w0) = x
                                   in (g a0, w0)

instance Functor (MyReader e) where
  fmap g (MyReader h) = MyReader $ \e0 -> let a0 = h e0
                                          in g a0

instance Functor (MyError e b) where
  --fmap :: (a->b) -> MyError a b a -> MyError a b b
  fmap g (MyError (MyLeft x)) = MyError (MyLeft x)
  fmap g (MyError (Middle x)) = MyError (Middle x)
  fmap g (MyError (MyRight x)) = MyError (MyRight (g x))

-- | Applicative instances
-- | Define Applicative instance for MyMaybe
instance Applicative MyMaybe where
   pure = Only
   Only f  <*> m       = fmap f m
   Empty <*> _m      = Empty
   Only _m1 *> m2      = m2
   Empty  *> _m2     = Empty

-- | IList Applicative instance
{-instance Functor (Num n) => Applicative (IList n) where
   -- pure :: a -> (IList n a)
   --(<*>) :: IList n (a -> b) -> IList n a -> IList n b
   pure x = Cons x Nil :: IList 1 a
   (Cons g Nil) <*> (Cons x Nil) = Cons (g x) Nil
   (Cons g gs) <*> (Cons x xs) = Cons (g x) (gs <*> xs)
-}
-- | Either instance
{-instance Applicative (Either e) where
   --pure :: a -> (Either e a)
   pure = Right
   -- <*> :: (Either e (a -> b)) -> (Either e a) -> (Either e b)
   Left e <*> _ = Left e
   Right f <*> r = fmap f r -}

-- | MyEither instance
instance Applicative (MyEither e d) where
   --pure :: a -> (MyEither e d a)
   pure  = MyRight
   MyLeft e <*> _ = MyLeft e
   Middle d <*> _ = Middle d
   MyRight f <*> r = fmap f r

-- | MyError instance
instance Applicative (MyError e d) where
   pure x = MyError (MyRight x)
   MyError (MyLeft e) <*> _ = MyError (MyLeft e)
   MyError (Middle d) <*> _ = MyError (Middle d)
   MyError (MyRight f) <*> r =  fmap f r

-- | MyState instance
instance Applicative (MyState s) where
   -- pure :: a -> (MyState s a)
   pure x = MyState (\s -> (x,s))
   -- <*> :: (MyState s (a -> b)) -> (MyState s a) -> (MyState s b)
   MyState g <*> MyState h = MyState $ \s0 -> let (a0, s1) = h s0
                                                  (f, s2) = g s0
                                               in (f a0, s1)
-- | MyReader instance
instance Applicative (MyReader e) where
   pure x = MyReader (\_ -> x)
   MyReader g <*> MyReader h = MyReader (\x -> g x (h x))

-- | MyWriter instance

instance (Monoid w) => Applicative (MyWriter w) where
   pure x = MyWriter (x, mempty)
   MyWriter g <*> MyWriter h = MyWriter $ let (a0, w0) = h
                                              (f, _) = g
                                          in (f a0, w0)
instance Monad m => Applicative (MyMaybeT m) where
   --pure :: a -> MyMaybeT m a
   -- Since m is monad,
   pure = MyMaybeT . return . Only
   -- <*> :: (MyMaybeT m (a -> b)) -> (MyMaybeT m a) -> (MyMaybeT m b)
   mg <*> mh = MyMaybeT $ do
         mb_g <- runMyMaybeT mg
         case mb_g of
            Empty -> return Empty
            Only g -> do
               mb_x <- runMyMaybeT mh
               case mb_x of
                  Empty -> return Empty
                  Only x -> return (Only (g x))

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

-- | Functor instances for monad transformers
instance Functor m => Functor (MyMaybeT m) where
  --fmap :: (a->b) -> MyMaybeT m a -> MyMaybeT m b
  --from functor m, we have that
  --fmap :: (a -> b) -> m a -> m b
  --fmap :: (a -> b) -> (MyMaybe a) -> (MyMaybe b)
  -- fmapg :: (MyMaybe a) -> (MyMaybe b) ->  m (MyMaybe a) -> m (MyMaybe b)
  --fmaph :: (m (MyMaybe a) -> m (MyMaybe b)) -> m (MyMaybe a) -> m (MyMaybe b)
  fmap g (MyMaybeT h) = MyMaybeT (fmap (fmap g) h)

-- | Functor MyErrorT
instance Functor m => Functor (MyErrorT e b m) where
  fmap g (MyErrorT h) = MyErrorT (fmap (fmap g) h)

-- | Functor MyReaderT
instance Functor m => Functor (MyReaderT e m) where
-- | fmap :: (a -> b) -> (MyReaderT e m a) -> (MyReader e m b)
  fmap f x = MyReaderT $ runMyReaderT (fmap f x)

-- | Functor MyWriterT
instance Functor m => Functor (MyWriterT w m) where
  fmap f x = MyWriterT $ runMyWriterT (fmap f x)

-- | Functor MyStateT
instance Functor m => Functor (MyStateT s m) where
  fmap f x = MyStateT $ runMyStateT (fmap f x)

-- | Applicative instances for monad transformers
instance Monad m => Applicative (MyErrorT e b m) where
   pure = MyErrorT . return . MyRight
   mg <*> mh = MyErrorT $ do
      mb_g <- runMyErrorT mg
      case mb_g of
         MyLeft x -> return (MyLeft x)
         Middle x -> return (Middle x)
         MyRight g -> do
            mb_x <- runMyErrorT mh
            case mb_x of
               MyRight x -> return (MyRight (g x))
               MyLeft x -> return (MyLeft x)
               Middle x -> return (Middle x)

instance Applicative m => Applicative (MyReaderT e m) where
   -- Given pure :: a -> m a
   -- pure :: a -> MyReader e m a
   pure x = MyReaderT (\_ -> pure x)
   g <*> h = MyReaderT $ \e0 -> runMyReaderT g e0 <*> runMyReaderT h e0

{-instance (Monoid w, Applicative m) => Applicative (MyWriterT w m) where
   pure x = MyWriterT $ (pure (x, mempty))
   f <*> v = MyWriterT $ (runMyWriterT f) <*> (runMyReaderT v) -}
instance (Functor m, Monad m) => Applicative (MyStateT s m) where
    pure a = MyStateT $ \ s -> return (a, s)
    MyStateT mf <*> MyStateT mx = MyStateT $ \s -> do
        (f, s') <- mf s
        (x, s'') <- mx s'
        return (f x, s'')

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

-- | Monad instance for MyState
instance Monad (MyState s) where
  --return :: a -> MyState s a
  return x = MyState (\s0 -> (x, s0))
  --(>>=)  :: MyState s a -> (a -> MyState s b) -> MyState s b
  MyState g >>= h = MyState $ \s0 -> let (a0, s1) = g s0
                                         MyState f = h a0
                                     in f s1
-- | Monad instance for MyMonad
instance Monoid w => Monad (MyWriter w) where
  return x = MyWriter (x, mempty)
  MyWriter (a0, w0) >>= h = h a0

-- | Monad instance MyError
instance Monad (MyError e a) where
  return x = MyError (MyRight x)
  MyError (MyRight x) >>= f = f x

instance Monad (MyReader e) where
  return x = MyReader (\_ -> x)
  MyReader g >>= h = MyReader (\e0 -> let a0 = g e0
                                          MyReader f = h a0
                                        in f e0)
instance Monad m => Monad (MyMaybeT m) where
  return = MyMaybeT . return . Only
  mx >>= g = MyMaybeT $ do
    v <- runMyMaybeT mx
    case v of
      Empty -> return Empty
      Only x -> runMyMaybeT (g x)

instance Monad m => Monad (MyStateT s m) where
  return a = MyStateT $ \s -> return (a, s)
  m >>= k = MyStateT $ \s -> do
          (a, s') <- runMyStateT m s
          runMyStateT (k a) s'
