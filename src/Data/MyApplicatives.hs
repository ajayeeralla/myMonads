{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.MyApplicatives where
import Data.Nat
import Data.MyTypes
import Data.MyFunctors
import Control.Monad (liftM,ap)

-- | Applicative Class
{-class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b-}


-- | Define Applicative instance for MyMaybe
instance Applicative MyMaybe where
   pure = Only
   Only f  <*> m       = fmap f m
   Empty <*> _m      = Empty
   Only _m1 *> m2      = m2
   Empty  *> _m2     = Empty

-- | IList Applicative instance
{-instance Functor (IList n) => Applicative (IList n) where
   -- pure :: a -> (IList n a)
   --(<*>) :: IList n (a -> b) -> IList n a -> IList n b
   pure x = Cons x Nil :: IList 1 a
   (Cons g Nil) <*> (Cons x Nil) = Cons (g x) Nil
   (Cons g gs) <*> (Cons x xs) = Cons (g x) (gs <*> xs) -}

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

instance Monoid m => Applicative (MyWriterT w m) where
   pure x = MyWriterT (pure (x, mempty))
   MyWriter g <*> MyWriter h = MyWriterT $ (\w0 -> runMyWriterT g w0 <*> runMyWriterT h wo)
