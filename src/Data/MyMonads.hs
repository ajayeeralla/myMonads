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
