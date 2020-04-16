module Data.MyApplicatives where
import Data.Nat
import Data.MyTypes
import Data.MyFunctors

-- | Define Applicative instance for MyMaybe

instance Applicative MyMaybe where
   pure = Only

   Only f  <*> m       = fmap f m
   Empty <*> _m      = Empty

   Only _m1 *> m2      = m2
   Empty  *> _m2     = Empty
