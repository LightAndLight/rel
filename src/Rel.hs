{-# language ConstraintKinds, FlexibleInstances #-}
module Rel where

import Data.Kind

data family Proj a :: * -> *
data family Table be :: * -> *
type family Value be a :: Constraint
type family Equal be :: * -> Constraint
type family Order be :: * -> Constraint

type family Scheme a b :: Constraint

data Prop be a b where
  (:&&) :: Prop be a Bool -> Prop be a Bool -> Prop be a Bool
  (:||) :: Prop be a Bool -> Prop be a Bool -> Prop be a Bool
  Not :: Prop be a Bool -> Prop be a Bool
  Prj :: Proj a b -> Prop be a b
  (:==) :: Equal be b => Prop be a b -> Prop be a b -> Prop be a Bool
  Lt :: Order be b => Prop be a b -> Prop be a b -> Prop be a Bool
  Val :: Value be a => a -> Prop be t a

data Rel be a b where
  Union :: Equal be b => Rel be t b -> Rel be t b -> Rel be t b
  Difference :: Equal be b => Rel be a b -> Rel be a b -> Rel be a b
  Product :: (Scheme be a, Scheme be b) => Rel be t a -> Rel be t b -> Rel be t (a, b)
  Project :: Scheme be a => Rel be t a -> Proj a b -> Rel be t b
  Select :: Scheme be a => Rel be t a -> Prop be a Bool -> Rel be t a
  Rel :: Table be a -> Rel be a a

class Vacuous a where
instance Vacuous a where
