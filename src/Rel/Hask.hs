module Rel.Hask where

import Data.List ((\\), union)

import Rel

data Hask
type instance Equal Hask = Eq
type instance Order Hask = Ord
type instance Scheme Hask a = (Equal Hask a, ProjHask a)
type instance Value Hask a = ()

newtype instance Table Hask a where
  TableHask :: [a] -> Table Hask a

class ProjHask a where
  projFun :: Proj a b -> a -> b

check :: ProjHask a => Prop Hask a Bool -> a -> Bool
check = go
  where
    go :: (ProjHask a, Eq b) => Prop Hask a b -> a -> b
    go (a :&& b) = (&&) <$> go a <*> go b
    go (a :|| b) = (||) <$> go a <*> go b
    go (Not a) = not <$> go a
    go (a :== b) = (==) <$> go a <*> go b
    go (Lt a b) = (<) <$> go a <*> go b
    go (Prj p) = projFun p
    go (Val a) = const a

eval :: (Eq a, Eq b, ProjHask a, ProjHask b) => Rel Hask a b -> [b]
eval (Union a b) = union (eval a) (eval b)
eval (Difference a b) = eval a \\ eval b
eval (Product a b) = (,) <$> eval a <*> eval b
eval (Project r p) = projFun p <$> eval r
eval (Select r p) = filter (check p) (eval r)
eval (Rel (TableHask as)) = as
