{-# language RankNTypes, TypeApplications, ScopedTypeVariables #-}
module Rel.SQL where

import Data.List (intercalate)
import Rel

data Expr
  = E_LT Expr Expr
  | E_EQ Expr Expr
  | E_AND Expr Expr
  | E_OR Expr Expr
  | E_NOT Expr
  | E_PRJ String
  | E_INT Int
  | E_STRING String
  | E_UNSAFE String
  deriving (Eq, Show)

data Target
  = All
  | Attr String
  deriving (Eq, Show)

data SQL
  = SELECT
  { _targets :: [Target]
  , _from :: [Either String SQL]
  , _where :: Maybe Expr
  }
  | UNION SQL SQL
  | EXCEPT SQL SQL
  deriving (Eq, Show)

type instance Equal SQL = Vacuous
type instance Order SQL = Vacuous
type instance Scheme SQL a = ProjSQL a
type instance Value SQL a = ValueSQL a

data SQLType b where
  SQLString :: Int -> SQLType String
  SQLInt :: SQLType Int

data Constrained f b where
  Nullable :: f a -> Constrained f (Maybe a)
  NotNull :: f a -> Constrained f a

class ValueSQL a where
  type TypeOf a
  toSQL :: a -> Expr
  getSQLType :: a -> SQLType (TypeOf a)

instance ValueSQL Int where
  type TypeOf Int = Int
  toSQL = E_INT
  getSQLType _ = SQLInt

renderSQLType :: SQLType b -> String
renderSQLType (SQLString n) = "varchar(" ++ show n ++ ")"
renderSQLType SQLInt = "int"

renderConstrained :: Constrained SQLType b -> String
renderConstrained (Nullable a) = renderSQLType a
renderConstrained (NotNull a) = renderSQLType a ++ " not null"

class ProjSQL a where
  attrName :: Proj a b -> String
  sqlType :: Proj a b -> Constrained SQLType b

class ProjSQL a => TableSQL a where
  tableName :: Table SQL a -> String
  enumAttrs :: (forall b. Proj a b -> r) -> [r]

sqlSchema :: forall d. TableSQL d => Table SQL d -> String
sqlSchema d =
  "CREATE TABLE " ++ tableName d ++ " (" ++
  intercalate
    ", "
    (zipWith (\a b -> a ++ " " ++ b)
       (enumAttrs (attrName @d))
       (enumAttrs (renderConstrained . sqlType @d))) ++ ")"

compileProp :: ProjSQL a => Prop SQL a Bool -> Expr
compileProp = go
  where
    go :: ProjSQL a => Prop SQL a b -> Expr
    go (a :&& b) = E_AND (go a) (go b)
    go (a :|| b) = E_OR (go a) (go b)
    go (Not a) = E_NOT (go a)
    go (a :== b) = E_EQ (go a) (go b)
    go (Lt a b) = E_LT (go a) (go b)
    go (Prj f) = E_PRJ (attrName f)
    go (Val a) = toSQL a

compile :: (TableSQL a) => Rel SQL a b -> SQL
compile (Union a b) =
  SELECT [All] [Right $ compile a] Nothing `UNION`
  SELECT [All] [Right $ compile b] Nothing
compile (Difference a b) =
  SELECT [All] [Right $ compile a] Nothing `EXCEPT`
  SELECT [All] [Right $ compile b] Nothing
compile (Product a b) =
  SELECT [All, All] [Right $ compile a, Right $ compile b] Nothing
compile (Project r f) =
  SELECT [Attr (attrName f)] [Right $ compile r] Nothing
compile (Select r p) =
  SELECT [All] [Right $ compile r] $ Just (compileProp p)
compile (Rel t) =
  SELECT [All] [Left $ tableName t] Nothing

optimization :: SQL -> Maybe SQL
optimization (SELECT [All] [Right (SELECT n b c)] Nothing) =
  Just $ SELECT n b c
optimization (SELECT n [Right (SELECT [All] b c)] Nothing) =
  Just $
  SELECT [All] [Right $ SELECT n b c] Nothing
optimization (SELECT n [Right (SELECT n' b (Just c))] (Just c'))
  | n == n' = Just $ SELECT n b (Just $ c `E_AND` c')
optimization _ = Nothing
