{-# language RankNTypes, TypeApplications, ScopedTypeVariables #-}
module Rel.SQL where

import Data.List (intercalate)
import Rel

data SQL
type instance Equal SQL = Vacuous
type instance Order SQL = Vacuous
type instance Scheme SQL a = ProjSQL a
type instance Value SQL a = ValueSQL a

class ValueSQL a where
  type TypeOf a
  toSQL :: a -> String
  getSQLType :: a -> SQLType (TypeOf a)

renderSQLType :: SQLType b -> String
renderSQLType (SQLString n) = "varchar(" ++ show n ++ ")"
renderSQLType SQLInt = "int"

renderConstrained :: Constrained SQLType b -> String
renderConstrained (Pass a) = renderSQLType a
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

renderProp :: ProjSQL a => Prop SQL a Bool -> String
renderProp = go
  where
    go :: ProjSQL a => Prop SQL a b -> String
    go (And a b) = go a ++ " AND " ++ go b
    go (Or a b) = go a ++ " OR " ++ go b
    go (Not a) = "NOT " ++ go a
    go (Equals a b) = go a ++ " = " ++ go b
    go (Lt a b) = go a ++ " < " ++ go b
    go (Prj f) = attrName f

renderRel :: (ProjSQL b, TableSQL a) => Rel SQL a b -> String
renderRel (Union a b) =
  "SELECT * FROM (" ++ renderRel a ++ ") UNION SELECT * FROM (" ++ renderRel b ++ ")"
renderRel (Difference a b) =
  "SELECT * FROM (" ++ renderRel a ++ ") EXCEPT SELECT * FROM (" ++ renderRel b ++ ")"
renderRel (Product a b) =
  "SELECT (*, *) FROM (" ++ renderRel a ++ ", " ++ renderRel b ++ ")"
renderRel (Project r f) =
  "SELECT " ++ attrName f ++ " FROM (" ++ renderRel r ++ ")"
renderRel (Select r p) =
  "SELECT * FROM (" ++ renderRel r ++ ") WHERE (" ++ renderProp p ++ ")"
renderRel (Rel t) = "SELECT * FROM " ++ tableName t
