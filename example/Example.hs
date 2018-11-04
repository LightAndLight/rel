{-# language DataKinds, TypeFamilies, GADTs #-}
module Example where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (symbolVal)

import Rel
import Rel.Hask
import Rel.SQL

-- NameAge relates names to ages
data NameAge
  = NameAge
  { name :: String
  , age :: Int
  }

data instance Proj NameAge b where
  PrjName :: Proj NameAge String
  PrjAge :: Proj NameAge Int

instance ProjSQL NameAge where
  attrName PrjName = "name"
  attrName PrjAge = "age"

  sqlType PrjName = NotNull $ SQLString 100
  sqlType PrjAge = NotNull SQLInt

instance TableSQL NameAge where
  tableName (TableNameAge a) = symbolVal a
  enumAttrs f = [f PrjName, f PrjAge]

instance ProjHask NameAge where
  projFun PrjName = name
  projFun PrjAge = age

data instance Table SQL NameAge where
  TableNameAge :: Proxy "nameAge" -> Table SQL NameAge

query
  :: ( Scheme be NameAge
     , Value be Int
     , Order be Int
     )
  => Table be NameAge
  -> Rel be NameAge String
query nameAge =
  let
    lt10 = Select (Rel nameAge) (Prj PrjAge `Lt` Val 10)
  in
    Project lt10 PrjName

main :: IO ()
main = pure ()
