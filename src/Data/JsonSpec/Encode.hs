{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.JsonSpec.Encode (
  HasJsonEncodingSpec(..),
  StructureToJSON(..),
  encode,
) where


import Data.Aeson (ToJSON(toJSON), Key, Value(Object, Null))
import Data.Maybe (catMaybes)
import Data.JsonSpec.Spec
  ( Field(Field), FieldSpec(JsonField), JStruct, JStructVal(JStructVal)
  , JSONStructure, JsonSum(getJsonSum), KnownOptionality(optionalitySing)
  , Ref(unRef), SOptionality(SRequired, SOptional)
  , Specification(JsonArray), Tag, sym
  )
import Data.Proxy (Proxy(Proxy))
import Data.Scientific (Scientific)
import Data.SOP (NP, All, K(K), HCollapse(hcollapse), hcmap)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.TypeLits (KnownSymbol)
import Prelude
  ( Functor(fmap), Maybe(Just, Nothing)
  , ($), (.), Bool, Int, id, maybe
  )
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set


{- |
  Types of this class can be encoded to JSON according to a type-level
  'Specification'.
-}
class HasJsonEncodingSpec a where
  {- | The encoding specification. -}
  type EncodingSpec a :: Specification

  {- | Encode the value into the structure appropriate for the specification. -}
  toJSONStructure :: a -> JSONStructure (EncodingSpec a)
instance (HasJsonEncodingSpec a) => HasJsonEncodingSpec (Set a) where
  type EncodingSpec (Set a) = JsonArray (EncodingSpec a)
  toJSONStructure = fmap toJSONStructure . Set.toList


{- |
  This is like 'ToJSON', but specialized for our custom "json
  representation" types (i.e. the 'JSONStructure' type family). It is
  also closed (i.e. not exported, so the user can't add instances),
  because our json representation is closed.

  see 'Data.JsonSpec.Decode.StructureFromJSON' for an explanation about why we don't just use
  'ToJSON'.
-}
class StructureToJSON a where
  reprToJSON :: a -> Value
instance StructureToJSON Value where
  reprToJSON = id
instance StructureToJSON Bool where
  reprToJSON = toJSON
instance StructureToJSON Text where
  reprToJSON = toJSON
instance StructureToJSON Scientific where
  reprToJSON = toJSON
instance StructureToJSON Int where
  reprToJSON = toJSON
instance (All ToFieldPair specs) => StructureToJSON (NP Field specs) where
  reprToJSON np = Object $ KM.fromList . catMaybes $
    hcollapse $ hcmap (Proxy @ToFieldPair) (K . toFieldPair) np
instance (All AltToJSON specs) => StructureToJSON (JsonSum specs) where
  reprToJSON = hcollapse . hcmap (Proxy @AltToJSON) (K . altToJSON) . getJsonSum
instance (KnownSymbol const) => StructureToJSON (Tag const) where
  reprToJSON _proxy = toJSON (sym @const @Text)
instance (StructureToJSON a) => StructureToJSON [a] where
  reprToJSON = toJSON . fmap reprToJSON
instance StructureToJSON UTCTime where
  reprToJSON = toJSON
instance (StructureToJSON a) => StructureToJSON (Maybe a) where
  reprToJSON = maybe Null reprToJSON
instance
    (HasJsonEncodingSpec a, StructureToJSON (JStruct (EncodingSpec a)))
  =>
    StructureToJSON (Ref a)
  where
  reprToJSON = reprToJSON . toJSONStructure . unRef

{- |
  Encode a single t'Field' to a key-value pair (or nothing, for absent
  optional fields).

  Used by the 'StructureToJSON' instance for @NP Field@ to encode all
  fields in an object.
-}
class ToFieldPair field where
  toFieldPair :: Field field -> Maybe (Key, Value)
instance
  (KnownSymbol key, KnownOptionality req, StructureToJSON (JStruct spec))
  => ToFieldPair (JsonField key req spec)
  where
  toFieldPair (Field mval) =
    case optionalitySing @req of
      SRequired -> Just (sym @key, reprToJSON mval)
      SOptional -> case mval of
        Just val -> Just (sym @key, reprToJSON val)
        Nothing -> Nothing

class AltToJSON (spec :: Specification) where
  altToJSON :: JStructVal spec -> Value
instance (StructureToJSON (JStruct spec)) => AltToJSON spec where
  altToJSON (JStructVal val) = reprToJSON val

{-|
  Given a raw Haskell structure, directly encode it directly into an
  aeson Value without having to go through any To/FromJSON instances.

  See also: `Data.JsonSpec.eitherDecode`.
-}
encode :: StructureToJSON (JSONStructure spec) => Proxy spec -> JSONStructure spec -> Value
encode Proxy = reprToJSON
