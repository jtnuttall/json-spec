{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Decoding using specs. -}
module Data.JsonSpec.Decode (
  StructureFromJSON(..),
  HasJsonDecodingSpec(..),
  eitherDecode,
) where


import Data.Aeson ((<?>))
import Data.Aeson.Types
  ( FromJSON(parseJSON), Object, Parser, Value(Null), JSONPathElement(Key)
  , parseEither, withArray, withObject, withScientific, withText
  )
import Data.JsonSpec.Spec
  ( Field(Field), FieldSpec(JsonField), JStruct, JStructVal(JStructVal)
  , JSONStructure, JsonSum(JsonSum), KnownOptionality(optionalitySing)
  , Ref(Ref), SOptionality(SRequired, SOptional), Specification, Tag(Tag)
  , sym
  )
import Data.Foldable (foldMap')
import Data.Monoid (Alt(getAlt, Alt))
import Data.Proxy (Proxy(Proxy))
import Data.Scientific (Scientific)
import Data.SOP (NP, All, (:.:)(Comp), hcpure, hsequence', hapInjs)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.TypeLits (KnownSymbol)
import Prelude
  ( Applicative(pure), Either, Eq((==)), Functor(fmap)
  , Maybe(Just, Nothing), MonadFail(fail), Semigroup((<>))
  , Traversable(traverse), ($), (.), (<$>), Bool, Int, String
  )
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as Vector
import Control.Applicative ((<|>))


{- |
  Types of this class can be JSON decoded according to a type-level
  'Specification'.
-}
class HasJsonDecodingSpec a where
  {- | The decoding 'Specification'. -}
  type DecodingSpec a :: Specification

  {- |
    Given the structural encoding of the JSON data, parse the structure
    into the final type. The reason this returns a @'Parser' a@ instead of
    just a plain @a@ is because there may still be some invariants of the
    JSON data that the 'Specification' language is not able to express,
    and so you may need to fail parsing in those cases. For instance,
    'Specification' is not powerful enough to express "this field must
    contain only prime numbers".
  -}
  fromJSONStructure :: JSONStructure (DecodingSpec a) -> Parser a


{- |
  Analog of 'Data.Aeson.FromJSON', but specialized for decoding our
  "json representations", and closed to the user because the haskell
  representation scheme is fixed and not extensible by the user.

  We can't just use 'Data.Aeson.FromJSON' because the types we are using
  to represent "json data" (i.e. the 'JSONStructure' type family) already
  have 'Data.Aeson.ToJSON' instances. Even if we were to make a bunch of newtypes
  or whatever to act as the json representation (and therefor also force
  the user to do a lot of wrapping and unwrapping), that still wouldn't
  be sufficient because someone could always write an overlapping (or
  incoherent) 'Data.Aeson.ToJSON' instance of our newtype! This way we don't have
  to worry about any of that, and the types that the user must deal with
  when implementing 'reprParseJSON' can be simple tuples and such.
-}
class StructureFromJSON a where
  reprParseJSON :: Value -> Parser a
instance StructureFromJSON Value where
  reprParseJSON = pure
instance StructureFromJSON Text where
  reprParseJSON = withText "string" pure
instance StructureFromJSON Scientific where
  reprParseJSON = withScientific "number" pure
instance StructureFromJSON Int where
  reprParseJSON = parseJSON
instance StructureFromJSON Bool where
  reprParseJSON = parseJSON
-- NOTE: The previous approach called 'withObject' per field, which likely allocated a closure per field
-- (the simplifier is unlikely to merge or eliminate the validation branch in 'withObject') and created
-- a data dependency chain via per-field recursion on 'reprParseJSON'. This version calls 'withObject'
-- once and parses all fields from the same object reference.
instance (All FieldFromJSON specs) => StructureFromJSON (NP Field specs) where
  reprParseJSON = withObject "object" $ \o -> hsequence' $
      hcpure (Proxy @FieldFromJSON) $ Comp (parseField o)
instance (All AltFromJSON specs) => StructureFromJSON (JsonSum specs) where
  reprParseJSON = parseJsonEither
instance (KnownSymbol const) => StructureFromJSON (Tag const) where
  reprParseJSON =
    withText "constant" $ \c ->
      if c == sym @const then pure Tag
      else fail "unexpected constant value"
instance (StructureFromJSON a) => StructureFromJSON [a] where
  reprParseJSON =
    withArray
      "list"
      (fmap Vector.toList . traverse reprParseJSON)
instance StructureFromJSON UTCTime where
  reprParseJSON = parseJSON
instance (StructureFromJSON a) => StructureFromJSON (Maybe a) where
  reprParseJSON val = do
    case val of
      Null -> pure Nothing
      _ -> Just <$> reprParseJSON val
instance
    (HasJsonDecodingSpec a, StructureFromJSON (JStruct (DecodingSpec a)))
  =>
    StructureFromJSON (Ref a)
  where
  reprParseJSON val = do
    parsed <- reprParseJSON val
    Ref <$> fromJSONStructure parsed


{-|
  Directly decode some JSON accoring to a spec without going through
  any To/FromJSON instances.
-}
eitherDecode
  :: forall spec.
     (StructureFromJSON (JStruct spec))
   => Proxy (spec :: Specification)
  -> Value
  -> Either String (JStruct spec)
eitherDecode _spec =
  parseEither reprParseJSON


class FieldFromJSON (spec :: FieldSpec) where
  parseField :: Object -> Parser (Field spec)
instance (KnownSymbol key, KnownOptionality req, StructureFromJSON (JStruct spec)) => FieldFromJSON (JsonField key req spec) where
  parseField o =
    case KM.lookup (sym @key) o of
      Nothing -> case optionalitySing @req of
        SRequired -> fail $ "could not find key: " <> sym @key
        SOptional -> pure $ Field Nothing
      Just raw -> case optionalitySing @req of
        SRequired -> Field <$> (reprParseJSON raw <?> Key (sym @key))
        SOptional ->
          {- Symmetry with Encode requires that optional null round-trips -}
          let parse = Field . Just <$> (reprParseJSON raw <?> Key (sym @key))
           in case raw of
                Null -> parse <|> pure (Field Nothing)
                _ -> parse

class AltFromJSON (spec :: Specification) where
  parseAlt :: Value -> Parser (JStructVal spec)
instance (StructureFromJSON (JStruct spec)) => AltFromJSON spec where
  parseAlt = fmap JStructVal . reprParseJSON

{-|
  Try each branch of the sum left-to-right, taking the first that
  parses successfully:

  1. 'hcpure' builds a product of parsers, one per branch:
       @NP (Parser :.: JStructVal) specs@
  2. 'hapInjs' explodes the product into a list of sums, each injecting
     one branch into the coproduct:
       @[NS (Parser :.: JStructVal) specs]@
  3. @hsequence'@ on each 'NS' sequences the parser out, yielding
       @Parser (NS JStructVal specs)@.
  4. @foldMap' (Alt . hsequence')@ combines them via @(\<|>)@, picking
     the first success. 'foldMap'' is strict in the 'Alt' spine to
     avoid thunk buildup.
-}
parseJsonEither :: All AltFromJSON specs => Value -> Parser (JsonSum specs)
parseJsonEither v = fmap JsonSum .
  getAlt . foldMap' (Alt . hsequence') . hapInjs $
    hcpure (Proxy @AltFromJSON) $ Comp (parseAlt v)

