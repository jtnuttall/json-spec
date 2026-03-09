# Type-aligned generic deriving

This is a literate Haskell file that doubles as a test. I find these somewhat easier
to read than inline code for proposals so I thought I'd write it up this way.

Conversion back to Haskell is mechanical.

I explored 3 implementations for generic deriving, with increasing complexity:

1. Exact field alignment - not the most useful, but simple
2. Field alignment modulo a prefix - IMO the nicest middle ground
3. Totally heterogeneous fields - uses an auxiliary type level structure, most complex

I find it interesting that `HasJsonEncodingSpec` can be mechanically derived
with nothing other than `JStruct` and `HasField` at any level of complexity.
Not sure if this is novel, but it is neat.

It's basically a free ergonomic win and the type-level machinery enforces field
alignment statically, so _structurally correct_ instances should be guaranteed
by the typechecker.

The only thing that these deriving mechanisms lose is the ability to duplicate the JSON
field in the spec and in a type application on `Field`. I think this is a worthwhile
tradeoff. If I were going to provide this with the library, I'd probably just make it
optional instead of using `DefaultSignatures` to encourage a conscious choice to use
the generic path instead of manual instances.

The names in this module are chosen to be distinct and relatively clear for the
purposes of the document, but are generally not names I'd put in a public API.

## Module header

Literate Haskell requires me to declare the module header first:

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- ^ triggered by HGDecode's use of SameShapeAs; avoidable with custom type error,
-- but less verbose here and nice for totality guarantees.

module Main (main) where

import Data.Coerce (Coercible, coerce)
import Data.JsonSpec
  ( EncodingSpec, Field(Field), FieldSpec(JsonField), FieldValue
  , HasJsonDecodingSpec(DecodingSpec, fromJSONStructure), HasJsonEncodingSpec(toJSONStructure), JSONStructure
  , NP(Nil, (:*)), Ref(Ref)
  , Specification(JsonBool, JsonInt, JsonNum, JsonObject, JsonSpecOf, JsonString, JsonNullable)
  , SpecJSON(SpecJSON) , field , refield , type (:::) , type (::?)
  )
import Data.Kind (Type)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Records (HasField(getField))
import GHC.TypeLits (Symbol, AppendSymbol)
import Generics.SOP (All, HPure(hcpure), I(I), Proxy(Proxy), AllZip, htrans, SameShapeAs)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
```

## Exact Field Alignment

The simplest case.

### HasJsonEncodingSpec

Requires a single typeclass `Autofield`:

```haskell
class Autofield r (f :: FieldSpec) where
  autofield :: r -> Field f
```

The general case uses `field` directly. It is overlapped by the `JsonSpecOf` instance.

```haskell
instance {-# OVERLAPPABLE #-} (HasField k r (FieldValue req spec)) => Autofield r (JsonField k req spec) where
  autofield = field
```
> [!NOTE]
> We need to special-case on `JsonSpecOf` to coerce the `Ref` onto the field value.
> Since `Ref` is a newtype, `Coercible a (Ref a)` holds whenever `Ref`'s constructor is
> in scope, and `Coercible` lifts through `Maybe` by its representational role, so a
> single `Coercible fv (FieldValue req (JsonSpecOf a))` constraint handles both required
> fields (`FieldValue 'Required ... = Ref a`) and optional fields
> (`FieldValue 'Optional ... = Maybe (Ref a)`).

```haskell
instance
    ( HasField k r fv
    , Coercible fv (FieldValue req (JsonSpecOf a))
    ) =>
    Autofield r (JsonField k req (JsonSpecOf a))
  where
  autofield r = Field (coerce (getField @k r :: fv))
```

Then a straightforward use of `hcpure`:

```haskell
gtoJSONStructure
  :: forall r xs.
     (EncodingSpec r ~ JsonObject xs, All (Autofield r) xs)
  => r
  -> JSONStructure (EncodingSpec r)
gtoJSONStructure r = hcpure (Proxy @(Autofield r)) (autofield r)
```

### HasJsonDecodingSpec

To get the full benefit of `json-spec`, we'll want to verify that the JSON fields
align with the Haskell fields. Pretty straightforward to do this with `AllZip`, which
gives us a pairwise constraint between each `FieldSpec` and each type in the product
representation (`SOP.Code a ~ '[hs]`).

```haskell
class FieldDecode r (spec :: FieldSpec) (h :: Type) where
  fieldDecode :: Field spec -> I h
instance (Coercible (FieldValue req s) h, HasField key r h) => FieldDecode r (JsonField key req s) h where
  fieldDecode (Field v) = I (coerce v)
```

> [!NOTE]
> The interesting thing here: `HasField key r h` is never called in the body. It's just
> a type-level witness asserting that `r` has a field named `key` of type `h`. If the
> field is missing or has the wrong type, instance resolution fails.

This gives a very terse generic function:


```haskell
gfromJSONStructure
  :: forall xs hs a.
      ( SOP.Code a ~ '[hs]
      , SOP.Generic a
      , AllZip (FieldDecode a) xs hs
      )
  => NP Field xs -> A.Parser a
gfromJSONStructure = pure . SOP.productTypeTo . htrans (Proxy @(FieldDecode a)) (fieldDecode @a)
```

So long as any types requiring custom parsers are behind `Ref`s to their own datatypes
or newtypes with `HasJsonDecodingSpec` instances, this should _just work_.

#### Negative compilation test

The following block should fail to compile if tagged with `haskell` (untagged blocks are
excluded by `markdown-unlit`). This confirms that the generic deriving mechanism preserves
`json-spec`'s static field-alignment guarantees.

```
data Bad = Bad
  { bad_field1 :: Text
  , bad_field2 :: Int
  }
  deriving (Show, GHC.Generic)

instance SOP.Generic Bad

type JsonBad =
  JsonObject
    '[ "field1" ::: JsonString
     , "field2" ::: JsonInt
     ]

instance HasJsonDecodingSpec Bad where
  type DecodingSpec Bad = JsonBad
  fromJSONStructure = gfromJSONStructure
```

Expected error:

```
• No instance for ‘HasField "field1" Bad Text’
    arising from a use of ‘gfromJSONStructure’
• In the expression: gfromJSONStructure
  In an equation for ‘fromJSONStructure’:
      fromJSONStructure = gfromJSONStructure
  In the instance declaration for ‘HasJsonDecodingSpec Bad’ [GHC-39999]
```

## Field Alignment Modulo Prefix

Handles the common case where Haskell fields are prefixed to avoid name collisions.
The key mechanism is `hsKey ~ AppendSymbol prefix jsonKey`, which resolves the Haskell
field name from the JSON key at the type level.

### HasJsonEncodingSpec

We'll use a class `AutofieldPrefixed` which is essentially identical to `Autofield`, but
takes a `prefix` parameter and uses `refield` instead of `field` in the base case.

```haskell
class AutofieldPrefixed (prefix :: Symbol) r (f :: FieldSpec) where
  autofieldPrefixed :: r -> Field f
instance
  ( hsKey ~ AppendSymbol prefix jsonKey
  , HasField hsKey r fv
  , Coercible fv (FieldValue req (JsonSpecOf a))
  ) => AutofieldPrefixed prefix r (JsonField jsonKey req (JsonSpecOf a))
  where
  autofieldPrefixed r = Field (coerce (getField @hsKey r))
instance {-# OVERLAPPABLE #-}
  ( hsKey ~ AppendSymbol prefix jsonKey
  , HasField hsKey r (FieldValue req spec)
  ) => AutofieldPrefixed prefix r (JsonField jsonKey req spec)
  where
  autofieldPrefixed = refield @hsKey
```

And we get a similarly simple generic method:


```haskell
gtoJSONStructurePrefixed
  :: forall prefix r xs.
     (EncodingSpec r ~ JsonObject xs, All (AutofieldPrefixed prefix r) xs)
  => r
  -> JSONStructure (EncodingSpec r)
gtoJSONStructurePrefixed r = hcpure (Proxy @(AutofieldPrefixed prefix r)) (autofieldPrefixed @prefix r)
```

### HasJsonDecodingSpec

As with encoding:

```haskell
class FieldDecodePrefixed r (prefix :: Symbol) (spec :: FieldSpec) (h :: Type) where
  fieldDecodePrefixed :: Field spec -> I h
instance
  ( hsKey ~ AppendSymbol prefix jsonKey
  , HasField hsKey r h
  , Coercible (FieldValue req s) h
  )
  => FieldDecodePrefixed r prefix (JsonField jsonKey req s) h where
  fieldDecodePrefixed (Field v) = I (coerce v)
```

This gives a very terse generic function:


```haskell
gfromJSONStructurePrefixed
  :: forall prefix xs hs a.
      ( SOP.Code a ~ '[hs]
      , SOP.Generic a
      , AllZip (FieldDecodePrefixed a prefix) xs hs
      )
  => NP Field xs -> A.Parser a
gfromJSONStructurePrefixed = pure . SOP.productTypeTo .
  htrans (Proxy @(FieldDecodePrefixed a prefix)) (fieldDecodePrefixed @a @prefix)
```

## Totally Heterogeneous Fields

First, we need a way to declare both the JSON field and the Haskell field.
I'll use a simple type-level GADT and a list of mappings.

```haskell
data KeyMapping where
  FromHaskell :: FieldSpec -> Symbol -> KeyMapping

type SpecMap = [KeyMapping]
```

We'll also need a way to extract `JsonObject`s from mappings:

```haskell
type family JsonObjectFromMap (mp :: SpecMap) :: Specification where
    JsonObjectFromMap mp = JsonObject (SpecMapToFields mp)

type family SpecMapToFields (mp :: SpecMap) :: [FieldSpec] where
    SpecMapToFields '[] = '[]
    SpecMapToFields (fspec `FromHaskell` _ : more) = fspec : SpecMapToFields more
```


### HasJsonEncodingSpec

We'll need a typeclass to destructure the product into its fields.

> [!NOTE]
> A recursive typeclass is unfortunate here. `hcpure` requires a single function that
> works uniformly across all indices, but in the heterogeneous case the mapping between
> JSON key and Haskell key varies per position. `htrans`/`hcoerce` don't help either,
> since the source and target index lists differ in kind.
> So AFAICT we're stuck with per-index dispatch via recursion.

```haskell
class HAutoFields r (map :: SpecMap) where
  hautofields :: r -> NP Field (SpecMapToFields map)
instance HAutoFields r '[] where
  hautofields _ = Nil
instance
    ( HAutoFields r more
    , HasField hskey r fv
    , Coercible fv (FieldValue req (JsonSpecOf a))
    ) =>
    HAutoFields r (JsonField jsonkey req (JsonSpecOf a) `FromHaskell` hskey : more)
  where
  hautofields r = Field (coerce (getField @hskey r :: fv)) :* hautofields @r @more r
instance {-# OVERLAPPABLE #-}
    (HAutoFields r more, HasField hskey r (FieldValue req spec)) =>
    HAutoFields r (JsonField jsonkey req spec `FromHaskell` hskey : more)
  where
  hautofields r = refield @hskey r :* hautofields @r @more r
```

Which gives us a relatively simple helper for the public API:

```haskell
hgtoJsonStructure
  :: forall map r.
     (HAutoFields r map, EncodingSpec r ~ JsonObjectFromMap map)
  => r
  -> JSONStructure (EncodingSpec r)
hgtoJsonStructure = hautofields @r @map
```

### HasJsonDecodingSpec

We'll need a way to zip down the `SpecMap` and the list of Haskell types. As before,
I'll pass a phantom `r` so that we can inspect the record with `HasField`.

Since the `SpecMap` and `SOP.Code a` lists have different element kinds, the same
caveat as earlier applies: We'll need to manually recurse and destructure.

I'll use `SameShapeAs` to guarantee that the lists are the same length. This could
be done with a custom `TypeError` on degenerate cases, but `SameShapeAs` is less
noisy for a demo.

```haskell
class (SameShapeAs map tys) => HGDecode r (map :: SpecMap) (tys :: [Type]) where
  hgDecode :: NP Field (SpecMapToFields map) -> NP I tys
instance HGDecode r '[] '[] where
  hgDecode _ = Nil
instance
  ( HasField hsKey r ty
  , Coercible (FieldValue req s) ty
  , HGDecode r map tys
  )
  => HGDecode r (JsonField jsonKey req s `FromHaskell` hsKey : map) (ty : tys)
  where
  hgDecode (Field f :* fields) = I (coerce f) :* hgDecode @r @map @tys fields

hgfromJSONStructure
  :: forall map hs a.
      ( SOP.Code a ~ '[hs]
      , SOP.Generic a
      , HGDecode a map hs
      )
  => NP Field (SpecMapToFields map) -> A.Parser a
hgfromJSONStructure = pure . SOP.productTypeTo . hgDecode @a @map @hs
```

## Test Cases

Basic test cases to verify that the implementation works as intended.

### Exact Field Match

```haskell
data ExactFieldMatch = ExactFieldMatch
  { field1 :: Bool
  , field2 :: Text
  }
  deriving stock (Eq, GHC.Generic, Show)
  deriving (A.ToJSON, A.FromJSON) via (SpecJSON ExactFieldMatch)

instance SOP.Generic ExactFieldMatch

instance HasJsonEncodingSpec ExactFieldMatch where
  type EncodingSpec ExactFieldMatch =
    JsonObject
      '[ "field1" ::: JsonBool
       , "field2" ::: JsonString
       ]
  toJSONStructure = gtoJSONStructure

instance HasJsonDecodingSpec ExactFieldMatch where
  type DecodingSpec ExactFieldMatch =
    JsonObject
      '[ "field1" ::: JsonBool
       , "field2" ::: JsonString
       ]
  fromJSONStructure = gfromJSONStructure

testExactFieldMatch :: Spec
testExactFieldMatch =
  describe "ExactFieldMatch" $ do
    it "encodes to JSON" $
      A.toJSON (ExactFieldMatch True "blah blah")
        `shouldBe` A.object [("field1", A.Bool True), ("field2", A.String "blah blah")]
    it "decodes from JSON" $
      A.eitherDecode @ExactFieldMatch "{ \"field1\": false, \"field2\": \"hello world!\" }"
        `shouldBe` Right (ExactFieldMatch {field1 = False, field2 = "hello world!"})
    it "roundtrips" $ do
      let val = ExactFieldMatch False "roundtrip"
      A.eitherDecode (A.encode val) `shouldBe` Right val
```

#### Nested Ref

```haskell
data ExactFieldMatchNested = ExactFieldMatchNested
  { field1 :: Maybe Int
  , field2 :: ExactFieldMatch
  }
  deriving stock (Eq, GHC.Generic, Show)
  deriving (A.ToJSON, A.FromJSON) via (SpecJSON ExactFieldMatchNested)

instance SOP.Generic ExactFieldMatchNested

instance HasJsonEncodingSpec ExactFieldMatchNested where
  type EncodingSpec ExactFieldMatchNested =
    JsonObject
      '[ "field1" ::? JsonInt
       , "field2" ::: JsonSpecOf ExactFieldMatch
       ]
  toJSONStructure = gtoJSONStructure

instance HasJsonDecodingSpec ExactFieldMatchNested where
  type DecodingSpec ExactFieldMatchNested = EncodingSpec ExactFieldMatchNested
  fromJSONStructure = gfromJSONStructure

testExactFieldMatchNested :: Spec
testExactFieldMatchNested =
  describe "ExactFieldMatchNested" $ do
    it "encodes nested structure" $
      A.toJSON (ExactFieldMatchNested (Just 123) (ExactFieldMatch False "blah blah blah"))
        `shouldBe` A.object
          [ ("field1", A.Number 123)
          , ("field2", A.object [("field1", A.Bool False), ("field2", A.String "blah blah blah")])
          ]
    it "omits Nothing fields" $
      A.toJSON (ExactFieldMatchNested Nothing (ExactFieldMatch True "present"))
        `shouldBe` A.object
          [("field2", A.object [("field1", A.Bool True), ("field2", A.String "present")])]
    it "decodes nested JsonSpecOf" $
      A.eitherDecode @ExactFieldMatchNested
        "{ \"field1\": 42, \"field2\": { \"field1\": true, \"field2\": \"nested\" } }"
        `shouldBe` Right (ExactFieldMatchNested (Just 42) (ExactFieldMatch True "nested"))
    it "decodes with missing optional" $
      A.eitherDecode @ExactFieldMatchNested
        "{ \"field2\": { \"field1\": false, \"field2\": \"only\" } }"
        `shouldBe` Right (ExactFieldMatchNested Nothing (ExactFieldMatch False "only"))
    it "roundtrips" $ do
      let val = ExactFieldMatchNested (Just 99) (ExactFieldMatch True "rt")
      A.eitherDecode (A.encode val) `shouldBe` Right val
```

### Prefixed fields

```haskell
data PrefixedFields = PrefixedFields
  { prefixedFields_field1 :: Scientific
  , prefixedFields_field2 :: Maybe Int
  }
  deriving stock (Eq, GHC.Generic, Show)
  deriving (A.ToJSON, A.FromJSON) via (SpecJSON PrefixedFields)

instance SOP.Generic PrefixedFields

type PrefixedFields_Prefix = "prefixedFields_"

type JsonPrefixedFields =
  JsonObject
    '[ "field1" ::: JsonNum
     , "field2" ::? JsonInt
     ]

instance HasJsonEncodingSpec PrefixedFields where
  type EncodingSpec PrefixedFields = JsonPrefixedFields
  toJSONStructure = gtoJSONStructurePrefixed @PrefixedFields_Prefix

instance HasJsonDecodingSpec PrefixedFields where
  type DecodingSpec PrefixedFields = JsonPrefixedFields
  fromJSONStructure = gfromJSONStructurePrefixed @PrefixedFields_Prefix

testPrefixedFields :: Spec
testPrefixedFields = do
  describe "PrefixedFields" $ do
    it "encodes with remapped keys" $
      A.toJSON (PrefixedFields 187.5 (Just 3))
        `shouldBe` A.object [("field1", A.Number 187.5), ("field2", A.Number 3)]
    it "omits Nothing fields" $
      A.toJSON (PrefixedFields 187.5 Nothing)
        `shouldBe` A.object [("field1", A.Number 187.5)]
    it "decodes with remapped keys" $
      A.eitherDecode @PrefixedFields "{ \"field1\": 187.5, \"field2\": 3 }"
        `shouldBe` Right (PrefixedFields 187.5 (Just 3))
    it "decodes missing optional fields" $
      A.eitherDecode @PrefixedFields "{ \"field1\": 187.5 }"
        `shouldBe` Right (PrefixedFields 187.5 Nothing)
    it "roundtrips" $ do
      let val = PrefixedFields 42.0 (Just 7)
      A.eitherDecode (A.encode val) `shouldBe` Right val
```

#### Nested Ref

```haskell
data PrefixedNested = PrefixedNested
  { pn_inner :: ExactFieldMatch
  , pn_label :: Text
  }
  deriving stock (Eq, GHC.Generic, Show)
  deriving (A.ToJSON, A.FromJSON) via (SpecJSON PrefixedNested)

instance SOP.Generic PrefixedNested

instance HasJsonEncodingSpec PrefixedNested where
  type EncodingSpec PrefixedNested =
    JsonObject
      '[ "inner" ::: JsonSpecOf ExactFieldMatch
       , "label" ::: JsonString
       ]
  toJSONStructure = gtoJSONStructurePrefixed @"pn_"

instance HasJsonDecodingSpec PrefixedNested where
  type DecodingSpec PrefixedNested = EncodingSpec PrefixedNested
  fromJSONStructure = gfromJSONStructurePrefixed @"pn_"

testPrefixedNested :: Spec
testPrefixedNested = do
  describe "PrefixedNested (JsonSpecOf)" $ do
    it "encodes nested ref with prefix stripping" $
      A.toJSON (PrefixedNested (ExactFieldMatch True "hi") "lbl")
        `shouldBe` A.object
          [ ("inner", A.object [("field1", A.Bool True), ("field2", A.String "hi")])
          , ("label", A.String "lbl")
          ]
    it "decodes nested ref with prefix stripping" $
      A.eitherDecode @PrefixedNested
        "{ \"inner\": { \"field1\": false, \"field2\": \"x\" }, \"label\": \"y\" }"
        `shouldBe` Right (PrefixedNested (ExactFieldMatch False "x") "y")
    it "roundtrips" $ do
      let val = PrefixedNested (ExactFieldMatch True "rt") "lab"
      A.eitherDecode (A.encode val) `shouldBe` Right val
```

### Totally Heterogeneous Fields

```haskell
data HeterogeneousFields = HeterogeneousFields
  { theField_heterogeneous_01 :: Maybe Text
  , don't_write_fields_like_this :: Maybe (Maybe Scientific)
  , nested :: ExactFieldMatch
  }
  deriving stock (Eq, GHC.Generic, Show)
  deriving (A.ToJSON, A.FromJSON) via (SpecJSON HeterogeneousFields)

instance SOP.Generic HeterogeneousFields

type HeterogeneousFieldsMapping =
  '[ "theFirstField" ::? JsonString `FromHaskell` "theField_heterogeneous_01"
   , "theSecondField" ::? JsonNullable JsonNum `FromHaskell` "don't_write_fields_like_this"
   , "nestedObj" ::: JsonSpecOf ExactFieldMatch `FromHaskell` "nested"
   ]

instance HasJsonEncodingSpec HeterogeneousFields where
  type EncodingSpec HeterogeneousFields = JsonObjectFromMap HeterogeneousFieldsMapping
  toJSONStructure = hgtoJsonStructure @HeterogeneousFieldsMapping

instance HasJsonDecodingSpec HeterogeneousFields where
  type DecodingSpec HeterogeneousFields = JsonObjectFromMap HeterogeneousFieldsMapping
  fromJSONStructure = hgfromJSONStructure @HeterogeneousFieldsMapping

testHeterogeneousFields :: Spec
testHeterogeneousFields = do
  let inner = ExactFieldMatch True "inner"
  describe "HeterogeneousFields" $ do
    it "encodes with remapped keys including JsonSpecOf" $
      A.toJSON (HeterogeneousFields (Just "hello world") (Just Nothing) inner)
        `shouldBe` A.object
          [ ("theFirstField", "hello world")
          , ("theSecondField", A.Null)
          , ("nestedObj", A.object [("field1", A.Bool True), ("field2", A.String "inner")])
          ]
    it "omits Nothing optional fields" $
      A.toJSON (HeterogeneousFields Nothing (Just Nothing) inner)
        `shouldBe` A.object
          [ ("theSecondField", A.Null)
          , ("nestedObj", A.object [("field1", A.Bool True), ("field2", A.String "inner")])
          ]
    it "omits all absent optional fields" $
      A.toJSON (HeterogeneousFields Nothing Nothing inner)
        `shouldBe` A.object
          [("nestedObj", A.object [("field1", A.Bool True), ("field2", A.String "inner")])]
    it "decodes with remapped keys including JsonSpecOf" $
      A.eitherDecode
        "{ \"theFirstField\": \"the first field\", \"theSecondField\": 3, \
        \  \"nestedObj\": { \"field1\": false, \"field2\": \"dec\" } }"
        `shouldBe` Right (HeterogeneousFields (Just "the first field") (Just (Just 3)) (ExactFieldMatch False "dec"))
    it "decodes missing optional fields" $
      A.eitherDecode
        "{ \"theFirstField\": \"Should be here\", \
        \  \"nestedObj\": { \"field1\": true, \"field2\": \"req\" } }"
        `shouldBe` Right (HeterogeneousFields (Just "Should be here") Nothing (ExactFieldMatch True "req"))
    it "roundtrips" $ do
      let val = HeterogeneousFields (Just "blah") (Just (Just 1234)) inner
      A.eitherDecode (A.encode val) `shouldBe` Right val
```

## Test Runner

```haskell
main :: IO ()
main = hspec $ do
  describe "generics" $ do
    testExactFieldMatch
    testExactFieldMatchNested
    testPrefixedFields
    testPrefixedNested
    testHeterogeneousFields
```

## Summary

All three strategies rely on `HasField`, `hcpure`/`htrans`, and `Coercible` -- no
Template Haskell or custom `Rep` traversal required. The encoding side is particularly
clean: `hcpure` builds the entire `NP Field` product from a single record value, with
`JsonSpecOf` fields handled via `Coercible` to lift through `Ref` (and `Maybe (Ref ...)`
for optional fields). Decoding mirrors this via `htrans` with a phantom `HasField`
constraint for alignment verification.

### JsonEither

These patterns could be extended with hopefully only a little difficulty to `JsonEither`.
I stopped my exploration before doing so, though.

I think the main tricky thing with `JsonEither` would be mapping the injections cleanly,
although if you simplified `JsonEither` to determine the structure from a `Tag` alone
(perhaps with a fallback), this would probably be a lot easier.

It might be worth changing `JsonEither` to only work on tags (or at least to special case
on tags), because that would eliminate the risk of degenerate $`O(n^2)`$ decoding cases.
