{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.JsonSpec.Spec (
  JSONStructure,
  Specification(..),
  sym,
  Tag(..),
  Ref(..),
  Field(Field),
  FieldValue,
  field,
  refield,
  JStruct,
  Optionality(..),
  SOptionality(..),
  KnownOptionality(..),
  FieldSpec(..),
  JStructVal(JStructVal, getJStructVal),
  JsonSum(JsonSum, getJsonSum, L, R),
  Required,
  Optional,
  (:::),
  (::?),
) where


import Data.Aeson (Value)
import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import Data.Scientific (Scientific)
import Data.SOP (NP((:*)), NS(S, Z), All, Compose)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Records (HasField(getField))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified GHC.TypeError as GE
import Prelude (($), type (~), Bool, Eq, Int, Maybe, Show)


{-|
  Simple DSL for defining type level "specifications" for JSON
  data. Similar in spirit to (but not isomorphic with) JSON Schema.

  Intended to be used at the type level using @-XDataKinds@

  See 'JSONStructure' for how these map into Haskell representations.
-}
data Specification where
  JsonObject :: [FieldSpec] -> Specification
    {-^
      An object with the specified properties, each having its own
      specification.
    -}
  JsonString :: Specification
    {-^ An arbitrary JSON string. -}
  JsonNum :: Specification
    {-^ An arbitrary (floating point) JSON number. -}
  JsonInt :: Specification
    {-^ A JSON integer.  -}
  JsonArray :: Specification -> Specification
    {-^ A JSON array of values which conform to the given spec. -}
  JsonBool :: Specification
    {-^ A JSON boolean value. -}
  JsonNullable :: Specification -> Specification
    {-^
      A value that can either be @null@, or else a value conforming to
      the specification.

      E.g.:

      > type SpecWithNullableField =
      >   JsonObject '[
      >     Required "nullableProperty" (JsonNullable JsonString)
      >   ]
    -}
  JsonEither :: [Specification] -> Specification
    {-^
      One of several different specifications. Corresponds to json-schema
      "oneOf". Useful for encoding sum types.

      Takes a type-level list of specs. In the structural representation
      ('JStruct'), @JsonEither@ maps to 't:JsonSum', an n-ary sum built on
      @sop-core@'s 'NS'. Use 'L' to inject into the first branch and 'R'
      to shift to a later branch.

      Example:

      > data MyType
      >   = Foo Text
      >   | Bar Int
      >   | Baz UTCTime
      > instance HasJsonEncodingSpec MyType where
      >   type EncodingSpec MyType =
      >     JsonEither
      >       '[
      >         JsonObject '[
      >           Required "tag" (JsonTag "foo"),
      >           Required "content" JsonString
      >         ],
      >         JsonObject '[
      >           Required "tag" (JsonTag "bar"),
      >           Required "content" JsonInt
      >         ],
      >         JsonObject '[
      >           Required "tag" (JsonTag "baz"),
      >           Required "content" JsonDateTime
      >         ]
      >       ]
      >
      >   toJSONStructure = \case
      >     Foo t ->
      >       L (Field @"tag" (Tag @"foo")
      >         :* Field @"content" t
      >         :* Nil)
      >     Bar i ->
      >       R (L (Field @"tag" (Tag @"bar")
      >         :* Field @"content" i
      >         :* Nil))
      >     Baz dt ->
      >       R (R (L (Field @"tag" (Tag @"baz")
      >         :* Field @"content" dt
      >         :* Nil)))
    -}
  JsonTag :: Symbol -> Specification
    {-^ A constant string value -}
  JsonDateTime :: Specification
    {-^
      A JSON string formatted as an ISO-8601 string. In Haskell this
      corresponds to `Data.Time.UTCTime`, and in json-schema it corresponds
      to the "date-time" format.
    -}
  JsonSpecOf :: Type -> Specification
    {-^
      Embed another type's JSON spec by reference. This replaces the old
      @JsonLet@\/@JsonRef@ mechanism: instead of defining named bindings in a
      type-level environment, you simply point at the type whose
      'Data.JsonSpec.HasJsonEncodingSpec' \/ 'Data.JsonSpec.HasJsonDecodingSpec'
      instances should be used.
    -}
  JsonRaw :: Specification
    {-^ Some raw, uninterpreted JSON value -}
  JsonAnnotated :: forall k. [(Symbol, k)] -> Specification -> Specification
    {-^
      An annotation on a specification. This is purely for documentation
      purposes and has no effect on encoding or decoding. The annotations
      are a list of key-value pairs at the type level. Keys are always
      symbols (type-level strings). Values can be any kind @k@: strings
      ('Symbol'), booleans ('Bool'), natural numbers ('GHC.TypeLits.Nat'), or any
      custom promoted type the user defines. Within one list, all values
      must have the same kind.

      E.g.:

      > type AnnotatedUser =
      >   JsonAnnotated
      >     '[ '("description", "A user record")
      >      , '("example", "...")
      >      ]
      >     (JsonObject '[
      >       Required "name" JsonString,
      >       Optional "last-login" JsonDateTime
      >      ])
      >
      > type ReadOnlyObject =
      >   JsonAnnotated '[ '("readOnly", 'True) ] (JsonObject '[])
    -}


{-| Whether a field in a 'JsonObject' is required or optional. -}
data Optionality
  = Required {-^ The field is required -}
  | Optional {-^ The field is optional -}

{-| Singleton witness for 'Optionality', used to branch on required\/optional
    at the value level within a single typeclass instance. -}
data SOptionality (req :: Optionality) where
  SRequired :: SOptionality 'Required
  SOptional :: SOptionality 'Optional

{-| Demote a type-level 'Optionality' to its singleton witness. -}
class KnownOptionality (req :: Optionality) where
  optionalitySing :: SOptionality req
instance KnownOptionality 'Required where optionalitySing = SRequired
instance KnownOptionality 'Optional where optionalitySing = SOptional

{-| Specify a field in an object.  -}
data FieldSpec = JsonField Symbol Optionality Specification

{-| The field is required. -}
type Required key spec = JsonField key 'Required spec

{-| The field is optional. -}
type Optional key spec = JsonField key 'Optional spec

{-| Alias for 't:Required'. -}
type key ::: spec = Required key spec


{-| Alias for 't:Optional'. -}
type key ::? spec = Optional key spec

{-| The Haskell type that carries a field's value. Required fields hold
    the structural type directly; optional fields wrap it in 'Maybe'. -}
type family FieldValue (req :: Optionality) (spec :: Specification) :: Type where
  FieldValue 'Required s = JStruct s
  FieldValue 'Optional s = Maybe (JStruct s)

{-| Extract the value type from a fully-applied 'FieldSpec'. -}
type family FieldSpecValue (field :: FieldSpec) :: Type where
  FieldSpecValue (JsonField _ req spec) = FieldValue req spec

{-| Synonym for 'JStruct'. Retained for backwards compatibility with
    type signatures that reference the old name. -}
type JSONStructure spec = JStruct spec

{-| Structural representation of a single object field.

    Use the 'v:Field' pattern synonym for @Field \@\"name\" value@ syntax
    via type applications.
-}
newtype Field (field :: FieldSpec) = MkField (FieldSpecValue field)
{-# COMPLETE Field #-}

deriving stock instance (Show (FieldSpecValue field)) => Show (Field field)
deriving stock instance (Eq (FieldSpecValue field)) => Eq (Field field)

pattern Field :: forall key req spec. FieldSpecValue (JsonField key req spec) -> Field (JsonField key req spec)
pattern Field v = MkField v

{-|
  Given a Haskell record whose fields align with its 'Specification', use 'HasField' to lift into 't:Field'.
-}
field :: forall key src req spec. (HasField key src (FieldValue req spec)) => src -> Field (JsonField key req spec)
field = refield @key

{-|
  Given a Haskell record whose fields _do not_ align with its 'Specification', use 'HasField' to lift into 't:Field'
  by mapping the source key in the Haskell type to the key in the 'JsonObject' spec.
-}
refield
  :: forall srcKey dstKey src req spec
  .  (HasField srcKey src (FieldValue req spec))
  => src -> Field (JsonField dstKey req spec)
refield src = Field (getField @srcKey src)

{-| Wrapper around 'JStruct' that provides a type constructor suitable
    for use as an argument to 'NS'. 'JStruct' is a type family and
    cannot be partially applied; this newtype bridges that gap. -}
newtype JStructVal (spec :: Specification) = JStructVal {getJStructVal :: JStruct spec}

deriving stock instance (Show (JStruct spec)) => Show (JStructVal spec)
deriving stock instance (Eq (JStruct spec)) => Eq (JStructVal spec)

{-| Structural representation of 'JsonEither'. An n-ary sum built on
    @sop-core@'s 'NS'. Use the 'L' and 'R' pattern synonyms for
    construction and pattern matching. -}
newtype JsonSum (specs :: [Specification]) = JsonSum {getJsonSum :: NS JStructVal specs}

deriving stock instance All (Compose Show JStructVal) specs => Show (JsonSum specs)
deriving stock instance All (Compose Eq JStructVal) specs => Eq (JsonSum specs)

{-| Shift to a later branch of a 't:JsonSum'. -}
pattern R :: JsonSum specs -> JsonSum (spec ': specs)
pattern R specs <- JsonSum (S (JsonSum -> specs)) where
  R (JsonSum specs) = JsonSum (S specs)

{-| Inject into the first branch of a 't:JsonSum'. -}
pattern L :: JStruct spec -> JsonSum (spec ': specs)
pattern L spec = JsonSum (Z (JStructVal spec))

{-# COMPLETE L, R #-}

{-| Map a 'Specification' to its Haskell structural representation. -}
type family JStruct (spec :: Specification) :: Type where
  JStruct (JsonObject fields) =
    NP Field fields
  JStruct JsonString = Text
  JStruct JsonNum = Scientific
  JStruct JsonInt = Int
  JStruct (JsonArray spec) = [JStruct spec]
  JStruct JsonBool = Bool
  JStruct (JsonEither '[]) =
    GE.TypeError (GE.Text "JsonEither requires at least one branch")
  JStruct (JsonEither specs) = JsonSum specs
  JStruct (JsonTag tag) = Tag tag
  JStruct JsonDateTime = UTCTime
  JStruct (JsonNullable spec) = Maybe (JStruct spec)
  JStruct (JsonSpecOf ty) = Ref ty
  JStruct JsonRaw = Value
  JStruct (JsonAnnotated _annotations spec) =
    JStruct spec

instance (v ~ FieldValue req spec) => HasField k (NP Field (JsonField k req spec ': more)) v where
  getField (Field v :* _) = v

instance {-# overlappable #-} (HasField k (NP Field more) v) => HasField k (NP Field (notIt ': more)) v where
  getField (_ :* more) = getField @k more

{-|
  Newtype wrapper that marks a boundary between the closed structural
  encoding ('Data.JsonSpec.StructureToJSON'/'Data.JsonSpec.StructureFromJSON') and the user-defined
  'Data.JsonSpec.HasJsonEncodingSpec' and 'Data.JsonSpec.HasJsonDecodingSpec' instances.

  When @JsonSpecOf SomeType@ appears in a spec, the corresponding structural
  position is Ref SomeType, which delegates encoding and decoding to
  @SomeType@'s own instances.

  Example:

  > data Foo = Foo [Foo]
  > instance HasJsonEncodingSpec Foo where
  >   type EncodingSpec Foo = JsonArray (JsonSpecOf Foo)
  >   toJSONStructure (Foo fs) = map Ref fs
-}
newtype Ref a = Ref { unRef :: a }
  deriving stock (Show, Eq)


{-| Structural representation of 'JsonTag'. (I.e. a constant string value.) -}
data Tag (a :: Symbol) = Tag
  deriving stock (Show, Eq)

{- |
  Shorthand for demoting type-level strings.
  Use with -XTypeApplication, e.g.:

  This function doesn't really "go" in this module, it is only here because
  this module happens to be at the bottom of the dependency tree and so it is
  easy to stuff "reusable" things here, and I don't feel like creating a whole
  new module just for this function (although maybe I should).

  > sym @var
-}
sym
  :: forall a b.
     ( IsString b
     , KnownSymbol a
     )
  => b
sym = fromString $ symbolVal (Proxy @a)
