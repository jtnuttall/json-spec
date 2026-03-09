# Dev Notes

I've summarized various notes I made as I explored `json-spec`.

## Why?

The library is cool. I got very curious whether `json-spec` could
support type-aligned generic deriving of `ToJSON` and `FromJSON`
instances. It can.

## What?

### Type-aligned generic deriving

See `GENERICS.md` for a full, compiling draft of how this could work
(it's symlinked to `test/generics.lhs` and built as a test suite).

The coolest part of this IMO: `json-spec`'s DSL is rich enough to
support JSON encoding **without generics at all**.

For both encoding and decoding, the `Specification` DSL can be paired
with a simple auxiliary structure like:

```haskell
data KeyMapping where
  FromHaskell :: FieldSpec -> Symbol -> KeyMapping
```

The generic function can then traverse this:

- Use `HasField` to populate `JStruct` on encode
- Use `HasField` + `SOP.Generic` to populate the user type on decode

This lets users have their cake and eat it too in the vast majority
of cases. Since the `KeyMapping` type contains the JSON key
_and_ the expected Haskell key, changing one will result in a type
error.

### `sop-core`

I explored `NP` and `NS` for `JsonObject` and `JsonEither`. I noticed
`json-spec` was using a custom `eot` representation and thought
that a SOP representation might be beneficial.

I think SOP provides some distinct advantages over `eot`.
Notably, adopting SOP also forces the `JsonLet`/`JsonRef` removal
(see [below](#option-3-defunctionalization)).

- Simplifies the type families _and_ the encode/decode resolution
- Consolidates ad-hoc recursive typeclasses into SOP combinators:
  - With `NP`/`NS`, each of these becomes a single-instance class used
    via SOP combinators.
  - This somewhat improves composability, I think
- Makes generic deriving of `HasJsonEncodingSpec` and `HasJsonDecodingSpec`
  straightforward.
  - NOTE: Could do this without SOP at all. A recursive typeclass over
    the `eot` representation works too. Construction is harder, though, and
    requires traversal of the `Rep` - in which case it is worth having
    `productTypeTo`.
- Better error messages (SOP resolution happens per-field)
- Works better with formatters. `ormolu` and `fourmolu` do not really
  format deeply nested tuples very nicely IMO. Lots of rightward
  drift. An `infixr` operator like `:*` results in straight-line
  construction/deconstruction when passed through a formatter.
  - NOTE: Could technically do this with an optional `infixr` pattern
    synonym for tuple construction.

### `JsonLet`/`JsonRef`

`JsonLet`/`JsonRef` can't be nested for named references because `toJSONStructure`
and `fromJSONStructure` require the empty `env ~ '[]`. See the [Nested external refs reproducer](#nested-external-refs-reproducer)
below.

Generally, I think `JsonLet` may be trying to do two things at once, which creates
some tension:

1. Handling of anonymous/structural local bindings to DRY up specs
2. Handling of named external bindings to types having `toJSONStructure`/`fromJSONStructure`

This produces a few problems:

1. Composing across nested `JsonLet` boundaries is impossible. `Ref env spec` wraps
   `JStruct env spec`, but `JSONStructure = JStruct '[] spec`. If you have any environment
   around the external type you're trying to nest these can't unify.
2. Anonymous references can only be serialized between some sort of anonymous row-polymorphic
   structure; all references outside documentation in the project are named datatypes.
3. Specifications with `JsonLet` must use `EncodingSpec`/`DecodingSpec`. This is potentially
   very expensive during typechecking for nested objects.
4. It is not possible to share a type alias between encoding and decoding if you want to,
   unless all downstream structural types also share encoding and decoding.
5. You don't get a super-clean ref boundary at the type level for emission of JSON schema
   and/or OpenAPI.

I considered a few solutions here. I ended up going with option 3
(defunctionalization), which is admittedly the most radical, but is
also forced by the SOP migration.

#### Option 1: Sever the env in `Ref`

Instead of:

```haskell
newtype Ref env spec = Ref (JStruct env spec)
```

We could do:

```haskell
newtype Ref env spec = Ref (JStruct '[] spec)
```

Pros:

- Simple
- Solves the external ref problem

Cons:

- References can no longer refer to one another, supposing we want
  mutual recursion or the like.
- Anonymous internal references are impossible.

This is probably the simplest solution.

#### Option 2: Make `Ref` existential

This would require carrying dictionaries in a GADT:

```haskell
data Ref spec where
  Ref :: (StructureToJSON (JStruct env spec), StructureFromJSON (JStruct env spec))
      => JStruct env spec -> Ref spec
```

Pros:

- Solves the external ref problem (I think, I only briefly considered this)

Cons:

- Not simple
- Carries around dictionaries
- Breaks user-facing pattern matches, existential is not easily inspectable
- Makes `env` provenance confusing at best, nonlocal at worst

IMO strictly worse than options 1 and 3. Avoid.

#### Option 3: Defunctionalization

This involves **replacing both** `JsonLet` and `JsonRef` with a single type:

```haskell
JsonSpecOf :: Type -> Specification
```

Now, for `Ref` instead of:

```haskell
newtype Ref env spec = Ref (JStruct env spec)
```

I have:

```haskell
newtype Ref a = Ref a
```

Pros:

- Resolves the external reference problem.
- We resolve `JsonSpecOf` via `HasJsonEncodingSpec` or `HasJsonDecodingSpec` instances.
- No type-level recursion issues. The parameter has kind `Type`, and resolution
  of the underlying structure is offloaded onto instance resolution, which GHC
  does lazily.
- `JStruct` is simpler: `JStruct env spec` becomes `JStruct spec`
  - This should also make typeclass reduction easier on GHC
- Clear ref boundaries: `Ref` processing no longer needs to recurse over
  structure. It's a clean break. See [Ref naming](#ref-naming).

Cons:

- Huge breaking change
- Ability to create anonymous aliases in local scope is gone.
  - I am not sure this was load bearing? For primitives a `JsonAnnotated`
    is probably enough? For more complex types it seems like you'd want
    a concrete Haskell type for anything worth naming.
- Less clear story for ref naming. `JsonLet` gives clear names as `Symbol`s.
  See [Ref naming](#ref-naming).
- `JsonAnnotated` annotations cannot be trivially and globally modified for `JsonSpecOf`
  structures. When you reference another type via `JsonSpecOf`, that type's own
  spec controls its annotations. There is no mechanism to layer additional
  annotations from the referencing site.
  - The positive spin: Local reasoning is stronger.

> [!NOTE]
> SOP migration effectively forces defunctionalization. Adopting `NP`
> for `JsonObject` means `JStruct (JsonObject fields) = NP Field fields`,
> which requires `Field :: FieldSpec -> Type`.
>
> To keep `JsonLet`/`JsonRef` under SOP, the `env` has to go somewhere:
>
> 1. **Parameterize `Field` by `env`**: `Field env (field :: FieldSpec)`,
>    giving `NP (Field env) fields`. This prevents any composition of `JsonRef`
>    under `JsonObject`, which is unacceptable.
> 2. **Existentially quantify `env` inside `Field`**: This breaks pattern
>    matching and composability. Two `Field`s from different environments
>    become incomparable even if they hold the same data.

##### Ref naming

A few possible solutions:

1. Resolve names via `Typeable`. Cheap and couples ref name to Haskell
   name.
   - This might actually be a desirable coupling because ref names
     are mostly useful for OpenAPI schemas, in which case their naming is
     documentation more than anything, and being able to trace back to
     Haskell having just a ref name seems good.
   - This also gives easy access to the module and package name, from which
     the ref can be namespaced.
2. A type family `type family RefName a :: Symbol` for naming each data type
   at its definition site.
   - Coupled with the above solution, this seems nice to me. You can use
     `RefName` if it exists and `Typeable.tyConName` if it doesn't.

I think you can do a combination of the two: `Typeable` by default, with a
type family for overrides.

#### Nested external refs reproducer

Verified against upstream `json-spec-1.3.0.1` at commit `ec4bdb3`. `MyTest`
compiles; `MyTest2` does not.

```haskell
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.JsonSpec
  ( Field(Field), HasJsonEncodingSpec(EncodingSpec, toJSONStructure)
  , Ref(Ref), Specification(JsonLet, JsonNum, JsonObject, JsonRef, JsonString)
  , (:::), (::?)
  )
import Data.Scientific (Scientific)
import Data.Text (Text)
import Prelude (IO, Maybe, pure, (.), (<$>))

data MyTest = MyTest
  { thing1 :: Text
  , thing2 :: Maybe Scientific
  }

type JsonMyTest =
  JsonLet '[ '("theThing", JsonString), '("theOtherThing", JsonNum) ]
  (JsonObject
    '[ "thing1" ::: JsonRef "theThing"
     , "thing2" ::? JsonRef "theOtherThing"
     ]
  )

instance HasJsonEncodingSpec MyTest where
  type EncodingSpec MyTest = JsonMyTest
  toJSONStructure x = (Field (Ref x.thing1), (Field . Ref <$> x.thing2, ()))

data MyTest2 = MyTest2
  { myTest :: MyTest }

type JsonMyTest2 =
  JsonLet '[ '("field", EncodingSpec MyTest) ]
  (JsonObject
    '[ "myTest" ::: JsonRef "field" ])

instance HasJsonEncodingSpec MyTest2 where
  type EncodingSpec MyTest2 = JsonMyTest2
  toJSONStructure x = (Field (Ref (toJSONStructure x.myTest)), ())

main :: IO ()
main = pure ()
```

Expected error:

```
    • Couldn't match type: '[]
                     with: '[ '[ '("field", JsonMyTest)]]
      Expected: JStruct
                  '[ '[ '("field", JsonMyTest)]]
                  (JsonLet
                     ['("theThing", JsonString), '("theOtherThing", JsonNum)]
                     (JsonObject
                        ["thing1" ::: JsonRef "theThing",
                         "thing2" ::? JsonRef "theOtherThing"]))
        Actual: JSONStructure (EncodingSpec MyTest)
    • In the first argument of 'Ref', namely
        '(toJSONStructure x.myTest)'
```

### Open design questions

#### Re-export vs wrap `NP`

There is some question whether to re-export `NP`/`(:*)`/`Nil` from `sop-core`
directly or wrap them in a pattern synonym or newtype. Re-exporting is simpler
and avoids abstraction overhead, but coupling the public API to `sop-core` types
means users see SOP internals in type errors.
