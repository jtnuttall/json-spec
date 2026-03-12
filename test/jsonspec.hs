{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-
  Because of GHC-69797, we need to disable all warnings in order to
  disable the very specific warning about TypeAbstractions that can't
  be disabled individually, but then we re-enable the specific warnings
  we most care about.
-}
{-# OPTIONS_GHC -Werror=missing-import-lists #-}

module Main (main) where

import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.JsonSpec
  ( Field(Field), Required, Optional
  , NP((:*), Nil)
  , JsonSum(L, R)
  , HasJsonDecodingSpec(DecodingSpec, fromJSONStructure)
  , HasJsonEncodingSpec(EncodingSpec, toJSONStructure), Ref(Ref, unRef)
  , SpecJSON(SpecJSON)
  , Specification
    ( JsonAnnotated, JsonArray, JsonBool, JsonDateTime, JsonEither, JsonInt
    , JsonNullable, JsonNum, JsonObject, JsonSpecOf, JsonRaw, JsonString
    , JsonTag
    )
  , Tag(Tag), (:::), (::?), eitherDecode
  , encode
  )
import Data.Proxy (Proxy(Proxy))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime(UTCTime))
import OM.Show (ShowJ(ShowJ))
import Prelude
  ( Applicative(pure), Bool(False, True), Either(Left, Right), Enum(toEnum)
  , Functor(fmap), Maybe(Just, Nothing), Monad((>>=)), Num(negate)
  , ($), (.), Eq, IO, Int, Show, String, realToFrac
  , map
  )
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified Data.Aeson as A


main :: IO ()
main =
  hspec $ do
    describe "json" $ do
      it "encodes product" $
        let
          actual :: ByteString
          actual = A.encode sampleTestObject
          expected :: ByteString
          expected = "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qoo\":true,\"qux\":100}"
        in
          actual `shouldBe` expected

      it "decodes product" $
        let
          actual :: Either String TestObj
          actual =
            A.eitherDecode
              "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":100,\"qoo\":true}"
          expected :: Either String TestObj
          expected = Right sampleTestObject
        in
          actual `shouldBe` expected

      it "encodes sum1" $
        let
          actual :: ByteString
          actual = A.encode $ TestA 0 "bar"
          expected :: ByteString
          expected = "{\"content\":{\"int-field\":0,\"txt-field\":\"bar\"},\"tag\":\"a\"}"
        in
          actual `shouldBe` expected

      it "encodes sum2" $
        let
          actual :: ByteString
          actual = A.encode TestB
          expected :: ByteString
          expected = "{\"tag\":\"b\"}"
        in
          actual `shouldBe` expected

      it "decodes sum1" $
        let
          actual :: Either String TestSum
          actual =
            A.eitherDecode
              "{\"content\":{\"int-field\":0,\"txt-field\":\"bar\"},\"tag\":\"a\"}"
          expected :: Either String TestSum
          expected = Right (TestA 0 "bar")
        in
          actual `shouldBe` expected

      it "decodes sum2" $
        let
          actual :: Either String TestSum
          actual = A.eitherDecode "{\"tag\":\"b\"}"
          expected :: Either String TestSum
          expected = Right TestB
        in
          actual `shouldBe` expected

      it "decodes UTCTime" $
        let
          actual :: Either String User
          actual =
            A.eitherDecode
              "{ \"name\": \"foo\", \"last-login\": \"1858-11-17T00:00:00Z\" }"

          expected :: Either String User
          expected =
            Right
              User
                { name = "foo"
                , lastLogin =
                    UTCTime (toEnum 0) 0
                }
        in
          actual `shouldBe` expected

      describe "optionality" $ do
        let
          obj :: TestOptionality
          obj =
            TestOptionality
              { toFoo = Nothing
              , toBar = Nothing
              , toBaz = Nothing
              , toQux = 1
              }

        it "encodes" $
          let
            actual :: ByteString
            actual = A.encode obj

            expected :: ByteString
            expected = "{\"bar\":null,\"baz\":null,\"qux\":1}"
          in
            actual `shouldBe` expected

        it "decodes missing fields" $
          let
            actual :: Either String TestOptionality
            actual = A.eitherDecode "{\"bar\":null,\"qux\":1}"

            expected :: Either String TestOptionality
            expected = Right obj
          in
            actual `shouldBe` expected

        it "decodes explicit null" $
          let
            actual :: Either String TestOptionality
            actual = A.eitherDecode "{\"bar\":null,\"baz\":null,\"qux\":1}"

            expected :: Either String TestOptionality
            expected = Right obj
          in
            actual `shouldBe` expected

        it "optional nullable round-trip: absent fields re-encode as null" $
          let
            -- Decode with all optional fields absent
            decoded :: Either String TestOptionality
            decoded = A.eitherDecode "{\"bar\":null,\"qux\":1}"

            -- Re-encode: absent 'baz' reappears as explicit null because
            -- toJSONStructure wraps in Just, and join collapses the distinction.
            reencoded :: ByteString
            reencoded = case decoded of
              Right v -> A.encode v
              Left _ -> ""
          in do
            decoded `shouldBe` Right obj
            reencoded `shouldBe` "{\"bar\":null,\"baz\":null,\"qux\":1}"

      describe "JsonSpecOf" $ do
        it "decodes nested ref" $
          let
            actual :: Either String Triangle
            actual =
              A.eitherDecode
                "{ \"vertex1\" : { \"x\": 1, \"y\": 2, \"z\": 3 }, \
                \  \"vertex2\" : { \"x\": 4, \"y\": 5, \"z\": 6 }, \
                \  \"vertex3\" : { \"x\": 7, \"y\": 8, \"z\": 9 } }"

            expected :: Either String Triangle
            expected =
              Right
                Triangle
                  { vertex1 = Vertex 1 2 3
                  , vertex2 = Vertex 4 5 6
                  , vertex3 = Vertex 7 8 9
                  }
          in
            actual `shouldBe` expected
        it "encodes nested ref" $
            let
              actual :: ByteString
              actual =
                A.encode
                  Triangle
                    { vertex1 = Vertex 1 2 3
                    , vertex2 = Vertex 4 5 6
                    , vertex3 = Vertex 7 8 9
                    }

              expected :: ByteString
              expected = "{\"vertex1\":{\"x\":1,\"y\":2,\"z\":3},\"vertex2\":{\"x\":4,\"y\":5,\"z\":6},\"vertex3\":{\"x\":7,\"y\":8,\"z\":9}}"
            in
              actual `shouldBe` expected

      describe "recursive types" $ do
        it "decodes" $
          let
            actual :: Either String LabelledTree
            actual =
              A.eitherDecode
                "{\"children\":[{\"children\":[{\"children\":[],\"label\":\"child1\"},{\"children\":[],\"label\":\"child2\"}],\"label\":\"parent\"}],\"label\":\"grandparent\"}"

            expected :: Either String LabelledTree
            expected =
              Right
                LabelledTree
                  { label = "grandparent"
                  , children =
                      [ LabelledTree
                          { label = "parent"
                          , children =
                              [ LabelledTree
                                  { label = "child1"
                                  , children = []
                                  }
                              , LabelledTree
                                  { label = "child2"
                                  , children = []
                                  }
                              ]
                          }
                      ]
                  }
          in
            actual `shouldBe` expected
        it "encodes" $
          let
            actual :: ByteString
            actual =
              A.encode
                LabelledTree
                  { label = "grandparent"
                  , children =
                      [ LabelledTree
                          { label = "parent"
                          , children =
                              [ LabelledTree
                                  { label = "child1"
                                  , children = []
                                  }
                              , LabelledTree
                                  { label = "child2"
                                  , children = []
                                  }
                              ]
                          }
                      ]
                  }
            expected :: ByteString
            expected = "{\"children\":[{\"children\":[{\"children\":[],\"label\":\"child1\"},{\"children\":[],\"label\":\"child2\"}],\"label\":\"parent\"}],\"label\":\"grandparent\"}"
          in
            actual `shouldBe` expected

      describe "nullable" $ do
        it "encodes product" $
          let
            actual :: ByteString
            actual = A.encode sampleTestObjectWithNull
            expected :: ByteString
            expected = "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qoo\":false,\"qux\":null}"
          in
            actual `shouldBe` expected

        it "decodes product" $
          let
            actual :: Either String TestObj
            actual =
              A.eitherDecode
                "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":null,\"qoo\":false}"
            expected :: Either String TestObj
            expected = Right sampleTestObjectWithNull
          in
            actual `shouldBe` expected

        it "explicit null on Optional (non-nullable) field decodes as Nothing" $
          let
            actual :: Either String TestObj
            actual =
              A.eitherDecode
                "{\"foo\":\"foo\",\"bar\":null,\"baz\":{\"foo\":\"foo2\",\"bar\":0},\"qux\":100,\"qoo\":true}"
            expected :: Either String TestObj
            expected = Right TestObj
              { foo = "foo"
              , bar = Nothing
              , baz = TestSubObj { foo2 = "foo2", bar2 = 0 }
              , qux = Just 100
              , qoo = True
              }
          in
            actual `shouldBe` expected

      it "Bad tag does not decode" $
        let
          actual :: Either String TestSum
          actual = A.eitherDecode "{\"tag\":\"c\"}"
          expected :: Either String TestSum
          expected = Left "Error in $.tag: unexpected constant value"
        in
          actual `shouldBe` expected

      describe "nested error paths" $ do
        it "annotates field key on type error" $
          let
            actual :: Either String TestObj
            actual = A.eitherDecode "{\"foo\":\"f\",\"bar\":1,\"baz\":{\"foo\":\"f2\",\"bar\":0},\"qux\":\"wrong\",\"qoo\":true}"
          in
            actual `shouldBe` Left "Error in $.qux: parsing Int failed, expected Number, but encountered String"

        it "annotates nested JsonSpecOf field path" $
          let
            actual :: Either String Triangle
            actual = A.eitherDecode
              "{\"vertex1\":{\"x\":1,\"y\":2,\"z\":3},\"vertex2\":{\"x\":1,\"y\":\"bad\",\"z\":3},\"vertex3\":{\"x\":1,\"y\":2,\"z\":3}}"
          in
            actual `shouldBe` Left "Error in $.vertex2.y: parsing Int failed, expected Number, but encountered String"

        it "annotates deeply nested path through JsonSpecOf" $
          let
            actual :: Either String AnnotatedTriangle
            actual = A.eitherDecode
              "{\"vertex1\":{\"x\":1,\"y\":2,\"z\":3},\"vertex2\":{\"x\":1,\"y\":2,\"z\":3},\"vertex3\":{\"x\":1,\"y\":2,\"z\":\"wrong\"}}"
          in
            actual `shouldBe` Left "Error in $.vertex3.z: parsing Int failed, expected Number, but encountered String"

      describe "direct encoding/decoding" $ do
        it "eitherDecode" $
          let
            actual =
              A.eitherDecode
                "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":null,\"qoo\":false}"
                >>= eitherDecode (Proxy @(EncodingSpec TestObj))
            expected =
              Right
                (Field @"foo" "foo"
                :* Field @"bar" (Just 1.0)
                :* Field @"baz"
                  (Field @"foo" "foo2"
                  :* Field @"bar" 0
                  :* Nil)
                :* Field @"qux" Nothing
                :* Field @"qoo" False
                :* Nil)
          in
            actual `shouldBe` expected

        it "encode" $
          let
            expected :: Maybe A.Value
            expected =
              A.decode "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":null,\"qoo\":false}"

            actual :: Maybe A.Value
            actual =
              Just $
                encode
                  (Proxy @(EncodingSpec TestObj))
                  ( Field @"foo" "foo"
                    :* Field @"bar" (Just 1.0)
                    :* Field @"baz"
                      (Field @"foo" "foo2"
                      :* Field @"bar" 0
                      :* Nil)
                    :* Field @"qux" Nothing
                    :* Field @"qoo" False
                    :* Nil)
          in
            actual `shouldBe` expected

      describe "raw values" $ do
        it "decodes" $
          let
            expected =
              Right
                (Field @"foo"
                  (
                    A.object
                      [ ("bar", A.String "barval")
                      , ("baz", A.toJSON [A.String "qux", A.Number 1.0, A.Bool False])
                      ]
                  )
                :* Nil)

            actual =
              A.eitherDecode
                "{ \"foo\": { \"bar\": \"barval\", \"baz\": [ \"qux\", 1, false ] } }"
              >>=
                eitherDecode (Proxy @( JsonObject '[ "foo" ::: JsonRaw ]))
          in
            actual `shouldBe` expected
        it "encodes" $
          let
            expected :: Maybe A.Value
            expected =
              A.decode
                "{ \"foo\": { \"bar\": \"barval\", \"baz\": [ \"qux\", 1, false ] } }"

            actual :: Maybe A.Value
            actual =
              Just $
                encode
                  (Proxy @( JsonObject '[ Required "foo" JsonRaw ]))
                  (Field @"foo"
                    (
                      A.object
                        [ ("bar", A.String "barval")
                        , ("baz", A.toJSON [A.String "qux", A.Number 1.0, A.Bool False])
                        ]
                    )
                  :* Nil)
          in
            actual `shouldBe` expected

      describe "HasField" $ do
        it "Basic HasField" $
          let
            expected :: Maybe TestHasField
            expected =
              Just
                TestHasField
                  { thfFoo = "foo"
                  , thfBar = 10
                  , thfBaz =
                      TestSubObj
                        { foo2 = "bar"
                        , bar2 = negate 10
                        }
                  }

            actual :: Maybe TestHasField
            actual =
              A.decode
                "{\
                \  \"foo\": \"foo\",\
                \  \"bar\": 10,\
                \  \"baz\": {\
                \    \"a_string\": \"bar\",\
                \    \"an_int\": -10\
                \  }\
                \}"
          in
            actual `shouldBe` expected

        it "missing optional fields" $
          let
            expected :: Maybe TestOptionalHasField
            expected =
              Just
                TestOptionalHasField
                  { foo = Nothing
                  , bar = Nothing
                  }

            actual :: Maybe TestOptionalHasField
            actual = A.decode "{}"
          in
            actual `shouldBe` expected

        it "supplied optional fields" $
          let
            expected :: Maybe TestOptionalHasField
            expected =
              Just
                TestOptionalHasField
                  { foo = Just "foo"
                  , bar = Just Nothing
                  }

            actual :: Maybe TestOptionalHasField
            actual = A.decode "{\"foo\": \"foo\", \"bar\": null}"
          in
            actual `shouldBe` expected
        it "mixed optional fields" $
          let
            expected :: Maybe TestOptionalHasField
            expected =
              Just
                TestOptionalHasField
                  { foo = Nothing
                  , bar = Just (Just "bar")
                  }

            actual :: Maybe TestOptionalHasField
            actual = A.decode "{\"bar\": \"bar\"}"
          in
            actual `shouldBe` expected

      describe "annotated" $ do
        it "encodes" $
          let
            actual :: ByteString
            actual =
              A.encode
                AnnotatedUser
                  { auName = "alice"
                  , auAge = 30
                  }

            expected :: ByteString
            expected = "{\"age\":30,\"name\":\"alice\"}"
          in
            actual `shouldBe` expected

        it "decodes" $
          let
            actual :: Either String AnnotatedUser
            actual = A.eitherDecode "{\"name\":\"alice\",\"age\":30}"

            expected :: Either String AnnotatedUser
            expected =
              Right
                AnnotatedUser
                  { auName = "alice"
                  , auAge = 30
                  }
          in
            actual `shouldBe` expected

        it "works with JsonSpecOf" $
          let
            actual :: ByteString
            actual =
              A.encode
                AnnotatedTriangle
                  { atVertex1 = AnnotatedVertex 1 2 3
                  , atVertex2 = AnnotatedVertex 4 5 6
                  , atVertex3 = AnnotatedVertex 7 8 9
                  }

            expected :: ByteString
            expected = "{\"vertex1\":{\"x\":1,\"y\":2,\"z\":3},\"vertex2\":{\"x\":4,\"y\":5,\"z\":6},\"vertex3\":{\"x\":7,\"y\":8,\"z\":9}}"
          in
            actual `shouldBe` expected

        it "decodes with JsonSpecOf" $
          let
            actual :: Either String AnnotatedTriangle
            actual =
              A.eitherDecode
                "{\"vertex1\":{\"x\":1,\"y\":2,\"z\":3},\"vertex2\":{\"x\":4,\"y\":5,\"z\":6},\"vertex3\":{\"x\":7,\"y\":8,\"z\":9}}"

            expected :: Either String AnnotatedTriangle
            expected =
              Right
                AnnotatedTriangle
                  { atVertex1 = AnnotatedVertex 1 2 3
                  , atVertex2 = AnnotatedVertex 4 5 6
                  , atVertex3 = AnnotatedVertex 7 8 9
                  }
          in
            actual `shouldBe` expected

        it "supports non-Symbol annotation values (e.g. Bool)" $
          let
            actual :: ByteString
            actual =
              A.encode
                AnnotatedWithBool
                  { awbName = "test"
                  }

            expected :: ByteString
            expected = "{\"name\":\"test\"}"
          in
            actual `shouldBe` expected

      describe "empty object" $ do
        it "decodes" $
          A.eitherDecode @EmptyObj "{}"
            `shouldBe` Right EmptyObj
        it "encodes" $
          A.encode EmptyObj
            `shouldBe` "{}"
        it "roundtrips" $
          A.eitherDecode (A.encode EmptyObj)
            `shouldBe` Right EmptyObj

      describe "optional JsonSpecOf" $ do
        it "encodes with present ref" $
          A.encode (OptionalRef (Just (Vertex 1 2 3)))
            `shouldBe` "{\"vertex\":{\"x\":1,\"y\":2,\"z\":3}}"
        it "encodes with absent ref" $
          A.encode (OptionalRef Nothing)
            `shouldBe` "{}"
        it "decodes with present ref" $
          A.eitherDecode @OptionalRef "{\"vertex\":{\"x\":1,\"y\":2,\"z\":3}}"
            `shouldBe` Right (OptionalRef (Just (Vertex 1 2 3)))
        it "decodes with absent ref" $
          A.eitherDecode @OptionalRef "{}"
            `shouldBe` Right (OptionalRef Nothing)
        it "roundtrips present" $ do
          let val = OptionalRef (Just (Vertex 4 5 6))
          A.eitherDecode (A.encode val) `shouldBe` Right val
        it "roundtrips absent" $ do
          let val = OptionalRef Nothing
          A.eitherDecode (A.encode val) `shouldBe` Right val


sampleTestObject :: TestObj
sampleTestObject =
  TestObj
    { foo = "foo"
    , bar = Just 1
    , baz =
        TestSubObj
          { foo2 = "foo2"
          , bar2 = 0
          }
    , qux = Just 100
    , qoo = True
    }


sampleTestObjectWithNull :: TestObj
sampleTestObjectWithNull =
  TestObj
    { foo = "foo"
    , bar = Just 1
    , baz =
        TestSubObj
          { foo2 = "foo2"
          , bar2 = 0
          }

    , qux = Nothing
    , qoo = False
    }


data TestSum
  = TestA Int Text
  | TestB
  deriving stock (Eq, Show)
  deriving ToJSON via (SpecJSON TestSum)
  deriving FromJSON via (SpecJSON TestSum)
instance HasJsonEncodingSpec TestSum where
  type EncodingSpec TestSum =
    JsonEither
      '[
        JsonObject '[
          Required "tag" (JsonTag "a"),
          Required "content" (JsonObject [
            Required "int-field" JsonInt,
            Required "txt-field" JsonString
          ])
        ],
        JsonObject '[
          Required "tag" (JsonTag "b")
        ]
      ]
  toJSONStructure = \case
    TestA i t ->
      L (Field @"tag" (Tag @"a")
        :* Field @"content"
          ( Field @"int-field" i
          :* Field @"txt-field" t
          :* Nil
          )
        :* Nil)
    TestB ->
      R (L
        ( Field @"tag" (Tag @"b")
        :* Nil
        ))
instance HasJsonDecodingSpec TestSum where
  type DecodingSpec TestSum = EncodingSpec TestSum
  fromJSONStructure = \case
    L (Field @"tag" Tag
        :* Field @"content"
          (Field @"int-field" int
          :* Field @"txt-field" txt
          :* Nil)
        :* Nil)
      ->
        pure (TestA int txt)
    R _ ->
      pure TestB


data TestOptionalHasField = TestOptionalHasField
  { foo :: Maybe Text
  , bar :: Maybe (Maybe Text)
  }
  deriving stock (Show, Eq)
  deriving FromJSON via (SpecJSON TestOptionalHasField)
instance HasJsonDecodingSpec TestOptionalHasField where
  type DecodingSpec TestOptionalHasField =
    JsonObject
     '[ "foo" ::? JsonString
      , "bar" ::? JsonNullable JsonString
      ]
  fromJSONStructure v =
    pure
      TestOptionalHasField
        { foo = v.foo
        , bar = v.bar
        }


data TestObj = TestObj
  { foo :: Text
  , bar :: Maybe Scientific
  , baz :: TestSubObj
  , qux :: Maybe Int
  , qoo :: Bool
  }
  deriving stock (Show, Eq)
  deriving ToJSON via (SpecJSON TestObj)
  deriving FromJSON via (SpecJSON TestObj)
instance HasJsonEncodingSpec TestObj where
  type EncodingSpec TestObj =
    JsonObject
      '[
        Required "foo" JsonString,
        Optional "bar" JsonNum,
        Required "baz" (EncodingSpec TestSubObj),
        Required "qux" (JsonNullable JsonInt),
        Required "qoo" JsonBool
      ]
  toJSONStructure TestObj { foo , bar , baz, qux, qoo } =
    Field @"foo" foo
    :* (Field @"bar" . fmap realToFrac) bar
    :* Field @"baz" (toJSONStructure baz)
    :* Field @"qux" qux
    :* Field @"qoo" qoo
    :* Nil
instance HasJsonDecodingSpec TestObj where
  type DecodingSpec TestObj = EncodingSpec TestObj
  fromJSONStructure
      (Field @"foo" foo
      :* Field @"bar" bar
      :* Field @"baz" rawBaz
      :* Field @"qux" qux
      :* Field @"qoo" qoo
      :* Nil)
    = do
      baz <- fromJSONStructure rawBaz
      pure TestObj { foo, bar, baz, qux, qoo }


data TestSubObj = TestSubObj
  { foo2 :: Text
  , bar2 :: Int
  }
  deriving stock (Show, Eq)
instance HasJsonEncodingSpec TestSubObj where
  type EncodingSpec TestSubObj =
    JsonObject
      '[ Required "foo" JsonString
       , Required "bar" JsonInt
       ]
  toJSONStructure TestSubObj { foo2 , bar2 } =
    Field @"foo" foo2
    :* Field @"bar" bar2
    :* Nil
instance HasJsonDecodingSpec TestSubObj where
  type DecodingSpec TestSubObj = EncodingSpec TestSubObj
  fromJSONStructure
      (Field @"foo" foo2
      :* Field @"bar" bar2
      :* Nil)
    =
      pure TestSubObj {foo2 , bar2}


data User = User
  { name :: Text
  , lastLogin :: UTCTime
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON User)
instance HasJsonEncodingSpec User where
  type EncodingSpec User =
    JsonObject
      '[ Required "name" JsonString
       , Required "last-login" JsonDateTime
       ]
  toJSONStructure user =
    Field @"name" (name user)
    :* Field @"last-login" (lastLogin user)
    :* Nil
instance HasJsonDecodingSpec User where
  type DecodingSpec User = EncodingSpec User
  fromJSONStructure
      (Field @"name" name
      :* Field @"last-login" lastLogin
      :* Nil)
    =
      pure User { name , lastLogin }


data Vertex = Vertex
  { x :: Int
  , y :: Int
  , z :: Int
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON Vertex)
instance HasJsonEncodingSpec Vertex where
  type EncodingSpec Vertex =
    JsonObject
      '[ Required "x" JsonInt
       , Required "y" JsonInt
       , Required "z" JsonInt
       ]
  toJSONStructure Vertex {x, y, z} =
    Field @"x" x
    :* Field @"y" y
    :* Field @"z" z
    :* Nil
instance HasJsonDecodingSpec Vertex where
  type DecodingSpec Vertex = EncodingSpec Vertex
  fromJSONStructure
      (Field @"x" x
      :* Field @"y" y
      :* Field @"z" z
      :* Nil)
    =
      pure Vertex { x, y, z }


data Triangle = Triangle
  { vertex1 :: Vertex
  , vertex2 :: Vertex
  , vertex3 :: Vertex
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON Triangle)

instance HasJsonEncodingSpec Triangle where
  type EncodingSpec Triangle =
      (JsonObject
        '[ Required "vertex1" (JsonSpecOf Vertex)
         , Required "vertex2" (JsonSpecOf Vertex)
         , Required "vertex3" (JsonSpecOf Vertex)
         ])
  toJSONStructure Triangle {vertex1, vertex2, vertex3} =
    Field @"vertex1" (Ref vertex1)
    :* Field @"vertex2" (Ref vertex2)
    :* Field @"vertex3" (Ref vertex3)
    :* Nil

instance HasJsonDecodingSpec Triangle where
  type DecodingSpec Triangle = EncodingSpec Triangle
  fromJSONStructure
      (Field @"vertex1" (Ref vertex1)
      :* Field @"vertex2" (Ref vertex2)
      :* Field @"vertex3" (Ref vertex3)
      :* Nil)
    = do
      pure Triangle{vertex1, vertex2, vertex3}


data LabelledTree = LabelledTree
  { label :: Text
  , children :: [LabelledTree]
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON LabelledTree)
instance HasJsonEncodingSpec LabelledTree where
  type EncodingSpec LabelledTree =
    JsonObject
      '[Required "label" JsonString
       ,Required "children" (JsonArray (JsonSpecOf LabelledTree))
       ]
  toJSONStructure LabelledTree {label , children } =
    Field @"label" label
    :* Field @"children" (map Ref children)
    :* Nil
instance HasJsonDecodingSpec LabelledTree where
  type DecodingSpec LabelledTree = EncodingSpec LabelledTree
  fromJSONStructure
      (Field @"label" label
      :* Field @"children" children_
      :* Nil
      )
    = do
      let children = map unRef children_
      pure LabelledTree { label , children }


data TestOptionality = TestOptionality
  { toFoo :: Maybe Int
  , toBar :: Maybe Int
  , toBaz :: Maybe Int
  , toQux :: Int
  }
  deriving (ToJSON, FromJSON) via (SpecJSON TestOptionality)
  deriving (Show) via (ShowJ TestOptionality)
  deriving stock (Eq)
instance HasJsonEncodingSpec TestOptionality where
  type EncodingSpec TestOptionality =
    JsonObject
      '[ "foo" ::? JsonInt
       , Required "bar" (JsonNullable JsonInt)
       , Optional "baz" (JsonNullable JsonInt)
       , Required "qux" JsonInt
       ]

  toJSONStructure TestOptionality { toFoo , toBar , toBaz , toQux } =
    Field @"foo" toFoo
    :* Field @"bar" toBar
    :* Field @"baz" (Just toBaz) -- when encoding, prefer explicit null for testing.
    :* Field @"qux" toQux
    :* Nil
instance HasJsonDecodingSpec TestOptionality where
  type DecodingSpec TestOptionality = EncodingSpec TestOptionality

  fromJSONStructure
      (Field @"foo" toFoo
      :* Field @"bar" toBar
      :* Field @"baz" (join -> toBaz)
      :* Field @"qux" toQux
      :* Nil)
    =
      pure TestOptionality { toFoo , toBar , toBaz, toQux }


data TestHasField = TestHasField
  { thfFoo :: Text
  , thfBar :: Int
  , thfBaz :: TestSubObj
  }
  deriving stock (Show, Eq)
  deriving (FromJSON) via (SpecJSON TestHasField)
instance HasJsonDecodingSpec TestHasField where
  type DecodingSpec TestHasField =
    JsonObject
      '[ "foo" ::: JsonString
       , "bar" ::: JsonInt
       , "baz" ::: JsonObject
                    '[ "a_string" ::: JsonString
                     ,   "an_int" ::: JsonInt
                     ]
       ]
  fromJSONStructure val =
    pure
      TestHasField
        { thfFoo = val.foo
        , thfBar = val.bar
        , thfBaz =
            TestSubObj
              { foo2 = val.baz.a_string
              , bar2 = val.baz.an_int
              }

        }
{- ========================================================================== -}


{- Empty object test. -}
{- ========================================================================== -}

data EmptyObj = EmptyObj
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON EmptyObj)
instance HasJsonEncodingSpec EmptyObj where
  type EncodingSpec EmptyObj = JsonObject '[]
  toJSONStructure EmptyObj = Nil
instance HasJsonDecodingSpec EmptyObj where
  type DecodingSpec EmptyObj = EncodingSpec EmptyObj
  fromJSONStructure Nil = pure EmptyObj


{- Optional JsonSpecOf test. -}
{- ========================================================================== -}

newtype OptionalRef = OptionalRef
  { vertex :: Maybe Vertex }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON OptionalRef)
instance HasJsonEncodingSpec OptionalRef where
  type EncodingSpec OptionalRef =
    JsonObject '[ "vertex" ::? JsonSpecOf Vertex ]
  toJSONStructure OptionalRef { vertex } =
    Field @"vertex" (fmap Ref vertex) :* Nil
instance HasJsonDecodingSpec OptionalRef where
  type DecodingSpec OptionalRef = EncodingSpec OptionalRef
  fromJSONStructure (Field @"vertex" v :* Nil) =
    pure OptionalRef { vertex = fmap unRef v }


{- Annotated test. -}
{- ========================================================================== -}

data AnnotatedUser = AnnotatedUser
  { auName :: Text
  ,  auAge :: Int
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON AnnotatedUser)
instance HasJsonEncodingSpec AnnotatedUser where
  type EncodingSpec AnnotatedUser =
    JsonAnnotated
      '[ '("description", "A user with a name and age")
       , '("example", "{\"name\": \"alice\", \"age\": 30}")
       ]
      (JsonObject
        '[ Required "name" JsonString
         , Required "age" JsonInt
         ])
  toJSONStructure AnnotatedUser { auName, auAge } =
    Field @"name" auName
    :* Field @"age" auAge
    :* Nil
instance HasJsonDecodingSpec AnnotatedUser where
  type DecodingSpec AnnotatedUser = EncodingSpec AnnotatedUser
  fromJSONStructure
      (Field @"name" auName
      :* Field @"age" auAge
      :* Nil)
    =
      pure AnnotatedUser { auName, auAge }


data AnnotatedVertex = AnnotatedVertex
  { avX :: Int
  , avY :: Int
  , avZ :: Int
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON AnnotatedVertex)
instance HasJsonEncodingSpec AnnotatedVertex where
  type EncodingSpec AnnotatedVertex =
    JsonAnnotated
      '[ '("description", "A 3D vertex") ]
      (JsonObject
        '[ Required "x" JsonInt
         , Required "y" JsonInt
         , Required "z" JsonInt
         ])
  toJSONStructure AnnotatedVertex { avX, avY, avZ } =
    Field @"x" avX
    :* Field @"y" avY
    :* Field @"z" avZ
    :* Nil
instance HasJsonDecodingSpec AnnotatedVertex where
  type DecodingSpec AnnotatedVertex = EncodingSpec AnnotatedVertex
  fromJSONStructure
      (Field @"x" avX
      :* Field @"y" avY
      :* Field @"z" avZ
      :* Nil)
    =
      pure AnnotatedVertex { avX, avY, avZ }


-- NOTE: Annotations cannot be trivially and globally modified for
-- 'JsonSpecOf' structures. This is a trade-off but has some advantages
-- for local reasoning.
data AnnotatedTriangle = AnnotatedTriangle
  { atVertex1 :: AnnotatedVertex
  , atVertex2 :: AnnotatedVertex
  , atVertex3 :: AnnotatedVertex
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON AnnotatedTriangle)

instance HasJsonEncodingSpec AnnotatedTriangle where
  type EncodingSpec AnnotatedTriangle =
      JsonAnnotated
        '[ '("description", "A triangle with three vertices") ]
        (JsonObject
          '[ Required "vertex1" (JsonSpecOf AnnotatedVertex)
           , Required "vertex2" (JsonSpecOf AnnotatedVertex)
           , Required "vertex3" (JsonSpecOf AnnotatedVertex)
           ])
  toJSONStructure AnnotatedTriangle { atVertex1, atVertex2, atVertex3 } =
    Field @"vertex1" (Ref atVertex1)
    :* Field @"vertex2" (Ref atVertex2)
    :* Field @"vertex3" (Ref atVertex3)
    :* Nil
instance HasJsonDecodingSpec AnnotatedTriangle where
  type DecodingSpec AnnotatedTriangle = EncodingSpec AnnotatedTriangle
  fromJSONStructure
      (Field @"vertex1" (Ref atVertex1)
      :* Field @"vertex2" (Ref atVertex2)
      :* Field @"vertex3" (Ref atVertex3)
      :* Nil)
    = do
      pure AnnotatedTriangle { atVertex1, atVertex2, atVertex3 }


data AnnotatedWithBool = AnnotatedWithBool
  { awbName :: Text
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON AnnotatedWithBool)
instance HasJsonEncodingSpec AnnotatedWithBool where
  type EncodingSpec AnnotatedWithBool =
    JsonAnnotated
      '[ '("readOnly", 'True)
       , '("deprecated", 'False)
       ]
      (JsonObject '[ Required "name" JsonString ])
  toJSONStructure AnnotatedWithBool { awbName } =
    Field @"name" awbName
    :* Nil
instance HasJsonDecodingSpec AnnotatedWithBool where
  type DecodingSpec AnnotatedWithBool = EncodingSpec AnnotatedWithBool
  fromJSONStructure
      (Field @"name" awbName
      :* Nil)
    =
      pure AnnotatedWithBool { awbName }

{- ========================================================================== -}

