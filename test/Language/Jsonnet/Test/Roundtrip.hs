{-# LANGUAGE OverloadedStrings #-}

-- |
module Language.Jsonnet.Test.Roundtrip where

import Data.Fix
import Data.Functor.Sum
import Data.Text (Text)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Error (Error)
import qualified Language.Jsonnet.Parser as Parser
import Language.Jsonnet.Pretty
import Language.Jsonnet.Syntax
import Prettyprinter
import Prettyprinter.Render.Text
import Test.Tasty
import Test.Tasty.Hedgehog

genString :: Gen String
genString =
  Gen.filterT
    (`notElem` Parser.reservedKeywords)
    (Gen.string (Range.constant 2 4) Gen.alpha)

genText :: Gen Text
genText = Gen.text (Range.constant 2 4) Gen.alpha

genField :: Gen a -> Gen (EField a)
genField a = EField <$> a <*> a <*> pure True <*> Gen.enumBounded <*> Gen.bool

genObject :: Gen (Fix ExprF')
genObject =
  Fix
    <$> ( mkObjectF
            <$> Gen.list (Range.linear 0 10) (genField genExpr)
            <*> Gen.list (Range.linear 0 10) ((,) <$> genString <*> genExpr)
        )

genArray :: Gen (Fix ExprF')
genArray = Fix <$> (mkArrayF <$> Gen.list (Range.linear 0 10) genExpr)

genIf :: Gen (Fix ExprF')
genIf = Gen.subterm2 genExpr genExpr (\a b -> Fix (mkIfF a b))

genIfElse :: Gen (Fix ExprF')
genIfElse = Gen.subterm3 genExpr genExpr genExpr (\a b c -> Fix (mkIfElseF a b c))

genSlice :: Gen (Fix ExprF')
genSlice =
  Fix
    <$> ( mkSliceF
            <$> genExpr
            <*> Gen.maybe genExpr
            <*> Gen.maybe genExpr
            <*> Gen.maybe genExpr
        )

genError :: Gen (Fix ExprF')
genError = Fix . mkErrorF <$> genExpr

genImport :: Gen (Fix ExprF')
genImport = Fix . mkImportF <$> genString

genIndex :: Gen (Fix ExprF')
genIndex = Gen.subterm2 genExpr genExpr (\a b -> Fix (mkIndexF a b))

genLookup :: Gen (Fix ExprF')
genLookup = Fix <$> (mkLookupF <$> genIdent <*> genString)

genAssert :: Gen (Fix ExprF')
genAssert =
  Fix
    <$> ( mkAssertF
            <$> genExpr
            <*> Gen.maybe genExpr
            <*> genExpr
        )

genLocal :: Gen (Fix ExprF')
genLocal =
  Fix
    <$> ( mkLocalF
            <$> Gen.nonEmpty (Range.linear 0 10) ((,) <$> genString <*> genExpr)
            <*> genExpr
        )

genLitStr :: Gen (Fix ExprF')
genLitStr = Fix <$> (mkTextF <$> genText)

genLiteral :: Gen (Fix ExprF')
genLiteral =
  Gen.choice
    [ pure (Fix mkNullF),
      Fix . mkBoolF <$> Gen.bool,
      Fix . mkIntF @Int <$> Gen.integral (Range.linear 0 10),
      genLitStr
    ]

genArg :: Gen a -> Gen (Arg a)
genArg a = Gen.choice [Pos <$> a, Named <$> genString <*> a]

genArgs :: Gen a -> Gen (Args a)
genArgs a =
  Args
    <$> Gen.list (Range.linear 0 10) (genArg a) <*> pure Lazy

genApply :: Gen (Fix ExprF')
genApply = Fix <$> (mkApplyF <$> genExpr <*> genArgs genExpr)

genFunc :: Gen (Fix ExprF')
genFunc =
  Fix
    <$> ( mkFunF
            <$> Gen.list (Range.linear 0 10) ((,) <$> genString <*> Gen.maybe genExpr)
            <*> genExpr
        )

genIdent :: Gen (Fix ExprF')
genIdent = Fix . mkIdentF <$> genString

genUnyOp :: Gen (Fix ExprF')
genUnyOp = Fix <$> (InL <$> (EUnyOp <$> Gen.enum Compl Minus <*> genExpr))

genBinOp :: Gen (Fix ExprF')
genBinOp = Fix <$> (InL <$> (EBinOp <$> Gen.enum Add In <*> genExpr <*> genExpr))

genCompSpec :: Gen a -> Gen (CompSpec a)
genCompSpec a = CompSpec <$> genString <*> a <*> Gen.list (Range.linear 0 10) a

genArrComp :: Gen (Fix ExprF')
genArrComp =
  Fix
    <$> ( mkArrCompF
            <$> genExpr
            <*> Gen.nonEmpty (Range.linear 0 10) (genCompSpec genExpr)
        )

genObjComp :: Gen (Fix ExprF')
genObjComp =
  Fix
    <$> ( mkObjCompF
            <$> genField genExpr
            <*> Gen.list (Range.linear 0 10) ((,) <$> genString <*> genExpr)
            <*> Gen.nonEmpty (Range.linear 0 10) (genCompSpec genExpr)
        )

genExpr :: Gen (Fix ExprF')
genExpr =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      genLiteral,
      genIdent,
      genImport
    ]
    [ -- recursive generators
      genIf,
      genIfElse,
      genObject,
      genArray,
      genSlice,
      genLocal,
      genError,
      genAssert,
      genLookup,
      genIndex,
      genApply,
      genFunc,
      genBinOp,
      genUnyOp,
      genObjComp,
      genArrComp
    ]

printExpr :: Fix ExprF' -> Text
printExpr =
  renderStrict
    . layoutPretty defaultLayoutOptions
    . prettyFixExprF'

parseExpr :: Text -> Either Error (Fix ExprF')
parseExpr = fmap forget' . Parser.parse ""

prop_roundtrip :: Property
prop_roundtrip =
  property $ do
    x <- forAll genExpr
    tripping x printExpr parseExpr

testRoundtripGroup :: TestTree
testRoundtripGroup =
  testGroup
    "Property tests"
    [ testProperty
        "roundtrip"
        prop_roundtrip
    ]
