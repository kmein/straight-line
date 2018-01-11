module Types where

import Utility

import Control.Applicative (liftA2)
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Map.Strict (Map)
import Numeric.Natural (Natural)
import Prelude
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as QC
import Text.PrettyPrint.Leijen hiding ((<$>))

type Memory = Map Variable Natural

data Variable
    = Output
    | Input Natural
    | X Natural
     deriving (Show, Eq, Ord)

data Expression
    = Add Variable
          Variable
    | Multiply Variable
               Variable
    | Constant Natural
     deriving (Show, Eq)

data Instruction =
    Assign Variable
           Expression
     deriving (Show, Eq)

newtype Program = Program
    { instructions :: NonEmptyList Instruction
    } deriving (Show, Eq)

instance Pretty Natural where
    pretty = int . fromIntegral

instance Pretty Variable where
    pretty Output = char 'o'
    pretty (Input n) = char 'i' <> pretty n
    pretty (X n) = char 'x' <> pretty n

instance Pretty Instruction where
    pretty (Assign v e) = pretty v <+> string ":=" <+> pretty e

instance Pretty Program where
    pretty =
        hcat . intersperse (semi <> line) . toList . fmap pretty . instructions

instance Pretty Expression where
    pretty (Add v0 v1) = pretty v0 <+> char '+' <+> pretty v1
    pretty (Multiply v0 v1) = pretty v0 <+> char '*' <+> pretty v1
    pretty (Constant n) = pretty n

instance QC.Arbitrary Natural where
    arbitrary = QC.sized (\n -> fromIntegral <$> QC.choose (0, n))

instance QC.Arbitrary Variable where
    arbitrary =
        QC.oneof [pure Output, Input <$> QC.arbitrary, X <$> QC.arbitrary]

instance QC.Arbitrary Expression where
    arbitrary =
        QC.oneof
            [ Constant <$> QC.arbitrary
            , Add <$> QC.arbitrary <*> QC.arbitrary
            , Multiply <$> QC.arbitrary <*> QC.arbitrary
            ]

instance QC.Arbitrary Instruction where
    arbitrary = Assign <$> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Program where
    arbitrary =
        fmap
            Program
            (QC.frequency
                 [ (1, fmap Last QC.arbitrary)
                 , (3, liftA2 Cons QC.arbitrary QC.arbitrary)
                 ])
