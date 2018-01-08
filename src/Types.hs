module Types where

import Control.Applicative (liftA2)
import Data.Map.Strict (Map)
import Numeric.Natural (Natural)
import Prelude hiding ((<$>))
import Text.PrettyPrint.Leijen
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as QC

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

data Program
    = Cons Instruction
           Program
    | Nil
     deriving (Show, Eq)

instance Pretty Natural where
    pretty = int . fromIntegral

instance Pretty Variable where
    pretty Output = char 'o'
    pretty (Input n) = char 'i' <> pretty n
    pretty (X n) = char 'x' <> pretty n

instance Pretty Instruction where
    pretty (Assign v e) = pretty v <+> string ":=" <+> pretty e

instance Pretty Program where
    pretty Nil = empty
    pretty (Cons x Nil) = pretty x
    pretty (Cons x xs) = pretty x <> semi <$> pretty xs

instance Pretty Expression where
    pretty (Add v0 v1) = pretty v0 <+> char '+' <+> pretty v1
    pretty (Multiply v0 v1) = pretty v0 <+> char '*' <+> pretty v1
    pretty (Constant n) = pretty n

instance QC.Arbitrary Natural where
    arbitrary = QC.sized (\n -> fmap fromIntegral $ QC.choose (0, n))

instance QC.Arbitrary Variable where
    arbitrary =
        QC.oneof
            [ pure Output
            , fmap Input QC.arbitrary
            , fmap X QC.arbitrary]

instance QC.Arbitrary Expression where
    arbitrary =
        QC.oneof
            [ fmap Constant QC.arbitrary
            , liftA2 Add QC.arbitrary QC.arbitrary
            , liftA2 Multiply QC.arbitrary QC.arbitrary]

instance QC.Arbitrary Instruction where
    arbitrary = liftA2 Assign QC.arbitrary QC.arbitrary

instance QC.Arbitrary Program where
    arbitrary =
        QC.frequency [(1, pure Nil), (5, liftA2 Cons QC.arbitrary QC.arbitrary)]
