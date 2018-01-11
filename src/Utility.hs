{-# LANGUAGE TypeFamilies, DeriveFoldable, DeriveTraversable,
  DeriveFunctor #-}

module Utility where

import GHC.Exts (IsList(..))
import Data.Foldable as F (Foldable, toList)
import Data.Traversable (Traversable)
import Text.PrettyPrint.Leijen (Pretty(..))
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Gen (suchThat)

data NonEmptyList a
    = Last a
    | Cons a
           !(NonEmptyList a)
    deriving (Eq, Functor, Foldable, Traversable)

instance Show a => Show (NonEmptyList a) where
    show = show . F.toList

instance Pretty a => Pretty (NonEmptyList a) where
    pretty = pretty . F.toList

instance Arbitrary a => Arbitrary (NonEmptyList a) where
    arbitrary = fromList <$> (arbitrary `suchThat` (not . null))

instance IsList (NonEmptyList a) where
    type Item (NonEmptyList a) = a
    toList = F.toList
    fromList [] = error "Non-empty list cannot be empty."
    fromList [x] = Last x
    fromList (x:xs) = Cons x (fromList xs)

