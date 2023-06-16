{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Arbitrary instances for the JSON @Value@.
module Data.Aeson.AutoType.Test (
    arbitraryTopValue
  ) where

import           Data.Aeson.AutoType.Pretty          () -- Generic instance for Value

import           Data.String (fromString)
import           Control.Applicative                 ((<$>), (<*>))
import           Data.Aeson
import           Data.Function                       (on)
import           Data.Hashable                       (Hashable)
import           Data.Generics.Uniplate.Data
import           Data.List
import           Data.Scientific
import qualified Data.Text                   as Text
import           Data.Text                           (Text)
import qualified Data.Vector                 as V
import qualified Data.HashMap.Strict         as Map
import           GHC.Generics
import qualified Data.Aeson.KeyMap           as Aeson
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck
import           Test.SmallCheck.Series

instance Arbitrary Text where
  arbitrary = Text.pack  <$> sized (`vectorOf` alphabetic)
    where
      alphabetic = choose ('a', 'z')

instance (Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary

instance (Arbitrary v) => Arbitrary (Map.HashMap Text v) where
  arbitrary = makeMap <$> arbitrary

-- | Helper function for generating Arbitrary and Series instances
-- for @Data.HashMap.Strict.Map@ from lists of pairs.
makeMap :: (Ord a, Hashable a) =>[(a, b)] -> Map.HashMap a b
makeMap  = Map.fromList
         . nubBy  ((==)    `on` fst)
         . sortBy (compare `on` fst)

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary <*> arbitrary

-- | Transformation to shrink top level of @Value@, doesn't consider nested sub-@Value@s.
simpleShrink           :: Value -> [Value]
simpleShrink (Array  a) = map (Array  .   V.fromList) $ shrink $ V.toList   a
simpleShrink (Object o) = map (Object . Aeson.fromList) $ shrink $ Aeson.toList o
simpleShrink _          = [] -- Nothing for simple objects

-- | Generator for compound @Value@s
complexGens ::  Int -> [Gen Value]
complexGens i = [Object . Aeson.fromList <$> resize i arbitrary,
                 Array                 <$> resize i arbitrary]

-- | Arbitrary JSON (must start with Object or Array.)
arbitraryTopValue :: Gen Value
arbitraryTopValue  = sized $ oneof . complexGens

-- * SmallCheck Serial instances
instance Monad m => Serial m Text where
  series = newtypeCons Text.pack

instance Monad m => Serial m Scientific where
  series = cons2 scientific

instance Serial m a => Serial m (V.Vector a) where
  series = newtypeCons V.fromList

instance Serial m v => Serial m (Map.HashMap Text v) where
  series = newtypeCons makeMap

instance Monad m => Serial m Aeson.Key where
  series = newtypeCons fromString

instance Serial m v => Serial m (Aeson.KeyMap v) where
  series = newtypeCons Aeson.fromList

-- This one is generated with Generics and instances above
instance Monad m => Serial m Value
