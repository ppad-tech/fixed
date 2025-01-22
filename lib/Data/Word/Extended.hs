{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Word.Extended where

import Data.Bits ((.|.))
import qualified Data.Bits as B
import Data.Word (Word64)
import GHC.Generics
import qualified Prelude (mod)
import Prelude hiding (sum, mod, compare)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

data Word256 = Word256
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
  deriving (Eq, Generic)

