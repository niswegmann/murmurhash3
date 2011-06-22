{-|
License         :  CC0 1.0 Universal Public Domain Dedication
Maintainer      :  niswegmann@gmail.com
Stability       :  provisional
Portability     :  portable (Haskell 2010)


For details of the implementation of MurmurHash3, see the following webpages:

  * <http://code.google.com/p/smhasher/>

  * <http://en.wikipedia.org/wiki/MurmurHash>
-}

module Data.Digest.Murmur
  ( Hash
  , hash
  , Hashable (..)
  , HashGen
  , salt
  , combine
  ) where

import Data.Array
import Data.Bits
import Data.Char
import Data.Complex
import Data.Int
import Data.List
import Data.Ratio
import Data.Word

--------------------------------------------------------------------------------

-- MurmurHash3 uses a mixer and a finalizer. The mixer maps a block (the data we
-- want to hash) and a hash state into a new hash state, where both blocks and
-- hash states are represented by 32-bit words. The finalizer forces all bits
-- in the hash state to avalanche.

-- Mixes two blocks.
{-# INLINE mix #-}
mix :: Word32 -> Word32 -> Word32
mix b0 h0 =
  let
    b1 = b0 * c1
    b2 = b1 `rotateL` r1
    b3 = b2 * c2
    h1 = h0 `xor` b3
    h2 = h1 `rotateL` r2
    h3 = h2 * m1 + m2
  in
    h3

  where

    c1 = 0xcc9e2d51
    c2 = 0x1b873593
    r1 = 15
    r2 = 13
    m1 = 5
    m2 = 0xe6546b64

-- Forces all bits of a hash block to avalanche.
{-# INLINE finalize #-}
finalize :: Word32 -> Word32
finalize h0 =
  let
    h1 = h0 `xor` (h0 `shiftR` r1)
    h2 = h1 * c1
    h3 = h2 `xor` (h2 `shiftR` r2)
    h4 = h3 * c2
    h5 = h4 `xor` (h4 `shiftR` r3)
  in
    h5

  where

    c1 = 0x85ebca6b
    c2 = 0xc2b2ae35
    r1 = 16
    r2 = 13
    r3 = 16

--------------------------------------------------------------------------------

-- Hash generators.

-- | A hash generator is a function that maps a hash state into a new hash state.
-- The internal representation of hash states is kept transparent.
newtype HashGen = HashGen (Word32 -> Word32)

-- Runs a hash generator on a seed.
runHashGen :: HashGen -> Word32 -> Hash
runHashGen (HashGen f) = finalize . f

-- | Returns a hash generator that mixes its input with a 32-bit word.
-- Is typically used for enumerating constructors when deriving 'Hashable'.
{-# INLINE salt #-}
salt :: Word32 -> HashGen
salt = HashGen . mix

-- | Combines two hash generators such that the output of the first generator is
-- piped into the next. This works similar to function composition.
-- Indeed, for all /f/, /g/, /h/, we have that
--
-- > f `combine` (g `combine` h) == (f `combine` g) `combine` h
{-# INLINE combine #-}
combine :: HashGen -> HashGen -> HashGen
combine (HashGen f) (HashGen g) = HashGen (g . f)

--------------------------------------------------------------------------------

-- | Type class for computing hash generators from values.
--
-- Making custom data types instantiate 'Hashable' is straightforward; given
-- the following tree data structure:
--
-- > data Tree a
-- >   = Tip
-- >   | Bin a (Tree a) (Tree a)
--
-- ...we make it instantiate 'Hashable' like this:
--
-- > instance Hashable a => Hashable (Tree a) where
-- >   hashGen Tip         = salt 0x0
-- >   hashGen (Bin x l r) = hashGen x `combine` hashGen l `combine` hashGen r
--
-- For sum data types such as 'Either' we typically want to avoid that
--
-- > Left "foo"
--
-- hashes to the same hash as
--
-- > Right "foo"
--
-- ...hence we add some 'salt' for each constructor:
--
-- > instance (Hashable a, Hashable b) => Hashable (Either a b) where
-- >   hashGen (Left  x) = salt 0x1 `combine` hashGen x
-- >   hashGen (Right y) = salt 0x2 `combine` hashGen y
--
class Hashable a where
  -- | Returns a hash generator for the argument.
  hashGen :: a -> HashGen

-- | A 32-bit hash.
type Hash = Word32

-- | Computes a 32-bit hash from a /hashable/ value.
hash :: Hashable a => a -> Hash
hash = flip runHashGen defaultSeed . hashGen

  where

    -- The default seed is an arbitrary 32-bit word.
    defaultSeed :: Word32
    defaultSeed = 4294967291

--------------------------------------------------------------------------------

-- Integral hash generators:

{-# INLINE hashWord8 #-}
hashWord8 :: Word8 -> HashGen
hashWord8 = hashWord32 . fromIntegral

{-# INLINE hashWord16 #-}
hashWord16 :: Word16 -> HashGen
hashWord16 = hashWord32 . fromIntegral

{-# INLINE hashWord32 #-}
hashWord32 :: Word32 -> HashGen
hashWord32 = HashGen . mix

{-# INLINE hashWord64 #-}
hashWord64 :: Word64 -> HashGen
hashWord64 x = hashWord32 lo `combine` hashWord32 hi
  where
    lo = fromIntegral x
    hi = fromIntegral (x `shiftR` 32)

{-# INLINE hashInt #-}
hashInt :: Int -> HashGen
hashInt =
  if bitSize (undefined :: Int)  <= 32
    then hashWord32 . fromIntegral
    else hashWord64 . fromIntegral

{-# INLINE hashInteger #-}
hashInteger :: Integer -> HashGen
hashInteger k
  | k < 0     = foldl' combine (salt 0x1) . blocks $ abs k
  | otherwise = foldl' combine (salt 0x0) . blocks $ k

  where

    blocks 0 = []
    blocks x = hashWord32 (fromInteger x) : blocks (x `shiftR` 32)

--------------------------------------------------------------------------------

-- Instances:

instance Hashable Char where
  hashGen = hashGen . ord

instance Hashable Word where
  hashGen = hashInt . fromIntegral

instance Hashable Int where
  hashGen = hashInt

instance Hashable Word8 where
  hashGen = hashWord8

instance Hashable Word16 where
  hashGen = hashWord16

instance Hashable Word32 where
  hashGen = hashWord32

instance Hashable Word64 where
  hashGen = hashWord64

instance Hashable Int8 where
  hashGen = hashWord8 . fromIntegral

instance Hashable Int16 where
  hashGen = hashWord16 . fromIntegral

instance Hashable Int32 where
  hashGen = hashWord32 . fromIntegral

instance Hashable Int64 where
  hashGen = hashWord64 . fromIntegral

instance Hashable Integer where
  hashGen = hashInteger

instance Hashable () where
  hashGen () = salt 0x0

instance Hashable Bool where
  hashGen False = salt 0x0
  hashGen True  = salt 0x1

instance Hashable a => Hashable (Maybe a) where
  hashGen Nothing  = salt 0x0
  hashGen (Just x) = hashGen x

instance (Hashable a, Hashable b) => Hashable (Either a b) where
  hashGen (Left  x) = salt 0x0 `combine` hashGen x
  hashGen (Right y) = salt 0x1 `combine` hashGen y

instance Hashable a => Hashable [a] where
  hashGen xs = foldl' combine (salt 0x0) (fmap hashGen xs)

instance (Hashable a, Ix i) => Hashable (Array i a) where
  hashGen = hashGen . elems

instance (Hashable a, Hashable b) => Hashable (a, b) where
  hashGen (x, y) = hashGen x `combine` hashGen y

instance (Hashable a, Hashable b, Hashable c) => Hashable (a, b, c) where
  hashGen (x, y, z) = hashGen x `combine` hashGen y `combine` hashGen z

instance (Hashable a, Hashable b, Hashable c, Hashable d)
  => Hashable (a, b, c, d) where
  hashGen (x, y, z, w) =
    hashGen x `combine` hashGen y `combine` hashGen z `combine` hashGen w

instance (Hashable a, Integral a) => Hashable (Ratio a) where
  hashGen x = hashGen (numerator x) `combine` hashGen (denominator x)

instance Hashable Float where
  hashGen = hashGen . toRational

instance Hashable Double where
  hashGen = hashGen . toRational

instance (Hashable a, RealFloat a) => Hashable (Complex a) where
  hashGen x = hashGen (realPart x) `combine` hashGen (imagPart x)
