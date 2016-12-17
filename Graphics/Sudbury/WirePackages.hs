{-|
Module      : Graphics.Sudbury.WirePackages
Description : Reads ByteStrings into packages containing individual messages
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
Portability : POSIX
-}
module Graphics.Sudbury.WirePackages where

import Data.Word
import Data.Store
import Data.Foldable
import Data.Store.Core
import Data.Store.Internal
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Builder as BB
import Control.Monad
import Foreign.Ptr (minusPtr)



data WirePackage = WirePackage
  { wirePackageSender  :: Word32
  , wirePackageSize    :: Word16
  , wirePackageOpcode  :: Word16
  , wirePackagePayload :: B.ByteString
  } deriving (Eq, Show)

-- Storable that writes in the Wayland binary format.
-- Allows us to use the optimized machinery for Data.Store
-- to gain faster performing decoding/encoding.
instance Store WirePackage where
  size = VarSize $ fromIntegral . wirePackageSize
  poke p = do
    poke $ wirePackageSender p
    poke $ wirePackageOpcode p 
    poke $ wirePackageSize p
    let (sourceFp, sourceOffset, sourceLength) = B.toForeignPtr $ wirePackagePayload p
    pokeFromForeignPtr sourceFp sourceOffset sourceLength
  peek = do
    sen <- peek
    op <- peek
    sz <- peek
    let payloadSize = fromIntegral sz - 8
    plraw <- peekToPlainForeignPtr "Data.ByteString.ByteString" payloadSize
    let pl = B.PS plraw 0 payloadSize
    return $ WirePackage 
      { wirePackageSender  = sen
      , wirePackageSize    = sz
      , wirePackageOpcode  = op
      , wirePackagePayload = pl
      }
  {-# INLINE size #-}
  {-# INLINE peek #-}
  {-# INLINE poke #-}

newtype WirePackageStream = WirePackageStream 
  { unWirePackageStream :: [WirePackage]
  } deriving (Eq, Show)

instance Store WirePackageStream where
  size = VarSize $ foldl' sumAlignedSize 0 . unWirePackageStream
    where
      VarSize f = size :: Size WirePackage
      sumAlignedSize :: Int -> WirePackage -> Int
      sumAlignedSize acc x = let pkgSize = f x in pkgSize + acc + (pkgSize `mod` 4)

  poke (WirePackageStream xs) = traverse_ pokeAlign xs
    where
      pokeAlign wp = let align = fromIntegral $ wirePackageSize wp `mod` 4
                         emptyByte = 0 :: Word8
                      in poke wp >> replicateM align (poke emptyByte)

  peek = WirePackageStream <$> go
    where  
      go :: Peek [WirePackage]
      go = 
        do rem <- remaining
           if rem < 8 then Peek $ \_ ptr -> return (ptr, [])
                      else do wp <- peek :: Peek WirePackage
                              let align = fromIntegral $ wirePackageSize wp `mod` 4
                              skip align
                              (wp :) <$> go

remaining :: Peek Int
remaining = Peek $ \ps ptr -> return (ptr, peekStateEndPtr ps `minusPtr` ptr)

wirePack :: WirePackage -> BB.Builder
wirePack = BB.byteString . encode
