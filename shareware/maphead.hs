import Control.Monad (replicateM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Binary
import Data.Binary.Get

data MapHeader = BadMagic | MapHeader [LevelOffset] BL.ByteString
  deriving Show

data LevelOffset = NoLevel | LevelOffset Int32
  deriving Show

parseHeader :: Get MapHeader
parseHeader = do
  magic <- getInt16le
  if magic == (-21555 :: Int16)-- CDAB as INT16
  then parseOffsets
  else return BadMagic

parseOffsets :: Get MapHeader
parseOffsets = do
  offsets <- replicateM 100 getInt32le
  tileinfo <- getRemainingLazyByteString
  return $! MapHeader (fmap toLevelOffset offsets) tileinfo

toLevelOffset :: Int32 -> LevelOffset
toLevelOffset n | n < 0 = NoLevel
                | otherwise = LevelOffset n

main = print . runGet parseHeader =<< BL.readFile "MAPHEAD.WL1"
