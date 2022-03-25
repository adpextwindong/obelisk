import Control.Monad (replicateM)
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Binary.Get

data MapHeader = BadMagic_MapHeader | MapHeader [LevelOffset] BL.ByteString
  deriving Show

data LevelOffset = NoLevel | LevelOffset Int32
  deriving (Show, Eq)

parseHeader :: Get MapHeader
parseHeader = do
  magic <- getInt16le
  if magic == (-21555 :: Int16)-- CDAB as INT16
  then parseOffsets
  else return BadMagic_MapHeader

parseOffsets :: Get MapHeader
parseOffsets = do
  offsets <- replicateM 100 getInt32le
  tileinfo <- getRemainingLazyByteString
  let truncMH = filter (\x -> x /= NoLevel)
  return $! MapHeader (truncMH $ fmap toLevelOffset offsets) tileinfo

toLevelOffset :: Int32 -> LevelOffset
toLevelOffset n | n <= 0 = NoLevel
                | otherwise = LevelOffset n

mhmain = print . runGet parseHeader =<< BL.readFile "MAPHEAD.WL1"


main = do
  mapheaderFile <- BL.readFile "MAPHEAD.WL1"
  let mapHeader = runGet parseHeader mapheaderFile
  print . runGet (parseGameMaps mapHeader) =<< BL.readFile "GAMEMAPS.WL1"

ted5v10 = 3471766746518799700 :: Int64 -- "TED5v1.0"

data GameMaps = BadMagic_GameMaps | GameMaps
  deriving Show

parseGameMaps :: MapHeader -> Get GameMaps
parseGameMaps mh = do
  magic <- getInt64le
  if magic == ted5v10
  then parseRest mh
  else return BadMagic_GameMaps

parseRest :: MapHeader -> Get GameMaps
parseRest = undefined

data LevelHeader = LevelHeader {
   plane0Offset :: Int32
  ,plane1Offset :: Int32
  ,plane2Offset :: Int32
  ,plane0Len :: Int16
  ,plane1Len :: Int16
  ,plane2Len :: Int16
  ,width :: Int16
  ,height :: Int16
  ,name :: String
}
