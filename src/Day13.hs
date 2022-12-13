module Day13 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (lines, null)
import qualified Data.Text.IO as TIO (readFile)
import Data.List (groupBy)
import Data.Function (on)
import Text.Megaparsec (Parsec, parse, some, sepBy)
import Text.Megaparsec.Char (char, numberChar)
import Data.Void (Void)
import Control.Applicative ((<|>))
import Data.List (sort)

data Packet = Integer Int | List [Packet] deriving Eq

data PacketPair = PacketPair Packet Packet

type PacketParser = Parsec Void Text Packet

instance Ord Packet where
    compare (Integer m) (Integer n) = compare m n
    compare (List ps) (List qs) = compare ps qs
    compare p@(Integer _) list = compare (List [p]) list
    compare list (p@(Integer _)) = compare list (List [p])

packetParser :: PacketParser
packetParser = intParser <|> listParser
    where listParser = List <$> (char '[' *> (packetParser `sepBy` char ',') <* char ']')
          intParser = (Integer . read) <$> some numberChar

parsePacket :: Text -> Packet
parsePacket line = case parse packetParser "" line of
    Left err -> error $ "error while parsing" ++ show err
    Right packet -> packet

parsePair :: [Text] -> PacketPair
parsePair [firstLine, secondLine] = (PacketPair `on` parsePacket) firstLine secondLine
parsePair _ = error "not exactly 2 lines in a group in the file??"

parseFile :: Text -> [PacketPair]
parseFile = map parsePair . filter (not . T.null . head) . groupBy ((==) `on` T.null) . T.lines

puzzleData :: IO [PacketPair]
puzzleData = fmap parseFile $ TIO.readFile "input/input13.txt"

isInOrder :: PacketPair -> Bool
isInOrder (PacketPair first second) = first <= second

solvePart1 :: [PacketPair] -> Int
solvePart1 = sum . map fst . filter (isInOrder . snd) . zip [1..]

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

dividerPackets :: [Packet]
dividerPackets = [List [List [Integer 2]], List [List [Integer 6]]]

solvePart2 :: [PacketPair] -> Int
solvePart2 pairs = let allPackets = dividerPackets ++ concatMap (\(PacketPair first second) -> [first, second]) pairs
                       sorted = sort allPackets
                   in product . map fst . filter ((`elem` dividerPackets) . snd) $ zip [1..] sorted

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
