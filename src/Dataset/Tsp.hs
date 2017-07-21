module Dataset.Tsp where

import Control.Monad
import qualified Data.Vector as V
import System.Exit
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as ML
import Data.Char
import Tsp

data Coordinates = Coordinates
    { coordLat :: Int
    , coordLng :: Int
    } deriving (Show, Eq)

type NodeId = Int
data Node = Node NodeId Coordinates deriving (Show)

instance Eq Node where
    (==) (Node n1 _) (Node n2 _) = n1 == n2

data TSPInstance = TSPInstance
    { tspName :: String
    , tspDimension :: Int
    , tspNodes :: [Node]
    } deriving (Show)

euclideanDistance :: Coordinates -> Coordinates -> Int
euclideanDistance c1 c2 = round $ sqrt $ fromIntegral $ xd^2 + yd^2
    where xd = coordLat c1 - coordLat c2
          yd = coordLng c1 - coordLng c2

attributeParser :: String -> Parser String
attributeParser attrib = do
    string' attrib >> space >> char ':' >> space
    value <- some $ alphaNumChar <|> separatorChar <|> printChar
    eol
    return value

intParser :: Parser Int
intParser = do
    i <- ML.integer
    return $ fromIntegral i

nodeParser :: Parser Node
nodeParser = do
    space
    id <- intParser
    space
    latitude <- intParser
    space
    longitude <- intParser
    eol
    return $ Node id $ Coordinates latitude longitude

tspLibParser :: Parser TSPInstance
tspLibParser = do
    name <- attributeParser "name"
    _ <- attributeParser "comment"
    _ <- attributeParser "type"
    dimension <- attributeParser "dimension"
    edge_type <- attributeParser "edge_weight_type"
    string' "node_coord_section" >> eol
    nodes <- many nodeParser
    string' "EOF"
    return $ TSPInstance name (read dimension) nodes
