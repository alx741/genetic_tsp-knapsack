module Dataset.Tsp where

import Control.Monad
import qualified Data.Vector as V
import System.Exit
import Text.Megaparsec
import Text.Megaparsec.String
import Data.Char
import Tsp

data Coordinates = Coordinates
    { coordLat :: Float
    , coordLng :: Float
    }

-- c1 = Coordinates 9860 14152
-- c2 = Coordinates 9396 14616

euclideanDistance :: Coordinates -> Coordinates -> Int
euclideanDistance c1 c2 = round $ sqrt $ xd^2 + yd^2
    where xd = coordLat c1 - coordLat c2
          yd = coordLng c1 - coordLng c2

-- ja :: Parser String
-- ja = string "NAME :"

attributeParser :: String -> Parser String
attributeParser attrib = do
    let attrib' = fmap toUpper attrib
    string attrib' >> space >> char ':' >> space
    value <- some $ alphaNumChar <|> separatorChar <|> printChar
    -- value <- some anyChar
    eol
    return value

tspLibParser :: Parser String
tspLibParser = do
    name <- attributeParser "name"
    _ <- attributeParser "comment"
    _ <- attributeParser "type"
    dimension <- attributeParser "dimension"
    edge_type <- attributeParser "edge_weight_type"
    return $ name

-- readData :: FilePath -> IO KnapsackProblem
-- readData fp = do
--     inputData <- readFile fp
--     case (parse knapsackParser fp inputData) of
--         Left err -> putStr (parseErrorPretty err) >> exitFailure
--         Right xs -> return xs
-- knapsackParser :: Parser KnapsackProblem
-- knapsackParser = do
--     numberItems <- liftM read $ some digitChar
--     space
--     maxWeight <- liftM read $ some digitChar
--     eol
--     eol
--     items <- some itemParser
--     eof
--     return $ KnapsackProblem numberItems maxWeight
--            $ sortByDensity $ V.fromList $ identifyItems items
-- itemParser :: Parser ItemSimple
-- itemParser = do
--     weight <- liftM read $ some digitChar
--     space
--     value <- liftM read $ some digitChar
--     eol
--     return $ ItemSimple weight value
-- data ItemSimple = ItemSimple
--     { itemWeight :: Float
--     , itemValue :: Float
--     } deriving (Show)
-- identifyItems :: [ItemSimple] -> [Item]
-- identifyItems is = fmap (\(i, ItemSimple w v) -> Item i w v) $ zip [1..] is
