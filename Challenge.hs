module Main where

import           Padelude                          hiding (option, try)
import           Prelude                           (error)

import           Text.Parsec
import           Text.Parsec.Text
import           Text.Parser.Char                  (text)
import           Text.Parser.Token                 (integer, whiteSpace)

import qualified Data.Vector                       as V

import           Data.Graph.Inductive.Graph        as G hiding (insEdge,
                                                         insEdges)
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.SP     (spLength)
import           Data.GraphViz                     hiding (parseList)

import           Statistics.Sample

fst4 :: (a, b, c, d) ->  a
fst4 (x, _, _, _) = x

snd4 :: (a, b, c, d) ->  b
snd4 (_, x, _, _) = x

trd4 :: (a, b, c, d) ->  c
trd4 (_, _, x, _) = x

frt4 :: (a, b, c, d) ->  d
frt4 (_, _, _, x) = x

type PrimGraph = V.Vector [Int]

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname
  = do{ input <- readFile fname
      ; return (runParser p () fname input)
      }

lineParser :: Parser (Int, Int)
lineParser = do
    beg <- (fromInteger <$> integer)
    whiteSpace
    end <- (fromInteger <$> integer)
    return (beg, end)

parseList :: Parser a -> Maybe (Parser Text) -> Parser [a]
parseList parseOne mSep
  = do
      initialVal <- parseOne
      rest  <- option [] parseRemaining
      return (initialVal : rest)
  where
      parseRemaining
        = let pSep = fromMaybe (text "") mSep
          in try $ pSep >> choice [parseList parseOne mSep, return []]

graphParser :: (Real b, DynGraph gr) => Parser (gr () b)
graphParser = do
    pairs <- parseList lineParser Nothing
    return $ alist2Graph pairs

foldlM_ :: (Foldable t, Monad m) => (b -> a -> m ()) -> b -> t a -> m ()
foldlM_ f def = void . foldlM (\x y -> f x y >> return x) def

alist2Graph :: (DynGraph gr, Real b) => [(Int, Int)] -> gr () b
alist2Graph es = insEdges (map (\(x, y) -> (x, y, 1)) es) G.empty

-- | Insert a 'LEdge' into the 'Graph'.
insEdge :: (DynGraph gr) => LEdge b -> gr () b -> gr () b
insEdge (v,w,l) g = (pr,v,la,(l,w):su) G.& g'
  where
    (mcxt',g'') = match v g
    (mcxt, g') = if isJust mcxt' then (mcxt', g'') else match v (insNode (v, ()) g)
    (pr,_,la,su) = fromMaybe
                     (error ("insEdge: cannot add edge from non-existent vertex " ++ show v))
                     mcxt

-- | Insert multiple 'LEdge's into the 'Graph'.
insEdges :: (DynGraph gr) => [LEdge b] -> gr () b -> gr () b
insEdges es g = foldl' (flip insEdge) g es
{-# INLINABLE insEdges #-}

followerList :: Graph gr => gr a b -> [(LNode a, Int, [LEdge b])]
followerList = map ((\x -> (labNode' x, indeg' x, inn' x))) . contexts

followerStatSample :: Graph gr => gr a b -> V.Vector Double
followerStatSample = V.fromList . map inDegAsDouble . contexts
    where inDegAsDouble = fromIntegral . indeg'

-- | Finds those nodes that have above 1 std deviation many followers
highlyFollowed :: Graph gr => gr a b -> [LNode a]
highlyFollowed g = map (\(x, _, _) -> x) . filter filterFunc . followerList $ g
    where std = stdDev . followerStatSample $ g
          filterFunc :: (LNode a, Int, [LEdge b]) -> Bool
          filterFunc (_, y, _) = fromIntegral y > std

contexts :: Graph gr => gr a b -> [Context a b]
contexts g = map (context g)  (nodes g)

-- | Calculates the distance from every node to `end'
distances :: (Real b, Graph gr) => gr a b -> LNode a -> [(LNode a, b)]
distances g (end, _) = map (\(x, y) -> (x, fromMaybe undefined y)) defDistances
    where getDistance = (\x -> spLength x end g) . node'
          maybeDistances = map (\x -> (labNode' x, getDistance x)) (contexts g)
          defDistances = filter (isJust . snd) maybeDistances

checkDirectednessVis :: (Graph gr, Ord el) => gr nl el -> DotGraph Node
checkDirectednessVis = setDirectedness graphToDot nonClusteredParams

-- Returns a list of LNodes within 1 stdDev distance away
closestDistances :: (Graph gr, Real b) => gr a b -> LNode a -> [(LNode a, b)]
closestDistances g v = filter ((< std) . (toDouble . snd)) $ dsts
    where dsts = distances g v
          std = stdDev . V.fromList . map (toDouble . snd) $ dsts
          toDouble :: (Real c) => c -> Double
          toDouble x =  rat2Double . toRational $ x
          rat2Double x = (fromIntegral $ numerator x) / (fromIntegral $ denominator x)

main :: IO ()
main = do
    args <- getArgs
    let inFile = atDef "test.csv" args 0
    parsedDataOrNot <- parseFromFile graphParser inFile
    case parsedDataOrNot of
      Left e  -> print e
      Right x -> do
          let graph = (x :: Gr () Integer)
              influencers = highlyFollowed graph
          -- putStrLn . printDotGraph . checkDirectednessVis $ graph
          prettyPrint graph
          print $ (map (indeg' . context graph)) (nodes graph)
          print influencers
          print . map (closestDistances graph) $ influencers
