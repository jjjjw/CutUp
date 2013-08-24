module CutUp() where
import Data.List as L
import Data.List.Split as S
import Data.Map as M
import System.Random


moduleRanGen = (mkStdGen 42)  -- Meaningful seed


ranRange :: [Int]
ranRange = randomRs (0, 9) moduleRanGen


isLucky :: (Int, String) -> Bool
isLucky (r, s) = r == 1


discardRandoms :: [(Int, String)] -> [String]
discardRandoms l = block
  where (r, block) = unzip l


makeBlocks :: [String] -> [[String]]
makeBlocks l = result
  where
    matrix = zip ranRange l
    predicate = whenElt isLucky
    blocks = S.split predicate matrix
    result = L.map discardRandoms blocks


-- http://www.haskell.org/haskellwiki/Random_shuffle: Section 3.1
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((M.insert j x . M.insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen


fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l =
  toElems $ L.foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen)


shuffleBlocks :: [[String]] -> [[String]]
shuffleBlocks l = r
    where (r, g) = (fisherYates moduleRanGen l)
