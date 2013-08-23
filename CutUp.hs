module CutUp() where
import Data.Hashable
import Data.List
import System.Random


flips :: [Int]
flips = randomRs (0, 9) (mkStdGen 42)  -- Meaningful seed


coinFlip :: String -> Bool
coinFlip el = flip > 3
    where flip = head flips


blockify :: [String] -> [String]
blockify li = takeWhile coinFlip li


consumeBlock :: [String] -> Maybe([String], [String])
consumeBlock [] = Nothing
consumeBlock li = Just(block, remainder)
    where block = blockify li
          blockLen = length block
          remainder = drop blockLen li


makeBlocks :: [String] -> [[String]]
makeBlocks li = unfoldr consumeBlock li
