module CutUp() where
import Data.Hashable
import Data.List


hashFlip :: String -> Bool
hashFlip el = mod hashedEl 2 > 0
    where hashedEl = hash el


blockify :: [String] -> [String]
blockify li = takeWhile hashFlip li


consumeBlock :: [String] -> Maybe([String], [String])
consumeBlock [] = Nothing
consumeBlock li = Just(block, remainder)
    where block = blockify li
          blockLen = length block
          remainder = drop blockLen li


makeBlocks :: [String] -> [[String]]
makeBlocks li = unfoldr consumeBlock li
