-- DO NOT MODIFY THE FOLLOWING LINES

module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, encode, compress, decompress) where

import Table
import PriorityQueue

import Test.HUnit

{- a bit code (of a character or string) is represented by a list of Booleans
   INVARIANT:
     the bit code is a concatenation of (0 or more) valid code words for some Huffman tree
 -}
type BitCode = [Bool]

-- END OF DO NOT MODIFY ZONE

--------------------------------------------------------------------------------
{- characterCountsAux str char acc
   Counts how many times a character occurs i a string.
   PRE: acc >= 0
   RETURNS: acc of how many times specified char occurs in str.
   EXAMPLES: characterCountsAux "" 'h' 0         == 0
             characterCountsAux "hej hopp" 'h' 0 == 2
             characterCountsAux "hej hopp" 'o' 0 == 1
-}
characterCountsAux :: String -> Char -> Int -> Int
characterCountsAux [] _ acc = acc
characterCountsAux (x:xs) y acc | y == x = 1 + characterCountsAux xs y acc
                                | otherwise = characterCountsAux xs y acc

{- characterCounts s
   RETURNS: a table that maps each character that occurs in s to the number of
         times the character occurs in s
   EXAMPLES: characterCounts "" == T []
             characterCounts "hej hopp" == T [('p',2),('o',1),('h',2),(' ',1),('j',1),('e',1)]
             characterCounts "this is an example of a huffman tree" == T [('e',4),('r',1),('t',2),(' ',7),('n',2),('a',4),('m',2),('f',3),('u',1),('h',2),('o',1),('l',1),('p',1),('x',1),('s',2),('i',2)]
 -}
characterCounts :: String -> Table Char Int
characterCounts [] = Table.empty
characterCounts (x:xs) = Table.insert (characterCounts xs) x (characterCountsAux (x:xs) x 0)


{- HuffmanTree - full binary tree such that
 - each leaf is labeled with a unique character.
 - each sub-tree (i.e., each leaf and each node) is labeled with the count of all characters
   in that sub-tree.
 INVARIANTS: sub-trees with larger character counts do not occur at a lower level of the tree than sub-trees with smaller character counts.
-}
data HuffmanTree = Void
                 | Leaf Char Int
                 | Node Int HuffmanTree HuffmanTree deriving Show

{- priorityQ lst
   Creates a PriorityQueue out of a specified list of tuples.
   RETURNS: PriorityQueue from lst
   EXAMPLES:
-}
priorityQ :: [(Char,Int)] -> PriorityQueue HuffmanTree
-- VARIANT: length lst
priorityQ [] = PriorityQueue.empty
priorityQ (x:xs) = PriorityQueue.insert (priorityQ xs) (Leaf (fst x) (snd x),snd x)

{- mergeQ pq acc
   Merges all of the trees in a specified PriorityQueue into one tree based on their priority.
   RETURNS: pg with only one tree containing all previous trees
   EXAMPLES:
-}
mergeQ :: PriorityQueue HuffmanTree -> Int -> PriorityQueue HuffmanTree
-- VARIANT: acc-1
mergeQ pq 0 = pq
mergeQ pq acc = let a = PriorityQueue.least
                    b = fst (PriorityQueue.least pq)
                    c = snd (PriorityQueue.least pq)
                    d = snd (fst (PriorityQueue.least pq)) + snd(fst (PriorityQueue.least (snd (PriorityQueue.least pq))))
                in  mergeQ (PriorityQueue.insert (snd(a c)) (Node d (fst b) (fst(fst(a c))),d)) (acc-1)

{- sizeQ pg
   Calculates how many trees there are in a specified queue.
   RETURNS: Int of how many trees in pq
   EXAMPLES:
-}
sizeQ :: PriorityQueue HuffmanTree -> Int
-- VARIANT: length of queue
sizeQ pq = if is_empty (snd (PriorityQueue.least pq))
            then 0
            else 1 + sizeQ (snd (PriorityQueue.least pq))

x = priorityQ (Table.iterate (characterCounts "this is an example of a huffman tree")(\y x -> x : y) [])

{- huffmanTree table
   Creates a HuffmanTree from a table.
   PRE:  table maps each key to a positive value
   RETURNS: a Huffman tree based on the character counts in table
   EXAMPLES:
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree t = fst(fst(PriorityQueue.least (mergeQ (priorityQ (reverse(Table.iterate t (\y x -> x : y) [])))(sizeQ (priorityQ (reverse(Table.iterate t (\y x -> x : y) [])))))))


{- codeTable h
   RETURNS: a table that maps each character in h to its Huffman code
   EXAMPLES:
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable = undefined


{- encode h s
   PRE: All characters in s appear in h
   RETURNS: the concatenation of the characters of s encoded using the Huffman code table of h.
   EXAMPLES:
 -}
encode :: HuffmanTree -> String -> BitCode
encode = undefined

{- compress s
   RETURNS: (a Huffman tree based on s, the Huffman coding of s under this tree)
   EXAMPLES:
 -}
compress :: String -> (HuffmanTree, BitCode)
compress = undefined


{- decompress h bits
   PRE:  bits is a concatenation of valid Huffman code words for h
   RETURNS: the decoding of bits under h
   EXAMPLES:
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress = undefined


--------------------------------------------------------------------------------
-- Test Cases
-- You may add your own test cases here:
-- Follow the pattern and/or read about HUnit on the interwebs.
--------------------------------------------------------------------------------

-- characterCounts
test1 = TestCase $ assertEqual "characterCounts"
            (Just 7) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')

-- codeTable
-- while the precise code for ' ' may vary, its length (for the given example string) should always be 3 bits
test2 = TestCase $ assertEqual "codeTable"
            3 (maybe (-1) length (Table.lookup (codeTable (huffmanTree (characterCounts "this is an example of a huffman tree"))) ' '))

-- compress
-- while the precise code for the given example string may vary, its length should always be 135 bits
test3 = TestCase $ assertEqual "compress"
            135 (length (snd (compress "this is an example of a huffman tree")))

-- decompress
test4 =
    let s = "this is an example of a huffman tree"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test5 =
    let s = "xxx"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test6 =
    let s = ""
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

-- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6]