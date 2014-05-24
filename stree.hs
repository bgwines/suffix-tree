{-# LANGUAGE OverloadedStrings #-}

{-
Terms
-----
T$
	The input string (of length n)

Suffix array
	an array of the suffixes of T$, stored in sorted order.

Suffix tree
	Patricia trie of T$ where each leaf is labeled with the index where the corresponding suffix starts in T$.

	// O(n) space, but large constant (15)

	// Usually just an array of start indices of suffixes

Patricia trie (radix trie)
	 A trie where nodes with only one child are merged with their parents.

LCP
	Longest common prefix of two strings

RMQ
	Range-minimum query

Algorithm
---------
Build a suffix tree in O(n) (all steps are O(n)):
	1. Build suffix array `S`
	2. Build LCP array `L` of adjacent elems in `S`
	3. Build suffix tree from suffix array and LCP array

1. Build suffix array (DC3)
	(i) Recursively get the sorted order of all suffixes starting at positions that aren't multiples of three.
		(a) Construct a new string based on suffixes starting at positions in T1 and T2.
			Begin by computing T$[1:] and T$[2:] and padding each with $ until the lengths are multiples of three, then strcat

			Treat each block of three characters as its own character.
			
			Can determine the relative ordering of those characters by an O(m)-time radix sort.

			To keep the alphabet small, replace each block of three characters with its index.

			Recursively compute the suffix array of that string.


		(b) Compute the suffix array of that string, recursively.


		(c) Use the resulting suffix array to deduce the orderings of the suffixes.

	(ii) Using this information, sort the suffixes at positions that are at multiples of three (call them T0).
		(a) For each position in T0, form a pair of the letter at that position and the index of the suffix right after it (which is in T1). These pairs are effectively strings drawn from an alphabet of size \Sigma + n.

		(b) Radix sort them. // O(n)

	(iii) Using a standard merge algorithm (à la mergesort), merge the sorted lists of suffixes together into the overall suffix array.
		// if two compared letters at indices are same, compare letters after them in string (slide 206)

2. Build LCP array `L` of adjacent elems in `S`
	// http://www.cs.iastate.edu/~cs548/references/linear_lcp.pdf

3. Build suffix tree from suffix array and LCP array
	(i) Construct a Cartesian tree from the LCP array, fusing together nodes with the same values if one becomes a parent of the other.
		http://www.stanford.edu/class/cs166/lectures/01/Slides01.pdf
			#112

	(ii) Run a DFS over the tree and add missing children in the order in which they appear in the suffix array.

	(iii) Assign labels to the edges based on the LCP values.
-}

{-
Issues:
	LL, not array => non-linear-time indexing...
-}

module STree
( STree(..)
, construct_stree
, export_for_graphing
) where

import qualified Data.Map as M
import qualified Data.Ord as O
import qualified Data.List as L
import qualified Data.Array as A
import qualified Data.Text.Lazy as Ly

import qualified CTree
import qualified FusedCTree

import HLib

type SuffixArray = A.Array Int Int

data STree
	= Leaf Int
	| Internal (M.Map String STree) deriving Show

-- just for putting into map before Zunctor
instance Eq STree where
	a@(Leaf _) == b@(Internal _) = False
	a@(Internal _) == b@(Leaf _) = False
	a@(Leaf i) == b@(Leaf i') = i == i'
	a@(Internal children) == b@(Internal children') = children == children'

-- just for putting into map before Zunctor
instance Ord STree where
	a > b = (not (a < b)) && (a /= b)
	a < b = (size a) < (size b)
		where
			size :: STree -> Int
			size (Leaf i) = 1
			size (Internal children) = 1 + (sum . map size . M.elems $ children)

data PreliminarySTree
	= PLeaf String Int
	| PInternal String Int [PreliminarySTree] deriving Show

update_string :: (String -> String) -> PreliminarySTree -> PreliminarySTree
update_string f node@(PLeaf s i) = PLeaf (f s) i
update_string f node@(PInternal s i children) = PInternal (f s) i children

get_string :: PreliminarySTree -> String
get_string node@(PLeaf s _) = s
get_string node@(PInternal s _ _) = s

index :: STree -> Int
index (Leaf i) = i
index (Internal _) = error "Internal nodes don't represent ends of suffixes"

is_leaf :: STree -> Bool
is_leaf (Leaf _) = True
is_leaf (Internal _) = False

-- 1. Build suffix array `S`
-- 2. Build LCP array `L` of adjacent elems in `S`
-- 3. Build suffix tree from suffix array and LCP array

---------------------------------------------------------
--                   Test Structures                   --
---------------------------------------------------------

str :: String
str = "nonsense$"

test_suffix_array :: SuffixArray
test_suffix_array = A.array (0,8) [(0,8),(1,7),(2,4),(3,0),(4,5),(5,2),(6,1),(7,6),(8,3)]

test_suffix_lcp_array :: [Int]
test_suffix_lcp_array = A.elems . get_pairwise_adjacent_lcps str $test_suffix_array

----------------------------------------------------------
--                     Suffix Array                     --
----------------------------------------------------------

construct_suffix_array :: String -> SuffixArray
construct_suffix_array str = A.listArray bounds . map fst . L.sortBy (O.comparing snd) . zip [0..] . init . L.tails $ str
	where
		bounds :: (Int, Int)
		bounds = (0, (length str) - 1)

---------------------------------------------------------
--          Suffix Array + LCP -> Suffix tree          --
---------------------------------------------------------

type Graph = ([Node], [Edge])
type Node = (Int, Ly.Text)
type Edge = (Int, Int, Ly.Text)
type Label = Int

export_for_graphing_test :: STree -> Graph
export_for_graphing_test _ = (v, e)
	where 
		v = zip [0..13] $ map Ly.pack ["", "", "1", "", "", "8", "3", "6", "", "0", "4", "7", "2", "5"]
		e = map (\(a, b, c) -> (a, b, Ly.pack c)) [(0, 1, "se"), (0, 2, "onsense$"), (0, 3, "n"), (0, 4, "e"), (0, 5, "$"), (1, 6, "nse$"), (1, 7, "$"), (3, 8, "se"), (3, 9, "onsense$"), (4, 10, "nse$"), (4, 11, "$"), (8, 12, "nse$"), (8, 13, "$")]

export_for_graphing :: STree -> Graph
export_for_graphing stree = (nodes, edges)
	where
		flattened_tree :: [STree]
		flattened_tree = flatten stree

		labelling :: [(Int, STree)]
		labelling = zip [0..] flattened_tree

		get_label :: STree -> Int
		get_label node = fst . find_guaranteed matches $ labelling
			where
				matches :: (Int, STree) -> Bool
				matches (_, node') = (node == node')

		nodes :: [Node]
		nodes = map nodeify labelling
			where
				nodeify :: (Int, STree) -> Node
				nodeify (i, (Leaf i')) = (i, (Ly.pack . show $ i'))
				nodeify (i, (Internal _)) = (i, (Ly.pack ""))

		edges :: [Edge]
		edges = concat . map edgeify $ flattened_tree
			where
				edgeify :: STree -> [(Int, Int, Ly.Text)]
				edgeify tree@(Leaf _) = []
				edgeify tree@(Internal children) = map edgeify_child $ M.keys children
					where
						edgeify_child :: String -> (Int, Int, Ly.Text)
						edgeify_child edge_label = (i, i', edge_label')
							where
								i :: Int
								i = get_label tree

								child :: STree
								child = from_just $ M.lookup edge_label children

								i' :: Int
								i' = get_label child 
								
								edge_label' :: Ly.Text
								edge_label' = Ly.pack edge_label

flatten :: STree -> [STree]
flatten leaf@(Leaf _) = [leaf]
flatten node@(Internal children) =
	(node) : (concat . map flatten . M.elems $ children)

construct_stree :: String -> STree
construct_stree str = suffix_array_to_stree str' suffix_array fused_ctree
	where
		str' :: String
		str' = if (last str) == '$'
			then str
			else str ++ "$"

		suffix_array :: SuffixArray
		suffix_array = construct_suffix_array str'

		lcps :: A.Array Int Int
		lcps = get_pairwise_adjacent_lcps str suffix_array

		fused_ctree :: FusedCTree.FusedCTree Int
		fused_ctree = FusedCTree.fuse . CTree.fromList . A.elems $ lcps

suffix_array_to_stree :: String -> SuffixArray -> FusedCTree.FusedCTree Int -> STree
suffix_array_to_stree str suffix_array tree = stree
	where
		prelim_prelim_stree :: PreliminarySTree
		prelim_prelim_stree = fst $ fctree_to_prelim_prelim_stree' str suffix_array tree 0	

		prelim_stree :: PreliminarySTree
		prelim_stree = fill_internal_nodes prelim_prelim_stree

		stree :: STree
		stree = prelim_stree_to_stree prelim_stree

{-
data STree
	= Leaf Int
	| Internal (M.Map String STree)


suffix_array:
                       i
                      /
                ______
                |
                v
      0           1            2           3             4        
	["$" (8), "e$" (7), "ense$" (4), "nonsense$" (0), "nse$" (5),

          5                   6            7          8
		"nsense$" (2), "onsense$" (1), "se$" (6), "sense$" (3)]

fused_ctree:
                               
                         _____/|\_____
                        /   |  |    | \
                      [2    @  1    1  @]
                      /|      /|    |\
                    [@ @]   [3 @]  [@ @]
                            / \
                          [@   @]


->

                               _
                         _____/|\______
                        /   |  |    |  \
                     [ se 1(*) n    e   8 ($) ]
                ______/|      / \    \___________________
                |      /   [ se  0 (*) ]     |           |
          [3 (nse$)  6 ($) ] / \          [ 4 (nse$)   7 ($) ]
                            /   \
                      [5 (nse$)  5 ($) ]

	*:"onsense$"


	Internal
		"$" -> 8

		"e" ->
			"$" -> 7
			"nse$" -> 4

		"n" ->
			"onsense$" -> 0

		"se" ->
			"$" -> 5
			"nse$" -> 2

		"onsense$" -> 1

		"se" ->
			"$" -> 6
			"nse$" -> 3



-- preliminary stree, without internal nodes filled:

	PInternal "" 0
		[PLeaf "$" 8,
		 PInternal "" 1
		 	[PLeaf "e$" 7,
		 	 PLeaf "ense$" 4],
		 PInternal "" 1
		 	[PLeaf "nonsense$" 0,
		 	 PInternal "" 3
		 	 	[PLeaf "nse$" 5,
		 	 	 PLeaf "nsense$" 2]
		 	],
		 	PLeaf "onsense$" 1,
		 	PInternal "" 2
		 		[PLeaf "se$" 6,
		 		 PLeaf "sense$" 3]
		]

-- preliminary stree:

	PInternal "" 0
		[PLeaf "$" 8,
		 
		 PInternal "e" 1
		 	[PLeaf "$" 7,
		 	 PLeaf "nse$" 4],
		 
		 PInternal "n" 1
		 	[PLeaf "onsense$" 0,
		 	 PInternal "se" 3
		 	 	[PLeaf "$" 5,PLeaf "nse$" 2]],

		 PLeaf "onsense$" 1,

		 PInternal "se" 2
		 	[PLeaf "$" 6,
		 	 PLeaf "nse$" 3]]

-- stree: 
	Internal
		"$" -> Leaf 8

		"e" -> Internal
			"$" -> Leaf 7
			"nse$" -> Leaf 4

		"n" -> Internal
			"onsense$" -> Leaf 0

		"se" -> Internal
			"$" -> Leaf 5
			"nse$" -> Leaf 2

		"onsense$" -> Leaf 1
-}

prelim_stree_to_stree :: PreliminarySTree -> STree
prelim_stree_to_stree (PLeaf s i) = Leaf i
prelim_stree_to_stree (PInternal s i children) = Internal children_map
	where
		children_map :: M.Map String STree
		children_map = foldr insert M.empty children

		insert :: PreliminarySTree -> M.Map String STree -> M.Map String STree
		insert child = M.insert (get_string child) (prelim_stree_to_stree child)

fill_internal_nodes :: PreliminarySTree -> PreliminarySTree
fill_internal_nodes node@(PLeaf _ _) = node
fill_internal_nodes node@(PInternal s i children) = PInternal s' i corrected_children
	where
		filled_children :: [PreliminarySTree]
		filled_children = map fill_internal_nodes children

		corrected_children :: [PreliminarySTree]
		corrected_children = map f filled_children

		f :: PreliminarySTree -> PreliminarySTree
		f node = update_string (drop i) node -- not O(1)!

		s' :: String
		s' = take i . get_string . head $ children

fctree_to_prelim_prelim_stree' :: String -> SuffixArray -> FusedCTree.FusedCTree Int -> Int -> (PreliminarySTree, Int)
fctree_to_prelim_prelim_stree' str suffix_array fused_ctree i =
	if (FusedCTree.is_empty fused_ctree)
		then
			let index = suffix_array A.! i
			in (PLeaf (drop index str) (index), i+1) -- not O(1)!
		else (PInternal "" lcp children, i') -- "" for now; this is preliminary-preliminary
			where
				lcp :: Int
				lcp = FusedCTree.value fused_ctree

				children_with_updated_is :: [(PreliminarySTree, Int)]
				children_with_updated_is = tail $ scanl f (undefined, i) $ FusedCTree.get_children fused_ctree
					where
						f :: (PreliminarySTree, Int) -> FusedCTree.FusedCTree Int -> (PreliminarySTree, Int)
						f (_, i'') child = fctree_to_prelim_prelim_stree' str suffix_array child i''

				i' :: Int
				i' = snd . last $ children_with_updated_is

				children :: [PreliminarySTree]
				children = map fst children_with_updated_is

---------------------------------------------------------
--                    Pair-wise LCP                    --
---------------------------------------------------------

alength :: (A.Array Int b) -> Int
alength = snd . A.bounds

invert :: (A.Array Int Int) -> (A.Array Int Int)
invert arr = A.array (0, alength arr) $ zip (A.elems arr) (A.indices arr)

get_prefix_overlap :: String -> String -> Int
get_prefix_overlap s1 s2 = length . takeWhile eq $ zip s1 s2
	where
		eq :: (Eq a) => (a, a) -> Bool
		eq (a, b) = a == b

{-
Citation:
	T. Kasai, G. Lee, H. Arimura, S. Arikawa, K. Park
	Linear-time longest-common-prefix computation in suffix arrays and its applications
-}
get_pairwise_adjacent_lcps :: String -> (A.Array Int Int) -> (A.Array Int Int)
get_pairwise_adjacent_lcps str pos = get_height' 0 0 height_initial
	where
		rank :: A.Array Int Int
		rank = invert pos

		n :: Int
		n = (alength pos) - 1

		height_initial :: A.Array Int Int
		height_initial = A.listArray (0, n) $ replicate (n+1) 0

		get_height' :: Int -> Int -> (A.Array Int Int) -> (A.Array Int Int)
		get_height' i h height
			| (i == (alength rank) - 1) = height  -- -1 to skip "$"
			| otherwise = get_height' (i+1) h' height'
				where
					-- drop not O(1)?
					h'' = get_prefix_overlap (drop i str) (drop k str)
						where
							k = pos A.! ((rank A.! i) - 1)
							
					height' = height A.// [((rank A.! i) - 1, h'')] -- -1 for 0-indexing
					h' = if h'' > 0 then h'' - 1 else h''




