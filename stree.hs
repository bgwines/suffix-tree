{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

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
import qualified Data.ByteString.Char8 as S

import Data.Monoid

import qualified CTree
import qualified FusedCTree

import HLib

----------------------------------------------------------
--                 Algebraic Data Types                 --
----------------------------------------------------------

type SuffixArray = A.Array Int Int

data STree
	= Leaf Int
	| Internal (M.Map String STree) deriving Show

instance Eq STree where
	a@(Leaf _) == b@(Internal _) = False
	a@(Internal _) == b@(Leaf _) = False
	a@(Leaf i) == b@(Leaf i') = i == i'
	a@(Internal children) == b@(Internal children') = children == children'

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

----------------------------------------------------------
--                     Suffix Array                     --
----------------------------------------------------------

construct_suffix_array :: String -> SuffixArray
construct_suffix_array str = A.listArray bounds . map fst . L.sortBy (O.comparing snd) . zip [0..] . init . L.tails $ str
	where
		bounds :: (Int, Int)
		bounds = (0, (length str) - 1)

---------------------------------------------------------
--                     Suffix tree                     --
---------------------------------------------------------

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
		lcps = get_pairwise_adjacent_lcps (S.pack str) suffix_array

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

lcp :: S.ByteString -> S.ByteString -> Int
lcp s1 s2
	| is_empty s1 = 0
	| is_empty s2 = 0
	| otherwise = if ch1 == ch2
		then 1 + (lcp s1' s2')
		else 0
		where
			ch1 :: Char
			ch1 = S.index s1 0

			ch2 :: Char
			ch2 = S.index s2 0

			s1' :: S.ByteString
			s1' = S.drop 1 s1

			s2' :: S.ByteString
			s2' = S.drop 1 s2

			is_empty :: S.ByteString -> Bool
			is_empty s = (S.length s) == 0			
{-
Citation:
	T. Kasai, G. Lee, H. Arimura, S. Arikawa, K. Park
	Linear-time longest-common-prefix computation in suffix arrays and its applications
-}
get_pairwise_adjacent_lcps :: S.ByteString -> (A.Array Int Int) -> (A.Array Int Int)
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
					h'' = lcp (S.drop i str) (S.drop k str)
						where
							k = pos A.! ((rank A.! i) - 1)
							
					height' = height A.// [((rank A.! i) - 1, h'')] -- -1 for 0-indexing
					h' = if h'' > 0 then h'' - 1 else h''

---------------------------------------------------------
--                       Graphing                      --
---------------------------------------------------------

type Graph = ([Node], [Edge])
type Node = (Int, Ly.Text)
type Edge = (Int, Int, Ly.Text)
type Label = Int

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

----------------------------------------
--Intermediate representation examples--
----------------------------------------
{-
    suffix array:
                           i
                          /
                    _____|
                    |
                    v
          0           1            2           3             4
        ["$" (8), "e$" (7), "ense$" (4), "nonsense$" (0), "nse$" (5),    

              5                   6            7          8
    		"nsense$" (2), "onsense$" (1), "se$" (6), "sense$" (3)]

	fused Cartesian tree:
                               
                         _____/|\_____
                        /   |  |    | \
                      [2    @  1    1  @]
                      /|      /|    |\
                    [@ @]   [3 @]  [@ @]
                            / \
                          [@   @]

	(which translates to a suffix tree:
                               _
                         _____/|\______
                        /   |  |    |  \
                     [ se 1(*) n    e   8 ($) ]
                ______/|      / \    \___________________
                |      /   [ se  0 (*) ]     |           |
          [3 (nse$)  6 ($) ] / \          [ 4 (nse$)   7 ($) ]
                            /   \
                      [5 (nse$)  5 ($) ]

		(*:"onsense$")
	)

	preliminary stree, without internal nodes filled:

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

	preliminary stree, with internal nodes filled:

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
-}