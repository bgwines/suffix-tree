{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module SuffixTree
( SuffixTree(..)
, SuffixTree.construct
, contains_substring
, export_for_graphing
) where

import qualified Data.Map as M
import qualified Data.Ord as O
import qualified Data.List as L
import qualified Data.Array as A
import qualified Data.Text.Lazy as Ly
import qualified Data.ByteString.Char8 as S

import Data.Maybe
import Data.Monoid

import qualified CTree
import qualified FusedCTree

import qualified HLib as H

import SuffixArray

----------------------------------------------------------
--                 Algebraic Data Types                 --
----------------------------------------------------------

{-
	Internal nodes may have any number of children (alphabet permitting), but none will share the same first character (otherwise they would share a parent, which would be a child of this node. Thus, for querying purposes, we represent an internal node as a map from first characters to (substring, child) pairs.
-}
type Childrenmap = M.Map Char (Substring, STree)
type Substring = (Int, Int)

data SuffixTree
	= Empty
	| SuffixTree S.ByteString STree deriving Show

data STree
	= Leaf Int
	| Internal Childrenmap deriving Show

data PreliminarySTree
	= PLeaf Substring Int
	| PInternal Substring [PreliminarySTree] deriving Show

data PreliminaryPreliminarySTree
	= PPLeaf Int
	| PPInternal Int [PreliminaryPreliminarySTree] deriving Show

----------------------------------------------------------
--                Instance Declarations                 --
----------------------------------------------------------

instance Eq STree where
	a@(Leaf _) == b@(Internal _) = False
	a@(Internal _) == b@(Leaf _) = False
	a@(Leaf i) == b@(Leaf i') = i == i'
	a@(Internal children) == b@(Internal children') = children == children'

----------------------------------------------------------
--                   Utility Functions                  --
----------------------------------------------------------

substring :: S.ByteString -> Substring -> S.ByteString
substring str (i, len) = S.take len . S.drop i $ str

substr_drop :: Int -> Substring -> Substring
substr_drop d (i, len) = (i + d, len - d)

substr_take :: Int -> Substring -> Substring
substr_take t (i, len) = (i, t)

get_substr :: PreliminarySTree -> Substring
get_substr (PLeaf substr _) = substr
get_substr (PInternal substr _) = substr

pad :: String -> String
pad str = if (last str) == '$'
	then str
	else str ++ "$"

---------------------------------------------------------
--                Suffix tree functions                --
---------------------------------------------------------

-- TODO: longest palindromic substring, longest common substring between n strings, etc.

contains_substring :: SuffixTree -> String -> Bool
contains_substring suffix_tree@Empty str = False
contains_substring suffix_tree@(SuffixTree str' stree) str =
	contains_substring' stree (S.pack str) str'

contains_substring' :: STree -> S.ByteString -> S.ByteString -> Bool
contains_substring' stree@(Leaf i) str original_str =
	str `is_prefix_of` (S.drop i original_str)
contains_substring' stree@(Internal children) str original_str =
	let
		child :: Maybe (Substring, STree)
		child = M.lookup (S.head str) children

		_' :: (Substring, STree)
		_'@((i, len), child') = fromJust child

		edge_label :: S.ByteString
		edge_label = S.take len . S.drop i $ original_str

		str' :: S.ByteString
		str' = S.drop (S.length edge_label) str
	in
		if child == Nothing
			then False
			else if is_prefix_of str edge_label
				then True
				else if is_prefix_of edge_label str
					then contains_substring' child' str' original_str
					else False

is_prefix_of :: S.ByteString -> S.ByteString -> Bool
is_prefix_of s s' = s == (S.take (S.length s) s')

---------------------------------------------------------
--                     Suffix tree                     --
---------------------------------------------------------

construct :: String -> SuffixTree
construct str = SuffixTree str' stree
	where
		str' :: S.ByteString
		str' = S.pack . pad $ str

		sarray :: SuffixArray
		sarray = SuffixArray.construct_naive str'

		lcps :: A.Array Int Int
		lcps = get_pairwise_adjacent_lcps str' sarray

		fused_ctree :: FusedCTree.FusedCTree Int
		fused_ctree = FusedCTree.fuse . CTree.fromList . A.elems $ lcps

		stree :: STree
		stree = sarray_to_stree str' sarray fused_ctree

sarray_to_stree :: S.ByteString -> SuffixArray -> FusedCTree.FusedCTree Int -> STree
sarray_to_stree str sarray tree = stree
	where
		prelim_prelim_stree :: PreliminaryPreliminarySTree
		prelim_prelim_stree = fst $ fctree_to_prelim_prelim_stree sarray tree 0	

		prelim_stree :: PreliminarySTree
		prelim_stree = fill_internal_nodes (S.length str) prelim_prelim_stree

		stree :: STree
		stree = prelim_stree_to_stree prelim_stree str

prelim_stree_to_stree :: PreliminarySTree -> S.ByteString -> STree
prelim_stree_to_stree (PLeaf s i) _ = Leaf i
prelim_stree_to_stree (PInternal s children) str = Internal children_map
	where
		children_map :: Childrenmap
		children_map = foldr insert M.empty children

		insert :: PreliminarySTree -> Childrenmap -> Childrenmap
		insert child = M.insert edge_label (substr, child')
			where
				substr :: Substring
				substr = get_substr child

				edge_label :: Char
				edge_label = S.index str (fst substr)

				child' :: STree
				child' = prelim_stree_to_stree child str

fill_internal_nodes :: Int -> PreliminaryPreliminarySTree -> PreliminarySTree
fill_internal_nodes strlen node@(PPLeaf i) = PLeaf (i, strlen - i) i
fill_internal_nodes strlen node@(PPInternal i children) =
	PInternal substr corrected_children
	where
		filled_children :: [PreliminarySTree]
		filled_children = map (fill_internal_nodes strlen) children

		corrected_children :: [PreliminarySTree]
		corrected_children = map correct_child filled_children

		correct_child :: PreliminarySTree -> PreliminarySTree
		correct_child child@(PLeaf child_substr starting_index) =
			PLeaf (substr_drop i child_substr) starting_index
		correct_child child@(PInternal child_substr children) =
			PInternal (substr_drop i child_substr) children

		substr :: Substring
		substr = substr_take i . get_substr . head $ filled_children

-- TODO: make `FusedCTree` an instance of `Foldable`, to make this easier?
fctree_to_prelim_prelim_stree :: SuffixArray -> FusedCTree.FusedCTree Int -> Int -> (PreliminaryPreliminarySTree, Int)
fctree_to_prelim_prelim_stree sarray fused_ctree i =
	if (FusedCTree.is_empty fused_ctree)
		then (PPLeaf (sarray A.! i), i+1)
		else (PPInternal lcp children, i')
			where
				lcp :: Int
				lcp = FusedCTree.value fused_ctree

				children_with_updated_is :: [(PreliminaryPreliminarySTree, Int)]
				children_with_updated_is = tail . scanl f (undefined, i) . FusedCTree.get_children $ fused_ctree
					where
						f :: (PreliminaryPreliminarySTree, Int) -> FusedCTree.FusedCTree Int -> (PreliminaryPreliminarySTree, Int)
						f (_, i'') child = fctree_to_prelim_prelim_stree sarray child i''

				i' :: Int
				i' = snd . last $ children_with_updated_is

				children :: [PreliminaryPreliminarySTree]
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

get_pairwise_adjacent_lcps :: S.ByteString -> SuffixArray -> (A.Array Int Int)
get_pairwise_adjacent_lcps str sarray = get_lpcs' 0 0 lpcs_initial
	where
		rank :: A.Array Int Int
		rank = invert sarray

		n :: Int
		n = (alength sarray) - 1

		lpcs_initial :: A.Array Int Int
		lpcs_initial = A.listArray (0, n) $ replicate (n+1) 0

		get_lpcs' :: Int -> Int -> (A.Array Int Int) -> (A.Array Int Int)
		get_lpcs' i overlap lpcs
			| (i == (alength rank) - 1) = lpcs  -- -1 to skip "$"
			| otherwise = get_lpcs' (i+1) overlap' lpcs'
				where
					overlap'' :: Int
					overlap'' = lcp (S.drop i str) (S.drop k str)
						where
							k :: Int
							k = sarray A.! ((rank A.! i) - 1)

					lpcs' :: A.Array Int Int							
					lpcs' = lpcs A.// [((rank A.! i) - 1, overlap'')] -- -1 for 0-indexing

					overlap' :: Int
					overlap' = if overlap'' > 0 then overlap'' - 1 else overlap''

---------------------------------------------------------
--                       Graphing                      --
---------------------------------------------------------

type Graph = ([Node], [Edge])
type Node = (Int, Ly.Text)
type Edge = (Int, Int, Ly.Text)
type Label = Int

export_for_graphing :: SuffixTree -> Graph
export_for_graphing suffix_tree@(SuffixTree str stree) = export_for_graphing' str stree

export_for_graphing' :: S.ByteString -> STree -> Graph
export_for_graphing' str stree = (nodes, edges)
	where
		flattened_tree :: [STree]
		flattened_tree = flatten stree

		flatten :: STree -> [STree]
		flatten leaf@(Leaf _) = [leaf]
		flatten node@(Internal children) =
			(node) : (concat . map (flatten . snd) . M.elems $ children)

		labelling :: [(Int, STree)]
		labelling = zip [0..] flattened_tree

		get_label :: STree -> Int
		get_label node = fst . H.find_guaranteed matches $ labelling
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
				edgeify tree@(Internal children) = map edgeify_child $ M.elems children
					where
						edgeify_child :: (Substring, STree) -> (Int, Int, Ly.Text)
						edgeify_child childpair = (parent_label, child_label, edge_label')
							where
								parent_label :: Int
								parent_label = get_label tree

								child_label :: Int
								child_label = get_label . snd $ childpair
								
								edge_label' :: Ly.Text
								edge_label' = Ly.pack . S.unpack $ substring str (fst childpair)
