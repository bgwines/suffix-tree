{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module SuffixTree
( SuffixTree(..)
, SuffixTree.construct
, contains_substring
, export_for_graphing
) where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Array as Array
import qualified Data.Text.Lazy as Ly
import qualified Data.ByteString.Char8 as ByteString

import Data.Char
import Data.Maybe
import Data.Monoid

import Test.QuickCheck

import qualified CTree
import qualified FusedCTree

import qualified HLib as H

import SuffixArray

----------------------------------------------------------
--                 Algebraic Data Types                 --
----------------------------------------------------------

{-
	Internal nodes may have any number of children (alphabet permitting), but none will share the same first character (otherwise they would share a parent, which would be a child of this node. Thus, for querying purposes, we represent an internal node as a map from first characters to (substring, child) pairByteString.
-}
type ChildrenMap = Map.Map Char (Substring, STree)
type Substring = (Int, Int)

data SuffixTree
	= Empty
	| SuffixTree ByteString.ByteString STree deriving Show

data STree
	= Leaf Int
	| Internal ChildrenMap deriving Show

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

nul :: Char
nul = chr 0

substring :: ByteString.ByteString -> Substring -> ByteString.ByteString
substring str (i, len) = ByteString.take len . ByteString.drop i $ str

substr_drop :: Int -> Substring -> Substring
substr_drop d (i, len) = (i + d, len - d)

substr_take :: Int -> Substring -> Substring
substr_take t (i, len) = (i, t)

get_substr :: PreliminarySTree -> Substring
get_substr (PLeaf substr _) = substr
get_substr (PInternal substr _) = substr

pad :: String -> String
pad str = if (last str) == nul
	then str
	else str ++ [nul]

---------------------------------------------------------
--                Suffix tree functions                --
---------------------------------------------------------

-- TODO: longest palindromic substring, longest common substring between n strings, etc.

contains_substring :: SuffixTree -> String -> Bool
contains_substring _ "" = True
contains_substring suffix_tree@Empty str = False
contains_substring suffix_tree@(SuffixTree str' stree) str =
	contains_substring' stree (ByteString.pack str) str'

contains_substring' :: STree -> ByteString.ByteString -> ByteString.ByteString -> Bool
contains_substring' stree@(Leaf i) str original_str =
	str `is_prefix_of` (ByteString.drop i original_str)
contains_substring' stree@(Internal children) str original_str =
	let
		child :: Maybe (Substring, STree)
		child = Map.lookup (ByteString.head str) children

		_' :: (Substring, STree)
		_'@((i, len), child') = fromJust child

		edge_label :: ByteString.ByteString
		edge_label = ByteString.take len . ByteString.drop i $ original_str

		str' :: ByteString.ByteString
		str' = ByteString.drop (ByteString.length edge_label) str
	in
		if child == Nothing
			then False
			else if is_prefix_of str edge_label
				then True
				else if is_prefix_of edge_label str
					then contains_substring' child' str' original_str
					else False

is_prefix_of :: ByteString.ByteString -> ByteString.ByteString -> Bool
is_prefix_of s s' = s == (ByteString.take (ByteString.length s) s')

runtests :: IO ()
runtests = quickCheckWith stdArgs { maxSuccess = 500 } test_stree_substr_query

test_stree_substr_query :: String -> Bool
test_stree_substr_query s = invalid || all_substrings_present
	where
		invalid :: Bool
		invalid = nul `elem` s

		all_substrings_present :: Bool
		all_substrings_present = and . map (contains_substring stree) $ all_substrings

		all_substrings :: [String]
		all_substrings = List.nub . concat . map List.inits . List.tails $ s

		stree :: SuffixTree
		stree = SuffixTree.construct s

---------------------------------------------------------
--                     Suffix tree                     --
---------------------------------------------------------

construct :: String -> SuffixTree
construct "" = Empty
construct str =
	if nul `elem` str
		then error "Input not accepted (input string contains the NUL-terminator ('\\0'))."
		else SuffixTree str' stree
		where
			str' :: ByteString.ByteString
			str' = ByteString.pack . pad $ str

			sarray :: SuffixArray
			sarray = SuffixArray.construct str

			lcps :: Array.Array Int Int
			lcps = get_pairwise_adjacent_lcps str' sarray

			fused_ctree :: FusedCTree.FusedCTree Int
			fused_ctree = FusedCTree.fuse . CTree.fromList . Array.elems $ lcps

			stree :: STree
			stree = sarray_to_stree str' sarray fused_ctree

sarray_to_stree :: ByteString.ByteString -> SuffixArray -> FusedCTree.FusedCTree Int -> STree
sarray_to_stree str sarray fused_ctree = stree
	where
		prelim_prelim_stree :: PreliminaryPreliminarySTree
		prelim_prelim_stree = fst $ fctree_and_sarray_to_prelim_prelim_stree sarray fused_ctree 0	

		prelim_stree :: PreliminarySTree
		prelim_stree = fill_internal_nodes (ByteString.length str) prelim_prelim_stree

		stree :: STree
		stree = prelim_stree_to_stree prelim_stree str

prelim_stree_to_stree :: PreliminarySTree -> ByteString.ByteString -> STree
prelim_stree_to_stree (PLeaf s i) _ = Leaf i
prelim_stree_to_stree (PInternal s children) str = Internal children_map
	where
		children_map :: ChildrenMap
		children_map = foldr insert Map.empty children

		insert :: PreliminarySTree -> ChildrenMap -> ChildrenMap
		insert child = Map.insert edge_label (substr, child')
			where
				substr :: Substring
				substr = get_substr child

				edge_label :: Char
				edge_label = ByteString.index str (fst substr)

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
fctree_and_sarray_to_prelim_prelim_stree :: SuffixArray -> FusedCTree.FusedCTree Int -> Int -> (PreliminaryPreliminarySTree, Int)
fctree_and_sarray_to_prelim_prelim_stree sarray fused_ctree i =
	if (FusedCTree.is_empty fused_ctree)
		then (PPLeaf (sarray Array.! i), i+1)
		else (PPInternal lcp children, i')
			where
				lcp :: Int
				lcp = FusedCTree.value fused_ctree

				children_with_updated_is :: [(PreliminaryPreliminarySTree, Int)]
				children_with_updated_is = tail . scanl f (undefined, i) . FusedCTree.get_children $ fused_ctree
					where
						f :: (PreliminaryPreliminarySTree, Int) -> FusedCTree.FusedCTree Int -> (PreliminaryPreliminarySTree, Int)
						f (_, i'') child = fctree_and_sarray_to_prelim_prelim_stree sarray child i''

				i' :: Int
				i' = snd . last $ children_with_updated_is

				children :: [PreliminaryPreliminarySTree]
				children = map fst children_with_updated_is

---------------------------------------------------------
--                    Pair-wise LCP                    --
---------------------------------------------------------

alength :: (Array.Array Int b) -> Int
alength = snd . Array.bounds

invert :: (Array.Array Int Int) -> (Array.Array Int Int)
invert arr = Array.array (0, alength arr) $ zip (Array.elems arr) (Array.indices arr)

lcp :: ByteString.ByteString -> ByteString.ByteString -> Int
lcp s1 s2
	| is_empty s1 = 0
	| is_empty s2 = 0
	| otherwise = if ch1 == ch2
		then 1 + (lcp s1' s2')
		else 0
		where
			ch1 :: Char
			ch1 = ByteString.index s1 0

			ch2 :: Char
			ch2 = ByteString.index s2 0

			s1' :: ByteString.ByteString
			s1' = ByteString.drop 1 s1

			s2' :: ByteString.ByteString
			s2' = ByteString.drop 1 s2

			is_empty :: ByteString.ByteString -> Bool
			is_empty s = (ByteString.length s) == 0			

get_pairwise_adjacent_lcps :: ByteString.ByteString -> SuffixArray -> (Array.Array Int Int)
get_pairwise_adjacent_lcps str sarray = get_lcps' 0 0 lcps_initial
	where
		rank :: Array.Array Int Int
		rank = invert sarray

		n :: Int
		n = (alength sarray) - 1 -- -1 because this is for pair-wise adjascent values

		lcps_initial :: Array.Array Int Int
		lcps_initial = Array.listArray (0, n) $ replicate (n+1) 0

		get_lcps' :: Int -> Int -> (Array.Array Int Int) -> (Array.Array Int Int)
		get_lcps' i overlap lcps
			| (i == (alength rank) - 1) = lcps  -- -1 to skip "$"
			| otherwise = get_lcps' (i+1) overlap' lcps'
				where
					overlap'' :: Int
					overlap'' = lcp (ByteString.drop i str) (ByteString.drop k str)
						where
							k :: Int
							k = sarray Array.! ((rank Array.! i) - 1)

					lcps' :: Array.Array Int Int
					lcps' = lcps Array.// [((rank Array.! i) - 1, overlap'')] -- -1 for 0-indexing

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
export_for_graphing suffix_tree@(Empty) = ([], [])
export_for_graphing suffix_tree@(SuffixTree str stree) = export_for_graphing' str' stree
	where
		str' :: ByteString.ByteString
		str' = ByteString.append (ByteString.init str) (ByteString.pack "$")

export_for_graphing' :: ByteString.ByteString -> STree -> Graph
export_for_graphing' str stree = (nodes, edges)
	where
		flattened_tree :: [STree]
		flattened_tree = flatten stree

		flatten :: STree -> [STree]
		flatten leaf@(Leaf _) = [leaf]
		flatten node@(Internal children) =
			(node) : (concat . map (flatten . snd) . Map.elems $ children)

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
				edgeify tree@(Internal children) = map edgeify_child $ Map.elems children
					where
						edgeify_child :: (Substring, STree) -> (Int, Int, Ly.Text)
						edgeify_child childpair = (parent_label, child_label, edge_label')
							where
								parent_label :: Int
								parent_label = get_label tree

								child_label :: Int
								child_label = get_label . snd $ childpair
								
								edge_label' :: Ly.Text
								edge_label' = Ly.pack . ByteString.unpack $ substring str (fst childpair)
