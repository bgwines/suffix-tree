
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
	1. Build suffix array `S` // O(n)
	2. Build LCP array `L` of adjacent elems in `S`
	3. Build RMQ structure over `L`
	4. Build suffix tree from suffix array and LCP array

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

3. Build RMQ structure over `L` (Fischer-Heun)

4. Build suffix tree from suffix array and LCP array
	(i) Construct a Cartesian tree from the LCP array, fusing together nodes with the same values if one becomes a parent of the `other.
		http://www.stanford.edu/class/cs166/lectures/01/Slides01.pdf
			#112

	(ii) Run a DFS over the tree and add missing children in the order in which they appear in the suffix array.

	(iii) Assign labels to the edges based on the LCP values.
-}

{-
Issues:
	LL, not array => non-linear-time indexing...
-}

import qualified Data.Map as M
import qualified Stack
import Data.Array

-- 1. Build suffix array `S`
-- 2. Build LCP array `L` of adjacent elems in `S`
-- 3. Build RMQ structure over `L`
-- 4. Build suffix tree from suffix array and LCP array

str :: String
str = "nonsense$"

suffix_array :: Array Int Int
suffix_array = array (0,8) [(0,8),(1,7),(2,4),(3,0),(4,5),(5,2),(6,1),(7,6),(8,3)]

test_suffix_lcp_array :: Array Int Int
test_suffix_lcp_array = get_pairwise_adjacent_lcps str suffix_array

--------------------------------------------------------
--               Kasai (pair-wise LCP)                --
--------------------------------------------------------

alength :: (Array Int b) -> Int
alength = snd . bounds

invert :: (Array Int Int) -> (Array Int Int)
invert arr = array (0, alength arr) $ zip (elems arr) (indices arr)

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
get_pairwise_adjacent_lcps :: String -> (Array Int Int) -> (Array Int Int)
get_pairwise_adjacent_lcps str pos = get_height' 0 0 height_initial
	where
		rank :: Array Int Int
		rank = invert pos

		n :: Int
		n = (alength pos) - 1

		height_initial :: Array Int Int
		height_initial = listArray (0, n) $ replicate (n+1) 0

		get_height' :: Int -> Int -> (Array Int Int) -> (Array Int Int)
		get_height' i h height
			| (i == (alength rank) - 1) = height  -- -1 to skip "$"
			| otherwise = get_height' (i+1) h' height'
				where
					-- drop not O(1)?
					h'' = get_prefix_overlap (drop i str) (drop k str)
						where
							k = pos ! ((rank ! i) - 1)
							
					height' = height // [((rank ! i) - 1, h'')] -- -1 for 0-indexing
					h' = if h'' > 0 then h'' - 1 else h''

data STree
	= Empty
	| Internal (M.Map String STree)




