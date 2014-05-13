
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
	
2. Build LCP array `L` of adjacent elems in `S` (Kasai)
	// http://www.cs.iastate.edu/~cs548/references/linear_lcp.pdf

3. Build RMQ structure over `L` (Fischer-Heun)

4. Build suffix tree from suffix array and LCP array
	(i) Construct a Cartesian tree from the LCP array, fusing together nodes with the same values if one becomes a parent of the  other.
	
	(ii) Run a DFS over the tree and add missing children in the order in which they appear in the suffix array.

	(iii) Assign labels to the edges based on the LCP values.
-}

{-
Issues:
	LL, not array => non-linear-time indexing...
-}

type Index = Integer
-- ([lcplen], [suffix])
type SuffixLCPArray = ([Integer], [Index])

-- 1. Build suffix array `S` // O(n)
-- 2. Build LCP array `L` of adjacent elems in `S`
-- 3. Build RMQ structure over `L`

-- 4. Build suffix tree from suffix array and LCP array

test_suffix_array_with_strs = [(8, "$"), (7, "e$"), (4, "ense$"), (0, "nonsense$"), (5, "nse$"), (2, "nsense$"), (1, "onsense$"), (6, "se$"), (3, "sense$")]
test_suffix_array = map fst test_suffix_array_with_strs
lcps = [0, 1, 0, 1, 3, 0, 0, 2]
test_suffix_lcp_array :: SuffixLCPArray
test_suffix_lcp_array = (lcps, test_suffix_array)









