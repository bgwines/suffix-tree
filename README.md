suffix-tree
===========

An implementation of a suffix tree.

How to run:
-----------

    > alias graph='runhaskell create_dot_file.hs > graph.dot; dot -Tpng graph.dot > graph.png'
    > graph
    cacao

(or whatever other string you want instead of `cacao`)

Dependencies
------------

- [Haskell](http://www.haskell.org/haskellwiki/Haskell)
- [GraphViz](http://graphviz.org/)
- [Haskell GraphViz](https://hackage.haskell.org/package/graphviz)
- [HLib](https://github.com/bgwines/hlib)

Known issues
------------

- Suffix array construction isn't DC3, so it's `O(n^2 log n)` instead of `O(n)`

Algorithm
---------

###Terms
- `T$`: The input string (of length n)

- Suffix array: an array of the suffixes of `T$`, stored in sorted order.

- Suffix tree: A Patricia trie of `T$` where each leaf is labeled with the index where the corresponding suffix starts in `T$`.

	- `O(n)` space, but large constant (`15`)

	- Usually just an array of start indices of suffixes

- Patricia trie (radix trie): A trie where nodes with only one child are merged with their parents.

- LCP: Longest common prefix of two strings

###Algorithm
(all steps are `O(n)`):
	
1. Build suffix array `S`
2. Build LCP array `L` of adjacent elems in `S`
3. Build suffix tree from suffix array and LCP array

####Step 1: Build suffix array `S` (DC3)

- Recursively get the sorted order of all suffixes starting at positions that aren't multiples of three.
	- Construct a new string based on suffixes starting at positions in `T1:` and `T2`.
		- Begin by computing `T1` and `T2` and padding each with `'$'` until the lengths are multiples of three, then strcat

		- Treat each block of three characters as its own character.
		
		- Can determine the relative ordering of those characters by an time radix sort.

		- To keep the alphabet small, replace each block of three characters with its index.

		- Recursively compute the suffix array of that string.

	- Compute the suffix array of that string, recursively.

	- Use the resulting suffix array to deduce the orderings of the suffixes.

- Using this information, sort the suffixes at positions that are at multiples of three (call them `T0`).
	- For each position in `T0`, form a pair of the letter at that position and the index of the suffix right after it (which is in `T1`). These pairs are effectively strings drawn from an alphabet of size `\Sigma + n`.

	- Radix sort them.

- 3-way merge the sorted lists of suffixes together into the overall suffix array.
	- if two compared letters at indices are same, compare letters after them in string

####Step 2. Build LCP array `L` of adjacent elems in `S`

- `pos[i]`: "what's the `i`^th lexicographically ordered suffix (== what _position_ does it start at?)?"

- `rank[i]`: "what's the lexicographic order (_rank_) of the suffix starting at `i`?"

- `i` represents the starting index of a substring

	- => `rank[i]` is the lexicographic order (rank) of suffix starting at `i`

	- => each it. of the forloop fills `height` at the index of the next substring, e.g.

		- 1st it. finds LCP for `"nonsense$"` and whatever suffix is lexicographically before that

		- 2nd it. finds LCP for  `"onsense$"` and whatever suffix is lexicographically before that

		- 3rd it.        ...      `"nsense$"`     ...

- `k` represents the starting index of the suffix we're comparing against. Simply put, `pos[rank[i]]` is just `i`, the index of the current suffix (one of the two in the comparison); to get the other, since we want to compare with an adjacent suffix in the suffix array (`pos`), we get precisely that suffix (it's adjacent in `pos`): `pos[rank[i] - 1]` (remember that `pos[j]` for any `j` is the starting index of the `j`th lexicographically ordered suffix)


- `if h > 0: h -= 1` only makes sense in context of comparing `s[i+h]` to `s[k+h]`. So, why `+h`? OK, this is pretty cool. So basically the idea is that if `h` > 0 then that means that last iteration we compared some suffix `S` with the suffix lexicographically neighbouring it (this implementation uses the one previous to it in `pos`, but you could do the one after it if you wanted) (let it be `T`) and found some overlap. Because of the way the forloop is defined (forwards over `s` / `rank`), the next current suffix (let it be `S'`) is just `S` with the first char lopped off, and, because of the lexicographic ordering of the suffix array (`pos`), the one to which we compare it (`pos[rank[i] - 1]`, `T'`) is just `T` with the first char lopped off, hence, the LCP length between `S'` and `T'` is just the LCP length between `S` and `T` minus one. What's more, that's why this runs in linear time -- even if the string was the same character repeated a whole bunch of times, we'd still be caching the previous iteration's overlap amount in `h`.

Here's an example of the interaction between `pos` and `height` during the construction of `height`:

    height:
    ====================
    j^* | height[j]
    --------------------
        |    LCP    |
        | length^** | suffix     (pos)
    (0) | ----------------------------
        |           | $           (8)
      1 |     0   <-|-----------------
        |           | e$          (7)
      2 |     1   <-|-----------------
        |           | ense$       (4)
      3 |     0   <-|-----------------
        |           | nonsense$   (0)
      4 |     1   <-|-----------------
        |           | nse$        (5)
      5 |     3   <-|-----------------
        |           | nsense$     (2)
      6 |     0   <-|-----------------
        |           | onsense$    (1)
      7 |     0   <-|-----------------
        |           | se$         (6)
      8 |     2   <-|-----------------
        |           | sense$      (3)

    	*	in forloop, this will be rank[i]
        **	(LCP length)[q] corresponds to LCP for suffix[q] and [q+1]

####Step 3. Build suffix tree from suffix array and LCP array

- Construct a Cartesian tree from the LCP array, fusing together nodes with the same values if one becomes a parent of the other.

- Run a DFS over the tree and add missing children in the order in which they appear in the suffix array.

- Assign labels to the edges based on the LCP values.

Citations
---------

- [Keith Schwarz](http://www.keithschwarz.com/) (explanation of the DC3 and suffix tree construction from suffix array and LCP values largely taken from [CS166](http://www.stanford.edu/class/cs166/))

- T. Kasai, G. Lee, H. Arimura, S. Arikawa, and K. Park. Linear-time longest-common-prefix computation in suffix arrays and its applications. In Proc. CPM, volume 2089 of LNCS, pages 181–192. Springer, 2001.

-  J. Kärkkäinen and P. Sanders. Simple linear work suﬃx array construction. In ICALP, 2003, pp. 943–955.

- Harold Carr for examples using GraphViz in Haskell: (<http://haroldcarr.com/posts/2014-02-28-using-graphviz-via-haskell.html>)