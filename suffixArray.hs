{-# LANGUAGE ScopedTypeVariables #-}

module SuffixArray
( SuffixArray(..)
, construct_naive
, dc3
) where

import qualified Data.Map as M
import qualified Data.Ord as O
import qualified Data.List as L
import qualified Data.Array as A
import qualified Data.Text.Lazy as Ly
import qualified Data.ByteString.Char8 as S

import qualified Data.Set as Set

import Data.Char
import Data.Maybe

import qualified HLib as H

type Index = Int
type Order = Int
type SuffixArray = A.Array Int Index
type Substring = (Index, Int)

substring :: S.ByteString -> Substring -> S.ByteString
substring str (i, len) = S.take len . S.drop i $ str

substr_drop :: Int -> Substring -> Substring
substr_drop d (i, len) = (i + d, len - d)

substr_take :: Int -> Substring -> Substring
substr_take t (i, len) = (i, t)

initial_array :: S.ByteString -> A.Array Int Int
initial_array str = A.listArray (0, (S.length str) - 1) $ repeat (-1)

construct_naive :: S.ByteString -> SuffixArray
construct_naive str' = A.listArray bounds . map fst . L.sortBy (O.comparing snd) . zip [0..] . init . L.tails $ str
	where
		str :: String
		str = S.unpack str'

		bounds :: (Int, Int)
		bounds = (0, (length str) - 1)

pad_and_double :: S.ByteString -> S.ByteString
pad_and_double str = (f 1) `S.append` (f 2)
	where
		f :: Int -> S.ByteString
		f k = pad_to_3 . S.drop k $ str

		pad_to_3 :: S.ByteString -> S.ByteString
		pad_to_3 s = s `S.append` (S.pack (replicate k '$'))
			where
				k :: Int
				k = if mod3 == 0
					then mod3
					else 3 - mod3 -- plain `mod3` is not how many to pad; we need 2 if it's 1 and 1 if it's 2

				mod3 :: Int
				mod3 = (S.length s) `mod` 3

order_blocks :: [Substring] -> S.ByteString -> [Order]
order_blocks blocks doubled_str = uniquely_ordered_blocks
	where
		block_ordering :: [Order]
		block_ordering = order_blocks' blocks doubled_str

		uniquely_ordered_blocks :: [Order]
		uniquely_ordered_blocks =
			if not . sorted_contains_duplicates $ block_ordering
				then block_ordering
				else apply_transformation . A.elems . dc3 . S.pack . map chr $ block_ordering
					-- The condition of the if-statement above acts as a base case since (the recursion can never reach a depth greater than one).

		-- input: array (0,9) [(0,9),(1,2),(2,3),(3,5),(4,7),(5,8),(6,6),(7,0),(8,1),(9,4)]
		-- output: apply ([9] = 0, [2] = 1, ...) to something
		apply_transformation :: [Order] -> [Order]
		apply_transformation arr = A.elems (A.array (0, (length arr) - 1) (zip arr [0..]))

-- input example:
--     blocks: ["$$$","nom","nso","omn","oms","onn","ons","oon","s$$"]
--     doubled_str: "onsoonnomnoms$$nsoonnomnoms$$$"
-- result example:
--     [6,7,1,1,8,2,5,3,4,0]
order_blocks' :: [Substring] -> S.ByteString -> [Order]
order_blocks' blocks doubled_str = map (sort_map M.!) blocks
	where
		sort_map :: M.Map Substring Order
		sort_map = foldr insert M.empty $ blocks_with_order
			where
				insert :: (Substring, Order) -> M.Map Substring Order -> M.Map Substring Order
				insert (s, i) m = M.insert s i m

		blocks_with_order :: [(Substring, Int)]
		blocks_with_order =
			concat
				. map clean
					. (flip zip) [0..]
						. L.groupBy substr_pair_eq
							. map make_substr_pair
								$ sorted_blocks
			where
				clean :: ([(Substring, S.ByteString)], Int) -> [(Substring, Int)]
				clean (substrs, i) = map (\ss -> (fst ss, i)) substrs

				make_substr_pair :: Substring -> (Substring, S.ByteString)
				make_substr_pair substr = (substr, substring doubled_str substr)

				substr_pair_eq :: (Substring, S.ByteString) -> (Substring, S.ByteString) -> Bool
				substr_pair_eq p1 p2 = (snd p1) == (snd p2)

				-- TODO: radix sort
				sorted_blocks :: [Substring]
				sorted_blocks = L.sortBy (O.comparing (substring doubled_str)) blocks

-- At index `i` is the sort order of the suffix starting at `i` (within `T1` \union `T2`)
get_suffix_ordering_T1T2 :: S.ByteString -> A.Array Index Order
get_suffix_ordering_T1T2 str = initial' A.// (zip
	(indices_in_Tk str 2)
	ordering_of_T2_suffixes)
		where
			doubled_str :: S.ByteString
			doubled_str = pad_and_double str

			blocks :: [Substring]
			blocks = map (\i -> (i, 3)) $ indices_in_Tk doubled_str 0

			block_ordering :: [Order]
			block_ordering = order_blocks blocks doubled_str

			partitioning :: [[Order]]
			partitioning = H.partition ((length block_ordering) `div` 2) $ block_ordering -- can assume it's divisible by 2 by construction of `blocks`

			-- by construction of `partitioning`, these will be the only two elems
			ordering_of_T1_suffixes :: [Order]
			ordering_of_T1_suffixes = partitioning !! 0

			ordering_of_T2_suffixes :: [Order]
			ordering_of_T2_suffixes = partitioning !! 1

			initial' :: A.Array Index Order
			initial' = (initial_array str) A.// (zip
				(indices_in_Tk str 1)
				ordering_of_T1_suffixes)

sorted_contains_duplicates :: (Eq a) => [a] -> Bool
sorted_contains_duplicates [] = False
sorted_contains_duplicates l = or $ zipWith (==) l (tail l)

mergeBy :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp as bs
	| (length as == 0) = bs
	| (length bs == 0) = as
	| otherwise =
		let
			a = head as
			b = head bs
			as' = tail as
			bs' = tail bs
		in
			case cmp a b of
					LT -> a : mergeBy cmp as' bs
					EQ -> a : mergeBy cmp as' bs
					GT -> b : mergeBy cmp as  bs'

alength :: (A.Array Int b) -> Int
alength = snd . A.bounds

{- The relative ordering array contains:
	the ordering of suffixes starting at indices 0 mod 3 relative to each other
	
	the ordering of suffixes starting at indices 1 mod 3 and 2 mod 3 relative to each other

e.g.
	relative ordering: 0	 1
	indices mod 3:	 0 1 2 0 1
	indices:		   0 1 2 3 4
	chars:			 c a c a o

unioned with

	relative ordering:   0 1   2
	indices mod 3:	 0 1 2 0 1
	indices:		   0 1 2 3 4
	chars:			 c a c a o

yields

	relative ordering: 0 0 1 1 2
	indices mod 3:	 0 1 2 0 1
	indices:		   0 1 2 3 4
	chars:			 c a c a o -}
get_relative_suffix_orderings :: A.Array Index Order -> A.Array Index Order -> A.Array Index Order
get_relative_suffix_orderings suffix_ordering_T0 suffix_ordering_T1T2 =
	A.listArray (0, length) $ zipWith take_existing
		(A.elems suffix_ordering_T0)
		(A.elems suffix_ordering_T1T2)
	where
		length :: Int
		length = alength suffix_ordering_T0 -- arbitrary which one

		take_existing :: Order -> Order -> Order
		take_existing a b = if a == (-1)
			then b -- b can't be by construction of the two arrays
			else a

array_of_sort_orders_to_sorted_array_of_indices :: A.Array Index Order -> A.Array Int Index
array_of_sort_orders_to_sorted_array_of_indices arr =
	A.listArray bounds sorted_array_of_indices
		where
			bounds :: (Int, Int)
			bounds = (0, (length sorted_array_of_indices) - 1)

			-- `elems` returns all values in the map in ascending order of their keys
			sorted_array_of_indices :: [Index]
			sorted_array_of_indices = M.elems . foldr insert M.empty $ arr'

			insert :: (Index, Order) -> M.Map Index Order -> M.Map Order Index
			insert (i, o) m = M.insert o i m

			arr' :: [(Index, Order)]
			arr' = filter valid . zip [0..] . A.elems $ arr

			valid :: (Index, Order) -> Bool
			valid (i, o) = (o /= -1)

indices_in_Tk :: S.ByteString -> Index -> [Index]
indices_in_Tk str k = takeWhile in_range . map ((+k) . (*3)) $ [0..]
	where
		in_range :: Index -> Bool
		in_range = (< (S.length str))

get_suffix_ordering_T0 :: S.ByteString -> A.Array Index Order -> A.Array Index Order
get_suffix_ordering_T0 str suffix_ordering_T1T2 =
	(initial_array str) A.// sorted_index_order_pairs
		where
			sorted_index_order_pairs :: [(Index, Order)]
			sorted_index_order_pairs = zip (map fst sorted_pairs) [0..]

			-- TODO: radix sort
			sorted_pairs :: [(Index, Order)]
			sorted_pairs = L.sortBy paircmpfn pairs

			paircmpfn :: (Index, Order) -> (Index, Order) -> Ordering
			paircmpfn (a, i) (b, j) =
				if (S.index str a) /= (S.index str b)
					then (S.index str a) `compare` (S.index str b)
					else i `compare` j

			-- pairs of (index of char in `T0`, suffix ordering of suffix starting at char directly after (one in `T1`))
			pairs :: [(Index, Order)]
			pairs = zip (indices_in_Tk str 0) suffix_ordering_T1
				where
					suffix_ordering_T1' :: [Order]
					suffix_ordering_T1' = map ((A.!) suffix_ordering_T1T2) $ indices_in_Tk str 1

					-- can pick any elem for last char; it won't ever need to be compared because (see `paircmpfn`) the char for it is '$', which is unique
					suffix_ordering_T1 :: [Order]
					suffix_ordering_T1 = if (length suffix_ordering_T1') == length (indices_in_Tk str 0)
						then suffix_ordering_T1'
						else suffix_ordering_T1' ++ [0]

suffix_merge_cmp :: S.ByteString -> A.Array Index Order -> Index -> Index -> Ordering
suffix_merge_cmp str relative_suffix_orderings i0 i12 =
	let
		ch_ordering :: Ordering
		ch_ordering = (S.index str i0) `compare` (S.index str i12)
	
		-- Don't need to worry about walking off end of array because fo the `+ 1`ing; there will only be one '$' in the string, so we won't hit this case
		-- Only call with k=1 or k=2 (not k=0)
		suffix_ordering_of_ch_k_after :: Int -> Ordering
		suffix_ordering_of_ch_k_after k =
			let
				i0' = f i0
				i12' = f i12

				-- when recursing on the blocks of `T$[1:] ++ T$[2:]` (plus padding), we don't append '$', so if we're at the last char and it's not '$', we *do* want to use its suffix ordering.
				is_last_char_and_shouldnt_use :: Int -> Bool
				is_last_char_and_shouldnt_use nk =
					(nk == (S.length str) - 1) &&
					((S.index str nk) == '$')

				f :: Int -> Int
				f n = if is_last_char_and_shouldnt_use (n + k)
					then n + k - 1 -- -1 is to not use ordering of '$, which will always be 0, defeating the purpose'
					else n + k
			in
				compare
					(relative_suffix_orderings A.! i0')
					(relative_suffix_orderings A.! i12')

		ch_and_suffix_ordering_of_ch_after :: Ordering
		ch_and_suffix_ordering_of_ch_after =
			case compare
				(S.index str (i0 + 1))
				(S.index str (i12 + 1))
			of
				LT -> LT
				GT -> GT
				EQ -> suffix_ordering_of_ch_k_after 2
	in
		case ch_ordering of
			LT -> LT
			GT -> GT
			EQ -> if (i12 + 1) `mod` 3 == 0 -- (i0 + 1) `mod` 3 /= 0 ever
				then ch_and_suffix_ordering_of_ch_after
				else case suffix_ordering_of_ch_k_after 1 of
					LT -> LT
					GT -> GT
					EQ -> error "The suffix ordering is between suffixes that are both not 0 mod 3, so we know this ordering won't be `EQ`. The fact that we're getting `EQ` now means there's a bug somewhere."

test0 :: Bool
test0 = (A.elems . dc3 . S.pack $ "monsoonnomnoms$") == [14,9,0,12,6,7,10,2,8,11,5,1,4,3,13]

dc3 :: S.ByteString -> SuffixArray
dc3 str = A.listArray (0, (S.length str) - 1) suffix_ordering_all_sorted
	where
		suffix_ordering_T1T2 :: A.Array Index Order
		suffix_ordering_T1T2 = get_suffix_ordering_T1T2 str

		suffix_ordering_T0 :: A.Array Index Order
		suffix_ordering_T0 = get_suffix_ordering_T0 str suffix_ordering_T1T2

		relative_suffix_orderings :: A.Array Index Order
		relative_suffix_orderings = get_relative_suffix_orderings suffix_ordering_T0 suffix_ordering_T1T2

		suffix_ordering_all_sorted :: [Order]
		suffix_ordering_all_sorted = mergeBy cmpfn
			index_order_T0
			index_order_T1T2
			where
				cmpfn :: Index -> Index -> Ordering
				cmpfn = (suffix_merge_cmp str relative_suffix_orderings)

				index_order_T0 :: [Index]
				index_order_T0 = (A.elems . f $ suffix_ordering_T0)

				index_order_T1T2 :: [Index]
				index_order_T1T2 = (A.elems . f $ suffix_ordering_T1T2)

				f :: A.Array Index Order -> A.Array Int Index
				f = array_of_sort_orders_to_sorted_array_of_indices

