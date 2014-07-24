{-# LANGUAGE ScopedTypeVariables #-}

module SuffixArray
( SuffixArray(..)
, construct
) where

import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Array as Array
import qualified Data.ByteString.Char8 as ByteString

import Test.QuickCheck

import Data.Char
import Data.Maybe

import qualified Zora.List as ZList

-- TODO: make SuffixArray a data type that stores the string

type Index = Int
type Order = Int
type SuffixArray = Array.Array Int Index
type Substring = (Index, Int)

nul :: Char
nul = chr 0

substring :: ByteString.ByteString -> Substring -> ByteString.ByteString
substring str (i, len) = ByteString.take len . ByteString.drop i $ str

substr_drop :: Int -> Substring -> Substring
substr_drop d (i, len) = (i + d, len - d)

substr_take :: Int -> Substring -> Substring
substr_take t (i, len) = (i, t)

initial_array :: ByteString.ByteString -> Array.Array Int Int
initial_array str = Array.listArray (0, (ByteString.length str) - 1) $ repeat (-1)

construct_naive :: String -> SuffixArray
construct_naive str = Array.listArray bounds
	. map fst
	. List.sortBy (Ord.comparing snd)
	. zip [0..]
	. init
	. List.tails
	. pad
	$ str
	where
		bounds :: (Int, Int)
		bounds = (0, (length (pad str)) - 1)

pad_and_double :: ByteString.ByteString -> ByteString.ByteString
pad_and_double str = (f 1) `ByteString.append` (f 2)
	where
		f :: Int -> ByteString.ByteString
		f k = pad_to_3 . ByteString.drop k $ str

		pad_to_3 :: ByteString.ByteString -> ByteString.ByteString
		pad_to_3 s = s `ByteString.append` (ByteString.pack (replicate k nul))
			where
				k :: Int
				k = if mod3 == 0
					then mod3
					else 3 - mod3 -- plain `mod3` is not how many to pad; we need 2 if it's 1 and 1 if it's 2

				mod3 :: Int
				mod3 = (ByteString.length s) `mod` 3

order_blocks :: [Substring] -> ByteString.ByteString -> [Order]
order_blocks blocks doubled_str = uniquely_ordered_blocks
	where
		block_ordering :: [Order]
		block_ordering = order_blocks' blocks doubled_str

		uniquely_ordered_blocks :: [Order]
		uniquely_ordered_blocks =
			if not . ZList.contains_duplicates $ block_ordering
				then block_ordering
				else apply_transformation
					. Array.elems
					. dc3
					. ByteString.pack
					. map chr
					$ block_ordering
					-- The condition of the if-statement above acts as a base case since (the recursion can never reach a depth greater than one).

		-- input: array (0,9) [(0,9),(1,2),(2,3),(3,5),(4,7),(5,8),(6,6),(7,0),(8,1),(9,4)]
		-- output: apply ([9] = 0, [2] = 1, ...) to something
		apply_transformation :: [Order] -> [Order]
		apply_transformation arr = Array.elems $ Array.array
			(0, (length arr) - 1)
			(zip arr [0..])

-- input example:
--     blocks: ["$$$","nom","nso","omn","oms","onn","ons","oon","s$$"]
--     doubled_str: "onsoonnomnoms$$nsoonnomnoms$$$"
-- result example:
--     [6,7,1,1,8,2,5,3,4,0]
order_blocks' :: [Substring] -> ByteString.ByteString -> [Order]
order_blocks' blocks doubled_str = map (sort_map Map.!) blocks
	where
		sort_map :: Map.Map Substring Order
		sort_map = foldr insert Map.empty blocks_with_order
			where
				insert :: (Substring, Order) -> Map.Map Substring Order -> Map.Map Substring Order
				insert (s, i) = Map.insert s i

		blocks_with_order :: [(Substring, Int)]
		blocks_with_order =
			concatMap clean
				. flip zip [0..]
				. List.groupBy substr_pair_eq
				. map make_substr_pair
				$ sorted_blocks
			where
				clean :: ([(Substring, ByteString.ByteString)], Int) -> [(Substring, Int)]
				clean (substrs, i) = map (\ss -> (fst ss, i)) substrs

				make_substr_pair :: Substring -> (Substring, ByteString.ByteString)
				make_substr_pair substr = (substr, substring doubled_str substr)

				substr_pair_eq :: (Substring, ByteString.ByteString) -> (Substring, ByteString.ByteString) -> Bool
				substr_pair_eq p1 p2 = (snd p1) == (snd p2)

				-- TODO: radix sort
				sorted_blocks :: [Substring]
				sorted_blocks = List.sortBy (Ord.comparing (substring doubled_str)) blocks

-- At index `i` is the sort order of the suffix starting at `i` (within `T1` \union `T2`)
-- Assumes an input string of length at least something like 3
get_suffix_ordering_T1T2 :: ByteString.ByteString -> Array.Array Index Order
get_suffix_ordering_T1T2 str = initial' Array.// (zip
	(indices_in_Tk str 2)
	ordering_of_T2_suffixes)
		where
			doubled_str :: ByteString.ByteString
			doubled_str = pad_and_double str

			blocks :: [Substring]
			blocks = map (\i -> (i, 3)) $ indices_in_Tk doubled_str 0

			block_ordering :: [Order]
			block_ordering = order_blocks blocks doubled_str

			partitioning :: [[Order]]
			partitioning = ZList.partition_into_k 2 block_ordering

			ordering_of_T1_suffixes :: [Order]
			ordering_of_T1_suffixes = partitioning !! 0

			ordering_of_T2_suffixes :: [Order]
			ordering_of_T2_suffixes = partitioning !! 1

			initial' :: Array.Array Index Order
			initial' = (initial_array str) Array.// (zip
				(indices_in_Tk str 1)
				ordering_of_T1_suffixes)

alength :: Array.Array Int b -> Int
alength = snd . Array.bounds

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
get_relative_suffix_orderings :: Array.Array Index Order -> Array.Array Index Order -> Array.Array Index Order
get_relative_suffix_orderings suffix_ordering_T0 suffix_ordering_T1T2 =
	Array.listArray (0, length) $ zipWith take_existing
		(Array.elems suffix_ordering_T0)
		(Array.elems suffix_ordering_T1T2)
	where
		length :: Int
		length = alength suffix_ordering_T0 -- arbitrary which one

		take_existing :: Order -> Order -> Order
		take_existing a b = if a == (-1)
			then b -- b can't be by construction of the two arrays
			else a

array_of_sort_orders_to_sorted_array_of_indices :: Array.Array Index Order -> Array.Array Int Index
array_of_sort_orders_to_sorted_array_of_indices arr =
	Array.listArray bounds sorted_array_of_indices
		where
			bounds :: (Int, Int)
			bounds = (0, (length sorted_array_of_indices) - 1)

			-- `elems` returns all values in the map in ascending order of their keys
			sorted_array_of_indices :: [Index]
			sorted_array_of_indices = Map.elems . foldr insert Map.empty $ arr'

			insert :: (Index, Order) -> Map.Map Index Order -> Map.Map Order Index
			insert (i, o) = Map.insert o i

			arr' :: [(Index, Order)]
			arr' = filter valid . zip [0..] . Array.elems $ arr

			valid :: (Index, Order) -> Bool
			valid (_, o) = o /= -1

indices_in_Tk :: ByteString.ByteString -> Index -> [Index]
indices_in_Tk str k = takeWhile in_range . map ((+k) . (*3)) $ [0..]
	where
		in_range :: Index -> Bool
		in_range = (< (ByteString.length str))

get_suffix_ordering_T0 :: ByteString.ByteString -> Array.Array Index Order -> Array.Array Index Order
get_suffix_ordering_T0 str suffix_ordering_T1T2 =
	(initial_array str) Array.// sorted_index_order_pairs
		where
			sorted_index_order_pairs :: [(Index, Order)]
			sorted_index_order_pairs = zip (map fst sorted_pairs) [0..]

			-- TODO: radix sort
			sorted_pairs :: [(Index, Order)]
			sorted_pairs = List.sortBy paircmpfn pairs

			paircmpfn :: (Index, Order) -> (Index, Order) -> Ordering
			paircmpfn (a, i) (b, j) =
				if (ByteString.index str a) /= (ByteString.index str b)
					then (ByteString.index str a) `compare` (ByteString.index str b)
					else i `compare` j

			-- pairs of (index of char in `T0`, suffix ordering of suffix starting at char directly after (one in `T1`))
			pairs :: [(Index, Order)]
			pairs = zip (indices_in_Tk str 0) suffix_ordering_T1
				where
					suffix_ordering_T1' :: [Order]
					suffix_ordering_T1' = map ((Array.!) suffix_ordering_T1T2) $ indices_in_Tk str 1

					-- can pick any elem for last char; it won't ever need to be compared because (see `paircmpfn`) the char for it is '$', which is unique
					suffix_ordering_T1 :: [Order]
					suffix_ordering_T1 = if (length suffix_ordering_T1') == length (indices_in_Tk str 0)
						then suffix_ordering_T1'
						else suffix_ordering_T1' ++ [0]

suffix_merge_cmp :: ByteString.ByteString -> Array.Array Index Order -> Index -> Index -> Ordering
suffix_merge_cmp str relative_suffix_orderings i0 i12 =
	let
		ch0  = ByteString.index str i0
		ch12 = ByteString.index str i12

		ch0_1  = ByteString.index str (i0  + 1)
		ch12_1 = ByteString.index str (i12 + 1)

		suffix_order0_1  = relative_suffix_orderings Array.! (i0  + 1)
		suffix_order12_1 = relative_suffix_orderings Array.! (i12 + 1)

		suffix_order0_2  = relative_suffix_orderings Array.! (i0  + 2)
		suffix_order12_2 = relative_suffix_orderings Array.! (i12 + 2)
	in
		if (i12 `mod` 3) == 1
			then (ch0,  suffix_order0_1) `compare`
				 (ch12, suffix_order12_1)
			else (ch0,  ch0_1,  suffix_order0_2) `compare` 
				 (ch12, ch12_1, suffix_order12_2)

runtests :: IO ()
runtests = quickCheckWith stdArgs { maxSuccess = 5000 } test_dc3

test_dc3 :: String -> Bool
test_dc3 s = invalid || ((construct s) == (construct_naive s))
	where
		invalid :: Bool
		invalid = nul `elem` s

pad :: String -> String
pad "" = "\0"
pad str = if (last str) == nul
	then str
	else str ++ [nul]

construct :: String -> SuffixArray
construct str = if nul `elem` str
	then error error_message
	else dc3 . ByteString.pack . pad $ str
	where
		error_message :: String
		error_message = "Input (" ++ str ++ ") not accepted (input string contains the NUL-terminator ('\\0'))."

dc3 :: ByteString.ByteString -> SuffixArray
dc3 str = if (ByteString.length str) <= 3
	then construct_naive (ByteString.unpack str) -- doubling-and-padding logic doesn't play incely with strings not long enough
	else Array.listArray (0, (ByteString.length str) - 1) suffix_ordering_all_sorted
	where
		suffix_ordering_T1T2 :: Array.Array Index Order
		suffix_ordering_T1T2 = get_suffix_ordering_T1T2 str

		suffix_ordering_T0 :: Array.Array Index Order
		suffix_ordering_T0 = get_suffix_ordering_T0 str suffix_ordering_T1T2

		relative_suffix_orderings :: Array.Array Index Order
		relative_suffix_orderings = get_relative_suffix_orderings suffix_ordering_T0 suffix_ordering_T1T2

		suffix_ordering_all_sorted :: [Order]
		suffix_ordering_all_sorted = ZList.merge_by cmpfn
			index_order_T0
			index_order_T1T2
			where
				cmpfn :: Index -> Index -> Ordering
				cmpfn = suffix_merge_cmp str relative_suffix_orderings

				index_order_T0 :: [Index]
				index_order_T0 = Array.elems . f $ suffix_ordering_T0

				index_order_T1T2 :: [Index]
				index_order_T1T2 = Array.elems . f $ suffix_ordering_T1T2

				f :: Array.Array Index Order -> Array.Array Int Index
				f = array_of_sort_orders_to_sorted_array_of_indices
