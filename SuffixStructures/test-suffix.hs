module Main where

import System.Exit

import qualified SuffixArray as SA
import qualified SuffixTree as ST

import qualified Data.List as List

import Test.QuickCheck

test_suffixarray :: IO ()
test_suffixarray = quickCheckWith stdArgs { maxSuccess = 5000 } test_dc3
	where
		test_dc3 :: String -> Bool
		test_dc3 s = invalid || ((SA.construct s) == (SA.construct_naive s))
			where
				invalid :: Bool
				invalid = SA.nul `elem` s

test_suffixtree :: IO ()
test_suffixtree = quickCheckWith stdArgs { maxSuccess = 500 } test_stree_substr_query
	where
		test_stree_substr_query :: String -> Bool
		test_stree_substr_query s = invalid || all_substrings_present
			where
				invalid :: Bool
				invalid = SA.nul `elem` s

				all_substrings_present :: Bool
				all_substrings_present = all (ST.contains_substring stree) all_substrings

				all_substrings :: [String]
				all_substrings = List.nub . concatMap List.inits . List.tails $ s

				stree :: ST.SuffixTree
				stree = ST.construct s

main :: IO ()
main = do
    test_suffixarray
    test_suffixtree
    exitSuccess
