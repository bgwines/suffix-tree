{-# LANGUAGE ScopedTypeVariables #-}

module FusedCTree
( FusedCTree(..)
, FusedCTree.fuse
, FusedCTree.value
, FusedCTree.get_children
, FusedCTree.is_empty
) where

import qualified CTree

data FusedCTree a
	= Empty
	| Node a [FusedCTree a] deriving Show

is_empty :: (FusedCTree a) -> Bool
is_empty Empty = True
is_empty _ = False

value :: (FusedCTree a) -> a
value Empty = error "Can't get the value out of an empty node"
value (Node e _) = e

get_children :: (FusedCTree a) -> [FusedCTree a]
get_children Empty = []
get_children (Node _ children) = children

fuse :: forall a . (Eq a) => (CTree.CTree a) -> (FusedCTree a)
fuse cnode
	| (CTree.is_empty cnode) = FusedCTree.Empty
	| otherwise = Node e children
		where
			e :: a
			e = CTree.value cnode

			l :: CTree.CTree a
			l = CTree.left_child cnode
			
			r :: CTree.CTree a
			r = CTree.right_child cnode

			children :: [FusedCTree a]
			children = nonmatching_children ++ (concat . map get_children $ matching_children)
				where
					initial_children :: [FusedCTree a]
					initial_children = [fuse l, fuse r]

					matches :: (FusedCTree a) -> Bool
					matches Empty = False
					matches node@(Node e' _) = (e == e')

					matching_children :: [FusedCTree a]
					matching_children = filter matches initial_children

					nonmatching_children :: [FusedCTree a]
					nonmatching_children = filter (not . matches) initial_children





