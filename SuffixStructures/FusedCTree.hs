{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module SuffixStructures.FusedCTree
( FusedCTree(..)
, fuse
, value
, get_children
, is_empty
, empty
, G.render
) where

import qualified SuffixStructures.CTree as CTree

import qualified Zora.Graphing.DAGGraphing as G

data FusedCTree a
	= Empty
	| Node a [FusedCTree a]
	deriving (Show, Eq)

instance (Show a) => G.DAGGraphable (FusedCTree a) where
	expand :: FusedCTree a -> Maybe (Maybe String, [(Maybe String, FusedCTree a)])
	expand Empty = Nothing
	expand (Node e children) = Just
		( Just (show e)
		, map (\x -> (Nothing, x)) children )

empty :: FusedCTree a
empty = Empty

is_empty :: FusedCTree a -> Bool
is_empty Empty = True
is_empty _ = False

value :: FusedCTree a -> a
value Empty = error "Can't get the value out of an empty node"
value (Node e _) = e

get_children :: FusedCTree a -> [FusedCTree a]
get_children Empty = []
get_children (Node _ children) = children

fuse :: forall a . (Eq a) => CTree.CTree a -> FusedCTree a
fuse cnode
	| (CTree.is_empty cnode) = Empty
	| otherwise = Node e children
		where
			e :: a
			e = CTree.value cnode

			children :: [FusedCTree a]
			children = concatMap
				( child_or_children_of_child
				. fuse
				. ($ cnode)
				) [CTree.left_child, CTree.right_child]
				where
					matches :: FusedCTree a -> Bool
					matches Empty = False
					matches node@(Node e' _) = e == e'

					child_or_children_of_child :: FusedCTree a -> [FusedCTree a]
					child_or_children_of_child child =
						if matches child
							then get_children child
							else [child]
