
module CTree
( CTree(..)
, CTree.fromList
) where

import qualified Data.Map as M
import Stack
import HLib

type CTree a = CartesianTree a

data Direction = L | R deriving (Show, Eq)

-- Node e l r p
data CartesianTree a
	= Empty
	| Node a (CartesianTree a) (CartesianTree a) deriving Show

instance (Eq a) => Eq (CartesianTree a) where
	Empty == Empty = True
	Empty == _ = False
	_ == Empty = False
	x == y =
		(value x) == (value y)
		&& (left  x) == (left  y)
		&& (right x) == (right y)

instance (Ord a) => Ord (CartesianTree a) where
	Empty < _ = True
	_ < Empty = False
	x < y = (value x) < (value y)

left   :: CTree a -> CTree a
right  :: CTree a -> CTree a
left   (Node _ l _) = l
right  (Node _ _ r) = r

value :: CTree a -> a
value Empty = error "Can't get the value out of an empty node."
value (Node e _ _) = e

fromList :: (Ord a) => [a] -> CTree a
fromList list = fromList' list Empty Empty

-- returns the new root of the tree
update_node :: (Ord a) => CTree a -> CTree a -> (CTree a, CTree a)
update_node old new = (new, new_root)
	where new_root = update_node' old new

update_node' :: (Ord a) => CTree a -> CTree a -> CTree a
update_node' old new
	| is_root old = new
	| otherwise =
		let
			old_parent@(Node e l r p dir) = parent old
			new_parent = if dir == L
				then Node e new r   p dir
				else Node e l   new p dir
		in
			update_node' old_parent new_parent

type CTMap a = M.Map (CTree a) (CTree a)

old_to_new_mapping :: (Ord a) => CTree a -> CTree a -> CTMap a
old_to_new_mapping old new = old_to_new_mapping' old new M.empty

old_to_new_mapping' :: (Ord a) => CTree a -> CTree a -> CTMap a -> CTMap a
old_to_new_mapping' old new m =
	let
		m' = M.insert old new m
	in
		if (is_root old)
			then m'
			else old_to_new_mapping' (parent old) (parent new) m'

-- circular dependency means never set properly?
set_parent :: CTree a -> CTree a -> (CTree a, CTree a)
set_parent n Empty = (n, n)
set_parent
	node_with_unset_parent@(Node e  l  r  p  dir)
	future_parent_of_node@(Node  e' l' r' p' dir')
	= (node_with_set_parent, new_root)
		where
			n = node_with_unset_parent
			parent_with_child_set = if dir == L
				then Node e' n  r' p' dir'
				else Node e' l' n  p' dir'
			(new_parent, new_root) = set_parent parent_with_child_set p'

			node_with_set_parent = Node e l r new_parent dir

-- elem of parent, other child of parent
data Focus a = Left a (CartesianTree a) | Right a (CartesianTree a) deriving Show
type Foci = [Focus]
type Zipper a = (Foci a, CartesianTree a)

fromList' :: (Ord a) => [a] -> Zipper a -> CTree a 
fromList' [] zipper@(foci, tree) _ = tree
fromList' list zipper@(foci, tree) = fromList' list' zipper'
	where
		e = head list
		list' = tail list

		(rspine_gt, rspine_lt) = takeWhileAndRest f $ rspine
			where
				rspine = takeWhile (not . is_root) . iterate parent $ rmost_node
				f node = (not . is_empty $ rspine) &&
						((value node) >= e)

		last_popped_elem = last_maybe rspine_gt
			where last_maybe l = if l /= [] then Just (last l) else Nothing
		
		new_node_with_unset_parent = if last_popped_elem /= Nothing
			then Node e (from_just last_popped_elem) Empty Empty R
			else Node e Empty Empty Empty R

		(new_root, tree') = if rspine_lt == []
			then (new_node_with_unset_parent, new_node_with_unset_parent)
			else (new_node, new_root)
				where
					future_parent_of_new_node = peek rspine_lt
					(new_node, new_root) = set_parent
						new_node_with_unset_parent
						future_parent_of_new_node

		zipper' =






