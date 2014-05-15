
module CTree
( CTree(..)
, CTree.fromList
) where

import qualified Data.Map as M
import Stack
import HLib

type CTree a = CartesianTree a

data CartesianTree a
	= Empty
	| Node a (CartesianTree a) (CartesianTree a) deriving Show

instance (Eq a) => Eq (CartesianTree a) where
	Empty == Empty = True
	Empty == _ = False
	_ == Empty = False
	x == y =
		(value x) == (value y)
		&& (left_child  x) == (left_child  y)
		&& (right_child x) == (right_child y)

instance (Ord a) => Ord (CartesianTree a) where
	Empty < _ = True
	_ < Empty = False
	x < y = (value x) < (value y)

left_child :: CTree a -> CTree a
left_child (Node _ l _) = l

right_child :: CTree a -> CTree a
right_child (Node _ _ r) = r

value :: CTree a -> a
value Empty = error "Can't get the value out of an empty node."
value (Node e _ _) = e

---------------------------
--         Zipper        --
---------------------------

-- elem of parent, other child of parent
data Focus a = L a (CartesianTree a) | R a (CartesianTree a) deriving Show
type Foci a = [Focus a]
type Zipper a = (Foci a, CartesianTree a)

go_to_top :: Zipper a -> Zipper a
go_to_top z = if has_parent z
	then go_to_top . go_up $ z
	else z

go_up :: Zipper a -> Zipper a
go_up z@([], tree) = error "Already at root"
go_up z@((L e r):foci, tree) = (foci, Node e tree r)
go_up z@((R e l):foci, tree) = (foci, Node e l tree)

go_left :: Zipper a -> Zipper a
go_left z@(foci, Empty) = error "Node is empty"
go_left z@(foci, Node e l r) = (focus:foci, l)
	where focus = L e r

go_right :: Zipper a -> Zipper a
go_right z@(foci, Empty) = error "Node is empty"
go_right z@(foci, Node e l r) = (focus:foci, r)
	where focus = R e l

has_parent :: Zipper a -> Bool
has_parent z@([], _) = False
has_parent z@(_, _) = True

has_left_child :: Zipper a -> Bool
has_left_child z@(_, Empty) = False
has_left_child z@(_, Node _ Empty _) = False
has_left_child _ = True

has_right_child :: Zipper a -> Bool
has_right_child z@(_, Empty) = False
has_right_child z@(_, Node _ _ Empty) = False
has_right_child _ = True

-- Cartesian tree insertion mechanism:
-- 
-- 
-- 
-- 
-- 
insert_at_point :: (Zipper a, Bool) -> a -> Zipper a
insert_at_point
	(z@(foci, root), False)
	new_elem
	= ([], new_root) -- foci is [] anyway
		where new_root = Node new_elem root Empty

insert_at_point
	(z@(foci, parent@(Node e l r)), True) -- can assume `parent` will never be `Empty` (see `parent_of_new_elem`)
	new_elem
	= go_right $ (foci, parent')
		where
			new_node_left_child = if has_right_child z
				then r
				else Empty
			
			parent' = Node e l (Node new_elem new_node_left_child Empty)

---------------------------
--       fromList        --
---------------------------

-- False signifies no parent, so we return `Left <root>`
-- True signifies yes parent (regular case)
parent_of_new_elem :: (Ord a) => a -> Zipper a -> (Zipper a, Bool)
parent_of_new_elem new_elem z@(foci, tree@(Node e l r)) =
	if e <= new_elem
		then (z, True)
		else if not . has_parent $ z
			then (z, False)
			else parent_of_new_elem new_elem (go_up z)
{-
(rspine_gt, rspine_lt) = takeWhileAndRest f $ rspine
			where
				rspine = takeWhile (not . is_root) . iterate parent $ rmost_node
				f node = (not . is_empty $ rspine) &&
						((value node) >= e)
-}

-- can make this just a wrapper, probably
fromList :: (Ord a) => [a] -> CTree a
fromList [] = Empty
fromList list =
	let
		(e, list') = (head list, tail list)
		node = Node e Empty Empty
	in
		fromList' list' ([], node)

fromList' :: (Ord a) => [a] -> Zipper a -> CTree a 
fromList' [] z = snd . go_to_top $ z
fromList' list z_rspine_last@(foci, tree) = fromList' list' zipper'
	where
		e = head list
		list' = tail list

		z_parent_of_new_elem = parent_of_new_elem e z_rspine_last

		-- is this the right location for the zipper? I think so, but...
		zipper' = insert_at_point z_parent_of_new_elem e





