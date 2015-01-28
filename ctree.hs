{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module CTree
( CTree(..)
, CTree.fromList
, CTree.left_child
, CTree.right_child
, CTree.value
, CTree.is_empty
, G.render
) where

import qualified Data.Map as M

import qualified Zora.Graphing.DAGGraphing as G

data CTree a
	= Empty
	| Node a (CTree a) (CTree a) deriving Show

instance (Eq a) => Eq (CTree a) where
	Empty == Empty = True
	Empty == _ = False
	_ == Empty = False
	x == y =
		(value x) == (value y)
		&& (left_child  x) == (left_child  y)
		&& (right_child x) == (right_child y)

instance (Ord a) => Ord (CTree a) where
	Empty < _ = True
	_ < Empty = False
	x < y = (value x) < (value y)
	x <= y = (x == y) || (x < y)

instance (Show a) => G.DAGGraphable (CTree a) where
	expand :: CTree a -> Maybe (Maybe String, [(Maybe String, CTree a)])
	expand Empty = Nothing
	expand (Node e l r) = Just (Just (show e), [(Nothing, l), (Nothing, r)])

left_child :: CTree a -> CTree a
left_child (Node _ l _) = l

right_child :: CTree a -> CTree a
right_child (Node _ _ r) = r

is_empty :: CTree a -> Bool
is_empty Empty = True
is_empty _ = False

value :: CTree a -> a
value Empty = error "Can't get the value out of an empty node."
value (Node e _ _) = e

---------------------------
--         Zipper        --
---------------------------

-- elem of parent, other child of parent
data Focus a = L a (CTree a) | R a (CTree a) deriving Show
type Foci a = [Focus a]
type Zipper a = (Foci a, CTree a)

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
go_left z@(foci, Node e l r) = ((L e r):foci, l)

go_right :: Zipper a -> Zipper a
go_right z@(foci, Empty) = error "Node is empty"
go_right z@(foci, Node e l r) = ((R e l):foci, r)

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

insert_at_point :: forall a . (Zipper a, Bool) -> a -> Zipper a
insert_at_point (z@(foci, root), False) new_elem = ([], Node new_elem root Empty) -- foci is [] anyway
insert_at_point
	(z@(foci, parent@(Node e l r)), True) -- can assume `parent` will never be `Empty` (see `parent_of_new_elem`)
	new_elem
	= go_right $ (foci, parent')
		where
			new_node_left_child :: CTree a
			new_node_left_child = if has_right_child z
				then r
				else Empty
			
			parent' :: CTree a
			parent' = Node e l (Node new_elem new_node_left_child Empty)

---------------------------
--       fromList        --
---------------------------

parent_of_new_elem :: (Ord a) => a -> Zipper a -> (Zipper a, Bool)
parent_of_new_elem new_elem z@(foci, tree@(Node e l r)) =
	if e <= new_elem
		then (z, True) -- True signifies yes parent (regular case)
		else if not . has_parent $ z
			then (z, False) -- False signifies no parent
			else parent_of_new_elem new_elem (go_up z)

-- can make this just a wrapper, probably
fromList :: forall a . (Ord a) => [a] -> CTree a
fromList [] = Empty
fromList list = fromList' list' zipper0
	where
		e :: a
		e = head list

		list' :: [a]
		list' = tail list

		node :: CTree a
		node = Node e Empty Empty

		zipper0 :: Zipper a
		zipper0 = ([], node)

fromList' :: forall a . (Ord a) => [a] -> Zipper a -> CTree a 
fromList' [] z = snd . go_to_top $ z
fromList' list z_rspine_last@(foci, tree) = fromList' list' zipper'
	where
		e :: a
		e = head list

		list' :: [a]
		list' = tail list

		z_parent_of_new_elem :: (Zipper a, Bool)
		z_parent_of_new_elem = parent_of_new_elem e z_rspine_last

		zipper' :: Zipper a
		zipper' = insert_at_point z_parent_of_new_elem e
