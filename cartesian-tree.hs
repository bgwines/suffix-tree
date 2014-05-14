
module CartesianTree
( CartesianTree(..)
, CartesianTree.fromList
) where

import Stack
import HLib

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

left  :: CartesianTree a -> CartesianTree a
right :: CartesianTree a -> CartesianTree a
left  (Node _ l _) = l
right (Node _ _ r) = r

value :: CartesianTree a -> a
value Empty = error "Can't get the value out of an empty node."
value (Node a _ _) = a

-- http://www.stanford.edu/class/cs166/lectures/01/Slides01.pdf
fromList :: (Ord a) => [a] -> CartesianTree a
fromList list = fromList' list Empty Stack.empty

fromList' :: (Ord a) => 
	[a]
	-> CartesianTree a
	-> Stack (CartesianTree a)
	-> CartesianTree a 
fromList' [] tree _ = tree
fromList' list tree stack = fromList' list' tree' stack'
	where
		e = head list
		list' = tail list

		(stack_front, stack_back) = takeWhileAndRest f stack
			where f _ = (not . is_empty $ stack) &&
						((value . peek $ stack) >= e)

		last_popped_elem = last_maybe stack_front
			where last_maybe l = if l /= [] then Just (last l) else Nothing
		
		new_node = if last_popped_elem /= Nothing
			then Node e (from_just last_popped_elem) Empty
			else Node e Empty Empty

		tree' = if stack_back == []
			then new_node
			else
				let
					root@(Node elem l r) = peek stack_back
				in
					Node elem l new_node -- r will be Empty all the time

		stack' = push new_node $
			if stack_back /= [] -- i.e. if we didn't just place a new root, but rather "modified" the old one
				then push tree' . pop $ stack_back
				else stack_back

