module Stack
( Stack(..)
, Stack.empty
, Stack.is_empty
, Stack.peek
, Stack.push
, Stack.pop
) where

type Stack a = [a]

empty :: Stack a
empty = []

is_empty :: Stack a -> Bool
is_empty stack = (length stack) == 0

peek :: Stack a -> a
peek = head

push :: a -> Stack a -> Stack a
push = (:)

pop :: Stack a -> Stack a
pop = tail