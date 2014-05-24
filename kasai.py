import copy
import pdb

def invert(pos):
    rank = copy.copy(pos)
    for i in xrange(len(pos)):
        rank[pos[i]] = i
    return rank

# pos[i]: "what's the i^th lexicographically ordered suffix (== what _position_ does it start at?)?
# rank[i]: "what's the lexicographic order (_rank_) of the suffix starting at i?"
def get_height(s, pos):
    rank = invert(pos)
    h = 0
    height = {}
    for i in xrange(len(rank)): # (*)
        if rank[i] > 0: # if not "$"
            k = pos[rank[i] - 1]
            while s[i+h] == s[k+h]:
                h += 1
            height[rank[i]] = h
            if h > 0:
                h -= 1
    return height

'''
    `i` represents the starting index of a substring

        => `rank[i]` is the lexicographic order (rank) of suffix starting at `i`

        => each it. of the forloop fills `height` at the index of the next substring
            e.g.
                1st it. finds LCP for "nonsense$" and whatever suffix is lexicographically before that
                2nd it. finds LCP for  "onsense$" and whatever suffix is lexicographically before that
                3rd it.        ...      "nsense$"     ...
                ...

    `k` represents the starting index of the suffix we're comparing against. Simply put, `pos[rank[i]]` is just `i`, the index of the current suffix (one of the two in the comparison); to get the other, since we want to compare with an adjacent suffix in the suffix array (`pos`), we get precisely that suffix (it's adjacent in `pos`): pos[rank[i] - 1] (remember that `pos[j]` for any `j` is the starting index of the `j`th lexicographically ordered suffix)


    `if h > 0: h -= 1` only makes sense in context of comparing `s[i+h]` to `s[k+h]`. So, why `+h`? OK, this is pretty cool. So basically the idea is that if `h` > 0 then that means that last iteration we compared some suffix `S` with the suffix lexicographically neighbouring it (this implementation uses the one previous to it in `pos`, but you could do the one after it if you wanted) (let it be `T`) and found some overlap. Because of the way the forloop is defined (forwards over `s` / `rank`), the next current suffix (let it be `S'`) is just `S` with the first char lopped off, and, because of the lexicographic ordering of the suffix array (`pos`), the one to which we compare it (`pos[rank[i] - 1]`, `T'`) is just `T` with the first char lopped off, hence, the LCP length between `S'` and `T'` is just the LCP length between `S` and `T` minus one.

    	What's more, that's why this runs in linear time -- even if the string was the same character repeated a whole bunch of times, we'd still be caching the previous iteration's overlap amount in `h`.

        `height`:
      ====================
      j^* | height[j]
      --------------------
          |    LCP    |
          | length^** | suffix     (pos)
      (0) | --------------------------------
          |           | $           (8)
        1 |     0   <-|-----------------
          |           | e$          (7)
        2 |     1   <-|-----------------
          |           | ense$       (4)
        3 |     0   <-|-----------------
          |           | nonsense$   (0)
        4 |     1   <-|-----------------
          |           | nse$        (5)
        5 |     3   <-|-----------------
          |           | nsense$     (2)
        6 |     0   <-|-----------------
          |           | onsense$    (1)
        7 |     0   <-|-----------------
          |           | se$         (6)
        8 |     2   <-|-----------------
          |           | sense$      (3)

        	*	in forloop, this will be rank[i]
            **	(LCP length)[q] corresponds to LCP for suffix[q] and [q+1]
'''

xs = "nonsense$"
xpos = [8, 7, 4, 0, 5, 2, 1, 6, 3]

pdb.set_trace()
print get_height(xs, xpos)