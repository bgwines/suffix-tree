import copy
import pdb

def invert(pos):
	rank = copy.copy(pos)
	for i in xrange(len(pos)):
		rank[pos[i]] = i
	return rank

def get_height(s, pos):
	rank = invert(pos)
	h = 0
	height = {}
	for i in xrange(len(pos)):
		if rank[i] > 0: # if not "$"
			k = pos[rank[i] - 1]
			while s[i+h] == s[k+h]:
				h += 1
			height[rank[i]] = h
			if h > 0:
				h -= 1
	return height

xs = "nonsense$"
xpos = [8, 7, 4, 0, 5, 2, 1, 6, 3]

pdb.set_trace()
print get_height(xs, xpos)