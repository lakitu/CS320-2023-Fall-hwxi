# Please implement the following function
# that enumerates all the pairs (i, j) of natural
# numbers satisfying $i <= j$; a pair (i1, j1) must
# be enumerated ahead of another pair (i2, j2) if the
# following condition holds:
#   i1*i1*i1 + j1*j1*j1 < i2*i2*i2 + j2*j2*j2
# //
# let
# theNatPairs_cubesum(): (int * int) stream = fn () =>
# //

def cube(n):
    return n * n * n

class NatPairs_cubesum:
    def __iter__(self):
        self.i = 0
        self.j = 0
        return self

    def __next__(self):
        i = self.i
        j = self.j
        if cube(self.i)+cube(self.j) <= cube(self.i)+cube(self.j+j):
            self.j += 1
            self.i = 0
        elif cube(self.i)+cube(self.j) <= cube(self.i)+cube(self.j+1):
            self.j += 1
        print(self.i, self.j)
        return [self.i, self.j]


# NatPairsInstance = NatPairs_cubesum()
# theNatPairs_cubesum = iter(NatPairsInstance)
theNatPairs_cubesum = NatPairs_cubesum

