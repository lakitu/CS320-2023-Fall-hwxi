# (*
# Q2-8: 20 points

# Recall the 'foreach' function and the 'get_at' function.
# For instance, list_foreach(xs)(work) applies 'work' to
# each element in the given list 'xs'; list_get_at(xs)(i)
# returns the element at position 'i' in 'xs' if 'i' is a
# valid index; otherwise the Subscript exception is raised.

# Please implement in *Python* a function 'foreach_to_get_at'
# that turns a 'foreach' function into a 'get_at' function.

# (*
# Following is the type for 'foreach_to_get_at' in ocaml:
# fun foreach_to_get_at
#   (foreach: ('xs, 'x0) foreach): ('xs -> int -> 'x0) = ...
# *)

#
# *)

def foreach_to_get_at(foreach): # your implementation below
    def get_at (xs, i0):
        if i0 >= len(xs) or i0 < 0:
            raise Subscript
        info = [0, 0]
        def work(x0):
            if info[0] == i0:
                info[1] = x0
            info[0] += 1
        foreach(xs, work)
        return info[1]
    return get_at
