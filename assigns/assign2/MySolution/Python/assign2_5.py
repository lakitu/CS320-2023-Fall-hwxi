import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *


# let list_make_fwork
# (fwork: ('x0 -> unit) -> unit): 'x0 list =
# let res=ref([]) in
# let work(x0) = (res := (x0 :: !res))
# in (*let*)(fwork(work); list_reverse(!res)); ;

def list_make_fwork (fwork):
    res = []
    fwork(lambda x: res.append(x))

    outlist = fnlist_nil()
    for a in reversed(res):
        outlist = fnlist_cons(a, outlist)
    return outlist


fnlist_print(list_make_fwork(lambda y: int1_foreach(5, y)))