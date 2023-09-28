import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *


# let list_make_fwork
# (fwork: ('x0 -> unit) -> unit): 'x0 list =
# let res=ref([]) in
# let work(x0) = (res := (x0 :: !res))
# in (*let*)(fwork(work); list_reverse(!res)); ;

def fnlist_make_fwork (fwork):
    res = list_make_fwork(fwork)

    outlist = fnlist_nil()
    for a in reversed(res):
        outlist = fnlist_cons(a, outlist)
    return outlist

def list_make_fwork (fwork):
    res = []
    fwork(lambda x: res.append(x))
    return res