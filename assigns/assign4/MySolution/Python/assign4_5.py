# let
# string_fset_at
# (cs: string)(i0: int)(c0: char) =
# string_tabulate
# (string_length(cs))
# (
# fun i ->
# if i <> i0 then string_get_at(cs)(i) else c0)
# ;;

def string_tabulate(n, work):
    outstr = ""
    for i in range(n):
        outstr += work(i)
    return outstr

def string_length(str):
    return len(str)

def string_get_at(str, i):
    return str[i]

def string_fset_at(cs, i0, c0):
    def work(i):
        if i == i0:
            return c0
        else:
            return string_get_at(cs, i)
    return string_tabulate(string_length(cs), work)

# let
# alphabet =
# string_tabulate(26)(fun i -> chr(ord('a') + i));;

def makeAlphabet():
    def work(i):
        return chr(ord('a')+i)
    return string_tabulate(26, work)

alphabet = makeAlphabet()

# let list_of_buddies(word: string): string list =
#     let n0 = string_length(word) in
#     list_make_fwork(fun work ->
#         int1_foreach(n0)(fun i0 ->
#             let c0 = string_get_at(word)(i0) in
#             string_foreach(alphabet)(fun c1 -> 
#                 if c1 <> c0 then work(string_fset_at(word)(i0)(c1))
#             )
#         )
#     )
# ;;

def list_of_buddies(word):
    n0 = string_length(word)
    outArr = []
    def work(elem):
        outArr.append(elem)
    for i0 in range(n0):
        c0 = string_get_at(word, i0)
        for c1 in alphabet:
            if c1 != c0:
                work(string_fset_at(word, i0, c1))

    return outArr
