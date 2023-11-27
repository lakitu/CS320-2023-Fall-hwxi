import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *
from assign2_5 import *

def string_merge (str1, str2):
    def foreach(i1, i2, work):
        if i1 < len(str1) and i2 < len(str2):
            c1 = string_get_at(str1, i1)
            c2 = string_get_at(str2, i2)
            if c1 <= c2:
                work(c1)
                foreach(i1+1, i2+0, work)
            else:
                work(c2)
                foreach(i1+0, i2+1, work)
        elif i1 < len(str1):
            int1_foreach(
                len(str1)-i1, 
                lambda i: work(string_get_at(str1, i1+i))
            )
        else:
            int1_foreach(
                len(str2)-i2,
                lambda i: work(string_get_at(str2, i2+i))
            )
    
    return string_make_fwork(lambda f: foreach(0, 0, f))

    # outstr = ""
    # while i1 < len(str1) and i2 < len(str2):
    #     if str1[i1] <= str2[i2]:
    #         outstr += str1[i1]
    #         i1 += 1
    #     else:
    #         outstr += str2[i2]
    #         i2 += 1
    # while i1 < len(str1):
    #     outstr += str1[i1]
    #     i1 += 1
    # while i2 < len(str2):
    #     outstr += str2[i2]
    #     i2 += 1
    # return outstr

def string_get_at(str, i):
    return str[i]

def string_make_fwork(fwork):
    xs = list_make_fwork(fwork)
    outstr = ""
    for c in xs:
        outstr += c
    return outstr
