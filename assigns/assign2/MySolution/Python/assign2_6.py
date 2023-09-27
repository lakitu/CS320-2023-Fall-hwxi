

def string_merge (str1, str2):
    i1 = i2 = 0
    outstr = ""
    while i1 < len(str1) and i2 < len(str2):
        if str1[i1] <= str2[i2]:
            outstr += str1[i1]
            i1 += 1
        else:
            outstr += str2[i2]
            i2 += 1
    while i1 < len(str1):
        outstr += str1[i1]
        i1 += 1
    while i2 < len(str2):
        outstr += str2[i2]
        i2 += 1
    return outstr

