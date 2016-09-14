def preproc(P):
    m = len(P)
    #bad char
    bc = [-1]*256
    for i in range(m):
        bc[ord(P[i])] = i

    #good suffix
    kmp = [-1]*(m+1)
    gs = [0]*(m+1)
    i = m
    j = m+1
    kmp[i]=j
    while i > 0:
        while j <= m and P[i-1] != P[j-1]:
            if gs[j] == 0:
                gs[j] = j-i
            j = kmp[j]
        i -= 1
        j -= 1
        kmp[i]=j

    j = kmp[0]
    for i in range(m+1):
        if gs[i] == 0:
            gs[i] = j
        if i == j:
            j = kmp[j]
    return P, bc, gs

def search(pp, T):
    i = 0
    n = len(T)
    P, bc, gs = pp
    m = len(P)
    while i <= n-m:
        print(T)
        print(" "*i + P)
        j = m-1
        while j >= 0 and P[j] == T[i+j]:
            j -= 1
        if j < 0:
            return i
        i += max(gs[j+1], j-bc[ord(T[i+j])])
    return -1

assert(search(preproc("EXAMPLE"), "HERE IS A SIMPLE EXAMPLE") == 17)
assert(search(preproc("EAAMPLEAMPLE"), "HERE IS A SIMPLE EAAMPLEAMPLE") == 17)
assert(search(preproc("EXAMPLE"), "HERE IS A SIMPLE EXAMPLA") == -1)
assert(search(preproc("ABCDABD"), "ABC ABCDAB ABCDABCDABDE") == 15)
assert(search(preproc("EXAMPLS"), "HERE IEXAMPLS") == 6)
assert(search(preproc("AMPNAM"), "AAANAMPNAM") == 4)
