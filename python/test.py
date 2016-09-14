def isSuffix(suf, T):
    return suf == T[-len(suf):]

def isPrefix(pref, T):
    return pref == T[:len(pref)]

#uses the fact that bct[l][j] will not be called if l == P[j]
def badcharTable(P):
    def aux(P, l):
        res = [0 if P[0] == l else -1]
        for i in range(1, len(P)):
            res.append(i if P[i] == l else res[-1])
        return res
    return [aux(P, chr(l)) for l in range(256)]

def checkBct(P, B):
    for i in range(256):
        for j in range(len(P)):
            if not ((B[i][j] == -1 and chr(i) not in P[:j]) or (P[B[i][j]] == chr(i) and chr(i) not in P[B[i][j]+1:j])):
                return False
    return True

def kmpLike(P):
    res = [0]*len(P)
    for i in reversed(range(len(P)-1)):
        if res[i+1] > 0:
            k = res[i+1]
            while k > 0 and P[i] != P[-(k+1)]:
                k = res[-k]
            if k > 0:
                res[i] = k+1
        elif P[i] == P[-1]:
            res[i] = 1
    return res


#Not enough (ex: "BACABABA BACABA", [2, 1, 0, 1, 0, 3, 2, 1, 0, 2, 1, 0, 1, 0, 0] gives True)
def checkKmpl(P, K):
    for i in range(len(P)):
        if not (K[i] == 0 or P[i:i+K[i]] == P[-K[i]:]):
            return False
    return True

def goodsuffixTable1(P):
    K = kmpLike(P)
    done = [False]*len(P)
    res = [0]*len(P)
    for i in reversed(range(len(P))):
        if K[i-1] < K[i] and not done[K[i]]:
            done[K[i]] = True
            res[len(P)-K[i]] = i+K[i]-1
    return res

def checkGst1(P, L):
    for i in range(len(P)):
        if L[i] != 0:
            if not (isSuffix(P[i:], P[:L[i]+1]) and (i == 0 or not isSuffix(P[i-1:], P[:L[i]+1]))):
                return False
        for j in range(L[i]+1, len(P)-1):
            if (isSuffix(P[i:], P[:j+1]) and (i == 0 or not isSuffix(P[i-1:], P[:j+1]))):
                return False
    return True

#PLEXAMPLE
def goodsuffixTable2(P):
    K = kmpLike(P[::-1])[::-1]
    tmp = [K[-1]]
    while tmp[-1] > 0:
        tmp.append(K[tmp[-1]-1])
    tmp = tmp[::-1]
    res = []
    for i in range(len(P)):
        while len(P)-i <= tmp[-1]:
            tmp.pop()
        res.append(tmp[-1])
    return res

def checkGst2(P, H):
    for i in range(len(P)):
        if H[i] > len(P)-i+1:
            print("bla", H[i], len(P), i, P[i:])
            return False
        if H[i] != 0:
            if not isPrefix(P[-H[i]:], P):
                print("blu")
                return False
        for j in range(H[i]+1, len(P)-i):
            if isPrefix(P[-j:], P):
                print("blo", i, j, P[-j:], P, len(P))
                return False
    return True


def search(P, T):
    n = len(P)
    m = len(T)
    B = badcharTable(P)
    assert(checkBct(P, B))
    L = goodsuffixTable1(P)
    assert(checkGst1(P, L))
    H = goodsuffixTable2(P)
    assert(checkGst2(P, H))
    i = n-1
    while i < m:
        print(T)
        print(" "*(i-n+1) + P)
        print()
        j = n-1
        k = i
        while P[j] == T[k] and j >= 0:
            j -= 1
            k -= 1
        if j < 0: #found pattern
            print()
            print()
            return i-n+1
        bcoff = j-B[ord(T[k])][j]
        gsoff = 0
        if j < n-1 and L[j+1] != 0:
            gsoff = n-L[j+1]-1
        elif H[j] != 0:
            gsoff = n-H[j]
        i += max(bcoff, gsoff)
    print()
    print()
    return -1 #found nothing

assert(search("EXAMPLE", "HERE IS A SIMPLE EXAMPLE") == 17)
assert(search("EAAMPLEAMPLE", "HERE IS A SIMPLE EAAMPLEAMPLE") == 17)
assert(search("EXAMPLE", "HERE IS A SIMPLE EXAMPLA") == -1)
assert(search("ABCDABD", "ABC ABCDAB ABCDABCDABDE") == 15)
assert(search("EXAMPLS", "HERE IEXAMPLS") == 6)
assert(search("AMPNAM", "AAANAMPNAM") == 4)
