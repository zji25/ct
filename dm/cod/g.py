from math import log, ceil
from fractions import Fraction


def meo():
    d = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
         'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
    # import sys
    # sys.stdin = open('data.txt')
    n = int(input())
    d = d[:n]
    nn = input()
    ln = len(nn)
    counters = []
    s = 0
    ds = {}
    de = {}
    for i in range(n):
        occ = nn.count(d[i])
        counters.append(occ)
        ds[d[i]] = s
        de[d[i]] = s + occ / Fraction(ln)
        s += occ / Fraction(ln)
    s, e = 0, 1
    for ch in nn:
        s1 = s + (e - s) * ds[ch]
        e1 = s + (e - s) * de[ch]
        s = s1
        e = e1

    temp = [(i / Fraction(ln)) * Fraction(log((i / Fraction(ln)), 2)) for i in counters if i > 0]
    aaaaa = ceil(-ln * sum(temp))
    length = aaaaa
    res = ceil(s * (2 ** aaaaa))
    for i in range(aaaaa - 1, -1, -1):
        s1, e1 = s * (2 ** i), e * (2 ** i)
        if ceil(s1) < e1:
            res = ceil(s1)
            length = i
        else:
            break
    res = bin(res)[2:]
    while len(res) < length:
        res = '0' + res
    print(n)
    print(' '.join([str(i) for i in counters]))
    print(res)


if __name__ == '__main__':
    meo()
