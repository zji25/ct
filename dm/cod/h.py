from fractions import Fraction


def ggg():
    d = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
         'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
    n = int(input())
    d = d[:n]
    occ = [int(i) for i in input().split()]
    lg = sum(occ)
    p = input()
    code = (int(p, 2) / Fraction(2 ** len(p)))
    dp = {}
    for i in range(n):
        dp[d[i]] = Fraction(occ[i], lg)
    ds = {}
    de = {}
    s = 0
    for i in range(n):
        ds[d[i]] = s
        de[d[i]] = s + dp[d[i]]
        s += dp[d[i]]
    res = ""
    for i in range(lg):
        for j in range(n):
            if ds[d[j]] <= code < de[d[j]]:
                res += d[j]
                code = Fraction((code - ds[d[j]]), (de[d[j]] - ds[d[j]]))
                break
    return res


if __name__ == '__main__':
    print(ggg())
