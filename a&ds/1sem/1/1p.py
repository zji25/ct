import sys
from fractions import Fraction


def f(x, n, dp, k):
    b = [0] * n
    for i in range(n):
        b[i] = dp[i][0] - dp[i][1] * x
    b.sort(reverse=True)
    sm = 0
    for i in range(k):
        sm += b[i]
    return sm >= 0


def get(x, n, k, dp):
    c = []
    for i in range(0, n):
        c.append([dp[i][0] - dp[i][1] * x, i])
    c.sort(key=lambda y: y[0], reverse=True)
    res = []
    for i in range(k):
        res.append(c[i][1])
    res.sort()
    for i in res:
        print(i + 1)


def qqq():
    n, k = map(int, sys.stdin.readline().split())
    dp = []
    for i in range(n):
        vv, ww = map(int, sys.stdin.readline().split())
        dp.append([vv, ww])
    left = 0
    right = 1e8
    x = 0
    while right - left > 1e-7:
        x = (right + left) / Fraction(2)
        if f(x, n, dp, k):
            left = x
        else:
            right = x

    get(x, n, k, dp)


if __name__ == '__main__':
    qqq()
