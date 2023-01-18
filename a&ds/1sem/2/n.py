import sys


def fuck():
    n, s = map(int, sys.stdin.readline().split())
    a = list(map(int, sys.stdin.readline().split()))
    dp = [0] * (s + 1)
    dp[0] = 1
    ddp = [-1 for _ in range(s + 1)]
    for i in range(n):
        for j in range(s, a[i] - 1, -1):
            if dp[j - a[i]] != 0 and dp[j] != 1:
                dp[j] = dp[j] or dp[j - a[i]]
                ddp[j] = a[i]
    sss = -1
    for i in range(len(dp) - 1, -1, -1):
        if dp[i] != 0:
            print(i)
            sss = i
            break
    res = []
    while sss > 0:
        res.append(ddp[sss])
        sss -= ddp[sss]
    print(len(res))
    print(' '.join([str(i) for i in res]))


if __name__ == '__main__':
    fuck()
