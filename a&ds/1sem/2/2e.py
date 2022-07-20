dp = [[-1] * 9003 for i in range(1003)]
dp[0][0] = 1


def fuck(kk, ss):
    if kk == 0 and ss == 0:
        return 1
    if kk <= 0 or ss < 0:
        return 0
    if dp[kk][ss] != -1:
        return dp[kk][ss]
    res = 0
    for i in range(10):
        if ss - i >= 0:
            res = (res + fuck(kk - 1, ss - i)) % (10 ** 9 + 7)
    dp[kk][ss] = res
    return dp[kk][ss]


def eee():
    k, s = map(int, input().split())
    res = 0
    for i in range(10):
        if s - i >= 0:
            res = (res + fuck(k - 1, s - i)) % (10 ** 9 + 7)
    print(res)


if __name__ == '__main__':
    eee()
