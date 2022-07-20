def mm():
    n, k, m = map(int, input().split())
    dp = [[0] * (n + 1) for _ in range(n)]
    res = []
    dp[0][1] = 1
    for i in range(1, n):
        for j in range(1, k + 1):
            dp[i][j] = dp[i - 1][j - 1] + dp[i - 1][j]
    num = 1
    while k > 0:
        if m < (dp[n - 1][k]):
            res.append(num)
            k -= 1
        else:
            m -= dp[n - 1][k]
        n -= 1
        num += 1
    print(" ".join([str(_) for _ in res]))


if __name__ == '__main__':
    mm()
