def mm():
    n, k = map(int, input().split())
    soc = [0] + list(map(int, input().split()))
    dp = [[0] * (n + 1) for _ in range(n + 1)]
    dp[0][1] = 1
    for i in range(1, n + 1):
        for j in range(1, k + 1):
            dp[i][j] = dp[i - 1][j - 1] + dp[i - 1][j]
    res = 0
    for i in range(1, k + 1):
        for j in range(soc[i - 1] + 1, soc[i]):
            res += dp[n - j][k - i + 1]
    print(res)


if __name__ == '__main__':
    mm()