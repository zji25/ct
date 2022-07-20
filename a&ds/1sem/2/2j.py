def jjj():
    n, m = map(int, input().split())
    inp = []
    for i in range(n):
        inp.append(list(map(int, input().split())))
    dp = [[0] * m for _ in range(n)]
    for i in range(n):
        for j in range(m):
            if i == 0 or j == 0:
                dp[i][j] = inp[i][j]
    res = 0
    for i in range(1, n):
        for j in range(1, m):
            if inp[i][j] == 1:
                cur = min(dp[i][j - 1], dp[i - 1][j], dp[i - 1][j - 1]) + 1
                dp[i][j] = cur
                if cur > res:
                    res = cur
            else:
                dp[i][j] = 0

    print(res)


if __name__ == '__main__':
    jjj()
