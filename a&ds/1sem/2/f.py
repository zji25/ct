dp = [[0] * 53 for _ in range(53)]
dp[0][0] = 1


def fff():
    global dp
    with open('knight.in', 'r') as inp:
        n, m = map(int, inp.readline().split())
    for i in range(1, n):
        for j in range(1, m):
            dp[i][j] = dp[i - 1][j - 2] + dp[i - 2][j - 1]
    res = dp[n - 1][m - 1]
    with open('knight.out', 'w') as out:
        out.write(str(res))


if __name__ == '__main__':
    fff()
