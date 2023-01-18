dp = [[-1] * 510 for _ in range(510)]


def fff():
    global dp
    with open('knight.in', 'r') as inp:
        n, m = map(int, inp.readline().split())

    def fuckk(i, j):
        if i not in range(n) or j not in range(m):
            return 0
        if i == 0 and j == 0:
            return 1
        if dp[i][j] != -1:
            return dp[i][j]
        dp[i][j] = (fuckk(i - 2, j - 1) + fuckk(i - 2, j + 1) + fuckk(i - 1, j - 2) + fuckk(i + 1, j - 2)) % (10 ** 6 + 7)
        return dp[i][j]
    res = fuckk(n - 1, m - 1)

    with open('knight.out', 'w') as out:
        out.write(str(res))


if __name__ == '__main__':
    fff()
