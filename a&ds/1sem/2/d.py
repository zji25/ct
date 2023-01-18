def aaa():
    with open('input.txt', 'r') as inp:
        n, m = map(int, inp.readline().split())
        dp = [[0]] * n
        for j in range(n):
            dp[j] = list(map(int, inp.readline().split()))
    mx = [[0] * m for _ in range(n)]
    mx[0][0] = dp[0][0]
    for j in range(1, m):
        mx[0][j] = mx[0][j - 1] + dp[0][j]
    for i in range(1, n):
        mx[i][0] = mx[i - 1][0] + dp[i][0]
        for j in range(1, m):
            if mx[i - 1][j] > mx[i][j - 1]:
                mx[i][j] = mx[i - 1][j] + dp[i][j]
            else:
                mx[i][j] = mx[i][j - 1] + dp[i][j]
    path = ''
    pos_i = n - 1
    pos_j = m - 1
    for _ in range(n + m - 2):
        if pos_i - 1 >= 0 and pos_j >= 0 and mx[pos_i - 1][pos_j] + dp[pos_i][pos_j] == mx[pos_i][pos_j]:
            path += 'D'
            pos_i -= 1
        elif pos_i >= 0 and pos_j - 1 >= 0:
            path += 'R'
            pos_j -= 1

    path = path[::-1]
    with open('output.txt', 'w') as out:
        out.write(str(mx[n - 1][m - 1]) + '\n')
        out.write(path)


if __name__ == '__main__':
    aaa()
