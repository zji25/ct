def mm():
    s = input()
    k = 0
    length = len(s)
    dp = [[0 for _ in range(length + 1)] for q in range(length + 1)]
    dp[0][0] = 1
    for i in range(1, length + 1):
        for j in range(0, length + 1):
            if j > 0:
                dp[i][j] += dp[i - 1][j - 1]
            if j + 1 <= length:
                dp[i][j] += dp[i - 1][j + 1]
    depth = 0
    for i in range(length):
        if s[i] == "(":
            depth += 1
        else:
            k += dp[length - i - 1][depth + 1]
            depth -= 1
    print(k)


if __name__ == '__main__':
    mm()
