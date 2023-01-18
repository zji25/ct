def iii():
    n = int(input())
    a = [0] * n
    if n == 0:
        print(0)
        print(0, 0)
    else:
        for i in range(n):
            a[i] = int(input())
        dp = [[10 ** 9] * (n + 2) for _ in range(n)]
        pp = [[-1] * (n + 2) for _ in range(n)]
        if a[0] > 100:
            dp[0][2] = a[0]
        else:
            dp[0][1] = a[0]
        for i in range(1, n):
            for j in range(1, i + 2):
                if a[i] > 100:
                    if j == i + 1:
                        dp[i][j + 1] = dp[i - 1][j] + a[i]
                        pp[i][j + 1] = j

                    if dp[i - 1][j - 1] + a[i] <= dp[i - 1][j + 1]:
                        dp[i][j] = dp[i - 1][j - 1] + a[i]
                        pp[i][j] = j - 1
                    else:
                        dp[i][j] = dp[i - 1][j + 1]
                        pp[i][j] = j + 1
                else:
                    if dp[i - 1][j] + a[i] <= dp[i - 1][j + 1]:
                        dp[i][j] = dp[i - 1][j] + a[i]
                        pp[i][j] = j
                    else:
                        dp[i][j] = dp[i - 1][j + 1]
                        pp[i][j] = j + 1
        mnn = dp[n - 1][1]
        k1 = 1
        for i in range(1, n + 2):
            if mnn >= dp[n - 1][i]:
                mnn = dp[n - 1][i]
                k1 = i
        print(mnn)
        ind = k1
        spent = []
        for i in range(n - 1, 0, -1):
            if pp[i][ind] > ind:
                spent.append(i + 1)
            ind = pp[i][ind]
        print(k1 - 1, len(spent))
        if len(spent) > 0:
            print('\n'.join([str(q) for q in reversed(spent)]))


if __name__ == '__main__':
    iii()
