from collections import deque


def mm():
    n, k = map(int, input().split())
    dp = [[0] * (n + 1) for _ in range(2 * n + 1)]
    dp[0][0] = 1
    for i in range(1, 2 * n + 1):
        dp[i][0] = dp[i - 1][1]
        for j in range(1, n):
            dp[i][j] = dp[i - 1][j - 1] + dp[i - 1][j + 1]
        dp[i][n] = dp[i - 1][n - 1]
    # for i in dp:
    #     print(i)
    res = ""
    q = 0
    stack = deque()
    for i in range(2 * n - 1, -1, -1):
        nn = 2 ** ((i - q - 1) // 2) * dp[i][q + 1] if n > q else 0
        if nn > k:
            res += '('
            q += 1
            stack.append(1)
        else:
            k -= nn
            nn = 2 ** ((i - q + 1) // 2) * dp[i][q - 1] if stack and stack[-1] and q > -1 else 0
            if nn > k:
                res += ')'
                q -= 1
                stack.pop()
            else:
                k -= nn
                nn = 2 ** ((i - q - 1) // 2) * dp[i][q + 1] if n > q else 0
                if nn > k:
                    res += '['
                    q += 1
                    stack.append(0)
                else:
                    k -= nn
                    res += ']'
                    q -= 1
                    stack.pop()
    print(res)


if __name__ == '__main__':
    mm()