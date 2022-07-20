import sys


def fuck():
    n, s = map(int, sys.stdin.readline().split())
    a = list(map(int, sys.stdin.readline().split()))
    dp = [0] * (s + 1)
    dp[0] = 1
    for i in range(n):
        for j in range(s, a[i] - 1, -1):
            dp[j] = dp[j] or dp[j - a[i]]
    if dp[-1]:
        print('YES')
    else:
        print('NO')


if __name__ == '__main__':
    fuck()
