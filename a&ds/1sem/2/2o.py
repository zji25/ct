import sys


def fuck():
    n, s = map(int, sys.stdin.readline().split())
    a = list(map(int, sys.stdin.readline().split()))
    dp = [10**8] * (s + 1)
    dp[0] = 0
    for i in range(n):
        for j in range(s, a[i] - 1, -1):
            dp[j] = min(dp[j], dp[j - a[i]] + 1)
    if dp[-1] == 10**8:
        print(0)
    else:
        print(dp[-1])


if __name__ == '__main__':
    fuck()
