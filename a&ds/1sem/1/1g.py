n, m = map(int, input().split())
a = [int(i) for i in input().split()]
dp = [0]
res = []
for i in range(n):
    dp.append(dp[i] + a[i])
for i in range(m):
    l, r = map(int, input().split())
    res.append(dp[r] - dp[l - 1])
for i in range(m):
    print(res[i])
