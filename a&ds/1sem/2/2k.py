n = int(input())
a = list(map(int, input().split()))
dp = [1] * 2002
pp = [-1] * 2002
for i in range(n):
    for j in range(i):
        if a[j] < a[i] and dp[j] + 1 > dp[i]:
            dp[i] = 1 + dp[j]
            pp[i] = j
res = dp[0]
pos = 0
for i in range(n):
    if dp[i] > res:
        res = dp[i]
        pos = i
print(res)
path = []
while pos != -1:
    path.append(pos)
    pos = pp[pos]
path.reverse()
print(' '.join(str(a[i]) for i in path))
