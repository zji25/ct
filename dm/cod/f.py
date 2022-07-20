n = int(input())
a = [int(i) for i in input().split()]

d = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
dp = {i: d[i] for i in range(len(d))}
code = 26
res = []
p = a[0]
s = dp[p]
c = s[0]
res.append(s)
for i in range(n - 1):
    m = a[i + 1]
    if m not in dp:
        s = dp[p]
        s += c
    else:
        s = dp[m]
    res.append(s)
    c = s[0]
    dp[code] = dp[p] + c
    code += 1
    p = m
print(''.join(res))
