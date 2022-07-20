s = input()
n = len(s)
d = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']

dp = {d[i]: i for i in range(len(d))}
res = []
code = 25
p = s[0]
c = ""
for i in range(0, n):
    if i != n - 1:
        c += s[i + 1]
    if p + c in dp:
        p = p + c
    else:
        res.append(dp[p])
        code += 1
        dp[p + c] = code
        p = c
    c = ""
res.append(dp[p])
print(" ".join([str(i) for i in res]))
