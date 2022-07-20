s = input()
n = len(s)
a = []
for i in range(n):
    s = s[1:] + s[0]
    a.append(s)
a.sort()
res = ""
for i in range(n):
    res += a[i][-1]
print(res)
