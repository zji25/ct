s = input()
n = len(s)
a = [""] * n
for j in range(n):
    for i in range(n):
        a[i] = s[i] + a[i]
    a.sort()
print(a[0])
