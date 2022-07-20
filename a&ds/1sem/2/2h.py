a = input()
b = input()
n, m = len(a), len(b)
d = [[0] * (n + 1) for i in range(m + 1)]
for j in range(1, n + 1):
    d[0][j] = d[0][j - 1] + 1
for i in range(1, m + 1):
    d[i][0] = d[i - 1][0] + 1
    for j in range(1, n + 1):
        if a[j - 1] != b[i - 1]:
            d[i][j] = min(d[i - 1][j], d[i][j - 1], d[i - 1][j - 1]) + 1
        else:
            d[i][j] = d[i - 1][j - 1]
print(d[m][n])
