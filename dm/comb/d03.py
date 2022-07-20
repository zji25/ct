def ternary(x, n):
    res = ''
    while x:
        x, m = divmod(x, 3)
        res += str(m)
    while len(res) < n:
        res += '0'
    return res[::-1]


n = int(input())
for i in range(3 ** (n - 1)):
    s = ternary(i, n)
    print(s)
    for j in 1, 2:
        res = ''
        for q in s:
            res += str((int(q) + j) % 3)
        print(res)
