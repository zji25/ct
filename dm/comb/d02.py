def bb(x):
    res = ''
    while x > 0:
        res += str(x % 2)
        x //= 2
    return res[::-1]


n = int(input())
for i in range(2 ** n):
    s = bb(i ^ (i // 2))
    while len(s) < n:
        s = '0' + s
    print(s)