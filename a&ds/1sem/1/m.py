n, k = map(int, input().split())
a = [int(i) for i in input().split()]
b = [int(i) for i in input().split()]
for i in range(k):
    number = b[i]
    l, r = 0, n
    while r != l + 1:
        m = (l + r) // 2
        if number < a[m]:
            r = m
        else:
            l = m
    res = a[l]
    if l + 1 != n:
        if abs(number - res) > abs(number - a[l + 1]):
            res = a[l + 1]
    print(res)
