n = int(input())
a = [int(i) for i in input().split()]
a.sort()
k = int(input())
res = []
for i in range(k):
    l, r = map(int, input().split())
    l1, r1 = -1, n
    while l1 + 1 < r1:
        m1 = (l1 + r1) // 2
        if a[m1] <= r:
            l1 = m1
        else:
            r1 = m1
    l2, r2 = -1, n
    while l2 + 1 < r2:
        m2 = (l2 + r2) // 2
        if a[m2] < l:
            l2 = m2
        else:
            r2 = m2
    res.append(l1 - l2)
print(' '.join([str(i) for i in res]))
