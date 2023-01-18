n, k = map(int, input().split())
a = [int(i) for i in input().split()]


def f():
    amount = 0
    sm = 0
    for i in range(n):
        if a[i] > m:
            return False
        sm += a[i]
        if sm > m:
            amount += 1
            sm = a[i]
    amount += 1
    if amount <= k:
        return True
    return False


l = max(a)
r = sum(a)
res = 0
while l <= r:
    m = (l + r) // 2
    if f():
        r = m - 1
        res = m
    else:
        l = m + 1
print(res)
