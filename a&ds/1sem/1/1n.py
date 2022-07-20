n, x, y = map(int, input().split())
l, r = 0, (n - 1) * max(x, y)
while r > l + 1:
    m = (l + r) // 2
    if m // x + m // y < n - 1:
        l = m
    else:
        r = m
print(r + min(x, y))
