c = float(input())
l, r = 0, c
for i in range(0, 10**5):
    m = (l + r) / 2
    x = m ** 2 + m ** 0.5
    if x < c:
        l = m
    else:
        r = m
print(r)
