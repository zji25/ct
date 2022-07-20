n = int(input())
a = [int(i) for i in input().split()]
a.sort()
res = 0
while len(a) > 2:
    min1, min2 = float('inf'), float('inf')
    for i in range(len(a)):
        if a[i] < min1:
            min2 = min1
            min1 = a[i]
        elif a[i] < min2:
            min2 = a[i]
    res += (min1 + min2)
    a.append(min1 + min2)
    a.remove(min1)
    a.remove(min2)
res += sum(a)
print(res)