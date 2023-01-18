n = int(input())
a = [int(i) for i in input().split()]
res = []
counters = [0 for i in range(101)]

for i in range(n):
    counters[a[i]] += 1

for i in range(len(counters)):
    for j in range(counters[i]):
        res.append(i)

print(' '.join([str(i) for i in res]))
