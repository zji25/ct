from collections import deque
n = int(input())
d = deque()
ld = 0
q = deque()
lq = 0
res = []
for i in range(n):
    string = input()
    if string[0] == '+':
        x = int(string[2:])
        while ld > 0 and d[-1] > x:
            d.pop()
            ld -= 1
        d.append(x)
        q.append(x)
        ld += 1
        lq += 1
    else:
        if d[0] == q[0]:
            d.popleft()
            ld -= 1
        q.popleft()
        lq -= 1
    if lq > 0:
        res.append(d[0])
    else:
        res.append(-1)

for i in range(n):
    print(res[i])
